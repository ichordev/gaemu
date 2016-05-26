module checker is aliced;

import std.stdio;

import gmlparser;
import ungmk;

import gmlparser.anal;


// ////////////////////////////////////////////////////////////////////////// //
NodeFunc[] loadScript (string filename, bool warnings=true) {
  import std.algorithm : startsWith;
  import std.file : readText;
  import std.path : baseName, extension;
  import std.stdio;
  import std.string : replace;

  auto s = readText(filename);

  NodeFunc[] res;
  auto parser = new Parser(s, filename);
  parser.warnings = warnings;
  bool asGmx = (filename.extension == ".gmx");
  try {
    if (asGmx) {
      while (!parser.lex.empty) res ~= parser.parseFunction();
    } else {
      string scname = filename.baseName(".gml");
      res ~= parser.parseFunctionBody(scname);
    }
    assert(parser.lex.empty);
  } catch (ErrorAt e) {
    import std.stdio;
    writeln("ERROR at ", e.loc, ": ", e.msg);
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    assert(0);
  } catch (Exception e) {
    import std.stdio;
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    assert(0);
  }
  return res;
}


// ////////////////////////////////////////////////////////////////////////// //
NodeFunc parseScript (string code, string scname, bool warnings=true) {
  auto parser = new Parser(code, scname);
  parser.warnings = warnings;
  try {
    return parser.parseFunctionBody(scname);
  } catch (ErrorAt e) {
    import std.stdio;
    writeln("ERROR at ", e.loc, ": ", e.msg);
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    writeln(code);
    throw e;
  } catch (Exception e) {
    import std.stdio;
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    throw e;
  }
  assert(0);
}


// ////////////////////////////////////////////////////////////////////////// //
NodeFunc[] gmkLoadScripts (Gmk gmk, bool doScripts, bool doActions) {
  NodeFunc[] funcs;

  import std.conv : to;

  void setupObject (GMObject obj, GMObject oparent) {
    string parent = (oparent !is null ? oparent.name : null);

    void parseECode (ref string evcode, string evname) {
      import iv.strex;
      import std.string : replace;
      scope(exit) evcode = null;
      evcode = evcode.replace("\r\n", "\n").replace("\r", "\n").outdentAll;
      //while (evcode.length && evcode[0] <= ' ') evcode = evcode[1..$];
      while (evcode.length && evcode[$-1] <= ' ') evcode = evcode[0..$-1];
      if (evcode.length) {
        auto fn = evcode.parseScript(obj.name~":"~evname);
        if (!isEmpty(fn)) {
          if (hasReturn(fn)) throw new Exception("event '"~evname~"' for object '"~obj.name~"' contains `return`");
          funcs ~= fn;
        }
      }
    }

    void createEvent (GMEvent.Type evtype) {
      import std.conv : to;
      string evcode;
      foreach (immutable evidx, auto ev; obj.events[evtype]) {
        foreach (immutable aidx, auto act; ev.actions) {
          if (act.type == act.Type.Nothing) continue; // comment
          if (act.kind == act.Kind.act_normal) {
            // normal action
            if (act.type == act.Type.Function) {
              if (act.funcname == "action_inherited") {
                assert(parent.length);
                evcode ~= "_action_inherited(\""~to!string(evtype)~"\", \""~parent~"\");\n";
                continue;
              }
              if (act.funcname == "action_kill_object") {
                evcode ~= "_action_kill_object();\n";
                continue;
              }
              if (act.funcname == "action_execute_script") {
                import std.conv : to;
                if (act.argused < 1 || act.argtypes[0] != act.ArgType.t_script) assert(0, "invalid action function arguments: '"~act.funcname~"' for object '"~obj.name~"'");
                string s = gmk.scriptByNum(to!int(act.argvals[0])).name~"(";
                foreach (immutable idx; 1..act.argused) {
                  if (act.argtypes[idx] != act.ArgType.t_expr) assert(0, "invalid action type for execscript: "~to!string(act.argtypes[idx])~" for object '"~obj.name~"'");
                  if (idx != 1) s ~= ", ";
                  s ~= act.argvals[idx];
                }
                s ~= "); // action_execute_script\n";
                evcode ~= s;
                continue;
              }
              assert(0, "invalid action function: '"~act.funcname~"' for object '"~obj.name~"'");
            }
            assert(0, "invalid normal action type");
          }
          if (act.kind == act.Kind.act_code) {
            // script
            if (act.type == act.Type.Code) {
              if (act.argused < 1 || act.argtypes[0] != act.ArgType.t_string) {
                import std.conv : to;
                assert(0, "invalid action code arguments for '"~obj.name~"': used="~to!string(act.argused)~"; kinds="~to!string(act.argtypes));
              }
              import std.string : format;
              evcode ~= act.argvals[0];
              while (evcode.length && evcode[$-1] <= ' ') evcode = evcode[0..$-1];
              if (evcode.length > 0) evcode ~= "\n";
              continue;
            }
            assert(0, "invalid code action type: "~to!string(act.type));
          }
          if (act.kind == act.Kind.act_var) {
            // variable assignment
            if (act.argused != 2 || act.argtypes[0] != act.ArgType.t_string || act.argtypes[1] != act.ArgType.t_expr) {
              assert(0, "invalid action code arguments for '"~obj.name~"': used="~to!string(act.argused)~"; kinds="~to!string(act.argtypes));
            }
            evcode ~= act.argvals[0]~" = "~act.argvals[1]~"; // act_var";
            continue;
          }
          {
            assert(0, "FUUUCK: "~to!string(act.kind));
          }
        }
        string baseevname = to!string(evtype);
        if (evidx > 0) baseevname ~= to!string(evidx);
        if (evtype == GMEvent.Type.ev_alarm) {
          parseECode(evcode, "ev_alarm:"~to!string(ev.id));
          //{ import std.stdio; writeln("alarm #", evidx, " for '", obj.name, "'"); }
        } else if (evtype == GMEvent.Type.ev_step) {
          if (ev.id == 0) {
            // normal
            parseECode(evcode, baseevname);
          } else if (ev.id == 1) {
            // begin
            parseECode(evcode, to!string(evtype)~":begin");
          } else if (ev.id == 2) {
            // end
            parseECode(evcode, to!string(evtype)~":end");
          } else {
            assert(0);
          }
        } else if (evtype == GMEvent.Type.ev_keypress || evtype == GMEvent.Type.ev_keyrelease || evtype == GMEvent.Type.ev_keyboard) {
          if (auto keyName = cast(uint)ev.id in evKeyNames) {
            import std.string : replace;
            string kn = (*keyName).replace(" ", "_");
            parseECode(evcode, to!string(evtype)~":"~kn);
          } else {
            parseECode(evcode, to!string(evtype)~":vcode_"~to!string(ev.id));
          }
        } else if (evtype == GMEvent.Type.ev_mouse) {
          if (auto msName = cast(uint)ev.id in evMouseNames) {
            import std.string : replace;
            string kn = (*msName).replace(" ", "_");
            parseECode(evcode, to!string(evtype)~":"~kn);
          } else {
            parseECode(evcode, to!string(evtype)~":mcode_"~to!string(ev.id));
          }
        } else if (evtype == GMEvent.Type.ev_collision) {
          auto co = gmk.objByNum(ev.id);
          if (co is null) assert(0, "no collision object for 'ev_collision' for '"~obj.name~"'");
          parseECode(evcode, to!string(evtype)~":"~co.name);
        } else if (evtype == GMEvent.Type.ev_other) {
          auto nmp = cast(uint)ev.id in evOtherNames;
          if (nmp is null) assert(0, "unknown event id "~to!string(ev.id)~" for 'ev_other' for '"~obj.name~"'");
          import std.string : replace;
          string nm = (*nmp).replace(" ", "_");
          parseECode(evcode, to!string(evtype)~":"~nm);
        } else if (evtype == GMEvent.Type.ev_draw || evtype == GMEvent.Type.ev_destroy || evtype == GMEvent.Type.ev_create) {
          parseECode(evcode, baseevname);
        } else {
          /*if (evidx > 0)*/ {
            { import std.stdio; writeln("fuck! ", evtype, " #", evidx, " for '", obj.name, "'"); }
            assert(0);
          }
          parseECode(evcode, to!string(evtype)~to!string(evidx)~"_"~to!string(ev.id));
        }
      }
    }

    foreach (immutable evtype; 0..GMEvent.Type.max+1) {
      if (evtype != GMEvent.Type.ev_create) {
        if (obj.name == "oGamepad") continue;
      }
      createEvent(cast(GMEvent.Type)evtype);
    }
  }

  void processChildren (string parent) {
    auto po = gmk.objByName(parent);
    if (po is null) assert(0, "wtf?! "~parent);
    gmk.forEachObject((o) {
      if (o.parentobjidx == po.idx) {
        if (doActions) setupObject(o, po);
        processChildren(o.name);
      }
      return false;
    });
  }

  // objects
  gmk.forEachObject((o) {
    if (o.parentobjidx < 0) {
      if (doActions) setupObject(o, null);
      processChildren(o.name);
    }
    return false;
  });

  // scripts
  gmk.forEachScript((sc) {
    assert(sc.name.length);
    if (doScripts) {
      NodeFunc fn = sc.code.parseScript(sc.name);
      assert(fn.ebody !is null);
      funcs ~= fn;
    }
    return false;
  });

  return funcs;
}


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  NodeFunc[] funcs;

  bool dumpFileNames = false;
  bool styleWarnings = false;
  bool doScripts = true, doActions = true;
  bool warnVars = false;
  bool warnWith = false;

  bool nomore = false;
  string[] scargs;
  foreach (string fname; args[1..$]) {
    import std.file;
    import std.path;
    if (nomore) {
      scargs ~= fname;
    } else {
      if (fname.length == 0) continue;
      if (fname == "--") { nomore = true; continue; }
      if (fname == "-d") { dumpFileNames = true; continue; }
      if (fname == "-w") { styleWarnings = true; continue; }
      if (fname == "-S") { doScripts = false; continue; }
      if (fname == "-A") { doActions = false; continue; }
      if (fname == "-wall") {
        warnVars = true;
        warnWith = true;
        continue;
      }
      if (fname == "-wvardecl") { warnVars = true; continue; }
      if (fname == "-wwith") { warnWith = true; continue; }
      if (fname[0] == '@') {
        if (fname.length < 2) assert(0, "gmk file?");
        auto gmk = new Gmk(fname[1..$]);
        funcs ~= gmkLoadScripts(gmk, doScripts:doScripts, doActions:doActions);
        continue;
      }
      if (isDir(fname)) {
        foreach (auto de; dirEntries(fname, "*.gm[lx]", SpanMode.breadth)) {
          bool doit = true;
          foreach (auto pt; pathSplitter(de.dirName)) {
            if (pt.length && pt[0] == '_') { doit = false; break; }
          }
          if (doit) {
            if (dumpFileNames) { import std.stdio; writeln("loading '", de.name, "'..."); }
            funcs ~= loadScript(de.name, true);
          }
        }
      } else {
        if (dumpFileNames) { import std.stdio; writeln("loading '", fname, "'..."); }
        funcs ~= loadScript(fname, true);
      }
    }
  }

  if (funcs.length > 0) {
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) {
      if (warnVars) {analVars(fn);
        bool skip = false;
        foreach (string name; [
          "scrCreateTile",
          "scrCreateTileObj",
          "scrLoadCheckpoint",
          "scrLoadLevel",
          "scrMakeItem",
          "scrSetCursorTile",
          "scrSetVendingItem",
          "scrTestLevel",
          "scrMagicSigns",
        ]) {
          if (fn.name == name) { skip = true; break; }
        }
        if (!skip && fn.name.length > 3 && fn.name[0..3] == "sui") skip = true;
        if (!skip) analVars(fn);
      }
      if (warnWith) analWith(fn);
    }
  }
}
