module checker is aliced;

import std.stdio;

import gmlparser;
import gmlparser.anal;

import ungmk;
import loader;


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  NodeFunc[] funcs;

  bool dumpFileNames = false;
  bool styleWarnings = false;
  bool doScripts = true, doActions = true;
  bool warnVars = false;
  bool warnWith = false;
  bool warnAss = false;

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
        warnAss = true;
        continue;
      }
      if (fname == "-wvardecl") { warnVars = true; continue; }
      if (fname == "-wwith") { warnWith = true; continue; }
      if (fname == "-wass") { warnAss = true; continue; }
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
      if (warnAss) analAss(fn);
      analUninited(fn);
    }
  }
}
