module ung is aliced;

import std.stdio;

import iv.vfs;
import iv.strex;

import gaem.ungmk;

import arsd.png;


string undir = "_src";


void exportActions (Gmk gmk, File fo, GMObject o, usize evidx, GMEvent ev, string dir) {
  import std.conv : to;
  import std.file;
  import std.path;
  import std.string : format, replace;
  if (ev is null) return;

  string destarg (int idx) {
    if (idx == -1) return "self";
    if (idx == -2) return "other";
    return gmk.objByNum(idx).name;
  }

  foreach (immutable aidx, auto act; ev.actions) {
    if (aidx != 0) fo.writeln("\n");
    fo.writeln("============ new_action ", act.kind, " for ", destarg(act.applyobj), " ============");
    if (act.type == act.Type.Nothing) {
      // comment
      fo.writeln("funcname=", act.funcname);
      fo.writeln("codename=", act.codename);
      continue;
    }
    switch (act.kind) {
      case GMAction.Kind.act_normal:
        if (act.type == act.Type.Function) {
          fo.write(act.funcname);
          foreach (immutable ai; 0..act.argused) {
            switch (act.argtypes[ai]) {
              case GMAction.ArgType.t_expr: fo.write(" e", act.argvals[ai].quote); break;
              case GMAction.ArgType.t_string: fo.write(" ", act.argvals[ai].quote); break;
              case GMAction.ArgType.t_boolean: fo.write(" ", act.argvals[ai]); break;
              case GMAction.ArgType.t_sprite: fo.write(" ", gmk.sprByNum(to!int(act.argvals[0])).name); break;
              case GMAction.ArgType.t_background: fo.write(" ", gmk.bgByNum(to!int(act.argvals[0])).name); break;
              case GMAction.ArgType.t_script: fo.write(" ", gmk.scriptByNum(to!int(act.argvals[0])).name); break;
              case GMAction.ArgType.t_object: fo.write(" ", gmk.objByNum(to!int(act.argvals[0])).name); break;
              case GMAction.ArgType.t_room: fo.write(" ", gmk.roomByNum(to!int(act.argvals[0])).name); break;
              default: assert(0, "bad argument type:"~to!string(act.argtypes[ai]));
            }
          }
          fo.writeln;
          break;
        }
        assert(0, "wtf?!");
      case GMAction.Kind.act_code:
        if (act.argused != 1 || act.argtypes[0] != act.ArgType.t_string) assert(0, "wtf?! "~to!string(act.argused)~" : "~to!string(act.argtypes[0]));
        string code = act.argvals[0].outdentAll;
        while (code.length && code[$-1] <= ' ') code = code[0..$-1];
        bool skip = true;
        foreach (string s; code.byLine) {
          //while (s.length && s[0] <= ' ') s = s[1..$];
          while (s.length && s[$-1] <= ' ') s = s[0..$-1];
          if (s.length == 0 && skip) continue;
          skip = false;
          fo.writeln(s);
        }
        break;
      case GMAction.Kind.act_var: // variable assignment
       if (act.argused != 2 || act.argtypes[0] != act.ArgType.t_string || act.argtypes[1] != act.ArgType.t_expr) {
          assert(0, "invalid action code arguments for '"~o.name~"': used="~to!string(act.argused)~"; kinds="~to!string(act.argtypes));
        }
        fo.writeln(act.argvals[0]," = ", act.argvals[1]);
        break;
      default: assert(0, "invalid action type: '"~to!string(cast(GMAction.Kind)act.kind)~"'");
    }
  }
}


void exportObject (Gmk gmk, GMObject o, string dir) {
  import std.conv : to;
  import std.file;
  import std.path;
  import std.string : format, replace, toLower;

  try { mkdirRecurse(dir); } catch (Exception) {}
  {
    auto fo = File(buildPath(dir, "object.ini"), "w");
    //fo.writeln("name=", o.name);
    if (o.parentobjidx >= 0) fo.writeln("parent=", gmk.objByNum(o.parentobjidx).name);
    if (o.spridx >= 0) fo.writeln("sprite=", gmk.sprByNum(o.spridx).name);
    if (o.maskspridx >= 0) fo.writeln("mask=", gmk.sprByNum(o.maskspridx).name);
    fo.writeln("solid=", o.solid);
    fo.writeln("persistent=", o.persistent);
    fo.writeln("visible=", o.visible);
    fo.writeln("depth=", o.depth);
  }
  foreach (immutable etidx, auto evl; o.events[]) {
    foreach (immutable eidx, auto ev; evl) {
      // we HAVE to export empty events, as they prevents inherited events to run
      string fname;
      switch (etidx) {
        case GMEvent.Type.ev_create:
          assert(ev.id == 0);
          fname = buildPath(dir, "%s.gma".format(cast(GMEvent.Type)etidx));
          break;
        case GMEvent.Type.ev_destroy:
          assert(ev.id == 0);
          fname = buildPath(dir, "%s.gma".format(cast(GMEvent.Type)etidx));
          break;
        case GMEvent.Type.ev_alarm:
          assert(ev.id < 12);
          fname = buildPath(dir, "%s_%02s.gma".format(cast(GMEvent.Type)etidx, ev.id));
          break;
        case GMEvent.Type.ev_step:
               if (ev.id == 0) fname = buildPath(dir, "%s_normal.gma".format(cast(GMEvent.Type)etidx));
          else if (ev.id == 1) fname = buildPath(dir, "%s_begin.gma".format(cast(GMEvent.Type)etidx));
          else if (ev.id == 2) fname = buildPath(dir, "%s_end.gma".format(cast(GMEvent.Type)etidx));
          else assert(0);
          break;
        case GMEvent.Type.ev_collision:
          fname = buildPath(dir, "%s_with_%s.gma".format(cast(GMEvent.Type)etidx, gmk.objByNum(ev.id).name));
          break;
        case GMEvent.Type.ev_keyboard:
        case GMEvent.Type.ev_keypress:
        case GMEvent.Type.ev_keyrelease:
          if (auto kn = ev.id in evKeyNames) fname = buildPath(dir, "%s_%s.gma".format(cast(GMEvent.Type)etidx, (*kn).replace(" ", "_").toLower));
          else assert(0, "wtf key "~to!string(ev.id));
          break;
        case GMEvent.Type.ev_mouse:
          if (auto mn = ev.id in evMouseNames) fname = buildPath(dir, "%s_%s.gma".format(cast(GMEvent.Type)etidx, (*mn).replace(" ", "_").toLower));
          else assert(0, "wtf mouse "~to!string(ev.id));
          break;
        case GMEvent.Type.ev_other:
          if (auto on = ev.id in evOtherNames) fname = buildPath(dir, "%s_%s.gma".format(cast(GMEvent.Type)etidx, (*on).replace(" ", "_").toLower));
          else assert(0, "wtf other "~to!string(ev.id));
          break;
        case GMEvent.Type.ev_draw:
          assert(ev.id == 0);
          fname = buildPath(dir, "%s.gma".format(cast(GMEvent.Type)etidx));
          break;
        case GMEvent.Type.ev_trigger:
          assert(0, "no triggers yet");
        default: assert(0);
      }
      exportActions(gmk, File(fname, "w"), o, eidx, ev, dir);
    }
  }
}


void exportSprite (Gmk gmk, GMSprite o, string dir) {
  import std.conv : to;
  import std.file;
  import std.path;
  import std.string : format, replace;

  try { mkdirRecurse(dir); } catch (Exception) {}
  {
    auto fo = File(buildPath(dir, "sprite.ini"), "w");
    //fo.writeln("name=", o.name);
    if (o.xofs || o.yofs) fo.writeln("ofs=", o.xofs, " ", o.yofs);
    if (o.shape != GMSprite.Shape.Rectangle) fo.writeln("shape=", o.shape);
    if (o.alphaTolerance != 0) fo.writeln("alphaTolerance=", o.alphaTolerance);
    fo.writeln("separateCollisionMasks=", o.separateCollisionMasks);
    if (o.bboxType != GMSprite.BBoxType.Automatic) fo.writeln("bboxType=", o.bboxType);
    fo.writeln("bbox=", o.bbleft, " ", o.bbtop, " ", o.bbright, " ", o.bbbottom);
  }

  // export images
  foreach (immutable iidx, auto img; o.images) {
    writePng(buildPath(dir, "image_%03s.png".format(iidx)), img);
  }
}


void exportBg (Gmk gmk, GMBackground o, string dir) {
  import std.conv : to;
  import std.file;
  import std.path;
  import std.string : format, replace;

  try { mkdirRecurse(dir); } catch (Exception) {}
  {
    auto fo = File(buildPath(dir, "sprite.ini"), "w");
    //fo.writeln("name=", o.name);
    if (o.tileset) {
      fo.writeln("tileset=true");
      // default: 16x16
      fo.writeln("tileWidth=", o.tileWidth);
      fo.writeln("tileHeight=", o.tileHeight);
    }
    if (o.xofs || o.yofs) fo.writeln("ofs=", o.xofs, " ", o.yofs);
    if (o.xsep || o.ysep) fo.writeln("sep=", o.xsep, " ", o.ysep);
  }

  // export image
  writePng(buildPath(dir, "image.png"), o.image);
}


void main () {
  import std.path : buildPath;
  auto gmk = new Gmk("/home/ketmar/back/D/prj/spel/spelunky_collection/yasmk8/yasm_k8.gmk");

  gmk.forEachObject((o) {
    auto path = gmk.tree.pathForName(GMResTree.Node.Type.Object, o.name);
    if (path.length) {
      writeln(o.name, " : ", path);
      gmk.exportObject(o, buildPath(undir, path));
    } else {
      assert(0, "object '"~o.name~"' has no path!");
    }
    return false;
  });

  gmk.forEachSprite((o) {
    auto path = gmk.tree.pathForName(GMResTree.Node.Type.Sprite, o.name);
    if (path.length) {
      writeln(o.name, " : ", path);
      gmk.exportSprite(o, buildPath(undir, path));
    } else {
      assert(0, "sprite '"~o.name~"' has no path!");
    }
    return false;
  });

  gmk.forEachBg((o) {
    auto path = gmk.tree.pathForName(GMResTree.Node.Type.Background, o.name);
    if (path.length) {
      writeln(o.name, " : ", path);
      gmk.exportBg(o, buildPath(undir, path));
    } else {
      assert(0, "sprite '"~o.name~"' has no path!");
    }
    return false;
  });
}
