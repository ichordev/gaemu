/* GML parser
 * coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module gmlparser.anal is aliced;

import gmlparser.lexer;
import gmlparser.tokens;
import gmlparser.utils;
import gmlparser.ast;
import gmlparser.parser;
import gmlparser.astools;


// ////////////////////////////////////////////////////////////////////////// //
void analVars (Node nn, Parser pp=null) {
  import std.stdio;

  static struct VarInfo {
    enum Type { Local, Global, Field }
    Type type;
    string name;
    string obj; // can be `null`
  }

  VarInfo[string] locals, globals, fields;

  static immutable string[$] knownFields = [
    "id",
    "object_index",
    "image_alpha",
    "image_angle",
    "image_blend",
    "image_xscale",
    "image_yscale",
    "sprite_width",
    "sprite_height",
    "event_type",
    "event_number",
    "ashShotgun",
    "held",
    "myGrav",
    "colLeft",
    "colRight",
    "colBot",
    "colTop",
    "x",
    "y",
    "inWeb",
    "hp",
    "countsAsKill",
    "ateShopkeeper",
    "facing",
    "sprite_index",
    "counter",
    "bounced",
    "holdItem",
    "pickupItemType",
    "image_speed",
    "image_index",
    "ateRock",
    "bone",
    "status",
    "background_index",
    "background_blend",
    "alarm",
    "room_height",
    "type",
    "argument",
    "argument0",
    "argument1",
    "argument2",
    "argument3",
    "argument4",
    "argument5",
    "argument6",
    "argument7",
    "argument8",
    "argument9",
    "argument10",
    "argument11",
    "argument12",
    "argument13",
    "argument14",
    "argument15",
    "cleanDeath",
    "room_width",
    "invincible",
    "shopType",
    "cost",
    "shopDesc",
    "buyMessage",
    "spirte_index",
    "depth",
    "visible",
    "persistent",
    // for level generator
    //"xpos", "ypos",
    // lava
    "spurt",
    "spurtTime",
    "spurtCounter",
    // water
    "upWater",
    "down",
    "left",
    "right",
    "up",
    //
    "treasure",
    "monkey",
    "dying",
    "sprung",
    "bloodless",
    "nudget",
    "state",
    "pressTimer",
    "pressLimit",
    "stunned",
    "stunTimer",
    "fallTimer",
    "paused",
    "screen",
    "helpPage",
    "nudged",
    "forSale",
    "forVending",
    "heavy",
    "trigger",
    "stuck",
    "sticky",
    "contents",
    "holds",
    "shakeToggle", //!!!
    "active",
    "madeOffer",
    "bowArmed",
    "bowStrength",
    "firing",
    "holdArrow",
    "bombArrowCounter",
    "whoaTimerMax",
    "whoaTimer",
    "whipping",
    "cantJump",
    "firingPistolMax",
    "firingShotgunMax",
    "lb", "tb", "rb", "bb",
    "collisionBoundsOffsetLeftX",
    "collisionBoundsOffsetTopY",
    "collisionBoundsOffsetRightX",
    "collisionBoundsOffsetBottomY",
    "room",
    //
    "altarL",
    "altarR",
    "background_x",
    "background_y",
    "blockArray",
    "canFly",
    "canFlyJump",
    "canPause",
    "canRun",
    "climbAcc",
    "climbAnimSpeed",
    "climbSndSpeed",
    "colIceBot",
    "colLadder",
    "colPlat",
    "colPlatBot",
    "colPointLadder",
    "colSolidLeft",
    "colSolidRight",
    "colWaterTop",
    "damselDropped",
    "darkSurf",
    "darkSurf544",
    "darkSurf608",
    "darkSurf672",
    "departLadderXVel",
    "departLadderYVel",
    "drawStats",
    "drawStatus",
    "dropSelect",
    "dropVal",
    "enemyArray",
    "flyAcc2Timer",
    "flyAccTimer",
    "flyJumpWasPressed",
    "flyJumpsTimer",
    "flyMaxJumps",
    "flySpeed",
    "flySpeedTimer",
    "frictionClimbingX",
    "frictionClimbingY",
    "frictionDuckingX",
    "frictionFlyingX",
    "frictionRunningFastX",
    "frictionRunningX",
    "helpJump",
    "helpPageConfig",
    "helpPageMax",
    "holdArrowToggle",
    "initialJumpAcc",
    "jetpackFuel",
    "joyid",
    "jumpButtonRelease",
    "jumpButtonReleased",
    "jumpTime",
    "jumpTimeTotal",
    "kcfgCurItem",
    "kcfgWantKey",
    "runAcc",
    "runAnimSpeed",
    "runHeld",
    "runKey",
    "screen544",
    "screen608",
    "screen672",
    "screen880",
    "screen_h",
    "screen_scale",
    "screen_w",
    "screen_x",
    "screen_y",
    "sprite_xoffset",
    "sprite_yoffset",
    "uiCurValue",
    "uiDisabled",
    "uiDrawCurItem",
    "uiDrawPageTop",
    "uiInfoText",
    "uiItemCount",
    "uiItemsOnPage",
    "uiText",
    "uiType",
    "uiVarName",
    "ycfgMessage",
    "ycfgMessageTime",
    "pauseAlpha",
    "glowAlpha",
    "mapTitleX", "mapX", "mapY",
    "moneyCount",
    "viscidTop",
    "statePrevPrev", "statePrev",
    "maxDownSlope",
    "slopeYPrev", // this may be local, doublecheck it
    "maxSlope",
    "swimming",
    "grav", "gravNorm",
    "hangCount", "jumps", "gravityIntensity",
    "ladderTimer", "pushTimer", "dead",
    "hangCount",
    "greenColor", "greenToggle",
    "redColor", "redToggle",
    "blinkToggle",
    "blink", "player",
  ];

  foreach (string name; knownFields) fields[name] = VarInfo(VarInfo.Type.Field, name);

  if (auto fn = cast(NodeFunc)nn) {
    import std.algorithm : startsWith;
    if (fn.name.startsWith("scrSurface")) {
      fields["px"] = VarInfo(VarInfo.Type.Field, "px");
      fields["py"] = VarInfo(VarInfo.Type.Field, "py");
    }
  }

  foreach (string name; [
    "all", "noone", "self", "other", "message", "message2",
  ]) globals[name] = VarInfo(VarInfo.Type.Global, name);

  bool inWith = false;

  void anal (Node nn, bool asAss=false, bool isCompound=false) {
    if (nn is null) return;
    if (cast(NodeStatement)nn) {
      selectNode!(void)(nn,
        (NodeVarDecl n) {
          foreach (immutable idx, string name; n.names) {
            if ((name in locals) || (name in globals)) {
              writeln(n.locs[idx], ": duplicate variable definition: '", name, "'");
              if (pp !is null) pp.printCaret(n.locs[idx]);
            }
            if (n.asGlobal) {
              globals[name] = VarInfo(VarInfo.Type.Global, name);
            } else {
              locals[name] = VarInfo(VarInfo.Type.Local, name);
            }
          }
        },
        (NodeBlock n) { foreach (Node st; n.stats) anal(st); },
        (NodeStatementEmpty n) {},
        (NodeStatementExpr n) { anal(n.e); },
        (NodeReturn n) { anal(n.e); },
        (NodeWith n) {
          anal(n.e);
          auto ow = inWith;
          scope(exit) inWith = ow;
          inWith = true;
          //writeln("::: ", n.ebody.toString);
          anal(n.ebody);
        },
        (NodeIf n) { anal(n.ec); anal(n.et); anal(n.ef); },
        (NodeStatementBreak n) {},
        (NodeStatementContinue n) {},
        (NodeFor n) { anal(n.einit); anal(n.econd); anal(n.enext); anal(n.ebody); },
        (NodeWhile n) { anal(n.econd); anal(n.ebody); },
        (NodeDoUntil n) { anal(n.econd); anal(n.ebody); },
        (NodeRepeat n) { anal(n.ecount); anal(n.ebody); },
        (NodeSwitch n) {
          anal(n.e);
          foreach (ref ci; n.cases) { anal(ci.e); anal(ci.st); }
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    } else {
      selectNode!(void)(nn,
        (NodeLiteral n) {},
        (NodeUnary n) { anal(n.e); },
        (NodeBinaryAss n) {
          if (inWith) {
            if (auto id = cast(NodeId)n.el) {
              //writeln("*** ", id.name);
              fields[id.name] = VarInfo(VarInfo.Type.Field, id.name);
            } else {
              //writeln("+++ ", n.el.toString, " : ", typeid(n.el).name);
            }
          }
          anal(n.er);
          anal(n.el, asAss:true);
        },
        (NodeBinary n) { anal(n.el); anal(n.er); },
        (NodeFCall n) {
          if (auto id = cast(NodeId)n.fe) {
            if (id.name !in locals && id.name !in globals && id.name !in fields) {
              fields[id.name] = VarInfo(VarInfo.Type.Local, id.name);
            }
          } else {
            anal(n.fe);
          }
          foreach (immutable idx, Node a; n.args) anal(a);
        },
        (NodeId n) {
          if (!isCompound) {
            if (n.name !in locals && n.name !in globals && n.name !in fields) {
              if (n.name.length > 2 && (n.name[0] >= 'a' && n.name[0] <= 'z') && (n.name[1] >= 'A' && n.name[1] <= 'Z')) {
              } else if (n.name.length > 2 && n.name[0] == 'c' && n.name[1] == '_') {
              } else if (n.name.length > 3 && n.name[0..3] == "ev_") {
              } else if (n.name.length > 5 && n.name[0..5] == "view_") {
              } else if (n.name.length > 3 && n.name[0..3] == "vk_") {
              } else if (n.name.length > 3 && n.name[0..3] == "cr_") {
              } else if (n.name.length > 3 && n.name[0..3] == "bm_") {
              } else if (n.name.length > 1 && (n.name[0] >= 'A' && n.name[0] <= 'Z')) {
              } else if (n.name.length > 3 && n.name[0..2] == "bg" && (n.name[2] >= 'A' && n.name[2] <= 'Z')) {
              } else {
                writeln(n.loc, ": undefined variable access: '", n.name, "'");
                if (pp !is null) pp.printCaret(n.loc);
              }
              fields[n.name] = VarInfo(VarInfo.Type.Field, n.name);
            }
          } else {
            fields[n.name] = VarInfo(VarInfo.Type.Field, n.name);
          }
        },
        (NodeDot n) {
          if (auto id = cast(NodeId)n.e) {
            if (id.name !in locals && id.name !in globals) {
              fields[n.name] = VarInfo(VarInfo.Type.Field, n.name);
            }
          } else {
            anal(n.e, isCompound:true);
          }
        },
        (NodeIndex n) {
          anal(n.ei0);
          anal(n.ei1);
          anal(n.e);
        },
        (NodeFunc n) { anal(n.ebody); },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }
  }
  anal(nn);
}


// ////////////////////////////////////////////////////////////////////////// //
void analWith (Node nn, Parser pp=null) {
  import std.stdio : writeln;

  alias anal = analWith;

  if (nn is null) return;
  if (cast(NodeStatement)nn) {
    selectNode!(void)(nn,
      (NodeVarDecl n) {},
      (NodeBlock n) { foreach (Node st; n.stats) anal(st, pp); },
      (NodeStatementEmpty n) {},
      (NodeStatementExpr n) { anal(n.e, pp); },
      (NodeReturn n) { anal(n.e, pp); },
      (NodeWith n) {
        anal(n.e, pp);
        if (auto blk = cast(NodeBlock)n.ebody) {
          if (blk.stats.length == 0) {
            writeln(n.loc, ": ???");
            if (pp !is null) pp.printCaret(n.loc);
          } else if (blk.stats.length == 1) {
            if (cast(NodeStatementExpr)blk.stats[0] || cast(NodeReturn)blk.stats[0]) {
              writeln(n.loc, ": possible excessive '{}' in 'with'");
              if (pp !is null) pp.printCaret(n.loc);
              return;
            }
            if (cast(NodeStatementEmpty)blk.stats[0]) {
              writeln(n.loc, ": empty statement in empty block in 'with'");
              if (pp !is null) pp.printCaret(n.loc);
              return;
            }
            if (cast(NodeStatementBreak)blk.stats[0]) {
              writeln(n.loc, ": single 'break' in 'with', noop");
              if (pp !is null) pp.printCaret(n.loc);
              return;
            }
            if (cast(NodeStatementContinue)blk.stats[0]) {
              writeln(n.loc, ": single 'continue' in 'with', noop");
              if (pp !is null) pp.printCaret(n.loc);
              return;
            }
          }
        }
        anal(n.ebody, pp);
      },
      (NodeIf n) { anal(n.ec, pp); anal(n.et, pp); anal(n.ef, pp); },
      (NodeStatementBreak n) {},
      (NodeStatementContinue n) {},
      (NodeFor n) { anal(n.einit, pp); anal(n.econd, pp); anal(n.enext, pp); anal(n.ebody, pp); },
      (NodeWhile n) { anal(n.econd, pp); anal(n.ebody, pp); },
      (NodeDoUntil n) { anal(n.econd, pp); anal(n.ebody, pp); },
      (NodeRepeat n) { anal(n.ecount, pp); anal(n.ebody, pp); },
      (NodeSwitch n) {
        anal(n.e, pp);
        foreach (ref ci; n.cases) { anal(ci.e, pp); anal(ci.st, pp); }
      },
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  } else {
    selectNode!(void)(nn,
      (NodeLiteral n) {},
      (NodeUnary n) { anal(n.e, pp); },
      (NodeBinaryAss n) { anal(n.el, pp); anal(n.er, pp); },
      (NodeBinary n) { anal(n.el, pp); anal(n.er, pp); },
      (NodeFCall n) {
        anal(n.fe, pp);
        foreach (immutable idx, Node a; n.args) anal(a, pp);
      },
      (NodeId n) {},
      (NodeDot n) { anal(n.e, pp); },
      (NodeIndex n) {
        anal(n.ei0, pp);
        anal(n.ei1, pp);
        anal(n.e, pp);
      },
      (NodeFunc n) { anal(n.ebody, pp); },
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void analAss (Node nn, Parser pp=null) {
  import std.stdio : writeln;

  void anal (Node nn, Loc wasCmp=Loc.init) {
    if (nn is null) return;
    if (cast(NodeStatement)nn) {
      selectNode!(void)(nn,
        (NodeVarDecl n) {},
        (NodeBlock n) { foreach (Node st; n.stats) anal(st); },
        (NodeStatementEmpty n) {},
        (NodeStatementExpr n) { anal(n.e); },
        (NodeReturn n) { anal(n.e); },
        (NodeWith n) { anal(n.e); anal(n.ebody); },
        (NodeIf n) { anal(n.ec, wasCmp); anal(n.et); anal(n.ef); },
        (NodeStatementBreak n) {},
        (NodeStatementContinue n) {},
        (NodeFor n) { anal(n.einit); anal(n.econd); anal(n.enext); anal(n.ebody); },
        (NodeWhile n) { anal(n.econd); anal(n.ebody); },
        (NodeDoUntil n) { anal(n.econd); anal(n.ebody); },
        (NodeRepeat n) { anal(n.ecount); anal(n.ebody); },
        (NodeSwitch n) {
          anal(n.e);
          foreach (ref ci; n.cases) { anal(ci.e); anal(ci.st); }
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    } else {
      selectNode!(void)(nn,
        (NodeLiteral n) {},
        (NodeUnaryParens n) { anal(n.e); },
        (NodeUnary n) { anal(n.e, wasCmp); },
        (NodeBinaryCmp n) {
          if (wasCmp.valid) {
            writeln(n.loc, ": double logic op in expression, previous was at ", wasCmp.toStringNoFile);
            if (pp !is null) pp.printCaret(n.loc);
          }
          anal(n.el, n.loc); anal(n.er, n.loc);
        },
        (NodeBinaryAss n) { anal(n.el, n.loc/*hack*/); anal(n.er, n.loc/*hack*/); },
        (NodeBinary n) { anal(n.el, wasCmp); anal(n.er, wasCmp); },
        (NodeFCall n) {
          anal(n.fe, wasCmp);
          foreach (immutable idx, Node a; n.args) anal(a);
        },
        (NodeId n) {},
        (NodeDot n) { anal(n.e); },
        (NodeIndex n) { anal(n.ei0); anal(n.ei1); anal(n.e); },
        (NodeFunc n) { anal(n.ebody); },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }
  }
  anal(nn);
}


// ////////////////////////////////////////////////////////////////////////// //
void analUninited (NodeFunc fn) {
  bool[string] locals;
  bool[string] globals;
  Loc[string] vdecls; // for error messages

  int argvar (string s) {
    switch (s) {
      case "argument0": return 0;
      case "argument1": return 1;
      case "argument2": return 2;
      case "argument3": return 3;
      case "argument4": return 4;
      case "argument5": return 5;
      case "argument6": return 6;
      case "argument7": return 7;
      case "argument8": return 8;
      case "argument9": return 9;
      case "argument10": return 10;
      case "argument11": return 11;
      case "argument12": return 12;
      case "argument13": return 13;
      case "argument14": return 14;
      case "argument15": return 15;
      case "self": return 16;
      case "other": return 17;
      default:
    }
    return -1;
  }

  void warning(A...) (Loc loc, A args) {
    import std.stdio;
    writeln(loc, ": ", args);
    if (fn.pp !is null) fn.pp.printCaret(loc);
  }

  // collect var declarations (gml is not properly scoped)
  visitNodes(fn.ebody, (Node n) {
    if (auto vd = cast(NodeVarDecl)n) {
      foreach (immutable idx, string name; vd.names) {
        if (name in locals) {
          if (vd.asGlobal) warning(vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
        } else if (name in globals) {
          if (!vd.asGlobal) warning(vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
        }
        vdecls[name] = vd.locs[idx];
        if (vd.asGlobal) {
          globals[name] = true;
        } else {
          locals[name] = true;
        }
      }
    }
    return VisitRes.Continue;
  });

  void findUninitialized () {
    bool[string] inited;
    bool[string] used;

    void processExpr (Node n, bool asAss=false) {
      if (n is null) return;
      visitNodes(n, (Node nn) {
        if (auto n = cast(NodeBinaryAss)nn) {
          if (cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null) {
            warning(nn.loc, "assignment to rvalue");
            return VisitRes.SkipChildren;
          }
          processExpr(n.er); // it is calculated first
          if (auto did = cast(NodeId)n.el) {
            inited[did.name] = true;
            used[did.name] = true;
          } else {
            processExpr(n.el, asAss:true);
          }
          return VisitRes.SkipChildren;
        }
        if (auto id = cast(NodeId)nn) {
          if (argvar(id.name) < 0) {
            if (!asAss && id.name in locals && id.name !in inited) {
              warning(nn.loc, "using uninitialized variable; declared at ", vdecls[id.name].toStringNoFile);
            }
          }
          inited[id.name] = true;
          used[id.name] = true;
          return VisitRes.SkipChildren;
        }
        if (auto n = cast(NodeFCall)nn) {
          if (cast(NodeId)n.fe is null) warning(n.loc, "invalid function call");
          if (n.args.length > 16) warning(n.loc, "too many arguments in function call");
          foreach (immutable idx, Node a; n.args) {
            // no assignments allowed there
            processExpr(a);
          }
          return VisitRes.SkipChildren;
        }
        return VisitRes.Continue;
      });
    }

    void processStatement (Node nn) {
      if (nn is null) return;
      return selectNode!void(nn,
        (NodeVarDecl n) {},
        (NodeBlock n) {
          foreach (Node st; n.stats) {
            if (cast(NodeStatementBreakCont)st !is null) break;
            processStatement(st);
            if (cast(NodeReturn)st !is null) break;
          }
        },
        (NodeStatementEmpty n) {},
        (NodeStatementExpr n) { processExpr(n.e); },
        (NodeReturn n) { processExpr(n.e); },
        (NodeWith n) {
          processExpr(n.e); // can't contain assignments
          // body can be executed zero times, so...
          auto before = inited.dup;
          processStatement(n.ebody);
          inited = before;
        },
        (NodeIf n) {
          processExpr(n.ec);
          auto before = inited.dup;
          processStatement(n.et);
          auto tset = inited.dup;
          inited = before.dup;
          processStatement(n.ef);
          // now copy to `before` all items that are set both in `tset` and in `inited`
          foreach (string name; inited.byKey) {
            if (name in tset) before[name] = true;
          }
          inited = before;
        },
        (NodeStatementBreakCont n) {},
        (NodeFor n) {
          processExpr(n.einit);
          // "next" and "cond" can't contain assignments, so it's safe here
          processExpr(n.econd);
          processExpr(n.enext);
          // yet body can be executed zero times, so...
          auto before = inited.dup;
          processStatement(n.ebody);
          inited = before;
        },
        (NodeWhile n) {
          // "cond" can't contain assignments, so it's safe here
          processExpr(n.econd);
          // yet body can be executed zero times, so...
          auto before = inited.dup;
          processStatement(n.ebody);
          inited = before;
        },
        (NodeDoUntil n) {
          // "cond" can't contain assignments, so it's safe here
          processExpr(n.econd);
          // body is guaranteed to execute at least one time
          processStatement(n.ebody);
        },
        (NodeRepeat n) {
          // "count" can't contain assignments, so it's safe here
          processExpr(n.ecount);
          // yet body can be executed zero times, so...
          auto before = inited.dup;
          processStatement(n.ebody);
          inited = before;
        },
        (NodeSwitch n) {
          // "expr" can't contain assignments, so it's safe here
          processExpr(n.e);
          auto before = inited.dup;
          foreach (ref ci; n.cases) {
            processExpr(ci.e); // can't contain assignments
            // and this one can
            if (ci.st !is null) {
              inited = before.dup;
              processStatement(ci.st);
            }
          }
          inited = before;
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }

    processStatement(fn.ebody);

    // now show unused locals
    static struct Info { Loc loc; string name; }
    Info[] unusedLocs;
    foreach (string name; locals.keys) {
      if (name !in used) {
        //warning(vdecls[name], "unused local '", name, "'");
        unusedLocs ~= Info(vdecls[name], name);
        locals.remove(name);
      }
    }
    import std.algorithm : sort;
    unusedLocs.sort!((ref a, ref b) { if (a.loc.line < b.loc.line) return true; if (a.loc.line > b.loc.line) return false; return (a.loc.col < b.loc.col); });
    foreach (ref nfo; unusedLocs) warning(nfo.loc, "unused local '", nfo.name, "'");
  }

  findUninitialized();
}
