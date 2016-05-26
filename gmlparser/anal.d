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
void analVars (Parser pp, Node nn) {
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
