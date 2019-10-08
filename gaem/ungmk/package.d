/* GML GMK splitter
 * coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module gaem.ungmk is aliced;

import std.datetime;
import std.stdio;

import iv.vfs;
import iv.strex;

import arsd.color;

//version = gmk_debug_dump;


// ////////////////////////////////////////////////////////////////////////// //
static immutable string[uint] evKeyNames;
static immutable string[uint] evMouseNames;
static immutable string[uint] evOtherNames;

shared static this () {
  // keys
  evKeyNames[37] = "left";
  evKeyNames[39] = "right";
  evKeyNames[38] = "up";
  evKeyNames[40] = "down";

  evKeyNames[17] = "control";
  evKeyNames[18] = "alt";
  evKeyNames[16] = "shift";
  evKeyNames[32] = "space";
  evKeyNames[13] = "enter";

  evKeyNames[96] = "numpad 0";
  evKeyNames[97] = "numpad 1";
  evKeyNames[98] = "numpad 2";
  evKeyNames[99] = "numpad 3";
  evKeyNames[100] = "numpad 4";
  evKeyNames[101] = "numpad 5";
  evKeyNames[102] = "numpad 6";
  evKeyNames[103] = "numpad 7";
  evKeyNames[104] = "numpad 8";
  evKeyNames[105] = "numpad 9";

  evKeyNames[111] = "numpad divide";
  evKeyNames[106] = "numpad multiply";
  evKeyNames[109] = "numpad subtract";
  evKeyNames[107] = "numpad add";
  evKeyNames[110] = "numpad decimal";

  evKeyNames[48] = "0";
  evKeyNames[49] = "1";
  evKeyNames[50] = "2";
  evKeyNames[51] = "3";
  evKeyNames[52] = "4";
  evKeyNames[53] = "5";
  evKeyNames[54] = "6";
  evKeyNames[55] = "7";
  evKeyNames[56] = "8";
  evKeyNames[57] = "9";

  evKeyNames[65] = "A";
  evKeyNames[66] = "B";
  evKeyNames[67] = "C";
  evKeyNames[68] = "D";
  evKeyNames[69] = "E";
  evKeyNames[70] = "F";
  evKeyNames[71] = "G";
  evKeyNames[72] = "H";
  evKeyNames[73] = "I";
  evKeyNames[74] = "J";
  evKeyNames[75] = "K";
  evKeyNames[76] = "L";
  evKeyNames[77] = "M";
  evKeyNames[78] = "N";
  evKeyNames[79] = "O";
  evKeyNames[80] = "P";
  evKeyNames[81] = "Q";
  evKeyNames[82] = "R";
  evKeyNames[83] = "S";
  evKeyNames[84] = "T";
  evKeyNames[85] = "U";
  evKeyNames[86] = "V";
  evKeyNames[87] = "W";
  evKeyNames[88] = "X";
  evKeyNames[89] = "Y";
  evKeyNames[90] = "Z";

  evKeyNames[112] = "f1";
  evKeyNames[113] = "f2";
  evKeyNames[114] = "f3";
  evKeyNames[115] = "f4";
  evKeyNames[116] = "f5";
  evKeyNames[117] = "f6";
  evKeyNames[118] = "f7";
  evKeyNames[119] = "f8";
  evKeyNames[120] = "f9";
  evKeyNames[121] = "f10";
  evKeyNames[122] = "f11";
  evKeyNames[123] = "f12";

  evKeyNames[8] = "backspace";
  evKeyNames[27] = "escape";
  evKeyNames[36] = "home";
  evKeyNames[35] = "end";
  evKeyNames[33] = "pageup";
  evKeyNames[34] = "pagedown";
  evKeyNames[46] = "delete";
  evKeyNames[45] = "insert";

  evKeyNames[0] = "no key";
  evKeyNames[1] = "any key";

  // mouse
  evMouseNames[0] = "mouse left button";
  evMouseNames[1] = "mouse right button";
  evMouseNames[2] = "mouse middle button";
  evMouseNames[3] = "mouse no button";
  evMouseNames[4] = "mouse left button pressed";
  evMouseNames[5] = "mouse right button pressed";
  evMouseNames[6] = "mouse middle button pressed";
  evMouseNames[7] = "mouse left button released";
  evMouseNames[8] = "mouse right button released";
  evMouseNames[9] = "mouse middle button released";
  evMouseNames[10] = "mouse enter";
  evMouseNames[11] = "mouse leave";
  evMouseNames[16] = "joystick 1 left";
  evMouseNames[17] = "joystick 1 right";
  evMouseNames[18] = "joystick 1 up";
  evMouseNames[19] = "joystick 1 down";
  evMouseNames[21] = "joystick 1 button 1";
  evMouseNames[22] = "joystick 1 button 2";
  evMouseNames[23] = "joystick 1 button 3";
  evMouseNames[24] = "joystick 1 button 4";
  evMouseNames[25] = "joystick 1 button 5";
  evMouseNames[26] = "joystick 1 button 6";
  evMouseNames[27] = "joystick 1 button 7";
  evMouseNames[28] = "joystick 1 button 8";
  evMouseNames[31] = "joystick 2 left";
  evMouseNames[32] = "joystick 2 right";
  evMouseNames[33] = "joystick 2 up";
  evMouseNames[34] = "joystick 2 down";
  evMouseNames[36] = "joystick 2 button 1";
  evMouseNames[37] = "joystick 2 button 2";
  evMouseNames[38] = "joystick 2 button 3";
  evMouseNames[39] = "joystick 2 button 4";
  evMouseNames[40] = "joystick 2 button 5";
  evMouseNames[41] = "joystick 2 button 6";
  evMouseNames[42] = "joystick 2 button 7";
  evMouseNames[43] = "joystick 2 button 8";
  evMouseNames[50] = "mouse global left button";
  evMouseNames[51] = "mouse global right button";
  evMouseNames[52] = "mouse global middle button";
  evMouseNames[53] = "mouse global left pressed";
  evMouseNames[54] = "mouse global right pressed";
  evMouseNames[55] = "mouse global middle pressed";
  evMouseNames[56] = "mouse global left released";
  evMouseNames[57] = "mouse global right released";
  evMouseNames[58] = "mouse global middle released";
  evMouseNames[60] = "mouse wheel up";
  evMouseNames[61] = "mouse wheel down";

  // other
  evOtherNames[0] = "outside room";
  evOtherNames[1] = "intersect boundary";
  evOtherNames[2] = "game start";
  evOtherNames[3] = "game end";
  evOtherNames[4] = "room start";
  evOtherNames[5] = "room end";
  evOtherNames[6] = "no more lives";
  evOtherNames[7] = "animation end";
  evOtherNames[8] = "end of path";
  evOtherNames[9] = "no more health";

  evOtherNames[10] = "user 0";
  evOtherNames[11] = "user 1";
  evOtherNames[12] = "user 2";
  evOtherNames[13] = "user 3";
  evOtherNames[14] = "user 4";
  evOtherNames[15] = "user 5";
  evOtherNames[16] = "user 6";
  evOtherNames[17] = "user 7";
  evOtherNames[18] = "user 8";
  evOtherNames[19] = "user 9";
  evOtherNames[20] = "user 10";
  evOtherNames[21] = "user 11";
  evOtherNames[22] = "user 12";
  evOtherNames[23] = "user 13";
  evOtherNames[24] = "user 14";
  evOtherNames[25] = "user 15";

  evOtherNames[30] = "close button";

  evOtherNames[40] = "outside view 0";
  evOtherNames[41] = "outside view 1";
  evOtherNames[42] = "outside view 2";
  evOtherNames[43] = "outside view 3";
  evOtherNames[44] = "outside view 4";
  evOtherNames[45] = "outside view 5";
  evOtherNames[46] = "outside view 6";
  evOtherNames[47] = "outside view 7";

  evOtherNames[50] = "boundary view 0";
  evOtherNames[51] = "boundary view 1";
  evOtherNames[52] = "boundary view 2";
  evOtherNames[53] = "boundary view 3";
  evOtherNames[54] = "boundary view 4";
  evOtherNames[55] = "boundary view 5";
  evOtherNames[56] = "boundary view 6";
  evOtherNames[57] = "boundary view 7";
}


// ////////////////////////////////////////////////////////////////////////// //
DateTime fromDelphiDT (double d) {
  import std.datetime;
  import core.stdc.math : modf;
  auto dt = Date(Date(1899, 12, 30).dayOfGregorianCal+cast(int)d);
  double i, f;
  f = modf(d, &i);
  auto seconds = cast(int)(24*60*60*f+0.5);
  if (seconds > 59) seconds = 59;
  auto tm = TimeOfDay(seconds/(60*60), (seconds/60)%60, seconds%60);
  return DateTime(dt, tm);
}


double toDelphiDT (DateTime dt) {
  double d = dt.dayOfGregorianCal-Date(1899, 12, 30).dayOfGregorianCal;
  d += (dt.hour*(60*60)+dt.minute*60+dt.second)/(24.0*60.0*60.0);
  return d;
}


// ////////////////////////////////////////////////////////////////////////// //
string readPStr (VFile fl) {
  auto sz = fl.readNum!uint;
  if (sz > 1024*1024) throw new Exception("string too long");
  if (sz == 0) return "";
  auto res = new char[](sz);
  fl.rawReadExact(res[]);
  return cast(string)res; // it's safe here
}


void writePStr (VFile fl, string s) {
  if (s.length > 1024*1024) throw new Exception("string too long");
  fl.writeNum!uint(cast(uint)s.length);
  if (s.length) fl.rawWriteExact(s[]);
}


// delphi TDateTime
DateTime readDateTime (VFile fl) {
  auto d = fl.readNum!double;
  version(none) {
    auto dt = fromDelphiDT(d);
    auto dd = toDelphiDT(dt);
    auto xd = fromDelphiDT(dd);
    writeln("dt: ", dt);
    writeln("xd: ", xd);
    assert(dt == xd);
  }
  return fromDelphiDT(d);
}


void writeDateTime (VFile fl, DateTime dt) {
  auto d = toDelphiDT(dt);
  fl.writeNum!double(d);
}


// ////////////////////////////////////////////////////////////////////////// //
TrueColorImage readImage (VFile fl) {
  auto w = fl.readNum!uint;
  auto h = fl.readNum!uint;
  if (w > 32767 || h > 32767) {
    import std.conv : to;
    throw new Exception("too big image: "~to!string(w)~"x"~to!string(h));
  }
  if (w > 0 && h > 0) {
    auto isz = fl.readNum!uint;
    if (isz < w*h*4) throw new Exception("image data size too small");
    isz -= w*h*4;
    auto img = new TrueColorImage(w, h);
    fl.rawReadExact(img.imageData.bytes[0..w*h*4]);
    // now swap bytes
    auto bp = img.imageData.bytes.ptr;
    foreach (immutable _; 0..w*h) {
      ubyte t = bp[0];
      bp[0] = bp[2];
      bp[2] = t;
      bp += 4;
    }
    //while (isz--) fl.readNum!ubyte;
    if (isz > 0) fl.seek(isz, Seek.Cur);
    return img;
  } else {
    return new TrueColorImage(0, 0);
  }
}


void writeImage (VFile fl, TrueColorImage img) {
  if (img.width > 32767 || img.height > 32767) throw new Exception("too big image");
  fl.writeNum!uint(cast(uint)img.width);
  fl.writeNum!uint(cast(uint)img.height);
  if (img.width > 0 && img.height > 0) {
    fl.writeNum!uint(cast(uint)img.width*cast(uint)img.height*4);
    foreach (int y; 0..img.height) {
      foreach (int x; 0..img.width) {
        Color clr = img.getPixel(x, y);
        fl.writeNum!ubyte(clr.b);
        fl.writeNum!ubyte(clr.g);
        fl.writeNum!ubyte(clr.r);
        fl.writeNum!ubyte(clr.a);
      }
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
struct XField(string typename) {
  enum TypeName = typename;
  string desc;
}


// ////////////////////////////////////////////////////////////////////////// //
mixin template GenIO() {
  private static alias Id(alias T) = T;

  private static string genRead(T) () {
    string res;
    foreach (immutable fname; __traits(derivedMembers, T)) {
      alias mem = Id!(__traits(getMember, T, fname));
      foreach (immutable uda; __traits(getAttributes, mem)) {
        static if (typeof(uda).stringof.length > 6 && typeof(uda).stringof[0..7] == "XField!") {
          static if (uda.TypeName[0] == '!') {
            // custom
            res ~= "read_"~fname~"(fl);\n";
          } else static if (uda.TypeName == "Color") {
            res ~= fname~".b = fl.readNum!ubyte;\n";
            res ~= fname~".g = fl.readNum!ubyte;\n";
            res ~= fname~".r = fl.readNum!ubyte;\n";
            res ~= fname~".a = fl.readNum!ubyte;\n";
          } else static if (uda.TypeName == "pstr") {
            res ~= fname~" = fl.readPStr;\n";
          } else static if (uda.TypeName == "tdatetime") {
            res ~= fname~" = fl.readDateTime;\n";
          } else static if (typeof(mem).stringof == uda.TypeName) {
            res ~= fname~" = fl.readNum!"~uda.TypeName~";\n";
          } else static if (is(typeof(mem) == bool)) {
            res ~= fname~" = (fl.readNum!"~uda.TypeName~" != 0);\n";
          } else {
            res ~= fname~" = cast("~typeof(mem).stringof~")fl.readNum!"~uda.TypeName~";\n";
            //static assert(0, "wtf for '"~fname~"': uda says '"~uda.TypeName~"', type is '"~typeof(mem).stringof~"'");
          }
        }
      }
    }
    return res;
  }

  private static string genWrite(T) () {
    string res;
    foreach (immutable fname; __traits(derivedMembers, T)) {
      alias mem = Id!(__traits(getMember, T, fname));
      foreach (immutable uda; __traits(getAttributes, mem)) {
        static if (typeof(uda).stringof.length > 6 && typeof(uda).stringof[0..7] == "XField!") {
          static if (uda.TypeName[0] == '!') {
            // custom
            res ~= "write_"~fname~"(fl);\n";
          } else static if (uda.TypeName == "Color") {
            res ~= "fl.writeNum!ubyte("~fname~".b);\n";
            res ~= "fl.writeNum!ubyte("~fname~".g);\n";
            res ~= "fl.writeNum!ubyte("~fname~".r);\n";
            res ~= "fl.writeNum!ubyte("~fname~".a);\n";
          } else static if (uda.TypeName == "pstr") {
            res ~= "fl.writePStr("~fname~");\n";
          } else static if (uda.TypeName == "tdatetime") {
            res ~= "fl.writeDateTime("~fname~");\n";
          } else static if (typeof(mem).stringof == uda.TypeName) {
            res ~= "fl.writeNum!"~uda.TypeName~"("~fname~");\n";
          } else static if (is(typeof(mem) == bool)) {
            res ~= "fl.writeNum!"~uda.TypeName~"(cast("~uda.TypeName~")("~fname~" ? 1 : 0));\n";
          } else {
            //static assert(0, "wtf for '"~fname~"'!");
            res ~= "fl.writeNum!"~uda.TypeName~"(cast("~uda.TypeName~")"~fname~");\n";
          }
        }
      }
    }
    return res;
  }

  /*
  private static string genDump(T) () {
    string res;
    foreach (immutable fname; __traits(derivedMembers, T)) {
      alias mem = Id!(__traits(getMember, T, fname));
      foreach (immutable uda; __traits(getAttributes, mem)) {
        static if (uda.TypeName[0] == '!') {
          // custom
        } else static if (typeof(uda).stringof.length > 6 && typeof(uda).stringof[0..7] == "XField!") {
          static if (uda.TypeName == "Color") {
            res ~= "writefln(\""~uda.desc~": rgba(%s,%s,%s,%s)\", "~fname~".r, "~fname~".g, "~fname~".b, "~fname~".a);\n";
          } else static if (uda.TypeName == "pstr") {
            res ~= "writeln(\""~uda.desc~": [\", "~fname~", \"]\");\n";
          } else static if (uda.TypeName == "tdatetime") {
            //res ~= "writeln(\""~uda.desc~": \", "~fname~");\n";
          } else {
            res ~= "writeln(\""~uda.desc~": \", "~fname~");\n";
          }
        }
      }
    }
    return res;
  }
  */

  //pragma(msg, genRead!(typeof(this)));
  //pragma(msg, genWrite!(typeof(this)));
  //pragma(msg, genDump!(typeof(this)));
  mixin("void read (VFile fl) {\n"~genRead!(typeof(this))~"}");
  mixin("void write (VFile fl) {\n"~genWrite!(typeof(this))~"}");
  //mixin("void dump () {\nimport std.stdio;\n"~genDump!(typeof(this))~"}");
}


// ////////////////////////////////////////////////////////////////////////// //
class GMSomething {
  Gmk gmk;
  this (Gmk agmk) pure nothrow @safe @nogc { gmk = agmk; }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMSprite : GMSomething {
public:
  enum Shape { Precise, Rectangle, Disk, Diamond }
  enum BBoxType { Automatic, Full, Manual }

public:
  uint idx;
  string name;
  DateTime lastmod;
  int xofs, yofs;
  TrueColorImage[] images;
  Shape shape;
  ubyte alphaTolerance;
  bool separateCollisionMasks;
  BBoxType bboxType;
  int bbleft, bbtop, bbright, bbbottom;

  this (Gmk agmk, uint aidx, VFile fl) { super(agmk); idx = aidx; load(fl); }

private:
  void load (VFile fl) {
    name = fl.readPStr;
    lastmod = fl.readDateTime;
    auto xver = fl.readNum!uint;
    //writeln("xver: ", xver);
    if (xver < 800 || xver > 810) throw new Exception("invalid sprite version");
    xofs = fl.readNum!int;
    yofs = fl.readNum!int;
    auto count = fl.readNum!uint;
    if (count > 65535) throw new Exception("too many images in sprite sprite");
    // load images
    foreach (immutable idx; 0..count) {
      xver = fl.readNum!uint;
      if (xver < 800) throw new Exception("invalid sprite image version");
      images ~= fl.readImage;
    }
    {
      auto v = fl.readNum!uint;
      if (v > Shape.max) throw new Exception("invalid sprite shape");
      shape = cast(Shape)v;
    }
    {
      auto v = fl.readNum!uint;
      if (v > alphaTolerance.max) throw new Exception("invalid sprite alpha tolerance");
      alphaTolerance = cast(ubyte)v;
    }
    separateCollisionMasks = (fl.readNum!uint != 0);
    {
      auto v = fl.readNum!uint;
      if (v > BBoxType.max) throw new Exception("invalid sprite bounding box type");
      bboxType = cast(BBoxType)v;
    }
    bbleft = fl.readNum!int;
    bbright = fl.readNum!int;
    bbbottom = fl.readNum!int;
    bbtop = fl.readNum!int;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMBackground : GMSomething {
public:
  uint idx;
  string name;
  DateTime lastmod;
  bool tileset;
  int tileWidth, tileHeight;
  int xofs, yofs;
  int xsep, ysep;
  TrueColorImage image;

  this (Gmk agmk, uint aidx, VFile fl) { super(agmk); idx = aidx; load(fl); }

private:
  void load (VFile fl) {
    name = fl.readPStr;
    lastmod = fl.readDateTime;
    auto xver = fl.readNum!uint;
    //writeln("xver: ", xver);
    if (xver != 710) throw new Exception("invalid background version");
    tileset = (fl.readNum!uint != 0);
    tileWidth = fl.readNum!int;
    tileHeight = fl.readNum!int;
    xofs = fl.readNum!int;
    yofs = fl.readNum!int;
    xsep = fl.readNum!int;
    ysep = fl.readNum!int;
    xver = fl.readNum!uint;
    //writeln("xver: ", xver);
    if (xver < 800 || xver > 810) throw new Exception("invalid background info version");
    image = fl.readImage;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMAction : GMSomething {
public:
  enum Type { Nothing, Function, Code }

  enum Kind {
    act_normal,
    act_begin,
    act_end,
    act_else,
    act_exit,
    act_repeat,
    act_var,
    act_code,
    act_placeholder,
    act_separator,
    act_label,
  }

  enum ArgType {
    t_expr,
    t_string,
    t_both,
    t_boolean,
    t_menu,
    t_sprite,
    t_sound,
    t_background,
    t_path,
    t_script,
    t_object,
    t_room,
    t_font,
    t_color,
    t_timeline,
    t_fonststring,
  }

public:
  uint libid;
  uint id;
  Kind kind;
  bool mayberelative;
  bool question;
  bool applied;
  Type type;
  string funcname;
  string codename;
  uint argused;
  ArgType[8] argtypes;
  int applyobj; // object index to apply; -1: self; -2: other
  uint relative;
  string[8] argvals;
  bool negated;

public:
  this (Gmk agmk, VFile fl) { super(agmk); load(fl); }

private:
  void load (VFile fl) {
    auto xver = fl.readNum!uint;
    if (xver != 440) throw new Exception("invalid action version");
    libid = fl.readNum!uint;
    id = fl.readNum!uint;
    {
      auto v = fl.readNum!uint;
      if (v > Kind.max) throw new Exception("invalid action kind");
      kind = cast(Kind)v;
    }
    mayberelative = (fl.readNum!uint != 0);
    question = (fl.readNum!uint != 0);
    applied = (fl.readNum!uint != 0);
    {
      auto v = fl.readNum!uint;
      if (v > Type.max) throw new Exception("invalid action type");
      type = cast(Type)v;
    }
    funcname = fl.readPStr;
    codename = fl.readPStr;
    argused = fl.readNum!uint;
    if (argused > 8) throw new Exception("invalid number of arguments used in action");
    auto akcount = fl.readNum!uint;
    if (akcount > 8) { import std.conv : to; throw new Exception("invalid number of argument kinds for action: "~to!string(akcount)); }
    argtypes[] = ArgType.t_string; //FIXME
    foreach (immutable idx; 0..akcount) {
      auto v = fl.readNum!uint;
      if (v > ArgType.max) throw new Exception("invalid argument type");
      argtypes[idx] = cast(ArgType)v;
    }
    applyobj = fl.readNum!int;
    relative = fl.readNum!uint;
    auto akvals = fl.readNum!uint;
    if (akvals != akcount) { import std.conv : to; throw new Exception("invalid number of argument values for action: "~to!string(akvals)); }
    argvals[] = null;
    foreach (immutable idx; 0..akvals) argvals[idx] = fl.readPStr;
    negated = (fl.readNum!uint != 0);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMEvent : GMSomething {
public:
  enum Type {
    ev_create,
    ev_destroy,
    ev_alarm,
    ev_step,
    ev_collision,
    ev_keyboard,
    ev_mouse,
    ev_other,
    ev_draw,
    ev_keypress,
    ev_keyrelease,
    ev_trigger,
  }

  Type type;
  uint id;
  GMAction[] actions;

public:
  this (Gmk agmk, VFile fl, Type atype, uint aid) { super(agmk); type = atype; load(fl, aid); }

private:
  void load (VFile fl, uint aid) {
    id = aid;
    auto xver = fl.readNum!uint;
    if (xver != 400) throw new Exception("invalid event version");
    auto count = fl.readNum!uint;
    if (count > 1024) throw new Exception("too many actions in event");
    foreach (immutable idx; 0..count) actions ~= new GMAction(gmk, fl);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMObject : GMSomething {
public:
  uint idx;
  string name;
  DateTime lastmod;
  int spridx; // -1: none
  bool solid;
  bool visible;
  int depth;
  bool persistent;
  int parentobjidx; // -100: none
  int maskspridx; // -1: none
  GMEvent[][GMEvent.Type.max+1] events;

public:
  this (Gmk agmk, uint aidx, VFile fl) { super(agmk); idx = aidx; load(fl); }

private:
  void load (VFile fl) {
    name = fl.readPStr;
    lastmod = fl.readDateTime;
    auto xver = fl.readNum!uint;
    if (xver != 430) throw new Exception("invalid object version");
    spridx = fl.readNum!int;
    solid = (fl.readNum!uint != 0);
    visible = (fl.readNum!uint != 0);
    depth = fl.readNum!int;
    persistent = (fl.readNum!uint != 0);
    parentobjidx = fl.readNum!int;
    maskspridx = fl.readNum!int;
    auto ecount = fl.readNum!uint;
    if (ecount != 10 && ecount != 11) throw new Exception("invalid number of event types");
    foreach (immutable evidx; 0..ecount+1) {
      GMEvent[] lst;
      for (;;) {
        int eid = fl.readNum!int;
        if (eid == -1) break;
        lst ~= new GMEvent(gmk, fl, cast(GMEvent.Type)evidx, eid);
      }
      events[evidx] = lst;
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMRoom : GMSomething {
public:
  static struct Back {
  public:
    bool visibleOnStart;
    bool fgimage;
    int bgimageidx; // -1: none
    int x, y;
    int xtile, ytile;
    int xspeed, yspeed;
    bool stretch;

  private:
    void load (VFile fl) {
      visibleOnStart = (fl.readNum!uint != 0);
      fgimage = (fl.readNum!uint != 0);
      bgimageidx = fl.readNum!int;
      x = fl.readNum!int;
      y = fl.readNum!int;
      xtile = fl.readNum!int;
      ytile = fl.readNum!int;
      xspeed = fl.readNum!int;
      yspeed = fl.readNum!int;
      stretch = (fl.readNum!uint != 0);
    }
  }

  static struct View {
  public:
    bool visibleOnStart;
    int x, y;
    int width, height;
    int portx, porty;
    int portw, porth;
    int xborder, yborder;
    int xspace, yspace;
    int objfollow; // -1: none

  private:
    void load (VFile fl, uint xver) {
      visibleOnStart = (fl.readNum!uint != 0);
      if (xver == 520) {
        x = fl.readNum!int;
        y = fl.readNum!int;
        width = fl.readNum!int;
        height = fl.readNum!int;
        portx = fl.readNum!int;
        porty = fl.readNum!int;
        portw = porth = 0;
      } else {
        x = fl.readNum!int;
        y = fl.readNum!int;
        width = fl.readNum!int;
        height = fl.readNum!int;
        portx = fl.readNum!int;
        porty = fl.readNum!int;
        portw = fl.readNum!int;
        porth = fl.readNum!int;
      }
      xborder = fl.readNum!int;
      yborder = fl.readNum!int;
      xspace = fl.readNum!int;
      yspace = fl.readNum!int;
      objfollow = fl.readNum!int;
    }
  }

  static final class Inst {
  public:
    int x, y;
    int objidx;
    uint id;
    string createcode;
    bool locked;

  private:
    void load (VFile fl) {
      x = fl.readNum!int;
      y = fl.readNum!int;
      objidx = fl.readNum!int;
      id = fl.readNum!uint;
      createcode = fl.readPStr;
      locked = (fl.readNum!uint != 0);
    }
  }

  static final class Tile {
  public:
    int x, y;
    int bgidx;
    int xtile, ytile;
    int wtile, htile;
    int layer;
    uint id;
    bool locked;

  private:
    void load (VFile fl) {
      x = fl.readNum!int;
      y = fl.readNum!int;
      bgidx = fl.readNum!int;
      xtile = fl.readNum!int;
      ytile = fl.readNum!int;
      wtile = fl.readNum!int;
      htile = fl.readNum!int;
      layer = fl.readNum!int;
      id = fl.readNum!uint;
      locked = (fl.readNum!uint != 0);
    }
  }

public:
  uint idx;
  string name;
  string caption;
  DateTime lastmod;
  uint width, height;
  int xsnap, ysnap;
  bool isogrid;
  uint speed;
  bool persistent;
  uint bgcolor;
  bool drawbgcolor;
  string createcode;
  Back[8] backs;
  bool viewsEnabled;
  View[8] views;
  Inst[] insts;
  Tile[] tiles;
  int tilew, tileh;
  int xtsep, ytsep;
  int xtofs, ytofs;

public:
  this (Gmk agmk, uint aidx, VFile fl) { super(agmk); idx = aidx; load(fl); }

private:
  void load (VFile fl) {
    name = fl.readPStr;
    lastmod = fl.readDateTime;
    auto xver = fl.readNum!uint;
    if (xver != 520 && xver != 541) throw new Exception("invalid room version");
    caption = fl.readPStr;
    width = fl.readNum!uint;
    height = fl.readNum!uint;
    xsnap = fl.readNum!int;
    ysnap = fl.readNum!int;
    isogrid = (fl.readNum!uint != 0);
    speed = fl.readNum!int;
    persistent = (fl.readNum!uint != 0);
    bgcolor = fl.readNum!uint;
    drawbgcolor = (fl.readNum!uint != 0);
    createcode = fl.readPStr;
    if (fl.readNum!uint != 8) throw new Exception("invalid number of backgrounds in room");
    foreach (ref Back b; backs) b.load(fl);
    viewsEnabled = (fl.readNum!uint != 0);
    if (fl.readNum!uint != 8) throw new Exception("invalid number of views in room");
    foreach (ref View v; views) v.load(fl, xver);
    auto count = fl.readNum!uint;
    if (count > 1024*1024) throw new Exception("too many instances in room");
    foreach (immutable idx; 0..count) { auto i = new Inst(); i.load(fl); insts ~= i; }
    count = fl.readNum!uint;
    if (count > 1024*1024) throw new Exception("too many tiles in room");
    foreach (immutable idx; 0..count) { auto t = new Tile; t.load(fl); tiles ~= t; }
    bool rei = (fl.readNum!uint != 0); // room editor info
    // ignore some REI
    fl.readNum!uint; // REI width
    fl.readNum!uint; // REI height
    fl.readNum!uint; // REI grid show bool
    fl.readNum!uint; // REI objects show bool
    fl.readNum!uint; // REI tiles show bool
    fl.readNum!uint; // REI bg show bool
    fl.readNum!uint; // REI fg show bool
    fl.readNum!uint; // REI views show bool
    fl.readNum!uint; // REI delete underlying objects bool
    fl.readNum!uint; // REI delete underlying tiles bool
    if (xver == 520) {
      tilew = fl.readNum!int;
      tileh = fl.readNum!int;
      xtsep = fl.readNum!int;
      ytsep = fl.readNum!int;
      xtofs = fl.readNum!int;
      ytofs = fl.readNum!int;
    } else {
      tilew = tileh = 16;
      xtsep = ytsep = 1;
      xtofs = ytofs = 0;
    }
    fl.readNum!uint; // REI tab
    fl.readNum!uint; // REI scroll x
    fl.readNum!uint; // REI scroll y
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMTrigger : GMSomething {
public:
  enum When { Begin, Middle, End }

public:
  uint idx;
  string name;
  string condition;
  When when;
  string constname;

public:
  this (Gmk agmk, uint aidx, VFile fl) { super(agmk); idx = aidx; load(fl); }

private:
  void load (VFile fl) {
    auto xver = fl.readNum!uint;
    writeln(xver);
    if (xver != 800) throw new Exception("invalid trigger version");
    name = fl.readPStr;
    condition = fl.readPStr;
    {
      auto v = fl.readNum!uint;
      if (v > When.max) throw new Exception("invalid trigger when");
      when = cast(When)v;
    }
    constname = fl.readPStr;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMGameSettings : GMSomething {
public:
  this (Gmk agmk) { super(agmk); }

  @XField!("uint")("fullscreen") bool fullscreen;
  @XField!("uint")("color interpolation") bool cinterp;
  @XField!("uint")("window border") bool winborder;
  @XField!("uint")("show cursor") bool showcursor;
  @XField!("int")("scale") int scale;
  @XField!("uint")("allow resize") bool allowresize;
  @XField!("uint")("always on top") bool alwaysontop;
  @XField!("Color")("background color") Color bgcolor;
  @XField!("uint")("change resolution") bool changeres;
  @XField!("uint")("color depth") uint colordepth;
  @XField!("uint")("resolution") uint resolution;
  @XField!("uint")("fps") uint fps;
  @XField!("uint")("window title buttons") bool wintitle;
  @XField!("uint")("vsync") bool vsync;
  @XField!("uint")("disable screensaver") bool noscreensaver;
  @XField!("uint")("enable F4 fullscreen switch") bool enablef4;
  @XField!("uint")("enable F1 game info") bool enablef1;
  @XField!("uint")("enable Esc game end") bool enableesc;
  @XField!("uint")("enable F5/F6 game save/load") bool enablef5f6;
  @XField!("uint")("enable F9 game screenshot") bool enablef9;
  @XField!("uint")("close button as Esc") bool closeasesc;
  @XField!("uint")("process priority") uint procprio;
  @XField!("uint")("vsync") bool pauseonbloor;

  @XField!("!")("loading progress bar") uint pbar;
  TrueColorImage bgpbar, fgpbar;

  @XField!("!")("show splashscreen") bool showsplash;
  //TrueColorImage splashimg;
  ubyte[] splashimg; // BMP

  @XField!("uint")("transparent splashscreen") bool slpashtransparent;
  @XField!("uint")("translucent splashscreen alpha") ubyte splashalpha;
  @XField!("uint")("scale progress bar image") bool pbarscale;
  @XField!("!")("icon data") /*TrueColorImage*/ubyte[] icon; // windoze .ico
  @XField!("uint")("display error messages") bool showerrors;
  @XField!("uint")("log error messages to 'game_errors.log'") bool logerrors;
  @XField!("uint")("abort on error") bool abortonerror;
  @XField!("uint")("tread uninited vars as '0'") bool zeroallvars;
  @XField!("pstr")("author") string author;
  @XField!("pstr")("version") string ver;
  @XField!("tdatetime")("lastmod for info") DateTime lastmodnfo;
  // information
  @XField!("pstr")("information") string information;
  @XField!("uint")("major") uint major;
  @XField!("uint")("minor") uint minor;
  @XField!("uint")("release") uint release;
  @XField!("uint")("build") uint build;
  @XField!("pstr")("company") string company;
  @XField!("pstr")("product") string product;
  @XField!("pstr")("copyright") string copyright;
  @XField!("pstr")("description") string descritpion;
  @XField!("tdatetime")("lastmod for global game settings") DateTime lastmod;

  mixin GenIO;

private:
  void read_pbar (VFile fl) {
    pbar = fl.readNum!uint;
    bgpbar = null;
    fgpbar = null;
    if (pbar == 2) {
      // back image
      if (fl.readNum!uint) {
        auto sz = fl.readNum!uint;
        auto npos = fl.tell+sz;
        //fl.seek(sz, Seek.Cur);
        bgpbar = readImage(fl);
        fl.seek(npos);
      }
      // front image
      if (fl.readNum!uint) {
        auto sz = fl.readNum!uint;
        auto npos = fl.tell+sz;
        //fl.seek(sz, Seek.Cur);
        fgpbar = readImage(fl);
        fl.seek(npos);
      }
    }
  }

  void write_pbar (VFile fl) {
    fl.writeNum!uint(pbar);
    if (pbar == 2) {
      if (bgpbar !is null) {
        fl.writeNum!uint(bgpbar.width*bgpbar.height*4+3*4); // data size
        fl.writeImage(bgpbar);
      } else {
        fl.writeNum!uint(0);
      }
      if (fgpbar !is null) {
        fl.writeNum!uint(fgpbar.width*fgpbar.height*4+3*4); // data size
        fl.writeImage(fgpbar);
      } else {
        fl.writeNum!uint(0);
      }
    }
  }

  void read_showsplash (VFile fl) {
    auto showsplash = fl.readNum!uint;
    splashimg = null;
    if (showsplash) {
      // image
      if (fl.readNum!uint) {
        auto sz = fl.readNum!uint;
        /+
        {
          import std.stdio;
          writeln("SPLASH! sz=", sz);
          auto pos = fl.tell;
          auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, pos, sz);
          ubyte[] buf;
          for (;;) {
            ubyte[1024] tmp = void;
            auto rd = zst.rawRead(tmp[]);
            if (rd.length == 0) break;
            buf ~= rd[];
          }
          auto fo = VFile("zsp00.bin", "w");
          /*
          fl.rawReadExact(buf[]);
          */
          fo.rawWriteExact(buf[]);
          fl.seek(pos);
        }
        +/
        auto npos = fl.tell+sz;
        //fl.seek(sz, Seek.Cur);
        auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, sz);
        splashimg.length = 0;
        for (;;) {
          ubyte[1024] tmp = void;
          auto rd = zst.rawRead(tmp[]);
          if (rd.length == 0) break;
          splashimg ~= rd[];
        }
        fl.seek(npos);
      }
    }
  }

  void write_showsplash (VFile fl) {
    fl.writeNum!uint(showsplash ? 1 : 0);
    if (showsplash) {
      if (splashimg !is null) {
        //fl.writeNum!uint(splashimg.width*splashimg.height*4+3*4); // data size
        //fl.writeImage(splashimg);
        fl.writeNum!uint(0);
      } else {
        fl.writeNum!uint(0);
      }
    }
  }

  void read_icon (VFile fl) {
    icon = null;
    // image
    auto sz = fl.readNum!uint;
    icon = new ubyte[](sz);
    fl.rawReadExact(icon[]);
  }

  void write_icon (VFile fl) {
    fl.writeNum!uint(cast(uint)icon.length);
    if (icon.length) fl.rawWriteExact(icon[]);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMGameInfo : GMSomething {
public:
  this (Gmk agmk) { super(agmk); }

  @XField!("Color")("background color") Color bgcolor;
  @XField!("uint")("show help in separate window") bool newwindow;
  @XField!("pstr")("caption") string caption;
  @XField!("int")("position x") int x;
  @XField!("int")("position y") int y;
  @XField!("int")("width") int w;
  @XField!("int")("height") int h;
  @XField!("uint")("show window border") bool windowborder;
  @XField!("uint")("allow window resizing") bool allowresize;
  @XField!("uint")("window on top") bool alwaysontop;
  @XField!("uint")("pause game") bool pause;
  @XField!("tdatetime")("lastmod") DateTime lastmod;
  @XField!("pstr")("help text") string text;

  mixin GenIO;
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMScript : GMSomething {
  uint idx;
  string name;
  string code;

  this (Gmk agmk, uint aidx, string aname, string acode) { super(agmk); idx = aidx; name = aname; code = acode; }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMResTree : GMSomething {
  static class Node {
    enum Type {
      Dir,
      Object,
      Sprite,
      Sound,
      Room,
      Background,
      Script,
      Path,
      Font,
      GameInfo,
      GameSettings,
      Timeline,
      Extensions,
    }
    Type type;
    string name;
    Node[] children; // for dir
    //GMSomething obj; // for others
  }

  Node[Node.Type.max+1] roots;

public:
  this (Gmk agmk, VFile fl) { super(agmk); load(fl); }

  // this is slow, but i don't care for now
  string pathForName (Node.Type type, const(char)[] name) {
    string res;

    bool descent (Node n) {
      if (n is null) return false;
      if (n.type == type && n.name == name) { /*res = "!"~n.name;*/ return true; }
      if (n.type == Node.Type.Dir) {
        foreach (Node c; n.children) if (descent(c)) { res = c.name~"/"~res; return true; }
      }
      return false;
    }

    foreach (Node c; roots[]) if (descent(c)) { res = c.name~"/"~res; break; }

    return (res.length ? res[0..$-1] : res);
  }

private:
  void load (VFile fl) {
    static Node.Type fg2t() (uint n) {
      switch (n) {
        case 0: return Node.Type.Sprite;
        case 1: return Node.Type.Sound;
        case 2: return Node.Type.Background;
        case 3: return Node.Type.Path;
        case 4: return Node.Type.Script;
        case 5: return Node.Type.Font;
        case 6: return Node.Type.Timeline;
        case 7: return Node.Type.Object;
        case 8: return Node.Type.Room;
        case 9: return Node.Type.GameInfo;
        case 10: return Node.Type.GameSettings;
        case 11: return Node.Type.Extensions;
        default: assert(0, "invalid tree restype");
      }
    }

    static Node.Type g2t() (uint n) {
      switch (n) {
        case 1: return Node.Type.Object;
        case 2: return Node.Type.Sprite;
        case 3: return Node.Type.Sound;
        case 4: return Node.Type.Room;
        case 6: return Node.Type.Background;
        case 7: return Node.Type.Script;
        case 8: return Node.Type.Path;
        case 9: return Node.Type.Font;
        case 10: return Node.Type.GameInfo;
        case 11: return Node.Type.GameSettings;
        case 12: return Node.Type.Timeline;
        case 13: return Node.Type.Extensions;
        default: assert(0, "invalid tree restype");
      }
    }

    Node loadTree (Node parent, Node.Type ctp) {
      enum Type { Invalid, Root, SubDir, Leaf }
      Type type;
      {
        auto v = fl.readNum!uint; // 0: root; 1: subdir; 2: leaf
        if (v == 0 || v > Type.max) { import std.conv : to; throw new Exception("invalid resource type: "~to!string(v)); }
        type = cast(Type)v;
      }
      if (type == Type.Root) if (parent !is null) assert(0, "wtf?!");
      Node res = new Node();
      auto grp = fl.readNum!uint;
      auto gt = g2t(grp);
      if (gt != ctp) assert(0, "wtf?!");
      auto ipa = fl.readNum!uint();
      //if (type == Type.Leaf) writeln("index: ", ipa); else writeln("parent: ", ipa);
      res.type = (type != Type.Leaf ? Node.Type.Dir : gt);
      res.name = fl.readPStr;
      auto childrenCount = fl.readNum!uint;
      if (type == Type.Leaf && childrenCount > 0) assert(0, "wtf?!");
      foreach (immutable _; 0..childrenCount) res.children ~= loadTree(res, ctp);
      return res;
    }

    foreach (immutable int ridx; 0..12) {
      auto tree = loadTree(null, fg2t(ridx));
      roots[cast(uint)fg2t(ridx)] = tree;
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class Gmk {
  uint gameid;
  ubyte[16] gameguid;
  GMGameSettings gameInfo;
  GMGameInfo gameHelp;
  string[string] constants;
  private GMTrigger[] triggers;
  private GMSprite[] sprites;
  private GMBackground[] backgrounds;
  private GMRoom[] rooms;
  private GMObject[] objects;
  private GMScript[] scripts;
  GMResTree tree;
  uint lastInstanceId;
  uint lastTileId;
  uint[] roomseq;
  string[] extensions;

  private GMObject[string] oByNameAA; // objects by name
  private GMRoom[string] rByNameAA; // rooms by name
  private GMSprite[string] sByNameAA; // sprites by name
  private GMBackground[string] bByNameAA; // backgrounds by name
  private GMScript[string] scrByNameAA; // scripts by name

  this (VFile fl) { load(fl); }
  this (const(char)[] fname) { load(VFile(fname)); }

  pure nothrow @trusted @nogc {
    GMObject objParent (GMObject o) {
      pragma(inline, true);
      return (o is null ? null : objByNum(o.parentobjidx));
    }

    GMObject objByNum (int num) { pragma(inline, true); return (num >= 0 && num < objects.length ? objects.ptr[num] : null); }
    GMRoom roomByNum (int num) { pragma(inline, true); return (num >= 0 && num < rooms.length ? rooms.ptr[num] : null); }
    GMSprite sprByNum (int num) { pragma(inline, true); return (num >= 0 && num < sprites.length ? sprites.ptr[num] : null); }
    GMBackground bgByNum (int num) { pragma(inline, true); return (num >= 0 && num < backgrounds.length ? backgrounds.ptr[num] : null); }
    GMScript scriptByNum (int num) { pragma(inline, true); return (num >= 0 && num < scripts.length ? scripts.ptr[num] : null); }

    GMObject objByName (const(char)[] name) { /*pragma(inline, true);*/ if (auto v = name in oByNameAA) return *v; else return null; }
    GMRoom roomByName (const(char)[] name) { /*pragma(inline, true);*/ if (auto v = name in rByNameAA) return *v; else return null; }
    GMSprite sprByName (const(char)[] name) { /*pragma(inline, true);*/ if (auto v = name in sByNameAA) return *v; else return null; }
    GMBackground bgByName (const(char)[] name) { /*pragma(inline, true);*/ if (auto v = name in bByNameAA) return *v; else return null; }
    GMScript scriptByName (const(char)[] name) { /*pragma(inline, true);*/ if (auto v = name in scrByNameAA) return *v; else return null; }
  }

  // `true` to stop
  GMObject forEachObject (bool delegate (GMObject o) dg) { foreach (auto v; objects) if (v !is null && dg(v)) return v; return null; }
  GMRoom forEachRoom (bool delegate (GMRoom o) dg) { foreach (auto v; rooms) if (v !is null && dg(v)) return v; return null; }
  GMSprite forEachSprite (bool delegate (GMSprite o) dg) { foreach (auto v; sprites) if (v !is null && dg(v)) return v; return null; }
  GMBackground forEachBg (bool delegate (GMBackground o) dg) { foreach (auto v; backgrounds) if (v !is null && dg(v)) return v; return null; }
  GMScript forEachScript (bool delegate (GMScript o) dg) { foreach (auto v; scripts) if (v !is null && dg(v)) return v; return null; }

private:
  void postProcess () {
    forEachObject((v) { oByNameAA[v.name] = v; return false; });
    forEachRoom((v) { rByNameAA[v.name] = v; return false; });
    forEachSprite((v) { sByNameAA[v.name] = v; return false; });
    forEachBg((v) { bByNameAA[v.name] = v; return false; });
    forEachScript((v) { scrByNameAA[v.name] = v; return false; });
  }

  void load (VFile fl) {
    auto sign = fl.readNum!uint;
    if (sign != 1234321) throw new Exception("invalid signature");
    auto ver = fl.readNum!uint;
    if (ver < 800) throw new Exception("invalid version");
    gameid = fl.readNum!uint;
    fl.rawReadExact(gameguid[]);
    // game settings
    {
      auto xver = fl.readNum!uint;
      assert(xver >= 800);
      auto pksize = fl.readNum!uint;
      auto npos = fl.tell+pksize;
      scope(exit) fl.seek(npos);
      auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
      gameInfo = new GMGameSettings(this);
      gameInfo.read(zst);
    }
    // triggers
    {
      auto xver = fl.readNum!uint;
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      if (count > 0) {
        auto pksize = fl.readNum!uint;
        auto npos = fl.tell+pksize;
        scope(exit) fl.seek(npos);
        auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
        foreach (uint idx; 0..count) {
          if (zst.readNum!uint) {
            auto tg = new GMTrigger(this, idx, zst);
            assert(triggers.length == idx);
            triggers ~= tg;
          } else {
            assert(triggers.length == idx);
            triggers ~= null;
          }
        }
      }
      auto lastmod = fl.readDateTime;
    }
    // constants
    {
      auto xver = fl.readNum!uint;
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      foreach (immutable idx; 0..count) {
        auto name = fl.readPStr;
        auto value = fl.readPStr;
        constants[name] = value;
      }
      auto lastmod = fl.readDateTime;
    }
    // resources
    enum ResType {
      Sounds,
      Sprites,
      Backrounds,
      Paths,
      Scripts,
      Fonts,
      Timelines,
      Objects,
      Rooms,
    }
    foreach (immutable idx; 0..ResType.max+1) {
      auto xver = fl.readNum!uint;
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      if (count > 0) {
        foreach (uint c; 0..count) {
          auto pksize = fl.readNum!uint;
          auto npos = fl.tell+pksize;
          scope(exit) fl.seek(npos);
          auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
          final switch (cast(ResType)idx) {
            case ResType.Sounds: throw new Exception("sound resources aren't supported");
            case ResType.Sprites:
              if (zst.readNum!uint) {
                auto spr = new GMSprite(this, c, zst);
                assert(sprites.length == c);
                sprites ~= spr;
              } else {
                assert(sprites.length == c);
                sprites ~= null;
              }
              break;
            case ResType.Backrounds:
              if (zst.readNum!uint) {
                auto bg = new GMBackground(this, c, zst);
                assert(backgrounds.length == c);
                backgrounds ~= bg;
              } else {
                assert(backgrounds.length == c);
                backgrounds ~= null;
              }
              break;
            case ResType.Paths: throw new Exception("path resources aren't supported");
            case ResType.Scripts:
              if (zst.readNum!uint) {
                string name = zst.readPStr;
                auto lastmod = zst.readDateTime;
                auto vv = zst.readNum!uint;
                if (vv != 400 && (vv < 800 || vv > 810)) throw new Exception("invalid script version");
                {
                  import iv.strex;
                  import std.string : replace;
                  string s = zst.readPStr.replace("\r\n", "\n").replace("\r", "\n");
                  s = s.outdentAll;
                  auto sc = new GMScript(this, c, name, s);
                  assert(scripts.length == c);
                  scripts ~= sc;
                }
              } else {
                assert(scripts.length == c);
                scripts ~= null;
              }
              break;
            case ResType.Fonts: throw new Exception("font resources aren't supported");
            case ResType.Timelines: throw new Exception("timeline resources aren't supported");
            case ResType.Objects:
              if (zst.readNum!uint) {
                auto obj = new GMObject(this, c, zst);
                assert(objects.length == c);
                objects ~= obj;
              } else {
                assert(objects.length == c);
                objects ~= null;
              }
              break;
            case ResType.Rooms:
              if (zst.readNum!uint) {
                auto room = new GMRoom(this, c, zst);
                assert(rooms.length == c);
                rooms ~= room;
              } else {
                assert(rooms.length == c);
                rooms ~= null;
              }
              break;
          }
        }
      }
    }
    // done with resources
    lastInstanceId = fl.readNum!uint;
    lastTileId = fl.readNum!uint;
    // includes
    {
      auto xver = fl.readNum!uint;
      if (/*xver != 620 &&*/ (xver < 800 || xver > 810)) throw new Exception("invalid include files version");
      auto count = fl.readNum!uint;
      if (count != 0) throw new Exception("include files are not supported");
    }
    // packages
    {
      auto xver = fl.readNum!uint;
      if (xver != 700) throw new Exception("invalid package list version");
      auto count = fl.readNum!uint;
      while (count--) extensions ~= fl.readPStr;
    }
    // game information
    {
      auto xver = fl.readNum!uint;
      if (xver < 800 || xver > 810) throw new Exception("invalid game information version");
      auto pksize = fl.readNum!uint;
      auto npos = fl.tell+pksize;
      scope(exit) fl.seek(npos);
      auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
      gameHelp = new GMGameInfo(this);
      gameHelp.read(zst);
    }
    // libdeps
    {
      auto xver = fl.readNum!uint;
      if (xver != 500) throw new Exception("invalid library dependency version");
      auto count = fl.readNum!uint;
      if (count != 0) throw new Exception("library dependencies are not supported");
    }
    // room execution index
    {
      auto xver = fl.readNum!uint;
      if (xver != 500 && xver != 540 && xver != 700) throw new Exception("invalid library dependency version");
      auto count = fl.readNum!uint;
      if (count > 1024*1024) throw new Exception("room execution sequence too long");
      if (count) {
        roomseq = new uint[](count);
        foreach (immutable idx, ref i; roomseq) i = fl.readNum!uint;
      }
    }
    // resource tree
    tree = new GMResTree(this, fl);
    postProcess();
  }
}
