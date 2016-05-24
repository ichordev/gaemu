module ungmk is aliced;

import std.datetime;
import std.stdio;

import iv.vfs;
import iv.strex;

import arsd.color;

//version = gmk_debug_dump;


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
    /*
    foreach (int y; 0..h) {
      foreach (int x; 0..w) {
        Color clr;
        clr.b = fl.readNum!ubyte;
        clr.g = fl.readNum!ubyte;
        clr.r = fl.readNum!ubyte;
        clr.a = fl.readNum!ubyte;
        img.setPixel(x, y, clr);
      }
    }
    */
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

  //pragma(msg, genRead!(typeof(this)));
  //pragma(msg, genWrite!(typeof(this)));
  //pragma(msg, genDump!(typeof(this)));
  mixin("void read (VFile fl) {\n"~genRead!(typeof(this))~"}");
  mixin("void write (VFile fl) {\n"~genWrite!(typeof(this))~"}");
  mixin("void dump () {\nimport std.stdio;\n"~genDump!(typeof(this))~"}");
}


/*
struct MyStruct {
  @XField!uint("this is some flag") bool flag;
  mixin GenIO;
}
*/


// ////////////////////////////////////////////////////////////////////////// //
final class GMSprite {
public:
  enum Shape { Precise, Rectangle, Disk, Diamond }
  enum BBox { Automatic, Full, Manual }

public:
  uint idx;
  string name;
  DateTime lastmod;
  int xofs, yofs;
  TrueColorImage[] images;
  Shape shape;
  ubyte alphaTolerance;
  bool separateCollisionMasks;
  BBox bbox;
  int bbleft, bbtop, bbright, bbbottom;

  this (uint aidx, VFile fl) { idx = aidx; load(fl); }

  void dump () {
    import std.stdio;
    writeln("name: [", name, "]");
    //writeln("lastmod: ", lastmod);
    writeln("ofs: ", xofs, ", ", yofs);
    writeln("images: ", images.length);
    foreach (immutable idx, TrueColorImage img; images) {
      writeln("  #", idx, "; ", img.width, "x", img.height);
    }
    writeln("shape: ", shape);
    writeln("alpha tolerance: ", alphaTolerance);
    writeln("separate collision masks: ", separateCollisionMasks);
    writeln("bbox: ", bbox, " (", bbleft, ",", bbtop, ")-(", bbright, ",", bbbottom, ")");
  }

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
      if (v > BBox.max) throw new Exception("invalid sprite bounding box type");
      bbox = cast(BBox)v;
    }
    bbleft = fl.readNum!int;
    bbright = fl.readNum!int;
    bbbottom = fl.readNum!int;
    bbtop = fl.readNum!int;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMBackground {
public:
  uint idx;
  string name;
  DateTime lastmod;
  bool tileset;
  int tileWidth, tileHeight;
  int xofs, yofs;
  int xsep, ysep;
  TrueColorImage image;

  this (uint aidx, VFile fl) { idx = aidx; load(fl); }

  void dump () {
    import std.stdio;
    writeln("name: [", name, "]");
    //writeln("lastmod: ", lastmod);
    writeln("tileset: ", tileset, "; ", tileWidth, "x", tileHeight);
    writeln("ofs: ", xofs, ", ", yofs);
    writeln("sep: ", xsep, ", ", ysep);
    writeln("image: ", image.width, "x", image.height);
  }

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
final class GMAction {
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
  this (VFile fl) { load(fl); }

  void dump () {
    import std.stdio;
    writeln("  libid: ", libid);
    writeln("  id: ", id);
    writeln("  kind: ", kind);
    writeln("  mayberelative: ", mayberelative);
    writeln("  question: ", question);
    writeln("  applied: ", applied);
    writeln("  type: ", type);
    writeln("  funcname: [", funcname, "]");
    writeln("  codename: [", codename, "]");
    writeln("  argused: ", argused);
    if (argused > 0) writeln("  argtypes: ", argtypes[0..argused]);
    writeln("  applyobj: ", applyobj);
    writeln("  relative: ", relative);
    if (argused > 0) {
      writeln("  argvals (", argused, "):");
      foreach (immutable idx, string s; argvals[0..argused]) {
        import iv.strex;
        s = s.detab.outdentAll;
        writeln("   #", idx, ":");
        foreach (auto ln; s.byLine) if (ln.length) writeln("     ", ln);
      }
      //writeln("  argvals: ", argvals[0..argused]);
    }
    writeln("  negated: ", negated);
  }

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
final class GMEvent {
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
  this (VFile fl, Type atype, uint aid) { type = atype; load(fl, aid); }

  void dump () {
    import std.stdio;
    writeln("event id=", id, "; type=", evType(type), "; action count: ", actions.length);
    foreach (immutable idx, GMAction act; actions) {
      writeln(" -- action #", idx, " --");
      act.dump;
    }
  }

  static string evType (Type t) {
    import std.string : format;
    return (t <= Type.max ? "%s".format(t) : "<%s>".format(cast(uint)t));
  }

private:
  void load (VFile fl, uint aid) {
    id = aid;
    auto xver = fl.readNum!uint;
    if (xver != 400) throw new Exception("invalid event version");
    auto count = fl.readNum!uint;
    if (count > 1024) throw new Exception("too many actions in event");
    foreach (immutable idx; 0..count) actions ~= new GMAction(fl);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMObject {
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
  this (uint aidx, VFile fl) { idx = aidx; load(fl); }

  void dump () {
    import std.stdio;
    writeln("name: [", name, "]");
    //writeln("lastmod: ", lastmod);
    writeln("spridx:", spridx);
    writeln("solid: ", solid);
    writeln("visible: ", visible);
    writeln("depth: ", depth);
    writeln("persistent: ", persistent);
    writeln("parentobjidx: ", parentobjidx);
    writeln("maskspridx: ", maskspridx);
    foreach (immutable evidx, GMEvent[] evs; events) {
      if (evs.length == 0) continue;
      writeln(" -- evidx: ", evidx, " --");
      foreach (immutable idx, GMEvent ev; evs) {
        writeln(" -- event #", idx, " --");
        ev.dump;
      }
    }
  }

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
        lst ~= new GMEvent(fl, cast(GMEvent.Type)evidx, eid);
      }
      events[evidx] = lst;
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
final class GMRoom {
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

  public:
    void dump () {
      import std.stdio;
      writeln("visibleOnStart: ", visibleOnStart);
      writeln("fgimage: ", fgimage);
      writeln("bgimageidx: ", bgimageidx);
      writeln("pos: (", x, ",", y, ")");
      writeln("tile: (", xtile, ",", ytile, ")");
      writeln("speed: (", xspeed, ",", yspeed, ")");
      writeln("stretch: ", stretch);
    }

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

  public:
    void dump () {
      import std.stdio;
      writeln("visibleOnStart: ", visibleOnStart);
      writeln("view: (", x, ",", y, "); ", width, "x", height);
      writeln("port: (", portx, ",", porty, "); ", portw, "x", porth);
      writeln("border: (", xborder, ",", yborder, ")");
      writeln("space: (", xspace, ",", yspace, ")");
      writeln("objfollow: ", objfollow);
    }

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

  static struct Inst {
  public:
    int x, y;
    int objidx;
    uint id;
    string createcode;
    bool locked;

  public:
    this (VFile fl) { load(fl); }

    void dump () {
      import std.stdio;
      writeln("position: ", x, "x", y);
      writeln("objidx: ", objidx);
      writeln("id: ", id);
      writeln("createcode: ", createcode.quote);
      writeln("locked: ", locked);
    }

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

  static struct Tile {
  public:
    int x, y;
    int bgidx;
    int xtile, ytile;
    int wtile, htile;
    int layer;
    uint id;
    bool locked;

  public:
    this (VFile fl) { load(fl); }

    void dump () {
      import std.stdio;
      writeln("position: ", x, "x", y);
      writeln("bgidx: ", bgidx);
      writeln("tile: (", xtile, ",", ytile, "); ", wtile, "x", htile);
      writeln("layer: ", layer);
      writeln("id: ", id);
      writeln("locked: ", locked);
    }

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
  this (uint aidx, VFile fl) { idx = aidx; load(fl); }

  void dump () {
    import std.stdio;
    writeln("name: [", name, "]");
    //writeln("lastmod: ", lastmod);
    writeln("caption: [", caption, "]");
    writeln("size: ", width, "x", height);
    writeln("snap: ", xsnap, "x", ysnap);
    writeln("isogrid: ", isogrid);
    writeln("speed: ", speed);
    writeln("persistent: ", persistent);
    writefln("bgcolor: 0x%08x", bgcolor);
    writeln("drawbgcolor: ", drawbgcolor);
    writeln("tile size: ", tilew, "x", tileh);
    writeln("tile sep: ", xtsep, "x", ytsep);
    writeln("tile ofs: ", xtofs, "x", ytofs);
    writeln("createcode: ", createcode.quote);
    foreach (immutable idx, ref Back b; backs) {
      if (b.bgimageidx == -1) continue;
      writeln("-- background #", idx, " --");
      b.dump;
    }
    writeln("viewsEnabled: ", viewsEnabled);
    foreach (immutable idx, ref View v; views) {
      writeln("-- view #", idx, " --");
      v.dump;
    }
    foreach (immutable idx, ref Inst i; insts) {
      writeln("-- instance #", idx, " --");
      i.dump;
    }
    foreach (immutable idx, ref Tile t; tiles) {
      writeln("-- tile #", idx, " --");
      t.dump;
    }
  }

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
    foreach (immutable idx; 0..count) insts ~= Inst(fl);
    count = fl.readNum!uint;
    if (count > 1024*1024) throw new Exception("too many tiles in room");
    foreach (immutable idx; 0..count) tiles ~= Tile(fl);
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
final class GMTrigger {
public:
  enum When { Begin, Middle, End }

public:
  uint idx;
  string name;
  string condition;
  When when;
  string constname;

public:
  this (uint aidx, VFile fl) { idx = aidx; load(fl); }

  void dump () {
    import std.stdio;
    writeln("name: [", name, "]");
    writeln("condition: [", condition.quote, "]");
    writeln("when: ", when);
    writeln("constname: [", constname, "]");
  }

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
final class GMGameInfo {
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
final class GMGameHelp {
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
final class GMScript {
  uint idx;
  string name;
  string code;

  this (uint aidx, string aname, string acode) { idx = aidx; name = aname; code = acode; }
}


// ////////////////////////////////////////////////////////////////////////// //
final class Gmk {
  GMGameInfo gameInfo;
  GMGameHelp gameHelp;
  private GMTrigger[] triggers;
  private GMSprite[] sprites;
  private GMBackground[] backgrounds;
  private GMRoom[] rooms;
  private GMObject[] objects;
  private GMScript[] scripts;

  private GMObject[string] oByNameAA; // objects by name
  private GMRoom[string] rByNameAA; // rooms by name
  private GMSprite[string] sByNameAA; // sprites by name
  private GMBackground[string] bByNameAA; // backgrounds by name
  private GMScript[string] scrByNameAA; // scripts by name

  this (VFile fl, bool dump=false) { load(fl, dump); }
  this (const(char)[] fname, bool dump=false) { load(VFile(fname), dump); }

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

  void load (VFile fl, bool dodump) {
    auto sign = fl.readNum!uint;
    if (sign != 1234321) throw new Exception("invalid signature");
    auto ver = fl.readNum!uint;
    if (dodump) writeln("version: ", ver/100, ".", ver%100);
    if (ver < 800) throw new Exception("invalid version");
    auto gameid = fl.readNum!uint;
    if (dodump) writeln("game id: ", gameid);
    ubyte[16] gameguid = void;
    fl.rawReadExact(gameguid[]);
    if (dodump) { write("game id: "); foreach (ubyte b; gameguid[]) writef("%02x", b); writeln; }
    // game settings
    {
      if (dodump) writeln("=== game settings ===");
      auto xver = fl.readNum!uint;
      if (dodump) writeln("xver: ", xver);
      assert(xver >= 800);
      auto pksize = fl.readNum!uint;
      auto npos = fl.tell+pksize;
      scope(exit) fl.seek(npos);
      //writeln("size: ", pksize);
      auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
      //writeln("unpacked size: ", zst.size);
      auto gi = new GMGameInfo();
      gi.read(zst);
      if (dodump) gi.dump;
      gameInfo = gi;
    }
    // triggers
    {
      if (dodump) writeln("=== triggers ===");
      auto xver = fl.readNum!uint;
      if (dodump) writeln("xver: ", xver);
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      if (dodump) writeln("count: ", count);
      if (count > 0) {
        auto pksize = fl.readNum!uint;
        auto npos = fl.tell+pksize;
        scope(exit) fl.seek(npos);
        if (dodump) writeln("pksize: ", pksize);
        auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
        if (dodump) writeln("upsize: ", zst.size);
        foreach (uint idx; 0..count) {
          if (zst.readNum!uint) {
            auto tg = new GMTrigger(idx, zst);
            if (dodump) writeln("-- trigger #", idx, " --");
            if (dodump) tg.dump;
            assert(triggers.length == idx);
            triggers ~= tg;
          } else {
            assert(triggers.length == idx);
            triggers ~= null;
          }
        }
      }
      auto lastmod = fl.readDateTime;
      //if (dodump) writeln("lastmod: ", lastmod);
    }
    // constants
    {
      if (dodump) writeln("=== constants ===");
      auto xver = fl.readNum!uint;
      if (dodump) writeln("xver: ", xver);
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      if (dodump) writeln("count: ", count);
      foreach (immutable idx; 0..count) {
        auto name = fl.readPStr;
        auto value = fl.readPStr;
        if (dodump) writeln("[", name, "]=[", value, "]");
      }
      auto lastmod = fl.readDateTime;
      //if (dodump) writeln("lastmod: ", lastmod);
    }
    // resources
    static immutable string[9] ResNames = [
      "Sounds",
      "Sprites",
      "Backrounds",
      "Paths",
      "Scripts",
      "Fonts",
      "Timelines",
      "Objects",
      "Rooms",
    ];
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
    foreach (immutable idx, string resname; ResNames) {
      if (dodump) writeln("=== res:", resname, " ===");
      auto xver = fl.readNum!uint;
      if (dodump) writeln("xver: ", ver);
      assert(xver >= 800);
      auto count = fl.readNum!uint;
      if (dodump) writeln("count: ", count);
      if (count > 0) {
        foreach (uint c; 0..count) {
          auto pksize = fl.readNum!uint;
          auto npos = fl.tell+pksize;
          scope(exit) fl.seek(npos);
          //writeln("zstream size: ", pksize);
          auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
          if (idx == ResType.Sprites) {
            // sprites
            if (zst.readNum!uint) {
              auto spr = new GMSprite(c, zst);
              if (dodump) writeln("-- sprite #", c, " --");
              if (dodump) spr.dump;
              assert(sprites.length == c);
              sprites ~= spr;
            } else {
              if (dodump) writeln("-- sprite #", c, " --");
              if (dodump) writeln("  NONE");
              assert(sprites.length == c);
              sprites ~= null;
            }
          } else if (idx == ResType.Backrounds) {
            // backgrounds
            if (zst.readNum!uint) {
              auto bg = new GMBackground(c, zst);
              if (dodump) writeln("-- background #", c, " --");
              if (dodump) bg.dump;
              assert(backgrounds.length == c);
              backgrounds ~= bg;
            } else {
              if (dodump) writeln("-- background #", c, " --");
              if (dodump) writeln("  NONE");
              assert(backgrounds.length == c);
              backgrounds ~= null;
            }
          } else if (idx == ResType.Scripts) {
            // scripts
            if (zst.readNum!uint) {
              string name = zst.readPStr;
              auto lastmod = zst.readDateTime;
              if (dodump) writeln("-- script #", c, " --");
              if (dodump) writeln("name: [", name, "]");
              //if (dodump) writeln("lastmod: ", lastmod);
              auto vv = zst.readNum!uint;
              if (vv != 400 && (vv < 800 || vv > 810)) throw new Exception("invalid script version");
              //writeln("===========\n", zst.readPStr, "\n===========");
              {
                import iv.strex;
                import std.string : replace;
                string s = zst.readPStr.replace("\r\n", "\n").replace("\r", "\n");
                s = s.outdentAll;
                auto sc = new GMScript(c, name, s);
                if (dodump) {
                  writeln("===========");
                  foreach (auto ln; s.byLine) if (ln.length) writeln("     ", ln);
                  writeln("===========");
                }
                assert(scripts.length == c);
                scripts ~= sc;
              }
            } else {
              if (dodump) writeln("-- script #", c, " --");
              if (dodump) writeln("  NONE");
              assert(scripts.length == c);
              scripts ~= null;
            }
          } else if (idx == ResType.Objects) {
            // objects
            if (zst.readNum!uint) {
              if (dodump) writeln("-- object #", c, " --");
              auto obj = new GMObject(c, zst);
              if (dodump) obj.dump;
              assert(objects.length == c);
              objects ~= obj;
            } else {
              //writeln("-- object #", c, " --");
              //writeln("  NONE");
              assert(objects.length == c);
              objects ~= null;
            }
          } else if (idx == ResType.Rooms) {
            // rooms
            if (zst.readNum!uint) {
              if (dodump) writeln("-- room #", c, " --");
              auto room = new GMRoom(c, zst);
              if (dodump) room.dump;
              assert(rooms.length == c);
              rooms ~= room;
            } else {
              //writeln("-- room #", c, " --");
              //writeln("  NONE");
              assert(rooms.length == c);
              rooms ~= null;
            }
          }
        }
      }
    }
    // done with resources
    auto lastInstanceId = fl.readNum!uint;
    auto lastTileId = fl.readNum!uint;
    if (dodump) writeln("last instance placed id: ", lastInstanceId);
    if (dodump) writeln("last tile placed id: ", lastTileId);
    // includes
    {
      auto xver = fl.readNum!uint;
      if (/*xver != 620 &&*/ (xver < 800 || xver > 810)) throw new Exception("invalid include files version");
      auto count = fl.readNum!uint;
      if (dodump) writeln("number of include files: ", count);
      while (count--) {
        auto pksize = fl.readNum!uint;
        fl.seek(pksize, Seek.Cur);
      }
    }
    // packages
    {
      auto xver = fl.readNum!uint;
      if (xver != 700) throw new Exception("invalid package list version");
      auto count = fl.readNum!uint;
      if (dodump) writeln("number of packages files: ", count);
      while (count--) {
        string name = fl.readPStr;
        if (dodump) writeln("  package: [", name, "]");
      }
    }
    // game information
    {
      auto xver = fl.readNum!uint;
      if (xver < 800 || xver > 810) throw new Exception("invalid game information version");
      auto pksize = fl.readNum!uint;
      auto npos = fl.tell+pksize;
      scope(exit) fl.seek(npos);
      auto zst = wrapZLibStreamRO(fl, VFSZLibMode.ZLib, -1, fl.tell, pksize);
      gameHelp = new GMGameHelp();
      gameHelp.read(zst);
      if (dodump) gameHelp.dump;
    }
    // libdeps
    {
      auto xver = fl.readNum!uint;
      if (xver != 500) throw new Exception("invalid library dependency version");
      auto count = fl.readNum!uint;
      while (count--) {
        auto lcc = fl.readPStr;
        if (dodump) if (lcc.length) writeln("libcreatecode: [", lcc.quote, "]");
      }
    }
    // room execution index
    {
      auto xver = fl.readNum!uint;
      if (xver != 500 && xver != 540 && xver != 700) throw new Exception("invalid library dependency version");
      auto count = fl.readNum!uint;
      if (count > 1024*1024) throw new Exception("room execution sequence too long");
      if (count) {
        auto sq = new uint[](count);
        foreach (immutable idx, ref i; sq) i = fl.readNum!uint;
        if (dodump) writeln("room sequence: ", sq);
      }
    }
    // resource tree
    {
      static immutable string[14] GroupName = [
        "unknown",
        "Objects",
        "Sprites",
        "Sounds",
        "Rooms",
        "Five",
        "Backgrounds",
        "Scripts",
        "Paths",
        "Fonts",
        "Game Information",
        "Global Game Settings",
        "Time Lines",
        "Extension Packages",
      ];

      static immutable string[12] FileGroupName = [
        "Sprites",
        "Sounds",
        "Backgrounds",
        "Paths",
        "Scripts",
        "Fonts",
        "Time Lines",
        "Objects",
        "Rooms",
        "Game Information",
        "Global Game Settings",
        "Extension Packages"
      ];

      static void writeIndent (int indent) { foreach (immutable n; 0..indent) write(' '); }

      static void dumpTree (VFile fl, int ind, int curgrp=-1) {
        enum Type { Invalid, Root, SubDir, Leaf }
        Type type;
        {
          auto v = fl.readNum!uint; // 0: root; 1: subdir; 2: leaf
          if (v == 0 || v > Type.max) { import std.conv : to; throw new Exception("invalid resource type: "~to!string(v)); }
          type = cast(Type)v;
        }
        if (curgrp >= 0) {
          writeIndent(ind); writeln("---- [", FileGroupName[curgrp], "] ---");
        } else {
          writeIndent(ind); writeln("----");
        }
        ++ind;
        writeIndent(ind); writeln("type: ", type);
        auto grp = fl.readNum!uint;
        if (grp < GroupName.length) {
          writeIndent(ind); writeln("grouping: ", grp, " <", GroupName[grp], ">");
        } else {
          writeIndent(ind); writeln("grouping: ", grp);
        }
        if (type == Type.Leaf) {
          writeIndent(ind); writeln("index: ", fl.readNum!uint);
        } else {
          writeIndent(ind); writeln("parent: ", fl.readNum!uint);
        }
        writeIndent(ind); writeln("name: [", fl.readPStr, "]");
        auto childrenCount = fl.readNum!uint;
        writeIndent(ind); writeln("childrenCount: ", childrenCount);
        ++ind;
        foreach (immutable _; 0..childrenCount) dumpTree(fl, ind);
      }

      //if (dodump) foreach (immutable ridx; 0..12) dumpTree(fl, 0, cast(int)ridx);
    }
    postProcess();
  }
}
