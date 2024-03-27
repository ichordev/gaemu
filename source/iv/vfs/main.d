/* Invisible Vector Library
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
// VFS pathes and data files
module iv.vfs.main /*is aliced*/;
private:

import core.time;
import iv.alice;
import iv.vfs.config;
import iv.vfs.pred;
import iv.vfs.error;
import iv.vfs.vfile;
import iv.vfs.posixci;
import iv.vfs.koi8;
static import core.sync.mutex;


// ////////////////////////////////////////////////////////////////////////// //
private shared int vfsLockedFlag = 0; // !0: can't add/remove drivers
shared bool vflagIgnoreCasePak = true; // ignore file name case in pak files; driver can ignore this, but it shouldn't
shared bool vflagIgnoreCaseDisk = false; // ignore file name case for disk files

/// get "ingore filename case" flag (default: true)
public @property bool vfsIgnoreCasePak() () nothrow @trusted @nogc { import core.atomic : atomicLoad; return atomicLoad(vflagIgnoreCasePak); }

/// set "ingore filename case" flag
public @property void vfsIgnoreCasePak() (bool v) nothrow @trusted @nogc { import core.atomic : atomicStore; return atomicStore(vflagIgnoreCasePak, v); }

/// get "ingore filename case" flag when no archive files are attached (default: false)
public @property bool vfsIgnoreCaseDisk() () nothrow @trusted @nogc { import core.atomic : atomicLoad; return atomicLoad(vflagIgnoreCaseDisk); }

/// set "ingore filename case" flag when no archive files are attached
public @property void vfsIgnoreCaseDisk() (bool v) nothrow @trusted @nogc { import core.atomic : atomicStore; return atomicStore(vflagIgnoreCaseDisk, v); }


// ////////////////////////////////////////////////////////////////////////// //
private import core.stdc.stdio : FILE;

private FILE* dofopen (const(char)* fname, const(char)* mode) nothrow @nogc {
  import core.stdc.stdio;
  return fopen(fname, mode);
}


// ////////////////////////////////////////////////////////////////////////// //
public struct VFSVariant {
public:
  enum Type : ubyte {
    Empty = 0,
    Bool,
    Int,
    Long,
    Float,
    Double,
    String,
  }

private:
  union {
    bool bv;
    int iv;
    long lv;
    float fv;
    double dv;
  }
  string sv; // moved here, so GC will not scan numbers
  Type vtype = Type.Empty;

public:
  string toString () const {
    import core.stdc.stdio : snprintf;
    char[256] buf = void;
    final switch (vtype) {
      case Type.Empty: return "(empty)";
      case Type.Bool: return (bv ? "true" : "false");
      case Type.Int: auto len = snprintf(buf.ptr, buf.length, "%d", iv); return buf[0..len].idup;
      case Type.Long: auto len = snprintf(buf.ptr, buf.length, "%lld", lv); return buf[0..len].idup;
      case Type.Float: auto len = snprintf(buf.ptr, buf.length, "%f", cast(double)fv); return buf[0..len].idup;
      case Type.Double: auto len = snprintf(buf.ptr, buf.length, "%f", dv); return buf[0..len].idup;
      case Type.String: return sv;
    }
  }

  nothrow @trusted @nogc {
    this (bool v) { vtype = Type.Bool; bv = v; }
    this (int v) { vtype = Type.Int; iv = v; }
    this (long v) { vtype = Type.Long; lv = v; }
    this (float v) { vtype = Type.Float; fv = v; }
    this (double v) { vtype = Type.Double; dv = v; }
    this(T:const(char)[]) (T v) {
      vtype = Type.String;
      static if (is(T == typeof(null))) sv = null;
      else static if (is(T == string)) sv = v;
      else sv = v.idup;
    }

    @property Type type () const pure { return vtype; }

    @property bool hasValue () const pure { return (vtype != Type.Empty); }
    @property bool isString () const pure { return (vtype == Type.String); }
    @property bool isInteger () const pure { return (vtype == Type.Int || vtype == Type.Long); }
    @property bool isFloating () const pure { return (vtype == Type.Float || vtype == Type.Double); }
  }

  @property T get(T) () const /*pure*/ {
    static if (is(T == bool)) {
      final switch (vtype) {
        case Type.Empty: return false;
        case Type.Bool: return bv;
        case Type.Int: return (iv != 0);
        case Type.Long: return (lv != 0);
        case Type.Float: return (fv != 0);
        case Type.Double: return (dv != 0);
        case Type.String: return (sv.length != 0);
      }
    } else static if (is(T == int) || is(T == uint) || is(T == long) || is(T == ulong) || is(T == float) || is(T == double)) {
      final switch (vtype) {
        case Type.Empty: return cast(T)0;
        case Type.Bool: return cast(T)(bv ? 1 : 0);
        case Type.Int: return cast(T)iv;
        case Type.Long: return cast(T)lv;
        case Type.Float: return cast(T)fv;
        case Type.Double: return cast(T)dv;
        case Type.String: return cast(T)0; //assert(0, "cannot get string value as number from VFSVariant");
      }
    } else static if (is(T == string)) {
      return this.toString();
    } else {
      static assert(0, "cannot convert VFSVariant to '"~T.stringof~"'");
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
__gshared core.sync.mutex.Mutex ptlock;
shared static this () { ptlock = new core.sync.mutex.Mutex; }

// non-empty msg: check lockflag
private struct VFSLock {
  int locked = 0;

  @disable this (this);

  ~this () {
    if (locked) {
      import core.atomic;
      if (locked < 0) assert(0, "vfs: internal error");
      atomicOp!"-="(vfsLockedFlag, 1);
      ptlock.unlock();
      locked = -1;
    }
  }
}

private auto vfsLockIntr() (string msg=null) {
  import core.atomic;
  ptlock.lock();
  if (msg.length) {
    if (atomicLoad(vfsLockedFlag)) {
      ptlock.unlock();
      throw new VFSException(msg);
    }
  }
  VFSLock lk;
  lk.locked = 1;
  atomicOp!"+="(vfsLockedFlag, 1);
  return lk;
}


// ////////////////////////////////////////////////////////////////////////// //
/// abstract class for VFS drivers
public abstract class VFSDriver {
  /// for dir range
  public static struct DirEntry {
    VFSDriverId drvid; // assigned only in `vfsFileList()` or `vfsForEachFile()`
    usize index; // should be set by the driver
    string name; // for disk: doesn't include base path; ends with '/'
    long size; // can be -1 if size is not known; for dirs means nothing

    this(T:const(char)[]) (T aname, long asize) {
           static if (is(T == typeof(null))) name = null;
      else static if (is(T == string)) name = aname;
      else name = aname.idup;
      index = index.max; // just in case
      size = asize;
    }

    this(T:const(char)[]) (uint idx, T aname, long asize) {
           static if (is(T == typeof(null))) name = null;
      else static if (is(T == string)) name = aname;
      else name = aname.idup;
      index = idx;
      size = asize;
    }

    VFSVariant stat (const(char)[] propname) const { return vfsStat(this, propname); }
  }

  /// this constructor is used for disk drivers
  this () {}

  /// this constructor is used for archive drivers.
  /// `prefixpath`: this will be prepended to each name from archive, unmodified.
  this (VFile fl, const(char)[] prefixpath) { throw new VFSException("not implemented for abstract driver"); }

  /// try to find and open the file in archive.
  /// should return `VFile.init` if no file was found.
  /// should not throw (except for VERY unrecoverable error).
  /// doesn't do any security checks, 'cause i don't care.
  abstract VFile tryOpen (const(char)[] fname, bool ignoreCase);

  /// get number of entries in archive directory.
  @property usize dirLength () { return 0; }

  /// get directory entry with the given index. can throw, but it's not necessary.
  DirEntry dirEntry (usize idx) { return DirEntry.init; }

  /** query various file properties; driver-specific.
   * properties of interest:
   *   "cretime" -- creation time; unixtime, UTC
   *   "modtime" -- modify time; unixtime, UTC
   *   "pksize" -- packed file size (for archives)
   *   "crc32" -- crc32 value for some archives
   */
  VFSVariant stat (usize idx, const(char)[] propname) { return VFSVariant(); }

  @property bool isDisk () { return false; }
}


/// abstract class for "pak" files
public abstract class VFSDriverDetector {
  /// return null if it can't open the thing.
  /// `prefixpath`: this will be prepended to each name from archive, unmodified.
  abstract VFSDriver tryOpen (VFile fl, const(char)[] prefixpath);
}


// ////////////////////////////////////////////////////////////////////////// //
/// you can register this driver to try disk files with the given data path
public VFSDriver vfsNewDiskDriver(T:const(char)[]) (T dpath=null) {
  return new VFSDriverDiskImpl!true(dpath);
}

/// you can register this driver to try disk files with the given data path
public VFSDriver vfsNewDiskDriverListed(bool needtime=false, T:const(char)[]) (T dpath=null) {
  return new VFSDriverDiskListedImpl!needtime(dpath);
}

/// you can register this driver to try disk files with the given data path
public VFSDriver vfsNewFailDriver () {
  return new VFSDriverAlwaysFail();
}


// ////////////////////////////////////////////////////////////////////////// //
// you can register this driver as "last" to prevent disk searches
final class VFSDriverAlwaysFail : VFSDriver {
  override VFile tryOpen (const(char)[] fname, bool ignoreCase) {
    throw new VFSException("can't open file '"~fname.idup~"'");
  }
}

// this is disk driver implementation; it is templated to cut compile times
class VFSDriverDiskImpl(bool dummy) : VFSDriver {
protected:
  string dataPath;

public:
  this () { dataPath = "./"; }

  this(T : const(char)[]) (T dpath) {
    static if (is(T == typeof(null))) {
      dataPath = "./";
    } else {
      if (dpath.length == 0) {
        dataPath = "./";
      } else if (dpath[$-1] == '/') {
        static if (is(T == string)) dataPath = dpath; else dataPath = dpath.idup;
      } else if (dpath[0] == '~') {
        import iv.vfs.util;
        auto buf = new char[](dpath.length+1025);
        auto dp = expandTilde(buf[0..$-1], dpath);
        if (dp is null) assert(0, "out of memory in iv.vfs disk driver");
        if (dp[$-1] != '/') { buf[dp.length] = '/'; dp = buf[0..dp.length+1]; }
        dataPath = cast(string)dp; // it is safe to cast here
      } else {
        static if (is(T == string)) dataPath = dpath~"/"; else dataPath = dpath.idup~"/";
      }
    }
  }

  /// doesn't do any security checks, 'cause i don't care
  override VFile tryOpen (const(char)[] fname, bool ignoreCase) {
    static import core.stdc.stdio;
    import core.stdc.stdlib : alloca;
    if (fname.length == 0) return VFile.init;
    if (fname.length > 1024*3) return VFile.init; // arbitrary limit
    char* nbuf;
    if (fname[0] == '/') {
      if (dataPath[0] != '/' || fname.length <= dataPath.length) {
        bool hit = (ignoreCase ? koi8StrCaseEqu(fname[0..dataPath.length], dataPath) : fname[0..dataPath.length] == dataPath);
        if (!hit) return VFile.init;
      }
      nbuf = cast(char*)alloca(fname.length+1);
      nbuf[0..fname.length] = fname[];
      nbuf[fname.length] = '\0';
    } else if (fname[0] == '~') {
      import iv.vfs.util;
      uint nepos = 1;
      while (nepos < fname.length && fname[nepos] != '/') ++nepos;
      uint len = cast(uint)fname.length-nepos+257;
      if (len > 1024*3) return VFile.init;
      nbuf = cast(char*)alloca(len);
      nbuf[0..len] = 0;
      auto up = expandTilde(nbuf[0..len-1], fname);
      if (up is null) return VFile.init;
      up.ptr[up.length] = 0;
    } else {
      if (dataPath.length > 4096 || fname.length > 4096 || dataPath.length+fname.length > 1024*3) return VFile.init;
      nbuf = cast(char*)alloca(dataPath.length+fname.length+1);
      nbuf[0..dataPath.length] = dataPath[];
      nbuf[dataPath.length..dataPath.length+fname.length] = fname[];
      nbuf[dataPath.length+fname.length] = '\0';
    }
    static if (VFS_NORMAL_OS) if (ignoreCase) {
      import core.stdc.string : strlen;
      uint len = cast(uint)strlen(nbuf);
      auto pt = findPathCI(nbuf[0..len]);
      if (pt is null) return VFile.init;
      nbuf[pt.length] = '\0';
    }
    static if (VFS_NORMAL_OS) {
      auto fl = dofopen(nbuf, "r");
    } else {
      auto fl = dofopen(nbuf, "rb");
    }
    if (fl is null) return VFile.init;
    usize nblen = 0; while (nbuf[nblen] != 0) ++nblen;
    try { return VFile(fl, nbuf[0..nblen]); } catch (Exception e) {}
    core.stdc.stdio.fclose(fl);
    return VFile.init;
  }

  override @property bool isDisk () { return true; }
}

// same as `VFSDriverDisk`, but provides file list too; it is templated to cut compile times
class VFSDriverDiskListedImpl(bool needtime) : VFSDriverDiskImpl!true {
  private static struct FileEntry {
    //uint index; // should be set by the driver
    string name;
    long size;
    ulong modtime; // 0: unknown; unixtime
  }

protected:
  FileEntry[] files;
  bool flistInited;

protected:
  final void buildFileList () {
    import std.file : DE = DirEntry, dirEntries, SpanMode;
    if (flistInited) return;
    try {
      foreach (DE de; dirEntries(dataPath, SpanMode./*breadth*/depth)) {
        if (!de.isFile) continue;
        if (de.name.length <= dataPath.length) continue;
        static if (needtime) {
          import std.datetime;
          files ~= FileEntry(de.name[dataPath.length..$], de.size, de.timeLastModified.toUTC.toUnixTime());
        } else {
          files ~= FileEntry(de.name[dataPath.length..$], de.size);
        }
      }
    } catch (Exception e) {}
    flistInited = true;
  }

public:
  this(T : const(char)[]) (T dpath) { super(dpath); }

  /// get number of entries in archive directory.
  override @property usize dirLength () { buildFileList(); return files.length; }
  /// get directory entry with the given index. can throw, but it's not necessary.
  override DirEntry dirEntry (usize idx) { buildFileList(); return DirEntry(idx, files[idx].name, files[idx].size); }

  override VFSVariant stat (usize idx, const(char)[] name) {
    buildFileList();
    if (name == "arcname") return VFSVariant("disk");
    if (idx < files.length && name == "packed") return VFSVariant(false);
    if (idx < files.length && name == "modtime" && files[idx].modtime) return VFSVariant(files[idx].modtime);
    if (idx < files.length && (name == "size" || name == "pksize")) { import std.file : getSize; return VFSVariant(files[idx].name.getSize); }
    return VFSVariant();
  }
}


// ////////////////////////////////////////////////////////////////////////// //
public struct VFSDriverId {
private:
  uint id;
  static VFSDriverId create () nothrow @trusted @nogc { return VFSDriverId(++lastIdNumber); }

  public const pure nothrow @safe @nogc {
    bool opCast(T) () if (is(T == bool)) { return (id != 0); }
    @property bool valid () { return (id != 0); }
    bool opEquals() (const VFSDriverId b) { return (id == b.id); }
  }

private:
  __gshared uint lastIdNumber;
}

struct DriverInfo {
  enum Mode { Normal, First, Last }
  Mode mode;
  VFSDriver drv;
  string fname;
  string prefixpath;
  VFSDriverId drvid;
  bool temp; // temporary? will not be used in listing, and eventually removed
  MonoTime tempUsedTime; // last used time for temp paks

  this (Mode amode, VFSDriver adrv, string afname, string apfxpath, VFSDriverId adid, bool atemp) {
    mode = amode;
    drv = adrv;
    fname = afname;
    prefixpath = apfxpath;
    drvid = adid;
    temp = atemp;
    if (atemp) tempUsedTime = MonoTime.currTime;
  }
}

__gshared DriverInfo[] drivers;
__gshared uint tempDrvCount = 0;

private void cleanupDrivers (VFSDriverId did=VFSDriverId(0)) {
  if (tempDrvCount == 0) return;
  bool ctValid = false;
  MonoTime ct;
  auto idx = drivers.length;
  while (idx > 0) {
    --idx;
    if (!drivers.ptr[idx].temp) continue;
    if (drivers.ptr[idx].drvid == did) continue; // keep this one
    if (!ctValid) { ct = MonoTime.currTime; ctValid = true; }
    if ((ct-drivers.ptr[idx].tempUsedTime).total!"seconds" >= 5) {
      // remove it
      --tempDrvCount;
      foreach (immutable c; idx+1..drivers.length) drivers.ptr[c-1] = drivers.ptr[c];
      drivers.length -= 1;
      drivers.assumeSafeAppend;
      if (idx == 0 || tempDrvCount == 0) return;
      --idx;
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/// register new VFS driver
/// driver order is: firsts, normals, lasts (obviously ;-).
/// search order is from first to last, in reverse order inside each driver.
/// you can use returned driver id to unregister the driver later.
public VFSDriverId vfsRegister(string mode="normal", bool temp=false) (VFSDriver drv, const(char)[] fname=null, const(char)[] prefixpath=null) {
  static assert(mode == "normal" || mode == "last" || mode == "first");
  import core.atomic : atomicOp;
  if (drv is null) return VFSDriverId.init;
  auto lock = vfsLockIntr("can't register drivers in list operations");
  cleanupDrivers();
  VFSDriverId did = VFSDriverId.create;
  static if (temp) ++tempDrvCount;
  static if (mode == "normal") {
    // normal
    usize ipos = drivers.length;
    while (ipos > 0 && drivers[ipos-1].mode == DriverInfo.Mode.First) --ipos;
    if (ipos == drivers.length) {
      drivers ~= DriverInfo(DriverInfo.Mode.Normal, drv, fname.idup, prefixpath.idup, did, temp);
    } else {
      drivers.length += 1;
      foreach_reverse (immutable c; ipos+1..drivers.length) drivers[c] = drivers[c-1];
      drivers[ipos] = DriverInfo(DriverInfo.Mode.Normal, drv, fname.idup, prefixpath.idup, did, temp);
    }
  } else static if (mode == "first") {
    // first
    drivers ~= DriverInfo(DriverInfo.Mode.First, drv, fname.idup, prefixpath.idup, did, temp);
  } else static if (mode == "last") {
    drivers = [DriverInfo(DriverInfo.Mode.Last, drv, fname.idup, prefixpath.idup, did, temp)]~drivers;
  } else {
    static assert(0, "wtf?!");
  }
  return did;
}


// ////////////////////////////////////////////////////////////////////////// //
/// find driver. returns invalid VFSDriverId if the driver was not found.
/// WARNING: don't add new drivers while this is in progress!
public VFSDriverId vfsFindDriver() (const(char)[] fname, const(char)[] prefixpath=null) {
  auto lock = vfsLockIntr();
  foreach_reverse (immutable idx, ref drv; drivers) {
    if (!drv.temp && drv.fname == fname && drv.prefixpath == prefixpath) return drv.drvid;
  }
  return VFSDriverId.init;
}


// ////////////////////////////////////////////////////////////////////////// //
/// unregister driver
/// WARNING: don't add new drivers while this is in progress!
public bool vfsUnregister() (VFSDriverId id) {
  auto lock = vfsLockIntr("can't unregister drivers in list operations");
  cleanupDrivers();
  foreach_reverse (immutable idx, ref drv; drivers) {
    if (drv.drvid == id) {
      // i found her!
      foreach (immutable c; idx+1..drivers.length) drivers[c-1] = drivers[c];
      drivers.length -= 1;
      drivers.assumeSafeAppend;
      return true;
    }
  }
  return false;
}


// ////////////////////////////////////////////////////////////////////////// //
/// list all files known to VFS.
/// WARNING: don't add new drivers while this is in process!
public VFSDriver.DirEntry[] vfsFileList() () {
  usize[string] filesSeen;
  VFSDriver.DirEntry[] res;

  auto lock = vfsLockIntr();
  cleanupDrivers();
  foreach_reverse (ref drvnfo; drivers) {
    if (drvnfo.temp) continue;
    foreach_reverse (immutable idx; 0..drvnfo.drv.dirLength) {
      auto de = drvnfo.drv.dirEntry(idx);
      if (de.name.length == 0) continue;
      de.drvid = drvnfo.drvid;
      if (auto iptr = de.name in filesSeen) {
        res.ptr[*iptr] = de;
      } else {
        filesSeen[de.name] = res.length;
        res ~= de;
      }
    }
  }
  return res;
}


/// call callback for each known file in VFS. return non-zero from callback to stop.
/// WARNING: don't add new drivers while this is in process!
public int vfsForEachFile() (scope int delegate (in ref VFSDriver.DirEntry de) cb) {
  if (cb is null) return 0;

  auto lock = vfsLockIntr();
  cleanupDrivers();
  bool[string] filesSeen;
  foreach_reverse (ref drvnfo; drivers) {
    if (drvnfo.temp) continue;
    foreach_reverse (immutable idx; 0..drvnfo.drv.dirLength) {
      auto de = drvnfo.drv.dirEntry(idx);
      if (de.name !in filesSeen) {
        filesSeen[de.name] = true;
        de.drvid = drvnfo.drvid;
        if (auto res = cb(de)) return res;
      }
    }
  }
  return 0;
}


/// Ditto.
public void vfsForEachFile() (scope void delegate (in ref VFSDriver.DirEntry de) cb) {
  if (cb !is null) vfsForEachFile((in ref VFSDriver.DirEntry de) { cb(de); return 0; });
}


/// call callback for each known file in the given driver. return non-zero from callback to stop.
/// WARNING: don't add new drivers while this is in process!
/// WARNING: can return duplicate dir entries!
public int vfsForEachFileInPak() (VFSDriverId did, scope int delegate (in ref VFSDriver.DirEntry de) cb) {
  if (cb is null || !did.valid) return 0;

  auto lock = vfsLockIntr();
  cleanupDrivers(did); // but keep did
  foreach_reverse (ref drvnfo; drivers) {
    if (drvnfo.drvid != did) continue;
    foreach_reverse (immutable idx; 0..drvnfo.drv.dirLength) {
      auto de = drvnfo.drv.dirEntry(idx);
      de.drvid = drvnfo.drvid;
      if (auto res = cb(de)) return res;
    }
    break;
  }
  return 0;
}


/// Ditto.
public void vfsForEachFileInPak() (VFSDriverId did, scope void delegate (in ref VFSDriver.DirEntry de) cb) {
  if (cb !is null) vfsForEachFileInPak(did, (in ref VFSDriver.DirEntry de) { cb(de); return 0; });
}


/// query various things from driver
public VFSVariant vfsStat() (in ref VFSDriver.DirEntry de, const(char)[] propname) {
  if (!de.drvid.valid) return VFSVariant();
  auto lock = vfsLockIntr();
  cleanupDrivers();
  foreach_reverse (ref drvnfo; drivers) {
    if (drvnfo.drvid == de.drvid) return drvnfo.drv.stat(de.index, propname);
  }
  return VFSVariant();
}


// ////////////////////////////////////////////////////////////////////////// //
struct ModeOptions {
  enum bool3 { def = -1, no = 0, yes = 1 }
  bool3 ignoreCase;
  bool allowPaks;
  bool3 allowGZ;
  bool wantWrite;
  bool wantRead;
  bool wantAppend;
  private char[16] newmodebuf=0; // 0-terminated
  private int nmblen; // 0 is not included
  @property const(char)[] mode () return const pure nothrow @safe @nogc { return newmodebuf[0..nmblen]; }

  this (const(char)[] mode) { parse(mode); }

  void parse (const(char)[] mode) {
    nmblen = 0;
    newmodebuf[] = 0;
    allowPaks = true;
    allowGZ = bool3.def;
    wantWrite = false;
    wantRead = false;
    ignoreCase = bool3.def;
    wantAppend = false;
    bool hasPlus = false;
    //static if (!VFS_NORMAL_OS) char btm = 'b';
    foreach (char ch; mode) {
      if (ch == 'i') { ignoreCase = bool3.yes; continue; }
      if (ch == 'I') { ignoreCase = bool3.no; continue; }
      if (ch == 'X') { allowPaks = false; continue; }
      if (ch == 'x') { allowPaks = true; continue; }
      if (ch == 'Z') { allowGZ = bool3.no; continue; }
      if (ch == 'z') { allowGZ = bool3.yes; continue; } // force gzip
      if (ch == 'r' || ch == 'R') { wantRead = true; continue; }
      if (ch == 'w' || ch == 'W') { wantWrite = true; continue; }
      if (ch == 'a' || ch == 'A') { wantAppend = true; continue; }
      if (ch == '+') { hasPlus = true; continue; }
      if (ch == 'b' || ch == 't' || ch == 'B' || ch == 'T') { /*btm = ch;*/ continue; }
    }
    // fix mode
    if (wantRead && wantWrite) hasPlus = true;
    if (!wantRead && !wantWrite && !wantAppend) wantRead = true;
    // build `newmodebuf`
         if (wantAppend) { newmodebuf.ptr[nmblen++] = 'a'; if (hasPlus) newmodebuf.ptr[nmblen++] = '+'; }
    else if (wantWrite) { newmodebuf.ptr[nmblen++] = 'w'; if (hasPlus) newmodebuf.ptr[nmblen++] = '+'; }
    else if (wantRead) { newmodebuf.ptr[nmblen++] = 'r'; if (hasPlus) newmodebuf.ptr[nmblen++] = '+'; }
    else assert(0, "internal VFS error");
    // add 'b' for idiotic shitdoze
    static if (!VFS_NORMAL_OS) newmodebuf.ptr[nmblen++] = 'b';
    newmodebuf[nmblen++] = '\0';
    // fix flags
    if (hasPlus) { wantRead = true; wantWrite = true; }
    if (wantAppend) wantWrite = true;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/** open file; this is the function used by `VFile("zub")`.
 *
 * funny additional file modes:
 *   i: ignore case (default is `vflagIgnoreCase` for paks, and `vflagIgnoreCaseNoDat` for no paks)
 *   I: case sensitive
 *   x: allow searching in paks (default)
 *   X: skip paks
 *   z: allow transparent gzip unpacking (default)
 *   Z: disable transparent gzip unpacking
 */
public VFile vfsOpenFile(T:const(char)[], bool usefname=true) (T fname, const(char)[] mode=null) {
  static import core.stdc.stdio;

  void error (string msg, Throwable e=null, string file=__FILE__, usize line=__LINE__) { throw new VFSException(msg, file, line, e); }

  void errorfn(T:const(char)[]) (T msg, Throwable e=null, string file=__FILE__, usize line=__LINE__) {
    static assert(!is(T == typeof(null)), "wtf?!");
    foreach (char cc; msg) {
      if (cc == '!') {
        //import core.memory : GC;
        char[] str;
        str.reserve(msg.length+fname.length);
        //if (str.ptr is GC.addrOf(str.ptr)) GC.setAttr(str.ptr, GC.BlkAttr.NO_INTERIOR);
        foreach (char ch; msg) {
          if (ch == '!') {
            //foreach (char xch; fname) s.put(xch);
            str ~= fname;
          } else {
            //s.put(ch);
            str ~= ch;
          }
        }
        throw new VFSException(cast(string)str, file, line, e); // it is safe to cast here
      }
    }
    throw new VFSException(msg, file, line, e);
  }

  static if (is(T == typeof(null))) {
    error("can't open file ''");
  } else {
    if (fname.length == 0) error("can't open file ''");

    auto mopt = ModeOptions(mode);

    // try ".gz"
    //TODO: transparently read gzipped files from archives
    if (mopt.allowGZ != ModeOptions.bool3.no && !mopt.wantWrite) {
      if (mopt.allowGZ == ModeOptions.bool3.yes) {
        import etc.c.zlib;
        import std.internal.cstring;
        gzFile gf = gzopen(fname[].tempCString, "rb");
        if (gf !is null) return VFile.OpenGZ(gf, fname);
      } else {
        int epe = cast(int)fname.length;
        while (epe > 1 && fname[epe-1] != '.') --epe;
        if (fname.length-epe == 2 && (fname[epe] == 'G' || fname[epe] == 'g') && (fname[epe+1] == 'Z' || fname[epe+1] == 'z')) {
          //{ import core.stdc.stdio : stderr, fprintf; stderr.fprintf("TRYING GZ: '%.*s'\n", cast(int)fname.length, fname.ptr); }
          import etc.c.zlib;
          import std.internal.cstring;
          gzFile gf = gzopen(fname[].tempCString, "rb");
          if (gf !is null) return VFile.OpenGZ(gf, fname[0..epe-1]);
        }
      }
    }

    if (mopt.allowGZ == ModeOptions.bool3.yes && mopt.wantWrite) {
      import etc.c.zlib;
      import std.internal.cstring;
      gzFile gf = gzopen(fname[].tempCString, (mopt.wantRead ? "r+\0" : "w\0").ptr);
      if (gf !is null) return VFile.OpenGZ(gf, fname);
    }

    if (mopt.wantRead) {
      auto lock = vfsLockIntr();
      cleanupDrivers();
      // try all drivers
      bool ignoreCase = (mopt.ignoreCase == mopt.bool3.def ? vfsIgnoreCasePak : (mopt.ignoreCase == mopt.bool3.yes));
      foreach_reverse (ref di; drivers) {
        try {
          if (!mopt.wantWrite || !di.drv.isDisk) {
            auto fl = di.drv.tryOpen(fname, ignoreCase);
            if (fl.isOpen) {
              if (mopt.wantWrite) errorfn("can't open file '!' in non-binary non-readonly mode");
              if (di.temp) di.tempUsedTime = MonoTime.currTime;
              return fl;
            }
          }
        } catch (Exception e) {
          // chain
          errorfn("can't open file '!'", e);
        }
      }
    }

    // no drivers found, try disk file
    //{ import core.stdc.stdio : stderr, fprintf; stderr.fprintf("TRYING DISK: '%.*s'\n", cast(int)fname.length, fname.ptr); }
    return vfsDiskOpen!(T, usefname)(fname, mode);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
struct DetectorInfo {
  enum Mode { Normal, First, Last }
  Mode mode;
  VFSDriverDetector dt;
}

__gshared DetectorInfo[] detectors;


// ////////////////////////////////////////////////////////////////////////// //
/// register pak (archive) format detector.
public void vfsRegisterDetector(string mode="normal") (VFSDriverDetector dt) {
  static assert(mode == "normal" || mode == "last" || mode == "first");
  if (dt is null) return;
  auto lock = vfsLockIntr("can't register drivers in list operations");
  static if (mode == "normal") {
    // normal
    usize ipos = detectors.length;
    while (ipos > 0 && detectors[ipos-1].mode == DetectorInfo.Mode.Last) --ipos;
    if (ipos == detectors.length) {
      detectors ~= DetectorInfo(DetectorInfo.Mode.Normal, dt);
    } else {
      detectors.length += 1;
      foreach_reverse (immutable c; ipos+1..detectors.length) detectors[c] = detectors[c-1];
      detectors[ipos] = DetectorInfo(DetectorInfo.Mode.Normal, dt);
    }
  } else static if (mode == "last") {
    detectors ~= DetectorInfo(DetectorInfo.Mode.First, dt);
  } else static if (mode == "first") {
    detectors = [DetectorInfo(DetectorInfo.Mode.Last, dt)]~detectors;
  } else {
    static assert(0, "wtf?!");
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/// `prefixpath`: this will be prepended to each name from archive, unmodified.
public VFSDriverId vfsAddPak(string mode="normal", bool temp=false) (const(char)[] fname, const(char)[] prefixpath=null) {
  foreach (char ch; fname) {
    if (ch == ':') {
      try {
        return vfsAddPak!(mode, temp)(vfsOpenFile(fname), fname, prefixpath);
      } catch (Exception) {}
      return vfsAddPak!(mode, temp)(vfsDiskOpen(fname), fname, prefixpath);
    }
  }
  try {
    return vfsAddPak!(mode, temp)(vfsDiskOpen(fname), fname, prefixpath);
  } catch (Exception) {}
  return vfsAddPak!(mode, temp)(vfsOpenFile(fname), fname, prefixpath);
}


/// `prefixpath`: this was prepended to each name from archive.
public VFSDriverId vfsFindPack (const(char)[] fname, const(char)[] prefixpath=null) {
  return vfsFindDriver(fname, prefixpath);
}


///
public bool vfsRemovePak() (VFSDriverId id) {
  return vfsUnregister(id);
}


/// `prefixpath`: this will be prepended to each name from archive, unmodified.
public VFSDriverId vfsAddPak(string mode="normal", bool temp=false, T : const(char)[]) (VFile fl, T fname="", const(char)[] prefixpath=null) {
  static assert(mode == "normal" || mode == "last" || mode == "first");
  static if (is(T == typeof(null))) {
    return vfsAddPak!(mode, temp, T)(fl, "", prefixpath);
  } else {
    void error (Throwable e=null, string file=__FILE__, usize line=__LINE__) {
      if (fname.length == 0) {
        throw new VFSException("can't open pak file", file, line, e);
      } else {
        throw new VFSException("can't open pak file '"~fname.idup~"'", file, line, e);
      }
    }

    if (!fl.isOpen) error();

    auto lock = vfsLockIntr("can't register drivers in list operations");
    // try all detectors
    foreach (ref di; detectors) {
      try {
        fl.seek(0);
        auto drv = di.dt.tryOpen(fl, prefixpath);
        if (drv !is null) {
          // hack!
          import core.atomic;
          atomicOp!"-="(vfsLockedFlag, 1);
          scope(exit) atomicOp!"+="(vfsLockedFlag, 1);
          return vfsRegister!(mode, temp)(drv, fname, prefixpath);
        }
      } catch (Exception e) {
        // chain
        error(e);
      }
    }
    error();
    assert(0);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/// takes into account `vfsIgnoreCaseDisk` flag. you can override it with 'i' (on) or 'I' (off) mode letter.
public VFile vfsDiskOpen(T:const(char)[], bool usefname=true) (T fname, const(char)[] mode=null) {
  static import core.stdc.stdio;
  static if (is(T == typeof(null))) {
    throw new VFSException("can't open file ''");
  } else {
    if (fname.length == 0) throw new VFSException("can't open file ''");
    if (fname.length > 2048) throw new VFSException("can't open file '"~fname.idup~"'");
    auto mopt = ModeOptions(mode);
    char[2049] nbuf = 0;
    uint nblen = 0;
    if (fname[0] == '~') {
      import iv.vfs.util;
      auto up = expandTilde(nbuf[0..$-1], fname);
      if (up is null) throw new VFSException("can't open file '"~fname.idup~"'");
      up.ptr[up.length] = 0;
      assert(nbuf[$-1] == 0);
      nblen = cast(uint)up.length;
    } else {
      nbuf[0..fname.length] = fname[];
      nbuf[fname.length] = '\0';
      nblen = cast(uint)fname.length;
    }
    static if (VFS_NORMAL_OS) if (mopt.ignoreCase == mopt.bool3.yes || (mopt.ignoreCase == mopt.bool3.def && vfsIgnoreCaseDisk)) {
      // we have to lock here, as `findPathCI()` is not thread-safe
      auto lock = vfsLockIntr();
      auto pt = findPathCI(nbuf[0..nblen]);
      if (pt is null) {
        // restore filename for correct error message
        nbuf[] = 0;
        if (fname[0] == '~') {
          import iv.vfs.util;
          auto up = expandTilde(nbuf[0..$-1], fname);
          assert(up !is null);
          assert(nbuf[$-1] == 0);
          up.ptr[up.length] = 0;
          assert(nbuf[$-1] == 0);
          nblen = cast(uint)up.length;
        } else {
          nbuf[0..fname.length] = fname[];
          nbuf[fname.length] = '\0';
          nblen = cast(uint)fname.length;
        }
      } else {
        nbuf[pt.length] = '\0';
        nblen = cast(uint)pt.length;
      }
    }
    // try packs?
    if (mopt.allowPaks && !mopt.wantWrite) {
      uint colonpos = 0;
      version(Windows) {
        // idiotic shitdoze
        if (nbuf[0] && nbuf[1] == ':') colonpos = 2;
      }
      while (nbuf[colonpos] && nbuf[colonpos] != ':') ++colonpos;
      if (nbuf[colonpos]) {
        try {
          import core.stdc.string : strlen;
          auto fl = openFileWithPaks!(char[], usefname)(nbuf[0..strlen(nbuf.ptr)], mode);
          if (fl.isOpen) return fl;
        } catch (Exception e) {
        }
      }
    }
    // normal disk file
    //{ import core.stdc.stdio : stderr, fprintf; stderr.fprintf("USING FOPEN: '%s' '%s'\n", nbuf.ptr, mopt.mode.ptr); }
    auto fl = dofopen(nbuf.ptr, mopt.mode.ptr);
    if (fl is null) throw new VFSException("can't open file '"~fname.idup~"'");
    scope(failure) core.stdc.stdio.fclose(fl); // just in case
    try {
      static if (usefname) {
        return VFile(fl, fname);
      } else {
        return VFile(fl, nbuf[0..strlen(nbuf)]); //???
      }
    } catch (Exception e) {
      // chain
      //{ import core.stdc.stdio : stderr, fprintf; stderr.fprintf("USING FOPEN ERROR!\n"); }
      throw new VFSException("can't open file '"~fname.idup~"'", __FILE__, __LINE__, e);
    }
  }
}


//FIXME: usefname!
VFile openFileWithPaks(T:const(char)[], bool usefname=true) (T name, const(char)[] mode) {
  static assert(!is(T == typeof(null)));
  assert(name.length < int.max/4);

  // check if name has any prefix at all
  int pfxend = cast(int)name.length;
  while (pfxend > 0 && name.ptr[pfxend-1] != ':') --pfxend;
  version(Windows) {
    // idiotic shitdoze
    if (pfxend <= 1) return VFile.init; // easy case
  } else {
    if (pfxend == 0) return VFile.init; // easy case
  }

  auto lock = vfsLockIntr();

  // cpos: after last succesfull prefix
  VFile openit (int cpos) {
    bool nopaks = (cpos == 0);
    version(Windows) {
      // idiotic shitdoze
      if (cpos == 0 && name.length > 2 && name.ptr[1] == ':') cpos = 2;
    }
    while (cpos < name.length) {
      auto ep = cpos;
      while (ep < name.length && name.ptr[ep] != ':') ++ep;
      if (ep >= name.length) return VFile(name, "rXIz");
      if (name.length-ep == 1) throw new VFSException("can't open file '"~name.idup~"'");
      {
        // hack!
        import core.atomic;
        atomicOp!"-="(vfsLockedFlag, 1);
        scope(exit) atomicOp!"+="(vfsLockedFlag, 1);
        vfsAddPak!("normal", true)(name[0..ep], name[0..ep+1]);
      }
      cpos = ep+1;
    }
    throw new VFSException("can't open file '"~name.idup~"'"); // just in case
  }

  // try to find the longest prefix which already exists
  while (pfxend > 0) {
    foreach (ref di; drivers) {
      if (di.prefixpath == name[0..pfxend]) {
        // i found her!
        if (di.temp) di.tempUsedTime = MonoTime.currTime;
        return openit(pfxend);
      }
    }
    --pfxend;
    while (pfxend > 0 && name.ptr[pfxend-1] != ':') --pfxend;
    version(Windows) {
      // idiotic shitdoze
      if (pfxend <= 1) break;
    }
  }
  return openit(0);
}


// ////////////////////////////////////////////////////////////////////////// //
/// open VFile with the given name. supports "file.pak:file1.pak:file.ext" pathes.
/// kept for compatibility with old code, standard `VFile(path)` can do that now.
public VFile openFileEx() (const(char)[] name) {
  if (name.length >= int.max/4) throw new VFSException("name too long");

  // check if name has any prefix at all
  int pfxend = cast(int)name.length;
  while (pfxend > 0 && name.ptr[pfxend-1] != ':') --pfxend;
  version(Windows) {
    // idiotic shitdoze
    if (pfxend <= 1) return VFile(name); // easy case
  } else {
    if (pfxend == 0) return VFile(name); // easy case
  }

  auto lock = vfsLockIntr();

  // cpos: after last succesfull prefix
  VFile openit (int cpos) {
    version(Windows) {
      // idiotic shitdoze
      if (cpos == 0 && name.length > 2 && name.ptr[1] == ':') cpos = 2;
    }
    while (cpos < name.length) {
      auto ep = cpos;
      while (ep < name.length && name.ptr[ep] != ':') ++ep;
      if (ep >= name.length) return VFile(name);
      if (name.length-ep == 1) throw new VFSException("can't open file '"~name.idup~"'");
      {
        // hack!
        import core.atomic;
        atomicOp!"-="(vfsLockedFlag, 1);
        scope(exit) atomicOp!"+="(vfsLockedFlag, 1);
        vfsAddPak!("normal", true)(name[0..ep], name[0..ep+1]);
      }
      cpos = ep+1;
    }
    throw new VFSException("can't open file '"~name.idup~"'"); // just in case
  }

  // try to find the longest prefix which already exists
  while (pfxend > 0) {
    foreach (ref di; drivers) {
      if (di.prefixpath == name[0..pfxend]) {
        // i found her!
        if (di.temp) di.tempUsedTime = MonoTime.currTime;
        return openit(pfxend);
      }
    }
    --pfxend;
    while (pfxend > 0 && name.ptr[pfxend-1] != ':') --pfxend;
    version(Windows) {
      // idiotic shitdoze
      if (pfxend <= 1) break;
    }
  }
  return openit(0);
}
