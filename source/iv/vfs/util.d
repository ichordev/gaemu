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
// VFS utils
module iv.vfs.util /*is aliced*/;
import iv.alice;


public void arrayAppendUnsafe(T) (ref T[] arr, auto ref T val) {
  auto xptr = arr.ptr;
  arr ~= val;
  if (arr.ptr !is xptr) {
    import core.memory : GC;
    xptr = arr.ptr;
    if (xptr is GC.addrOf(xptr)) GC.setAttr(xptr, GC.BlkAttr.NO_INTERIOR);
  }
}

public void arraySetLengthUnsafe(T) (ref T[] arr, usize len) {
  if (len < arr.length) {
    arr.length = len;
    arr.assumeSafeAppend;
  } else {
    auto xptr = arr.ptr;
    arr.length = len;
    if (arr.ptr !is xptr) {
      import core.memory : GC;
      xptr = arr.ptr;
      if (xptr is GC.addrOf(xptr)) GC.setAttr(xptr, GC.BlkAttr.NO_INTERIOR);
    }
  }
}


public uint crc32 (const(void)[] buf, uint crc=0) pure nothrow @trusted @nogc {
  /*
  static immutable uint[16] crctab = [
    0x00000000, 0x1db71064, 0x3b6e20c8, 0x26d930ac,
    0x76dc4190, 0x6b6b51f4, 0x4db26158, 0x5005713c,
    0xedb88320, 0xf00f9344, 0xd6d6a3e8, 0xcb61b38c,
    0x9b64c2b0, 0x86d3d2d4, 0xa00ae278, 0xbdbdf21c,
  ];
  */
  static immutable uint[16] crctab = {
    uint[16] res = void;
    // make exclusive-or pattern from polynomial (0xedb88320u)
    // terms of polynomial defining this crc (except x^32)
    //uint poly = 0; // polynomial exclusive-or pattern
    //foreach (immutable n; [0,1,2,4,5,7,8,10,11,12,16,22,23,26]) poly |= 1u<<(31-n);
    enum poly = 0xedb88320u;
    foreach (immutable n; 0..16) {
      uint c = cast(uint)n*16;
      foreach (immutable k; 0..8) c = (c&1 ? poly^(c>>1) : c>>1);
      res[n] = c;
    }
    return res;
  }();

  if (buf.length) {
    crc ^= 0xffff_ffffu;
    foreach (ubyte b; cast(const(ubyte)[])buf) {
      crc ^= b;
      crc = crctab.ptr[crc&0x0f]^(crc>>4);
      crc = crctab.ptr[crc&0x0f]^(crc>>4);
    }
    crc ^= 0xffff_ffffu;
  }
  return crc;
}


public uint mur3HashOf(T) (const(T)[] data, usize seed=0) pure nothrow @trusted @nogc if (T.sizeof == 1) {
  enum C1 = 0xcc9e2d51u;
  enum C2 = 0x1b873593u;

  uint hash = seed; // current value
  uint accum; // we operate 32-bit chunks; low 2 bits of accum used as counter
  ubyte n;
  uint k1;

  // process data blocks
  if (data.length) {
    auto bytes = cast(const(ubyte)*)data.ptr;
    auto len = data.length;
    // process 32-bit chunks
    foreach (immutable _; 0..len/4) {
      version(LittleEndian) {
        if (__ctfe) {
          k1 = (bytes[0])|(bytes[1]<<8)|(bytes[2]<<16)|(bytes[3]<<24);
        } else {
          k1 = *cast(const(uint)*)bytes;
        }
      } else {
        if (__ctfe) {
          k1 = (bytes[3])|(bytes[2]<<8)|(bytes[1]<<16)|(bytes[0]<<24);
        } else {
          import core.bitop : bswap;
          k1 = bswap(*cast(const(uint)*)bytes);
        }
      }
      bytes += 4;
      k1 *= C1;
      k1 = (k1<<15)|(k1>>(32-15));
      k1 *= C2;
      hash ^= k1;
      hash = (hash<<13)|(hash>>(32-13));
      hash = hash*5+0xe6546b64;
    }
    // advance over whole 32-bit chunks, possibly leaving 1..3 bytes
    len &= 0x03;
    n = cast(ubyte)len;
    // append any remaining bytes into carry
    while (len--) accum = (accum>>8)|(*bytes++<<24);
  }

  // finalize a hash
  if (n) {
    k1 = accum>>(4-n)*8;
    k1 *= C1;
    k1 = (k1<<15)|(k1>>(32-15));
    k1 *= C2;
    hash ^= k1;
  }
  hash ^= cast(uint)data.length;
  // fmix
  hash ^= hash>>16;
  hash *= 0x85ebca6bu;
  hash ^= hash>>13;
  hash *= 0xc2b2ae35u;
  hash ^= hash>>16;
  return hash;
}


public uint mur3HashOf(R) (auto ref R rng, uint seed=0) pure nothrow @trusted @nogc
if (is(typeof((inout int=0){
  bool e = rng.empty;
  ubyte b = rng.front;
  rng.popFront();
})))
{
  enum C1 = 0xcc9e2d51u;
  enum C2 = 0x1b873593u;

  uint hash = seed; // current value
  ubyte n;
  uint k1;
  uint len;

  // process data blocks
  // advance over whole 32-bit chunks, possibly leaving 1..3 bytes
  while (!rng.empty) {
    ++len;
    ubyte b = rng.front;
    rng.popFront();
    k1 |= (cast(uint)b)<<(n*8);
    n = (n+1)&0x03;
    if (n == 0) {
      // we have a chunk
      k1 *= C1;
      k1 = (k1<<15)|(k1>>(32-15));
      k1 *= C2;
      hash ^= k1;
      hash = (hash<<13)|(hash>>(32-13));
      hash = hash*5+0xe6546b64;
      k1 = 0; // reset accumulator
    }
  }

  // hash remaining bytes
  if (n) {
    k1 *= C1;
    k1 = (k1<<15)|(k1>>(32-15));
    k1 *= C2;
    hash ^= k1;
  }
  hash ^= len;

  // fmix
  hash ^= hash>>16;
  hash *= 0x85ebca6bu;
  hash ^= hash>>13;
  hash *= 0xc2b2ae35u;
  hash ^= hash>>16;
  return hash;
}


version(iv_vfs_hash_test) unittest {
  assert(mur3HashOf("Sample string") == 216753265u);
  assert(mur3HashOf("Alice & Miriel") == 694007271u);

  static struct StringRange {
    const(char)[] s;
    usize pos;
  pure nothrow @trusted @nogc:
    this (const(char)[] as) { s = as; pos = 0; }
    @property bool empty () const { return (pos >= s.length); }
    @property ubyte front () const { return (pos < s.length ? cast(ubyte)s.ptr[pos] : 0); }
    void popFront () { if (pos < s.length) ++pos; }
  }

  assert(mur3HashOf(StringRange("Sample string")) == 216753265u);
  assert(mur3HashOf(StringRange("Alice & Miriel")) == 694007271u);
}

// ////////////////////////////////////////////////////////////////////////// //
/// get user's home path (always terminated with '/'). if `username` is `null`, use current user name.
/// returns path in `destBuf` or `null` if there is no room in `destBuf`.
public char[] getUserHomePath (char[] destBuf, const(char)[] username=null) nothrow @trusted {
  if (destBuf.length == 0) return null;

  char[] putAsciiz (const(char)* s) nothrow @trusted {
    import core.stdc.string : strlen;
    assert(s !is null && s[0]);
    auto slen = strlen(s);
    if (destBuf.length < slen+(s[slen-1] == '/' ? 0 : 1)) return null; // oops
    destBuf[0..slen] = s[0..slen];
    if (s[slen-1] != '/') destBuf[slen] = '/';
    return destBuf[0..slen+(s[slen-1] == '/' ? 0 : 1)];
  }

  version(Posix) {
    import core.stdc.errno : errno, ERANGE;
    import core.stdc.stdlib : free, malloc, realloc/*, getenv*/;
    import core.sys.posix.pwd : passwd, getpwnam_r, getpwuid_r;
    import core.sys.posix.unistd : geteuid;
    import core.sys.posix.sys.types : uid_t;

    uid_t euid;
    char* unamez = null; // asciiz username; if null, use euid
    scope(exit) if (unamez !is null) free(unamez);

    // default user
    if (username.length == 0) {
      // try $HOME first
      import std.process;
      import std.string;
      string homedir;
      try{
        homedir = environment.get("HOME");
      }catch(Exception ex){
        homedir = null;
      }
      if (homedir !is null && homedir[0]) return putAsciiz(homedir.toStringz());
      // no $HOME, get effectife user id
      euid = geteuid();
    } else {
      // ok, we have user name
      unamez = cast(char*)malloc(username.length+1);
      if (unamez is null) assert(0, "out of memory in `getUserHomePath()`");
      unamez[0..username.length+1] = 0;
      unamez[0..username.length] = username[];
    }

    // Reserve C memory for the getpwnam_r() function.
    uint infoBufSize = 5 * 1024;
    char* infoBuf = null;
    scope(exit) if (infoBuf !is null) free(infoBuf);

    passwd result;
    for (;;) {
      infoBuf = cast(char*)realloc(infoBuf, infoBufSize*char.sizeof);
      if (infoBuf is null) assert(0, "out of memory in `getUserHomePath()`");
      passwd* verify = null;
      errno = 0;
      auto xres = (unamez !is null ?
        getpwnam_r(unamez, &result, infoBuf, infoBufSize, &verify) :
        getpwuid_r(euid, &result, infoBuf, infoBufSize, &verify));
      if (xres == 0) {
        // succeeded if verify points at result
        if (verify == &result) {
          if (result.pw_dir !is null && result.pw_dir[0]) return putAsciiz(result.pw_dir);
          return putAsciiz("./"); // last resort
        }
      }
      if (errno != ERANGE && errno != 0) assert(0, "out of memory in `getUserHomePath()`");
      import core.checkedint : mulu;
      bool overflow;
      infoBufSize = mulu(infoBufSize, 2, overflow);
      if (overflow) assert(0);
    }
    assert(0, "wtf?!");
  } else {
    return putAsciiz("./"); // nobody cares
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/// expands '~...' path to real path.
/// returns new path in `destBuf` or `null` if there is no room in `destBuf`.
/// `destBuf` cannot be the same as `inputPath`!
public char[] expandTilde (char[] destBuf, const(char)[] inputPath) nothrow @trusted {
  if (destBuf.length == 0) {
    if (inputPath.length == 0) return destBuf;
    if (destBuf.length < inputPath.length) return null; // no room anyway
    assert(destBuf.ptr !is inputPath.ptr); // just in case
  }

  if (inputPath[0] != '~') {
    // nothing to do
    destBuf[0..inputPath.length] = inputPath[];
    version(Posix) {} else {
      foreach (ref char ch; destBuf[0..inputPath.length]) if (ch == '\\') ch = '/';
      //if (inputPath.length > 2 && inputPath[1] == ':' && inputPath[2] == '/') destBuf[2] = '\\';
    }
    return destBuf[0..inputPath.length];
  }

  usize pathpos = 1;
  version(Posix) {
    while (pathpos < inputPath.length && inputPath.ptr[pathpos] != '/') ++pathpos;
  } else {
    while (pathpos < inputPath.length && inputPath.ptr[pathpos] != '/' && inputPath.ptr[pathpos] != '\\') ++pathpos;
  }
  const(char)[] uname = inputPath[1..pathpos];
  if (pathpos < inputPath.length) { assert(inputPath[pathpos] == '/' || inputPath[pathpos] == '\\'); ++pathpos; }
  // put path into destDir
  auto hp = getUserHomePath(destBuf, uname);
  if (hp is null) return null;
  // append rest of the path
  if (pathpos >= inputPath.length) return hp;
  immutable irest = inputPath.length-pathpos;
  if (destBuf.length-hp.length < irest) return null; // oops, no room
  destBuf[hp.length..hp.length+irest] = inputPath[pathpos..$];
  version(Posix) {} else {
    foreach (ref char ch; destBuf[0..hp.length+irest]) if (ch == '\\') ch = '/';
    //if (inputPath.length > 2 && inputPath[1] == ':' && inputPath[2] == '/') destBuf[2] = '\\';
  }
  return destBuf[0..hp.length+irest];
}


// ////////////////////////////////////////////////////////////////////////// //
inout(char)[] removeExtension (inout(char)[] fn) pure nothrow @trusted @nogc {
  foreach_reverse (immutable cidx, char ch; fn) {
    version(Posix) {
      if (ch == '/') break;
    } else {
      if (ch == '/' || ch == '\\' || ch == ':') break;
    }
    if (ch == '.') { fn = fn[0..cidx]; break; }
  }
  return fn;
}


inout(char)[] getExtension (inout(char)[] fn) pure nothrow @trusted @nogc {
  foreach_reverse (immutable cidx, char ch; fn) {
    version(Posix) {
      if (ch == '/') return null;
    } else {
      if (ch == '/' || ch == '\\' || ch == ':') return null;
    }
    if (ch == '.') {
      if (cidx == fn.length) return null;
      return fn[cidx..$];
    }
  }
  return null;
}


inout(char)[] baseName (inout(char)[] fn) pure nothrow @trusted @nogc {
  foreach_reverse (immutable cidx, char ch; fn) {
    version(Posix) {
      if (ch == '/') return fn[cidx+1..$];
    } else {
      if (ch == '/' || ch == '\\' || ch == ':') return fn[cidx+1..$];
    }
  }
  return fn;
}


// without last '/' (if it is not a root dir)
// also, returns null if there is no dir (not ".")
inout(char)[] dirName (inout(char)[] fn) pure nothrow @trusted @nogc {
  foreach_reverse (immutable cidx, char ch; fn) {
    version(Posix) {
      if (ch == '/') return (cidx != 0 ? fn[0..cidx] : fn[0..1]);
    } else {
      // this is wrong, but idc
      if (ch == '/' || ch == '\\' || ch == ':') return fn[0..cidx];
    }
  }
  return null;
}
