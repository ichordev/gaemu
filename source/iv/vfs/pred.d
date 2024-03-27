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
// stream predicates
module iv.vfs.pred /*is aliced*/;

public import iv.alice;
public import iv.vfs.types : Seek;
public import iv.vfs.error;
public import iv.vfs.vfile : IVVFSIgnore;


// ////////////////////////////////////////////////////////////////////////// //
/// is this "low-level" stream that can be read?
enum isLowLevelStreamR(T) = is(typeof((inout int=0) {
  auto t = T.init;
  ubyte[1] b;
  ssize r = t.read(b.ptr, 1);
}));

/// is this "low-level" stream that can be written?
enum isLowLevelStreamW(T) = is(typeof((inout int=0) {
  auto t = T.init;
  ubyte[1] b;
  ssize w = t.write(b.ptr, 1);
}));


/// is this "low-level" stream that can be seeked?
enum isLowLevelStreamS(T) = is(typeof((inout int=0) {
  auto t = T.init;
  long p = t.lseek(0, 0);
}));


// ////////////////////////////////////////////////////////////////////////// //
/// check if a given stream supports `eof`
enum streamHasEof(T) = is(typeof((inout int=0) {
  auto t = T.init;
  bool n = t.eof;
}));

/// check if a given stream supports `seek`
enum streamHasSeek(T) = is(typeof((inout int=0) {
  import core.stdc.stdio : SEEK_END;
  auto t = T.init;
  t.seek(0);
  t.seek(0, SEEK_END);
}));

/// check if a given stream supports `tell`
enum streamHasTell(T) = is(typeof((inout int=0) {
  auto t = T.init;
  long pos = t.tell;
}));

/// check if a given stream supports `tell`
enum streamHasClose(T) = is(typeof((inout int=0) {
  auto t = T.init;
  t.close();
}));

/// check if a given stream supports `name`
enum streamHasName(T) = is(typeof((inout int=0) {
  auto t = T.init;
  const(char)[] n = t.name;
}));

/// check if a given stream supports `size`
enum streamHasSize(T) = is(typeof((inout int=0) {
  auto t = T.init;
  long pos = t.size;
}));

/// check if a given stream supports `size`
enum streamHasSizeLowLevel(T) = is(typeof((inout int=0) {
  auto t = T.init;
  long pos = t.getsize;
}));

/// check if a given stream supports `isOpen`
enum streamHasIsOpen(T) = is(typeof((inout int=0) {
  auto t = T.init;
  bool op = t.isOpen;
}));

/// check if a given stream supports `flush()`
enum streamHasFlush(T) = is(typeof((inout int=0) {
  auto t = T.init;
  t.flush();
}));

// ////////////////////////////////////////////////////////////////////////// //
/// check if a given stream supports `rawRead()`.
/// it's enough to support `void[] rawRead (void[] buf)`
enum isReadableStream(T) = is(typeof((inout int=0) {
  auto t = T.init;
  ubyte[1] b;
  auto v = cast(void[])b;
  t.rawRead(v);
}));

/// check if a given stream supports `rawWrite()`.
/// it's enough to support `inout(void)[] rawWrite (inout(void)[] buf)`
enum isWriteableStream(T) = is(typeof((inout int=0) {
  auto t = T.init;
  ubyte[1] b;
  t.rawWrite(cast(void[])b);
}));

/// check if a given stream supports both reading and writing
enum isRWStream(T) = isReadableStream!T && isWriteableStream!T;

/// check if a given stream supports both reading and writing
enum isRorWStream(T) = isReadableStream!T || isWriteableStream!T;

/// check if a given stream supports `.seek(ofs, [whence])`, and `.tell`
enum isSeekableStream(T) = (streamHasSeek!T && streamHasTell!T);

/// check if we can get size of a given stream.
/// this can be done either with `.size`, or with `.seek` and `.tell`
enum isSizedStream(T) = (streamHasSize!T || isSeekableStream!T);


// ////////////////////////////////////////////////////////////////////////// //
version(vfs_test_stream) {
  import std.stdio;
  static assert(isReadableStream!File);
  static assert(isWriteableStream!File);
  static assert(isRWStream!File);
  static assert(isSeekableStream!File);
  static assert(streamHasEof!File);
  static assert(streamHasSeek!File);
  static assert(streamHasTell!File);
  static assert(streamHasName!File);
  static assert(streamHasSize!File);
  struct S {}
  static assert(!isReadableStream!S);
  static assert(!isWriteableStream!S);
  static assert(!isRWStream!S);
  static assert(!isSeekableStream!S);
  static assert(!streamHasEof!S);
  static assert(!streamHasSeek!S);
  static assert(!streamHasTell!S);
  static assert(!streamHasName!S);
  static assert(!streamHasSize!S);
}


// ////////////////////////////////////////////////////////////////////////// //
/// augment low-level streams with `rawRead`
T[] rawRead(ST, T) (auto ref ST st, T[] buf) if (isLowLevelStreamR!ST && !is(T == const) && !is(T == immutable)) {
  if (buf.length > 0) {
    auto res = st.read(buf.ptr, buf.length*T.sizeof);
    if (res == -1 || res%T.sizeof != 0) throw new VFSException("read error");
    return buf[0..res/T.sizeof];
  } else {
    return buf[0..0];
  }
}

/// augment low-level streams with `rawWrite`
void rawWrite(ST, T) (auto ref ST st, in T[] buf) if (isLowLevelStreamW!ST) {
  if (buf.length > 0) {
    auto res = st.write(buf.ptr, buf.length*T.sizeof);
    if (res == -1 || res%T.sizeof != 0) throw new VFSException("write error");
  }
}

/// read exact size or throw error
T[] rawReadExact(ST, T) (auto ref ST st, T[] buf) if (isReadableStream!ST && !is(T == const) && !is(T == immutable)) {
  if (buf.length == 0) return buf;
  auto left = buf.length*T.sizeof;
  auto dp = cast(ubyte*)buf.ptr;
  while (left > 0) {
    auto res = st.rawRead(cast(void[])(dp[0..left]));
    if (res.length == 0) throw new VFSException("read error");
    dp += res.length;
    left -= res.length;
  }
  return buf;
}

/// write exact size or throw error (just for convenience)
void rawWriteExact(ST, T) (auto ref ST st, in T[] buf) if (isWriteableStream!ST) { st.rawWrite(buf); }

/// if stream doesn't have `.size`, but can be seeked, emulate it
long size(ST) (auto ref ST st) if (isSeekableStream!ST && !streamHasSize!ST) {
  auto opos = st.tell;
  st.seek(0, Seek.End);
  auto res = st.tell;
  st.seek(opos);
  return res;
}


// ////////////////////////////////////////////////////////////////////////// //
public enum isGoodEndianness(string s) = (s == "LE" || s == "le" || s == "BE" || s == "be");

public template isLittleEndianness(string s) if (isGoodEndianness!s) {
  enum isLittleEndianness = (s == "LE" || s == "le");
}

public template isBigEndianness(string s) if (isGoodEndianness!s) {
  enum isLittleEndianness = (s == "BE" || s == "be");
}

public template isSystemEndianness(string s) if (isGoodEndianness!s) {
  version(LittleEndian) {
    enum isSystemEndianness = isLittleEndianness!s;
  } else {
    enum isSystemEndianness = isBigEndianness!s;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
/// write integer value of the given type, with the given endianness (default: little-endian)
/// usage: st.writeNum!ubyte(10)
void writeNum(T, string es="LE", ST) (auto ref ST st, T n) if (isGoodEndianness!es && isWriteableStream!ST && __traits(isIntegral, T)) {
  static assert(T.sizeof <= 8); // just in case
  static if (isSystemEndianness!es || T.sizeof == 1) {
    st.rawWriteExact((&n)[0..1]);
  } else {
    ubyte[T.sizeof] b = void;
    version(LittleEndian) {
      // convert to big-endian
      foreach_reverse (ref x; b) { x = n&0xff; n >>= 8; }
    } else {
      // convert to little-endian
      foreach (ref x; b) { x = n&0xff; n >>= 8; }
    }
    st.rawWriteExact(b[]);
  }
}


/// read integer value of the given type, with the given endianness (default: little-endian)
/// usage: auto v = st.readNum!ubyte
T readNum(T, string es="LE", ST) (auto ref ST st) if (isGoodEndianness!es && isReadableStream!ST && __traits(isIntegral, T)) {
  static assert(T.sizeof <= 8); // just in case
  static if (isSystemEndianness!es || T.sizeof == 1) {
    T v = void;
    st.rawReadExact((&v)[0..1]);
    return v;
  } else {
    ubyte[T.sizeof] b = void;
    st.rawReadExact(b[]);
    T v = 0;
    version(LittleEndian) {
      // convert from big-endian
      foreach (ubyte x; b) { v <<= 8; v |= x; }
    } else {
      // conver from little-endian
      foreach_reverse (ubyte x; b) { v <<= 8; v |= x; }
    }
    return v;
  }
}


private enum reverseBytesMixin = "
  foreach (idx; 0..b.length/2) {
    ubyte t = b[idx];
    b[idx] = b[$-idx-1];
    b[$-idx-1] = t;
  }
";


/// write floating value of the given type, with the given endianness (default: little-endian)
/// usage: st.writeNum!float(10)
void writeNum(T, string es="LE", ST) (auto ref ST st, T n) if (isGoodEndianness!es && isWriteableStream!ST && __traits(isFloating, T)) {
  static assert(T.sizeof <= 8);
  static if (isSystemEndianness!es) {
    st.rawWriteExact((&n)[0..1]);
  } else {
    import core.stdc.string : memcpy;
    ubyte[T.sizeof] b = void;
    memcpy(b.ptr, &v, T.sizeof);
    mixin(reverseBytesMixin);
    st.rawWriteExact(b[]);
  }
}


/// read floating value of the given type, with the given endianness (default: little-endian)
/// usage: auto v = st.readNum!float
T readNum(T, string es="LE", ST) (auto ref ST st) if (isGoodEndianness!es && isReadableStream!ST && __traits(isFloating, T)) {
  static assert(T.sizeof <= 8);
  T v = void;
  static if (isSystemEndianness!es) {
    st.rawReadExact((&v)[0..1]);
  } else {
    import core.stdc.string : memcpy;
    ubyte[T.sizeof] b = void;
    st.rawReadExact(b[]);
    mixin(reverseBytesMixin);
    memcpy(&v, b.ptr, T.sizeof);
  }
  return v;
}


// ////////////////////////////////////////////////////////////////////////// //
// first byte: bit 7 is sign; bit 6 is "has more bytes" mark; bits 0..5: first number bits
// next bytes: bit 7 is "has more bytes" mark; bits 0..6: next number bits
void writeXInt(T : ulong, ST) (auto ref ST fl, T vv) if (isWriteableStream!ST) {
  ubyte[16] buf = void; // actually, 10 is enough ;-)
       static if (T.sizeof == ulong.sizeof) ulong v = cast(ulong)vv;
  else static if (!__traits(isUnsigned, T)) ulong v = cast(ulong)cast(long)vv; // extend sign bits
  else ulong v = cast(ulong)vv;
  uint len = 1; // at least
  // now write as signed
  if (v == 0x8000_0000_0000_0000UL) {
    // special (negative zero)
    buf.ptr[0] = 0x80;
  } else {
    if (v&0x8000_0000_0000_0000UL) {
      v = (v^~0uL)+1; // negate v
      buf.ptr[0] = 0x80; // sign bit
    } else {
      buf.ptr[0] = 0;
    }
    buf.ptr[0] |= v&0x3f;
    v >>= 6;
    if (v != 0) buf.ptr[0] |= 0x40; // has more
    while (v != 0) {
      buf.ptr[len] = v&0x7f;
      v >>= 7;
      if (v > 0) buf.ptr[len] |= 0x80; // has more
      ++len;
    }
  }
  fl.rawWriteExact(buf.ptr[0..len]);
}


T readXInt(T : ulong, ST) (auto ref ST fl) if (isReadableStream!ST) {
  import std.conv : ConvOverflowException;
  ulong v = 0;
  ubyte c = void;
  // first byte contains sign flag
  fl.rawReadExact((&c)[0..1]);
  if (c == 0x80) {
    // special (negative zero)
    v = 0x8000_0000_0000_0000UL;
  } else {
    bool neg = ((c&0x80) != 0);
    v = c&0x3f;
    c <<= 1;
    // 63/7 == 9, so we can shift at most 56==(7*8) bits
    ubyte shift = 6;
    while (c&0x80) {
      if (shift > 62) throw new ConvOverflowException("readXInt overflow");
      fl.rawReadExact((&c)[0..1]);
      ulong n = c&0x7f;
      if (shift == 62 && n > 1) throw new ConvOverflowException("readXInt overflow");
      n <<= shift;
      v |= n;
      shift += 7;
    }
    if (neg) v = (v^~0uL)+1; // negate v
  }
  // now convert to output
  static if (T.sizeof == v.sizeof) {
    return v;
  } else static if (!__traits(isUnsigned, T)) {
    auto l = cast(long)v;
    if (v < T.min) throw new ConvOverflowException("readXInt underflow");
    if (v > T.max) throw new ConvOverflowException("readXInt overflow");
    return cast(T)l;
  } else {
    if (v > T.max) throw new ConvOverflowException("readXInt overflow");
    return cast(T)v;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
void readStruct(string es="LE", SS, ST) (auto ref ST fl, ref SS st)
if (is(SS == struct) && isGoodEndianness!es && isReadableStream!ST)
{
  import iv.vfs.vfile : VFile;
  static assert(!is(SS == VFile), "invalid argument order in `readStruct`");
  void unserData(T) (ref T v) {
    import std.traits : Unqual;
    alias UT = Unqual!T;
    static if (is(T : V[], V)) {
      // array
      static if (__traits(isStaticArray, T)) {
        foreach (ref it; v) unserData(it);
      } else static if (is(UT == char)) {
        // special case: dynamic `char[]` array will be loaded as asciiz string
        char c;
        for (;;) {
          if (fl.rawRead((&c)[0..1]).length == 0) break; // don't require trailing zero on eof
          if (c == 0) break;
          v ~= c;
        }
      } else {
        assert(0, "cannot load dynamic arrays yet");
      }
    } else static if (is(T : V[K], K, V)) {
      assert(0, "cannot load associative arrays yet");
    } else static if (__traits(isIntegral, UT) || __traits(isFloating, UT)) {
      // this takes care of `*char` and `bool` too
      v = cast(UT)fl.readNum!(UT, es);
    } else static if (is(T == struct)) {
      // struct
      import std.traits : FieldNameTuple, hasUDA;
      foreach (string fldname; FieldNameTuple!T) {
        static if (!hasUDA!(__traits(getMember, T, fldname), IVVFSIgnore)) {
          unserData(__traits(getMember, v, fldname));
        }
      }
    }
  }

  unserData(st);
}
