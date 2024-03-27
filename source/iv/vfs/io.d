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
/**
 * wrap any low-level (or high-level) stream into refcounted struct.
 * this struct can be used instead of `std.stdio.File` when you need
 * a concrete type instead of working with generic stream templates.
 * wrapped stream is thread-safe (i.e. reads, writes, etc), but
 * wrapper itself isn't.
 */
module iv.vfs.io /*is aliced*/;
private:

public import iv.vfs;
import iv.alice;


// ////////////////////////////////////////////////////////////////////////// //
// aaaaah, let's conflict with std.stdio!
public __gshared VFile stdin, stdout, stderr;

shared static this () {
  debug(vfs_rc) { import core.stdc.stdio : printf; printf("******** SHARED CTOR FOR iv.vfs.io\n"); }
  stdin = wrapStdin;
  stdout = wrapStdout;
  stderr = wrapStderr;
}


// ////////////////////////////////////////////////////////////////////////// //
public auto readTextFile(T=string) (const(char)[] fname, int maxlen=int.max/8)
if (is(T == string) || is(T == const(char)[]) || is(T == char[]) || is(T == const char[]))
{
  return readTextFile(VFile(fname), maxlen);
}


//TODO: fallback to chunked reader if `fl.hasSize` is false
public auto readTextFile(T=string) (VFile fl, int maxlen=int.max/8)
if (is(T == string) || is(T == const(char)[]) || is(T == char[]) || is(T == const char[]) ||
    is(T == immutable(ubyte)[]) || is(T == const(ubyte)[]) || is(T == ubyte[]) || is(T == const ubyte[]))
{
  auto fpos = fl.tell;
  auto flen = fl.size-fpos;
  if (flen < 1) return cast(T)null;
  if (flen > maxlen) throw new Exception("text file too big");
  auto res = new char[](cast(int)flen);
  scope(failure) delete res; // make GC life easier
  fl.rawReadExact(res);
  return cast(T)res; // it is safe to cast here
}


// ////////////////////////////////////////////////////////////////////////// //
/// read 0-terminated string from stream. very slow, no recoding.
/// eolhit will be set on EOF too.
public string readZString(ST) (auto ref ST fl, bool* eolhit=null, usize maxSize=1024*1024) if (isReadableStream!ST) {
  bool eh;
  if (eolhit is null) eolhit = &eh;
  *eolhit = false;
  if (maxSize == 0) return null;
  char[] res;
  ubyte ch;
  for (;;) {
    if (fl.rawRead((&ch)[0..1]).length == 0) { *eolhit = true; break; }
    if (ch == 0) { *eolhit = true; break; }
    if (maxSize == 0) break;
    res ~= ch;
    --maxSize;
  }
  return cast(string)res; // it is safe to cast here
}


// ////////////////////////////////////////////////////////////////////////// //
/// read stream line by line. very slow, no recoding.
// hack around "has scoped destruction, cannot build closure"
public auto byLineCopy(bool keepTerm=false) (VFile fl) { return byLineCopyImpl!keepTerm(fl); }
public auto byLineCopy(bool keepTerm=false, ST) (auto ref ST fl) if (!is(ST == VFile) && isRorWStream!ST) { return byLineCopyImpl!keepTerm(fl); }

public auto byLine(bool keepTerm=false) (VFile fl) { return byLineCopyImpl!(keepTerm, true)(fl); }
public auto byLine(bool keepTerm=false, ST) (auto ref ST fl) if (!is(ST == VFile) && isRorWStream!ST) { return byLineCopyImpl!(keepTerm, true)(fl); }

private auto byLineCopyImpl(bool keepTerm=false, bool reuseBuffer=false, ST) (auto ref ST fl) {
  static struct BLR(bool keepTerm, bool reuse, ST) {
  private:
    ST st;
    bool eof, futureeof;
    char[256] rdbuf;
    int rdpos, rdsize;
    char[] buf;
    usize bufused;
    static if (!reuse) string s;
  private:
    this() (auto ref ST ast) {
      st = ast;
      if (st.eof) eof = true; else popFront();
    }
  public:
    @property bool empty () const pure nothrow @safe @nogc { return eof; }
    @property auto front () inout nothrow @trusted @nogc { static if (reuse) return buf.ptr[0..bufused]; else return s; }
    void popFront () {
      char getch () {
        assert(!futureeof);
        if (rdpos >= rdsize) {
          rdpos = rdsize = 0;
          auto rd = st.rawRead(rdbuf[]);
          if (rd.length == 0) { futureeof = true; return 0; }
          rdsize = cast(int)rd.length;
        }
        assert(rdpos < rdsize);
        return rdbuf.ptr[rdpos++];
      }
      void putch (char ch) {
        if (bufused == buf.length) {
          auto optr = buf.ptr;
          buf ~= ch;
          if (buf.ptr !is optr) {
            import core.memory : GC;
            if (buf.ptr is GC.addrOf(buf.ptr)) GC.setAttr(buf.ptr, GC.BlkAttr.NO_INTERIOR);
          }
          ++bufused;
        } else {
          buf.ptr[bufused++] = ch;
        }
      }
      if (futureeof) { eof = true; futureeof = false; }
      bufused = 0;
      static if (!reuse) s = null;
      if (eof) return;
      bool wasChar = false;
      for (;;) {
        char ch = getch();
        if (futureeof) break;
        wasChar = true;
        if (ch == '\r') {
          // cr
          ch = getch();
          if (futureeof) {
            static if (keepTerm) putch('\r');
            break;
          }
          if (ch == '\n') {
            static if (keepTerm) { putch('\r'); putch('\n'); }
            break;
          }
        } else if (ch == '\n') {
          // lf
          static if (keepTerm) putch('\n');
          break;
        }
        putch(ch);
      }
      if (!wasChar) {
        assert(futureeof);
        futureeof = false;
        eof = true;
      } else {
        static if (!reuse) s = (bufused ? buf.ptr[0..bufused].idup : "");
      }
    }
  }
  return BLR!(keepTerm, reuseBuffer, ST)(fl);
}


// ////////////////////////////////////////////////////////////////////////// //
public auto readf(Char:dchar, A...) (VFile fl, const(Char)[] fmt, A args) { return readfImpl!(Char)(fl, fmt, args); }
public auto readf(ST, Char:dchar, A...) (auto ref ST fl, const(Char)[] fmt, A args) if (!is(ST == VFile) && isRorWStream!ST) { return readfImpl!(Char, ST)(fl, fmt, args); }

private auto readfImpl(Char:dchar, ST, A...) (auto ref ST fl, const(Char)[] fmt, A args) {
  import std.format : formattedRead;
  static struct Reader(ST) {
    ST fl;
    char ch;
    bool eof;
    this() (auto ref ST afl) { fl = afl; if (fl.eof) eof = true; else popFront(); }
    @property bool empty () const pure nothrow @safe @nogc { return eof; }
    @property char front () const pure nothrow @safe @nogc { return ch; }
    void popFront() { if (!eof) { eof = (fl.rawRead((&ch)[0..1]).length == 0); } }
  }
  auto rd = Reader!ST(fl);
  return formattedRead(rd, fmt, args);
}


// ////////////////////////////////////////////////////////////////////////// //
public string readln() (VFile fl) { return readlnImpl(fl); }
public string readln(ST) (auto ref ST fl) if (!is(ST == VFile) && isRorWStream!ST) { return readlnImpl!ST(fl); }
public string readln() () { return readlnImpl(stdin); }

// slow, but IDC
private string readlnImpl(ST) (auto ref ST fl) {
  enum MaxLen = 65536;
  if (fl.eof) return null;
  char[] res;
  char ch;
  for (;;) {
    if (fl.rawRead((&ch)[0..1]).length != 1) break;
    if (ch == '\n') break;
    if (ch == '\r') {
      if (fl.rawRead((&ch)[0..1]).length != 1) break;
      if (ch == '\n') break;
      if (res.length == MaxLen) throw new Exception("line too long");
      res ~= '\r';
    }
    if (res.length == MaxLen) throw new Exception("line too long");
    res ~= ch;
  }
  return cast(string)res; // it is safe to cast here
}


// ////////////////////////////////////////////////////////////////////////// //
void writeImpl(S...) (ref VFile.UnlockedWriterImpl wr, S args) {
  import std.traits : isBoolean, isIntegral, isAggregateType, isSomeString, isSomeChar;
  foreach (/*auto*/ arg; args) {
    alias A = typeof(arg);
    static if (isAggregateType!A || is(A == enum)) {
      import std.format : formattedWrite;
      formattedWrite(wr, "%s", arg);
    } else static if (isSomeString!A) {
      wr.put(arg);
    } else static if (isIntegral!A) {
      import std.conv : toTextRange;
      toTextRange(arg, wr);
    } else static if (isBoolean!A) {
      wr.put(arg ? "true" : "false");
    } else static if (isSomeChar!A) {
      wr.put(arg);
    } else {
      import std.format : formattedWrite;
      formattedWrite(wr, "%s", arg);
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
public void write(A...) (A args) {
  import std.traits : isAggregateType;
  static if (A.length == 0) {
    // do nothing
  } else static if (is(A[0] == VFile)) {
    // fl.write(...)
    static if (A.length == 1) {
      // nothing to do
    } else static if (A.length == 2 &&
                      is(typeof(args[1]) : const(char)[]) &&
                      !is(typeof(args[1]) == enum) &&
                      !is(Unqual!(typeof(args[1])) == typeof(null)) &&
                      !isAggregateType!(typeof(args[1])))
    {
      import std.traits : isStaticArray;
      // specialization for strings -- a very frequent case
      synchronized(args[0].wst) {
        //auto wr = args[0].lockedWriter;
        auto wr = args[0].unlockedWriter;
        static if (isStaticArray!(typeof(args[1]))) {
          wr.put(args[1][]);
        } else {
          wr.put(args[1]);
        }
      }
    } else {
      synchronized(args[0].wst) {
        //auto wr = args[0].lockedWriter;
        auto wr = args[0].unlockedWriter;
        writeImpl(wr, args[1..$]);
      }
    }
  } else static if (A.length == 1 &&
                    is(typeof(args[0]) : const(char)[]) &&
                    !is(typeof(args[0]) == enum) &&
                    !is(Unqual!(typeof(args[0])) == typeof(null)) &&
                    !isAggregateType!(typeof(args[0])))
  {
    import std.traits : isStaticArray;
    synchronized(stdout.wst) {
      //auto wr = stdout.lockedWriter;
      auto wr = stdout.unlockedWriter;
      // specialization for strings -- a very frequent case
      static if (isStaticArray!(typeof(args[0]))) {
        wr.put(args[0][]);
      } else {
        wr.put(args[0]);
      }
    }
  } else {
    synchronized(stdout.wst) {
      //auto wr = stdout.lockedWriter;
      auto wr = stdout.unlockedWriter;
      writeImpl(wr, args);
    }
  }
}

public void writeln(A...) (A args) { .write(args, "\n"); }


// ////////////////////////////////////////////////////////////////////////// //
public void writef(Char:dchar, A...) (VFile fl, const(Char)[] fmt, A args) { if (!fl.isOpen) throw new VFSException("can't write to closed stream"); import std.format : formattedWrite; synchronized(fl.wst) { auto wr = fl.unlockedWriter; formattedWrite(wr, fmt, args); } }
public void writef(Char:dchar, A...) (const(Char)[] fmt, A args) { if (!stdout.isOpen) return; import std.format : formattedWrite; synchronized(stdout.wst) { auto wr = stdout.lockedWriter; formattedWrite(wr, fmt, args); } }

public void writefln(Char:dchar, A...) (VFile fl, const(Char)[] fmt, A args) { if (!fl.isOpen) throw new VFSException("can't write to closed stream"); import std.format : formattedWrite; synchronized(fl.wst) { auto wr = fl.lockedWriter; formattedWrite(wr, fmt, args); wr.put("\n"); } }
public void writefln(Char:dchar, A...) (const(Char)[] fmt, A args) { if (!stdout.isOpen) return; import std.format : formattedWrite; synchronized(stdout.wst) { auto wr = stdout.lockedWriter; formattedWrite(wr, fmt, args); wr.put("\n"); } }
