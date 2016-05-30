/* GML runner
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
module gaem.runner.strpool is aliced;


// ////////////////////////////////////////////////////////////////////////// //
private:

struct Str {
  string val; // string value
  uint rc; // refcount; <0: persistent string; also serves as free list index with 31 bit set
}

__gshared Str[] spool; // pool of strings
__gshared uint spoolFree = 0x8000_0000; // none


shared static this () {
  // preallocate small strings
  spool ~= Str("", 0);
  foreach (ubyte c; 0..256) spool ~= Str(""~cast(char)c, 0);
}


// ////////////////////////////////////////////////////////////////////////// //
public:

uint newInternalStr(T) (T str) if (is(T : const(char)[])) {
  if (str.length == 0) return 0;
  if (str.length == 1) return cast(uint)str.ptr[0]+1;
  //FIXME: speed this up!
  foreach (immutable idx, ref st; spool) {
    if (st.val == str) return cast(uint)idx;
  }
  static if (is(T == string)) alias sv = str; else auto sv = str.idup;
  // allocate new
  auto sid = cast(uint)spool.length;
  if (sid > 0x3F_FFFF) assert(0, "too many strings");
  spool ~= Str(sv, 0);
  return sid;
}


// returnted string has rc of 1
uint newDynStr(T) (T str) if (is(T : const(char)[])) {
  if (str.length == 0) return 0;
  if (str.length == 1) return cast(uint)str.ptr[0]+1;
  static if (is(T == string)) alias sv = str; else auto sv = str.idup;
  if (spoolFree&0x7fff_ffff) {
    // reuse existing
    auto sid = spoolFree&0x7fff_ffff;
    auto ss = spool.ptr+sid;
    spoolFree = ss.rc;
    ss.val = sv;
    ss.rc = 1;
    return sid;
  } else {
    // allocate new
    auto sid = cast(uint)spool.length;
    if (sid > 0x3F_FFFF) assert(0, "too many dynamic strings");
    spool ~= Str(sv, 1);
    return sid;
  }
}


void dynStrIncRef (uint sid) {
  pragma(inline, true);
  if (sid < spool.length && spool.ptr[sid].rc > 0) {
    assert(spool.ptr[sid].rc < 0x8000_0000);
    ++spool.ptr[sid].rc;
  }
}


void dynStrDecRef (uint sid) {
  pragma(inline, true);
  if (sid < spool.length && spool.ptr[sid].rc > 0) {
    assert(spool.ptr[sid].rc < 0x8000_0000);
    if (--spool.ptr[sid].rc == 0) {
      spool.ptr[sid].rc = spoolFree;
      spoolFree = sid|0x8000_0000;
    }
  }
}


string getDynStr (uint sid) {
  pragma(inline, true);
  return (sid < spool.length && spool.ptr[sid].rc < 0x8000_0000 ? spool.ptr[sid].val : null);
}
