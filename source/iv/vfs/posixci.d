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
module iv.vfs.posixci /*is aliced*/;

import iv.alice;
import iv.vfs.config;

static if (VFS_NORMAL_OS) {

import core.sys.posix.dirent;
import iv.vfs.koi8;


// `name` will be modified
package bool findFileCI (const(char)[] path, char[] name, bool asDir) nothrow @nogc {
  import core.sys.posix.dirent;
  import core.stdc.stdlib : alloca;
  if (path.length == 0) path = ".";
  while (path.length && path[$-1] == '/') path = path[0..$-1];
  if (path.length == 0) path = "/";
  if (path.length+name.length > 4094) return false;
  char* tmpbuf = cast(char*)alloca(path.length+name.length+2);
  tmpbuf[0..path.length] = path[];
  // check if we have perfect match
  {
    import core.sys.posix.sys.stat;
    tmpbuf[path.length] = '/';
    tmpbuf[path.length+1..path.length+1+name.length] = name[];
    tmpbuf[path.length+1+name.length] = '\0';
    stat_t st = void;
    auto res = stat(tmpbuf, &st);
    if (res == 0) {
      // the file is here
      if ((asDir && (st.st_mode&(S_IFDIR|S_IFLNK)) != 0) || (!asDir && (st.st_mode&(S_IFREG|S_IFLNK)) != 0)) {
        return true;
      }
    }
  }
  // vanilla dmd has bug in dirent definition: it's not aligned
  /*static if (dirent.d_name.offsetof == 19)*/ {
    tmpbuf[path.length] = '\0';
    auto dir = opendir(tmpbuf);
    if (dir is null) return false;
    scope(exit) closedir(dir);
    for (;;) {
      auto de = readdir(dir);
      if (de is null) break;
      //{ import core.stdc.stdio : printf; printf("[%s]\n", de.d_name.ptr); }
      const(char)[] dename;
      uint pos = 0;
      while (pos < de.d_name.length && de.d_name.ptr[pos]) ++pos;
      dename = de.d_name[0..pos];
      if (dename != "." && dename != ".." && koi8StrCaseEqu(name, dename)) {
        // i found her... maybe
        import core.sys.posix.sys.stat;
        tmpbuf[path.length] = '/';
        tmpbuf[path.length+1..path.length+1+dename.length] = dename[];
        tmpbuf[path.length+1+dename.length] = '\0';
        stat_t st = void;
        auto res = stat(tmpbuf, &st);
        if (res == 0) {
          // the file is here
          if ((asDir && (st.st_mode&(S_IFDIR|S_IFLNK)) != 0) || (!asDir && (st.st_mode&(S_IFREG|S_IFLNK)) != 0)) {
            name[] = dename[];
            return true;
          }
        }
      }
    }
  }
  return false;
}


// `path` will be modified; returns new path (slice of `path`) or `null` (and `path` is not modified)
package char[] dirNormalize (char[] path) nothrow @nogc {
  char[1024*3] tmpbuf;
  uint tpos, ppos;
  if (path.length && path[0] == '/') tmpbuf.ptr[tpos++] = '/';
  while (ppos < path.length) {
    if (path.ptr[ppos] == '/') {
      while (ppos < path.length && path.ptr[ppos] == '/') ++ppos;
      continue;
    }
    uint pend = ppos;
    while (pend < path.length && path.ptr[pend] != '/') ++pend;
    assert(pend > ppos);
    if (pend < path.length) ++pend;
    const(char)[] nm = path[ppos..pend];
    if (nm == "../" || nm == "..") {
      if (tpos == 0) return null;
      --tpos; // skip slash
      while (tpos > 0 && tmpbuf.ptr[tpos-1] != '/') --tpos;
      //if (tpos == 0) return null;
    } else if (nm != "./" && nm != ".") {
      if (tpos+nm.length >= tmpbuf.length) return null;
      tmpbuf[tpos..tpos+nm.length] = nm[];
      tpos += pend-ppos;
    }
    ppos = pend;
  }
  if (tpos == 0) return null;
  if (tpos > 1 && tmpbuf.ptr[tpos-1] == '/') --tpos;
  path[0..tpos] = tmpbuf[0..tpos];
  return path[0..tpos];
}


// `asDir`: should last element be threated as directory?
// `path` will be modified; returns new path (slice of `path`) or `null`
package char[] findPathCI (char[] path, bool asDir=false) nothrow @nogc {
  path = dirNormalize(path);
  if (path is null) return null;
  if (path.length == 0) return null;
  uint ppos = 0;
  while (ppos < path.length) {
    // first slash?
    if (ppos == 0 && path[0] == '/') {
      ++ppos;
      continue;
    }
    uint pend = ppos;
    while (pend < path.length && path.ptr[pend] != '/') ++pend;
    assert(pend > ppos);
    if (!findFileCI(path[0..ppos], path[ppos..pend], (pend < path.length ? true : asDir))) return null;
    ppos = pend+1;
  }
  return path;
}

}


/*
void main () {
  import std.stdio;
  {
    //char[] path = "//foo/bar////./z/./../../..".dup;
    char[] path = "//foo/bar////./z/./././..".dup;
    writeln(findPathCI(path));
  }
  {
    char[] name = "PosixCI.D".dup;
    writeln(findPathCI(name));
  }
  {
    char[] name = "SaMpLES".dup;
    writeln(findPathCI(name, true));
  }
  {
    char[] name = "SaMpLES".dup;
    writeln(findPathCI(name, true));
  }
  {
    char[] name = "./SaMpLES/sAMPle00.d".dup;
    writeln(findPathCI(name));
  }
  {
    char[] name = "./SaMpLES/../sAMPle00.d/../PosixCI.d".dup;
    writeln(findPathCI(name));
  }
  {
    char[] name = "posixci.d".dup;
    writeln(findPathCI(name));
  }
}
*/
