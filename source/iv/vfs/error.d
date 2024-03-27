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
module iv.vfs.error /*is aliced*/;
import iv.alice;


// ////////////////////////////////////////////////////////////////////////// //
/*
mixin template VFSExceptionCtor() {
  this (string msg, string file=__FILE__, usize line=__LINE__, Throwable next=null) pure nothrow @safe @nogc {
    super(msg, file, line, next);
  }
}


// usage:
//   mixin(VFSExceptionMx!"MyEx");
//   mixin(VFSExceptionMx!("MyEx1", "MyEx"));
enum VFSExceptionMx(string name, string base="Exception") = `class `~name~` : `~base~` { mixin VFSExceptionCtor; }`;


mixin(VFSExceptionMx!("VFSException"));
mixin(VFSExceptionMx!("VFSNamedException(string name)", "VFSException"));
*/

class VFSException : Exception {
  this (string msg, string file=__FILE__, usize line=__LINE__, Throwable next=null) pure nothrow @safe @nogc {
    super(msg, file, line, next);
  }
}

class VFSExceptionArc : VFSException {
  this (string msg, string file=__FILE__, usize line=__LINE__, Throwable next=null) pure nothrow @safe @nogc {
    super(msg, file, line, next);
  }
}
