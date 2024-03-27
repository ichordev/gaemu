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
module iv.vfs.types /*is aliced*/;
public import iv.alice;

private import core.stdc.stdio : SEEK_SET, SEEK_CUR, SEEK_END;
public enum Seek : int {
  Set = SEEK_SET,
  Cur = SEEK_CUR,
  End = SEEK_END,
}


public mixin template VFSHiddenPointerHelper(T, string name) {
  mixin("
    private usize hptr_"~name~"_;
    final @property inout(T)* "~name~" () inout pure nothrow @trusted @nogc { pragma(inline, true); return cast(typeof(return))hptr_"~name~"_; }
    final @property void "~name~" (T* v) pure nothrow @trusted @nogc { pragma(inline, true); hptr_"~name~"_ = cast(usize)v; }
  ");
}
