/* GML parser
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
module gaem.parser.utils is aliced;


// ////////////////////////////////////////////////////////////////////////// //
// handy utility
auto selector(RetType=void, T, A...) (T obj, scope A args) if (A.length > 0) {
  import std.traits : arity, isCallable, Parameters, ReturnType;
  foreach (immutable aidx, auto arg; args) {
    static assert(isCallable!(args[aidx]), "non-callable case #"~aidx.stringof);
    static if (arity!(args[aidx]) == 0) {
      static if (is(ReturnType!(args[aidx]) == void)) {
        arg();
        static if (!is(RetType == void)) return RetType.init; else return;
      } else {
        return cast(RetType)arg();
      }
    } else {
      static assert(arity!(args[aidx]) == 1, "invalid arity for case #"~aidx.stringof);
      static assert(is(Parameters!(A[aidx])[0] : T), "invalid delegate argument for case #"~aidx.stringof);
      // check for common error: `=> {}`
      static assert(!isCallable!(ReturnType!(A[aidx])), "you probably wrote '=>{}' in case #"~aidx.stringof);
      if (auto o = cast(Parameters!(A[aidx])[0])obj) {
        // i found her! ;-)
        static if (is(ReturnType!(args[aidx]) == void)) {
          arg(o);
          static if (!is(RetType == void)) return RetType.init; else return;
        } else {
          return cast(RetType)arg(o);
        }
      }
    }
  }
  static if (!is(RetType == void)) return RetType.init;
}
