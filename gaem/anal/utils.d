/* GML analyzer
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
module gaem.anal.utils;

import std.stdio;

import gaem.parser;


// ////////////////////////////////////////////////////////////////////////// //
void message(A...) (File fl, NodeFunc fn, Loc loc, A args) {
  fl.writeln(loc, ": ", args);
  if (fn.pp !is null) fn.pp.printCaret(loc, fl);
}


void message(A...) (NodeFunc fn, Loc loc, A args) { message(stdout, fn, loc, args); }
