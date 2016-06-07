/* GML utils
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
module gaem.utils.cliarg is aliced;

import gaem.parser;
import gaem.utils.loader;
import gaem.ungmk;


// ////////////////////////////////////////////////////////////////////////// //
public bool cliLoadGmkScripts = true;


// ////////////////////////////////////////////////////////////////////////// //
NodeFunc[] cliProcessArgs(Opts...) (ref string[] args, void delegate (Gmk gmk) procgmk=null) {
  NodeFunc[] funcs;
  bool nomore = false;
  string[] scargs;
  bool doScripts = true;
  bool doActions = true;
  bool dumpFileNames = false;
  foreach (string fname; args[1..$]) {
    import std.file;
    import std.path;
    if (nomore) {
      scargs ~= fname;
    } else {
      if (fname.length == 0) continue;
      if (fname == "--") { nomore = true; continue; }
      if (fname == "-S") { doScripts = false; continue; }
      if (fname == "-A") { doActions = false; continue; }
      if (fname == "-d") { dumpFileNames = true; continue; }
      bool found = false;
      foreach (immutable idx, immutable val; Opts) {
        static if (idx%2 == 0) {
          if (fname == Opts[idx]) { found = true; Opts[idx+1](fname); break; }
        }
      }
      if (found) continue;
      if (fname[0] == '-') throw new Exception("invalid option: '"~fname~"'");
      if (fname[0] == '@') {
        if (fname.length < 2) assert(0, "gmk file?");
        if (dumpFileNames) { import std.stdio; writeln("loading '", fname[1..$], "'..."); }
        auto gmk = new Gmk(fname[1..$]);
        if (cliLoadGmkScripts) {
          funcs ~= gmkLoadScripts(gmk, doScripts:doScripts, doActions:doActions);
        }
        if (procgmk !is null) procgmk(gmk);
        continue;
      }
      if (isDir(fname)) {
        foreach (auto de; dirEntries(fname, "*.gm[lx]", SpanMode.breadth)) {
          bool doit = true;
          foreach (auto pt; pathSplitter(de.dirName)) {
            if (pt.length && pt[0] == '_') { doit = false; break; }
          }
          if (doit) {
            if (dumpFileNames) { import std.stdio; writeln("loading '", de.name, "'..."); }
            funcs ~= loadScript(de.name, true);
          }
        }
      } else {
        if (dumpFileNames) { import std.stdio; writeln("loading '", fname, "'..."); }
        funcs ~= loadScript(fname, true);
      }
    }
  }
  args = scargs;
  return funcs;
}
