module checker is aliced;

import std.stdio;

import gaem.parser;
import gaem.anal;
import gaem.utils;


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  NodeFunc[] funcs;

  bool styleWarnings = false;
  bool warnVars = false;
  bool warnWith = false;
  bool warnAss = false;
  bool warnUVars = false;

  funcs = cliProcessArgs!(
    "-w", (fname) { styleWarnings = true; },
    "-Wall", (fname) { warnVars = true; warnWith = true; warnAss = true; },
    "-Wvardecl", (fname) { warnVars = false; },
    "-Wwith", (fname) { warnWith = false; },
    "-Wass", (fname) { warnAss = false; },
    "-Wuvars", (fname) { warnUVars = false; },
  )(args);

  if (funcs.length > 0) {
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) {
      if (warnVars) {analVars(fn);
        bool skip = false;
        foreach (string name; [
          "scrCreateTile",
          "scrCreateTileObj",
          "scrLoadCheckpoint",
          "scrLoadLevel",
          "scrMakeItem",
          "scrSetCursorTile",
          "scrSetVendingItem",
          "scrTestLevel",
          "scrMagicSigns",
        ]) {
          if (fn.name == name) { skip = true; break; }
        }
        if (!skip && fn.name.length > 3 && fn.name[0..3] == "sui") skip = true;
        if (!skip) analVars(fn);
      }
      if (warnWith) analWith(fn);
      if (warnAss) analAss(fn);
      if (warnUVars) analUninited(fn);
    }
  }
}
