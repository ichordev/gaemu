module frex is aliced;

import std.stdio;

import gmlparser;
import gmlparser.anal;

import ungmk;
import loader;


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  bool dumpFileNames = false;
  bool doScripts = true;
  bool doActions = true;

  NodeFunc[] funcs;

  bool nomore = false;
  string[] scargs;
  foreach (string fname; args[1..$]) {
    import std.file;
    import std.path;
    if (nomore) {
      scargs ~= fname;
    } else {
      if (fname.length == 0) continue;
      if (fname == "--") { nomore = true; continue; }
      if (fname == "-d") { dumpFileNames = true; continue; }
      if (fname == "-S") { doScripts = false; continue; }
      if (fname == "-A") { doActions = false; continue; }
      if (fname[0] == '@') {
        if (fname.length < 2) assert(0, "gmk file?");
        auto gmk = new Gmk(fname[1..$]);
        funcs ~= gmkLoadScripts(gmk, doScripts:doScripts, doActions:doActions, warnings:false, checkReturns:false);
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
            funcs ~= loadScript(de.name, false);
          }
        }
      } else {
        if (dumpFileNames) { import std.stdio; writeln("loading '", fname, "'..."); }
        funcs ~= loadScript(fname, false);
      }
    }
  }

  if (funcs.length > 0) {
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) {
      //x > view_xview[0]-16 && x < view_xview[0]+view_wview[0]+16 &&
      //y > view_yview[0]-16 && y < view_yview[0]+view_hview[0]+16
      /*
      writeln("\n", fn.toString);
      visitNodes(fn.ebody, (n) {
        import std.string : replace;
        writeln(n.loc, ": (", typeid(n).name[14..$], "): ", n.toString.replace("\n", " "));
        return VisitRes.Continue;
      });
      */
      Node[] checks;
      findFrozenChecks(ref checks, fn);
      foreach (Node n; checks) {
        import std.string : replace;
        writeln(n.loc, ": frozen check");
        writeln("    ", (cast(NodeIf)n).ec.toString.replace("\n", " "));
      }
    }
  }
}
