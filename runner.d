module runner is aliced;

import std.stdio;

import gmlparser;
import gmlparser.anal;

import ungmk;
import loader;


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  NodeFunc[] funcs;

  bool dumpFileNames = false;
  bool styleWarnings = false;
  bool doScripts = true, doActions = true;
  bool measureTime = false;

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
      if (fname == "-w") { styleWarnings = true; continue; }
      if (fname == "-S") { doScripts = false; continue; }
      if (fname == "-A") { doActions = false; continue; }
      if (fname == "--time") { measureTime = true; continue; }
      if (fname[0] == '@') {
        if (fname.length < 2) assert(0, "gmk file?");
        auto gmk = new Gmk(fname[1..$]);
        funcs ~= gmkLoadScripts(gmk, doScripts:doScripts, doActions:doActions, warnings:false);
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

  if (funcs.length > 0) {
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) {
      writeln("\n", fn.toString);
    }
  }

  /*
    if (measureTime) writeln("executing...");
    runScript("main", scargs);
    if (measureTime) {
      auto dur = (MonoTime.currTime-stt).total!"msecs";
      writeln("total execution took ", dur, " milliseconds");
    }
  */
}
