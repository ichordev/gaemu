module frex is aliced;

import std.stdio;

import gmlparser;
import gmlparser.anal;

import ungmk;
import loader;

import gmlvm;


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  bool dumpFileNames = false;
  bool doScripts = true;
  bool doActions = true;
  bool measureTime = false;

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
      if (fname == "--time") { measureTime = true; continue; }
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
    import core.time;
    auto vm = new VM();
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) {
      vm.compile(fn);
    }
    if (measureTime) writeln("executing...");
    auto stt = MonoTime.currTime;
    auto res = vm.exec("main");
    auto dur = (MonoTime.currTime-stt).total!"msecs";
    writeln(res);
    if (measureTime) writeln("total execution took ", dur, " milliseconds");
  }
}
