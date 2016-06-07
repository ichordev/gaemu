module frex is aliced;

import std.stdio;

import gaem.parser;
import gaem.utils;
import gaem.runner;
import gaem.ungmk;


// ////////////////////////////////////////////////////////////////////////// //
void registerPrims () {
  VM["write"] = (Real[] args) {
    import std.stdio : stdout;
    foreach (Real v; args[VM.Slot.Argument0..$]) {
      if (v.isString) stdout.write(getDynStr(v.getStrId)); else stdout.write(v);
    }
    stdout.flush();
  };
  VM["writeln"] = (Real[] args) {
    import std.stdio : stdout;
    foreach (Real v; args[VM.Slot.Argument0..$]) {
      if (v.isString) stdout.write(getDynStr(v.getStrId)); else stdout.write(v);
    }
    stdout.writeln;
    stdout.flush();
  };

  VM["string_length"] = (string s) => s.length;
}


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  bool measureTime = false;
  bool doRun = true;

  NodeFunc[] funcs;

  cliLoadGmkScripts = false;

  funcs = cliProcessArgs!(
    "--time", (fname) { measureTime = true; },
    "--norun", (fname) { doRun = false; },
  )(args, (Gmk gmk) { VM.setGmk(gmk); });

  if (funcs.length > 0) {
    import core.time;
    registerPrims();
    writeln(funcs.length, " function", (funcs.length > 1 ? "s" : ""), " parsed");
    foreach (auto fn; funcs) compile(fn);
    if (doRun) {
      if (measureTime) writeln("executing...");
      auto stt = MonoTime.currTime;
      auto res = VM.exec("main");
      auto dur = (MonoTime.currTime-stt).total!"msecs";
      writeln(res);
      if (measureTime) writeln("total execution took ", dur, " milliseconds");
    }
  }
}
