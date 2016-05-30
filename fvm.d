module frex is aliced;

import std.stdio;

import gmlparser;
import gmlutils;
import gmlrunner;


// ////////////////////////////////////////////////////////////////////////// //
void registerPrims (VM vm) {
  vm["write"] = (VM self, Real* bp, ubyte argc) {
    import std.stdio : stdout;
    foreach (immutable idx; 0..argc) {
      auto v = bp[vm.Slot.Argument0+idx];
      if (v.isString) stdout.write(vm.getDynStr(v.getStrId)); else stdout.write(v);
    }
    stdout.flush();
  };
  vm["writeln"] = (VM self, Real* bp, ubyte argc) {
    import std.stdio : stdout;
    foreach (immutable idx; 0..argc) {
      auto v = bp[vm.Slot.Argument0+idx];
      if (v.isString) stdout.write(vm.getDynStr(v.getStrId)); else stdout.write(v);
    }
    stdout.writeln;
    stdout.flush();
  };

  vm["string_length"] = (string s) => s.length;
}


// ////////////////////////////////////////////////////////////////////////// //
void main (string[] args) {
  bool measureTime = false;

  NodeFunc[] funcs;

  funcs = cliProcessArgs!(
    "--time", (fname) { measureTime = true; },
  )(args);

  if (funcs.length > 0) {
    import core.time;
    auto vm = new VM();
    vm.registerPrims();
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
