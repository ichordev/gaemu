module fvm;

import std.stdio;

import gaem.parser;
import gaem.utils;
import gaem.runner;


// ////////////////////////////////////////////////////////////////////////// //
void registerPrims(){
	VM.opIndexAssign("write", (Real[] args){
		import std.stdio: stdout;
		foreach(Real v; args[VM.Slot.Argument0..$]){
			if(v.isString) stdout.write(getDynStr(v.getStrId)); else stdout.write(v);
		}
		stdout.flush();
	});
	VM.opIndexAssign("writeln", (Real[] args){
		import std.stdio: stdout;
		foreach(Real v; args[VM.Slot.Argument0..$]){
			if(v.isString) stdout.write(getDynStr(v.getStrId)); else stdout.write(v);
		}
		stdout.writeln;
		stdout.flush();
	});
	
	VM.opIndexAssign("string_length", (string s) => s.length);
}


// ////////////////////////////////////////////////////////////////////////// //
version(FVM){
	void main(string[] args){
		bool measureTime = false;
		
		NodeFunc[] funcs;
		
		funcs = cliProcessArgs!(
			"--time", (fname){ measureTime = true; },
		)(args);
		
		if(funcs.length > 0){
			import core.time;
			registerPrims();
			writeln(funcs.length, " function", (funcs.length > 1 ? "s": ""), " parsed");
			foreach(fn; funcs) compile(fn);
			if(measureTime) writeln("executing...");
			auto stt = MonoTime.currTime;
			auto res = VM.exec("main");
			auto dur = (MonoTime.currTime-stt).total!"msecs";
			writeln(res);
			if(measureTime) writeln("total execution took ", dur, " milliseconds");
		}
	}
}
