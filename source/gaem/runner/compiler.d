/* GML runner
 * coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module gaem.runner.compiler;

import std.traits;

import gaem.parser;

import gaem.runner.strpool;
import gaem.runner.value;
import gaem.runner.opcodes;
import gaem.runner.vm;
import gaem.runner.objects;


// ////////////////////////////////////////////////////////////////////////// //
void compile(NodeFunc fn){
	import std.stdio: stdout;
	auto spc = cast(uint)VM.code.length;
	doCompileFunc(fn);
	while(spc < VM.code.length) spc += dumpInstr(stdout, spc, VM.code);
}


// ////////////////////////////////////////////////////////////////////////// //
private:
__gshared NodeFunc curfn;
__gshared bool[256] slots;
__gshared uint firstFreeSlot;
__gshared uint maxUsedSlot;
__gshared ubyte[string] locals;
__gshared uint[string] globals;
__gshared Loc[string] vdecls; // for error messages
__gshared ubyte maxArgUsed; // maximum `argumentX` we've seen
__gshared string[16] aaliases; // argument aliases
__gshared uint breakChain; // current jump chain for `break`
__gshared uint contChain; // current jump chain for `continue`
__gshared bool contChainIsAddr; // is `contChain` an address, not a chain?



void setupSlots(){
	slots[] = false;
	//foreach(immutable idx; 0..VM.Slot.max+1) slots[idx] = true; // used
	slots[0..VM.Slot.max+1] = true; // used
	firstFreeSlot = VM.Slot.max+1;
	maxUsedSlot = firstFreeSlot-1;
	locals.clear;
	globals.clear;
	vdecls.clear;
	maxArgUsed = 0;
	aaliases[] = null;
	breakChain = 0;
	contChain = 0;
	contChainIsAddr = false;
}


bool isLocalSlot(ubyte slot){
	//TODO: use AA for mapping?
	foreach(ubyte r; locals.byValue) if(r == slot) return true;
	return false;
}


ubyte allocSlot(Loc loc, int ddest=-1){
	if(ddest >= 0){
		assert(ddest < slots.length);
		return cast(ubyte)ddest;
	}
	foreach(immutable idx; firstFreeSlot..slots.length){
		if(!slots[idx]){
			if(idx > maxUsedSlot) maxUsedSlot = cast(uint)idx;
			slots[idx] = true;
			return cast(ubyte)idx;
		}
	}
	compileError(loc, "out of free slots");
	assert(0);
}


ubyte allocSlots(Loc loc, int count){
	assert(count > 0 && count < slots.length);
	foreach(immutable idx; firstFreeSlot..slots.length-count){
		bool ok = true;
		foreach(immutable c; idx..idx+count) if(slots[c]){ ok = false; break; }
		if(ok){
			if(idx+count-1 > maxUsedSlot) maxUsedSlot = cast(uint)idx+count-1;
			foreach(immutable c; idx..idx+count) slots[c] = true;
			return cast(ubyte)idx;
		}
	}
	compileError(loc, "out of free slots");
	assert(0);
}


ubyte reserveCallSlots(Loc loc, uint resnum){
	foreach_reverse(immutable idx, bool v; slots){
		if(v){
			if(idx+resnum+1 > slots.length) compileError(loc, "out of free slots");
			return cast(ubyte)(idx+1);
		}
	}
	compileError(loc, "out of free slots");
	assert(0);
}


void freeSlot(ubyte num){
	if(num >= firstFreeSlot){
		assert(slots[num]);
		slots[num] = false;
	}
}


void freeSlots(ubyte num, int count){
	while(count-- > 0){
		freeSlot(num);
		if(++num == 0) break;
	}
}


// ////////////////////////////////////////////////////////////////////////// //
int argvar(string s){
	switch(s){
		case "argument0": return 0;
		case "argument1": return 1;
		case "argument2": return 2;
		case "argument3": return 3;
		case "argument4": return 4;
		case "argument5": return 5;
		case "argument6": return 6;
		case "argument7": return 7;
		case "argument8": return 8;
		case "argument9": return 9;
		case "argument10": return 10;
		case "argument11": return 11;
		case "argument12": return 12;
		case "argument13": return 13;
		case "argument14": return 14;
		case "argument15": return 15;
		default:
	}
	return -1;
}


void compileError(A...)(Loc loc, A args){
	if(curfn.pp !is null){
		curfn.pp.error(loc, args);
	}else{
		import std.stdio: stderr;
		stderr.writeln("ERROR at ", loc, ": ", args);
		string msg;
		foreach(immutable a; args){
			import std.string: format;
			msg ~= "%s".format(a);
		}
		throw new ErrorAt(loc, msg);
	}
}


uint sid4name(string name){
	if(auto sptr = name in VM.scripts){
		return *sptr;
	}else{
		auto sid = cast(uint)VM.scriptPCs.length;
		if(sid > 32767) compileError(curfn.loc, "too many scripts");
		assert(VM.scriptASTs.length == sid);
		// reserve slots
		VM.scriptPCs ~= 0;
		VM.scriptASTs ~= null;
		VM.scriptNum2Name[sid] = name;
		VM.scripts[name] = sid;
		return sid;
	}
}


uint pc(){ pragma(inline, true); return cast(uint)VM.code.length; }
void setpc(uint pc){ pragma(inline, true); VM.code.length = pc; VM.code.assumeSafeAppend; }


uint emit(Op op, ubyte dest=0, ubyte op0=0, ubyte op1=0){
	auto res = cast(uint)VM.code.length;
	VM.code ~= (op1<<24)|(op0<<16)|(dest<<8)|cast(ubyte)op;
	return res;
}


uint emit3Bytes(Op op, uint val){
	assert(val <= 0xffffff);
	auto res = cast(uint)VM.code.length;
	VM.code ~= (val<<8)|cast(ubyte)op;
	return res;
}


uint emit2Bytes(Op op, ubyte dest, short val){
	auto res = cast(uint)VM.code.length;
	VM.code ~= (val<<16)|(dest<<8)|cast(ubyte)op;
	return res;
}


uint emitJumpTo(uint addr, Op op=Op.jump){
	assert(addr <= 0xffffff);
	auto res = cast(uint)VM.code.length;
	VM.code ~= cast(uint)op|(addr<<8);
	return res;
}


// this starts "jump chain", return new chain id
uint emitJumpChain(uint chain, Op op=Op.jump){
	assert(chain <= 0xffffff);
	auto res = cast(uint)VM.code.length;
	VM.code ~= cast(uint)op|(chain<<8);
	return res;
}


void fixJumpChain(uint chain, uint addr){
	assert(chain <= 0xffffff);
	assert(addr <= 0xffffff);
	while(chain){
		auto nc = op3Byte(VM.code[chain]);
		VM.code[chain] = (VM.code[chain]&0xff)|(addr<<8);
		chain = nc;
	}
}


void opReplace(uint pc, Op op, ubyte dest){
	VM.code[pc] = (VM.code[pc]&0xff_ff_00_00)|(dest<<8)|cast(ubyte)op;
}


// ////////////////////////////////////////////////////////////////////////// //
void findUninitialized(){
	bool[string] inited;
	bool[string] used;

	void processExpr(Node n, bool asAss=false){
		if(n is null) return;
		visitNodes(n, (Node nn){
			if(auto id = cast(NodeId)nn){
				if(!asAss && argvar(id.name) < 0){
					if(id.name in locals && id.name !in inited){
						compileError(nn.loc, "using uninitialized variable; declared at ", vdecls[id.name].toStringNoFile);
					}
				}
				inited[id.name] = true;
				used[id.name] = true;
				return VisitRes.SkipChildren;
			}
			if(auto n = cast(NodeFCall)nn){
				if(cast(NodeId)n.fe is null) compileError(n.loc, "invalid function call");
				if(n.args.length > 16) compileError(n.loc, "too many arguments in function call");
				foreach(immutable idx, Node a; n.args){
					// no assignments allowed there
					processExpr(a);
				}
				return VisitRes.SkipChildren;
			}
			return VisitRes.Continue;
		});
	}

	void processStatement(Node nn){
		if(nn is null) return;
		return selectNode!void(nn,
			(NodeVarDecl n){},
			(NodeBlock n){
				foreach(Node st; n.stats){
					if(cast(NodeStatementBreakCont)st !is null) break;
					processStatement(st);
					if(cast(NodeReturn)st !is null) break;
				}
			},
			(NodeStatementEmpty n){},
			(NodeStatementAss n){
				if(cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null){
					compileError(nn.loc, "assignment to rvalue");
					return;
				}
				processExpr(n.er); // it is calculated first
				processExpr(n.el, asAss:true);
			},
			(NodeStatementExpr n){ processExpr(n.e); },
			(NodeReturn n){ processExpr(n.e); },
			(NodeWith n){
				processExpr(n.e); // can't contain assignments
				// body can be executed zero times, so...
				auto before = inited.dup;
				processStatement(n.ebody);
				inited = before;
			},
			(NodeIf n){
				processExpr(n.ec);
				auto before = inited.dup;
				processStatement(n.et);
				auto tset = inited.dup;
				inited = before.dup;
				processStatement(n.ef);
				// now copy to `before` all items that are set both in `tset` and in `inited`
				foreach(string name; inited.byKey){
					if(name in tset) before[name] = true;
				}
				inited = before;
			},
			(NodeStatementBreakCont n){},
			(NodeFor n){
				processStatement(n.einit);
				// "next" and "cond" can't contain assignments, so it's safe here
				processExpr(n.econd);
				processStatement(n.enext);
				// yet body can be executed zero times, so...
				auto before = inited.dup;
				processStatement(n.ebody);
				inited = before;
			},
			(NodeWhile n){
				// "cond" can't contain assignments, so it's safe here
				processExpr(n.econd);
				// yet body can be executed zero times, so...
				auto before = inited.dup;
				processStatement(n.ebody);
				inited = before;
			},
			(NodeDoUntil n){
				// "cond" can't contain assignments, so it's safe here
				processExpr(n.econd);
				// body is guaranteed to execute at least one time
				processStatement(n.ebody);
			},
			(NodeRepeat n){
				// "count" can't contain assignments, so it's safe here
				processExpr(n.ecount);
				// yet body can be executed zero times, so...
				auto before = inited.dup;
				processStatement(n.ebody);
				inited = before;
			},
			(NodeSwitch n){
				// "expr" can't contain assignments, so it's safe here
				processExpr(n.e);
				auto before = inited.dup;
				foreach(ref ci; n.cases){
					processExpr(ci.e); // can't contain assignments
					// and this one can
					if(ci.st !is null){
						inited = before.dup;
						processStatement(ci.st);
					}
				}
				inited = before;
			},
			(){ assert(0, "unimplemented node: "~typeid(nn).name); },
		);
	}

	processStatement(curfn.ebody);

	// now show(and remove) unused locals
	//static struct Info{ Loc loc; string name; }
	//Info[] unusedLocs;
	foreach(string name; locals.keys){
		if(name !in used){
		{ import std.stdio; writeln("removing unused local '", name, "'"); }
			//unusedLocs ~= Info(vdecls[name], name);
			locals.remove(name);
		}
	}
	//import std.algorithm: sort;
	//unusedLocs.sort!((ref a, ref b){ if(a.loc.line < b.loc.line) return true; if(a.loc.line > b.loc.line) return false; return(a.loc.col < b.loc.col); });
	//foreach(ref nfo; unusedLocs) compileError(nfo.loc, "unused local '", nfo.name, "'");
}


// ////////////////////////////////////////////////////////////////////////// //
void emitPLit(Loc loc, ubyte dest, Real v){
	uint vpidx = uint.max;
	if(v.isReal){
		// number
		import core.stdc.math: lrint;
		if(lrint(v) == v && lrint(v) >= short.min && lrint(v) <= short.max){
			emit2Bytes(Op.ilit, dest, cast(short)lrint(v));
			return;
		}
		//FIXME: speed it up!
		foreach(immutable idx, Real vp; VM.vpool) if(vp == v){ vpidx = cast(uint)idx; break; }
	}else if(v.isString){
		// string
		//FIXME: speed it up!
		auto sid = v.getStrId;
		if(sid > short.max) compileError(loc, "too many strings");
		//foreach(immutable idx, Real vp; VM.vpool) if(vp.isString && vp.getStrId == sid){ vpidx = cast(uint)idx; break; }
		emit2Bytes(Op.slit, dest, cast(short)sid);
		return;
	}else{
		assert(0, "wtf?!");
	}
	if(vpidx == uint.max){
		vpidx = cast(uint)VM.vpool.length;
		if(vpidx >= 0xffffff) compileError(loc, "too many constants");
		VM.vpool ~= v;
	}
	if(vpidx < ushort.max){
		emit2Bytes(Op.plit, dest, cast(ushort)vpidx);
	}else{
		// special form
		emit2Bytes(Op.plit, dest, cast(short)ushort.max);
		emit3Bytes(Op.skip, vpidx);
	}
}


uint allocStrConst(string s, Loc loc){ return newInternalStr(s); }


int varSlot(string name){
	auto avn = argvar(name);
	if(avn >= 0) return VM.Slot.Argument0+avn;
	switch(name){
		case "self": return VM.Slot.Self;
		case "other": return VM.Slot.Other;
		default:
	}
	// argument aliases
	foreach(immutable idx, string an; aaliases) if(an == name) return cast(int)VM.Slot.Argument0+idx;
	// locals
	if(auto v = name in locals) return *v;
	return -1;
}


// returns slot number or -1
int isKnownSlot(Node nn){
	if(auto n = cast(NodeId)nn){
		// keep track of maximum argument we've seen
		if(maxArgUsed < 15){
			if(auto ai = argvar(n.name)){
				if(ai > maxArgUsed) maxArgUsed = cast(ubyte)ai;
			}
		}
		auto vsl = varSlot(n.name);
		if(vsl >= 0) return vsl; // this is local variable
	}
	return -1; // alas
}


// asCond: it is used as condition, so we should emit `oval` for objects
ubyte compileVarAccess(Node nn, int ddest=-1, bool asCond=false, bool noLocalsCheck=false){
	return selectNode!ubyte(nn,
		(NodeId n){
			if(!noLocalsCheck){
				auto ksl = isKnownSlot(n);
				if(ksl >= 0){
					// this is local variable
					if(ddest < 0 || ddest == ksl) return ksl; // just use this slot directly
					auto dest = allocSlot(n.loc, ddest);
					assert(dest != ksl);
					emit(Op.copy, dest, cast(ubyte)ksl, 1);
					return dest;
				}
				// this may be object, or sprite, or background...
				if(auto oid = objectByName(n.name)){
					// object!
					auto dest = allocSlot(n.loc, ddest);
					if(asCond){
						emit2Bytes(Op.oval, dest, cast(short)oid);
					}else{
						//emit2Bytes(Op.ilit, dest, cast(short)oid); //FIXME: introduce new opcode for this?
						emitPLit(n.loc, dest, oid);
					}
					return dest;
				}
			}
		{
				// it's not local, so generate field access(Op.fval)
				// this is `self` field
				auto dest = allocSlot(n.loc, ddest);
				auto fid = allocSlot(n.loc);
				emit2Bytes(Op.xlit, fid, cast(short)allocateFieldId(n.name));
				freeSlot(fid);
				emit(Op.fval, dest, VM.Slot.Self, fid);
				return dest;
			}
		},
		(NodeDot n){
			// field access -- Op.fval
			auto dest = allocSlot(n.loc, ddest);
			if(auto oid = cast(NodeId)n.e){
				// we know object name directly
				if(oid.name == "self" || oid.name == "other" || oid.name == "global"){
					// well-known name
					auto fid = allocSlot(n.loc);
					emit2Bytes(Op.xlit, fid, cast(short)allocateFieldId(n.name));
					if(oid.name == "global"){
						auto oids = allocSlot(n.loc);
						emit2Bytes(Op.ilit, oids, -666);
						freeSlot(oids);
						emit(Op.fval, dest, oids, fid);
					}else{
						emit(Op.fval, dest, (oid.name == "self" ? VM.Slot.Self: VM.Slot.Other), fid);
					}
					freeSlot(fid);
					return dest;
				}
			}
			// this is some complex expression
			auto oids = compileExpr(n.e);
			freeSlot(oids);
			auto fid = allocSlot(n.loc);
			emit2Bytes(Op.xlit, fid, cast(short)allocateFieldId(n.name));
			emit(Op.fval, dest, oids, fid);
			return dest;
		},
		(NodeIndex n){
			assert(n.ei0 !is null);
			assert(ddest < 0 || ddest > VM.Slot.max);
			auto dest = allocSlot(n.loc, ddest);
			auto islots = allocSlots(n.loc, 2+(n.ei1 !is null ? 1: 0)); // field id, index
			compileExpr(n.ei0, islots+1, noLocalsCheck:noLocalsCheck);
			if(n.ei1 !is null) compileExpr(n.ei1, islots+2, noLocalsCheck:noLocalsCheck);
			// `islots+0` should be free here -- our slot alloc scheme guarantees it
			auto xfd = compileExpr(n.e, asCond:true, noLocalsCheck:noLocalsCheck); // asCond, to produce `oval`
			if(xfd <= VM.Slot.max || isLocalSlot(xfd)){ compileError(n.loc, "indexing locals is not supported yet"); assert(0); }
			freeSlot(xfd); // don't need it
			switch(VM.code[pc-1].opCode){
				case Op.oval: compileError(n.loc, "can't index object"); assert(0);
				case Op.fval:
					auto oid = VM.code[pc-1].opOp0;
					auto fid = VM.code[pc-1].opOp1;
					setpc(pc-1); // remove this opcode
					// copy field id
					if(fid != islots) emit(Op.copy, islots, fid, 1);
					// generate new opcode
					emit((n.ei1 is null ? Op.i1fval: Op.i2fval), dest, oid, islots);
					break;
				case Op.copy: compileError(n.loc, "indexing locals is not supported yet"); assert(0); // just in case, should not be reached
				default: compileError(n.loc, "can't index something"); assert(0);
			}
			freeSlots(islots, 2+(n.ei1 !is null ? 1: 0));
			return dest;
		},
		(){ assert(0, "unimplemented node: "~typeid(nn).name); },
	);
}


// assVal: "new value" slot
// i0, i1: indexes
void compileVarStore(Node nn, ubyte assVal, bool noLocalsCheck=false){
	selectNode!void(nn,
		(NodeId n){
			if(!noLocalsCheck){
				auto ksl = isKnownSlot(n);
				if(ksl >= 0){
					// this is local variable
					if(assVal != ksl) emit(Op.copy, cast(ubyte)ksl, assVal, 1);
					return;
				}
				// this may be object, or sprite, or background...
				if(objectByName(n.name)){ compileError(n.loc, "can't assign value to object"); assert(0); }
			}
		{
				// it's not local, so generate field store(Op.fval)
				// this is `self` field
				auto fid = allocSlot(n.loc);
				emit2Bytes(Op.xlit, fid, cast(short)allocateFieldId(n.name));
				freeSlot(fid);
				emit(Op.fstore, assVal, VM.Slot.Self, fid); // store value *from* dest into field; op0: obj id; op1: int! reg(field id); can create fields
				return;
			}
			assert(0);
		},
		(NodeDot n){ assert(0, "internal compiler error: NodeDot should not be here"); },
		(NodeIndex n){
			assert(n.ei0 !is null);
			auto islots = allocSlots(n.loc, 2+(n.ei1 !is null ? 1: 0)); // field id, index
			compileExpr(n.ei0, islots+1, noLocalsCheck:noLocalsCheck);
			if(n.ei1 !is null) compileExpr(n.ei1, islots+2, noLocalsCheck:noLocalsCheck);
			// `islots+0` should be free here -- our slot alloc scheme guarantees it
			auto spc = pc;
			compileVarStore(n.e, assVal, noLocalsCheck:noLocalsCheck);
			// storing to local can generate no code(it can't, but...)
			if(pc == spc){ compileError(n.loc, "indexing locals is not supported yet"); assert(0); }
			switch(VM.code[pc-1].opCode){
				case Op.fstore:
					auto oid = VM.code[pc-1].opOp0;
					auto fid = VM.code[pc-1].opOp1;
					setpc(pc-1); // remove this opcode
					// copy field id
					if(fid != islots) emit(Op.copy, islots, fid, 1);
					// generate new opcode
					emit((n.ei1 is null ? Op.i1fstore: Op.i2fstore), assVal, oid, islots);
					break;
				case Op.copy: compileError(n.loc, "indexing locals is not supported yet"); assert(0);
				default: compileError(n.loc, "can't index something"); assert(0);
			}
			freeSlots(islots, 2+(n.ei1 !is null ? 1: 0));
		},
		(){ assert(0, "unimplemented node: "~typeid(nn).name); },
	);
}


// returns dest slot
// can put value in desired dest
ubyte compileExpr(Node nn, int ddest=-1, bool asCond=false, bool noLocalsCheck=false){
	import core.stdc.math: lrint;
	import std.math: NaN, isNaN;

	ubyte doUnOp(Op op, NodeUnary n){
		auto dest = allocSlot(n.loc, ddest);
		auto o0 = compileExpr(n.e);
		emit(op, dest, o0);
		freeSlot(o0);
		return dest;
	}

	ubyte doBinOp(Op op, NodeBinary n){
		auto dest = allocSlot(n.loc, ddest);
		auto o0 = compileExpr(n.el);
		auto o1 = compileExpr(n.er);
		emit(op, dest, o0, o1);
		freeSlot(o0);
		freeSlot(o1);
		return dest;
	}

	// returns NaN for non-nums
	Real getNumArg(Node n){
		if(auto lit = cast(NodeLiteralNum)n) return lit.val;
		return NaN(-1); // arbitrary value
	}

	bool isStrArg(Node n){
		pragma(inline, true);
		return(cast(NodeLiteralStr)n !is null);
	}

	nn.pcs = pc;
	scope(exit) nn.pce = pc;
	return selectNode!ubyte(nn,
		(NodeLiteralNum n){
			auto dest = allocSlot(n.loc, ddest);
			emitPLit(n.loc, dest, n.val);
			return dest;
		},
		(NodeLiteralStr n){
			auto dest = allocSlot(n.loc, ddest);
			auto sid = allocStrConst(n.val, n.loc);
			emitPLit(n.loc, dest, buildStrId(sid));
			return dest;
		},
		(NodeUnaryParens n) => compileExpr(n.e, ddest, asCond, noLocalsCheck),
		(NodeUnaryNot n) => doUnOp(Op.lnot, n),
		(NodeUnaryNeg n) => doUnOp(Op.neg, n),
		(NodeUnaryBitNeg n) => doUnOp(Op.bneg, n),
		(NodeBinaryAdd n) => doBinOp(Op.add, n),
		(NodeBinarySub n) => doBinOp(Op.sub, n),
		(NodeBinaryMul n) => doBinOp(Op.mul, n),
		(NodeBinaryRDiv n) => doBinOp(Op.rdiv, n),
		(NodeBinaryDiv n) => doBinOp(Op.div, n),
		(NodeBinaryMod n) => doBinOp(Op.mod, n),
		(NodeBinaryBitOr n) => doBinOp(Op.bor, n),
		(NodeBinaryBitAnd n) => doBinOp(Op.band, n),
		(NodeBinaryBitXor n) => doBinOp(Op.bxor, n),
		(NodeBinaryLShift n) => doBinOp(Op.shl, n),
		(NodeBinaryRShift n) => doBinOp(Op.shr, n),
		(NodeBinaryLess n) => doBinOp(Op.lt, n),
		(NodeBinaryLessEqu n) => doBinOp(Op.le, n),
		(NodeBinaryGreat n) => doBinOp(Op.gt, n),
		(NodeBinaryGreatEqu n) => doBinOp(Op.ge, n),
		(NodeBinaryEqu n) => doBinOp(Op.eq, n),
		(NodeBinaryNotEqu n) => doBinOp(Op.ne, n),
		(NodeBinaryLogOr n) => doBinOp(Op.lor, n),
		(NodeBinaryLogAnd n) => doBinOp(Op.land, n),
		(NodeBinaryLogXor n) => doBinOp(Op.lxor, n),
		(NodeFCall n){
			if(cast(NodeId)n.fe is null) compileError(n.loc, "invalid function call");
			if(n.args.length > 16) compileError(n.loc, "too many arguments in function call");
			auto dest = allocSlot(n.loc, ddest);
			// preallocate frame
			// we can do this, as current slot allocation scheme guarantees
			// that we won't have used slots with higher numbert after compiling
			// argument expressions
			// `reserveCallSlots()` won't mark slots as used
			auto frameSize = cast(uint)n.args.length+VM.Slot.Argument0;
			auto fcs = reserveCallSlots(n.loc, frameSize+1); // +1 for script id
			// put arguments where we want 'em to be
			foreach(immutable idx, Node a; n.args){
				// reserve result slot, so it won't be overwritten
				assert(!slots[fcs+VM.Slot.Argument0+idx]);
				slots[fcs+VM.Slot.Argument0+idx] = true;
				auto dp = compileExpr(a, cast(uint)(fcs+VM.Slot.Argument0+idx), noLocalsCheck: noLocalsCheck);
				if(dp != fcs+VM.Slot.Argument0+idx) assert(0, "internal compiler error");
			}
			// now free result slots
			foreach(immutable idx; 0..n.args.length) freeSlot(cast(ubyte)(fcs+VM.Slot.Argument0+idx));
			// make sure that our invariant holds
			if(reserveCallSlots(n.loc, 1) != fcs) assert(0, "internal compiler error");
			// put script id
			// emit call
			uint sid = sid4name((cast(NodeId)n.fe).name);
			emit2Bytes(Op.xlit, cast(ubyte)(fcs+VM.Slot.Argument0+n.args.length), cast(short)sid);
			emit(Op.call, dest, fcs, cast(ubyte)n.args.length);
			return dest;
		},
		// only variable access is left
		(Node n) => compileVarAccess(n, ddest, asCond, noLocalsCheck),
	);
}


bool hasDotAccess(Node nn){
	if(nn is null) return false;
	auto rn = visitNodes(nn, (Node n){
		if(cast(NodeDot)n) return VisitRes.Stop;
		return VisitRes.Continue;
	});
	return(rn !is null);
}


// compile dotted assign as series of `with`
void lowerLeftAss(Node el, ubyte assVal){
	Node curelem = el;
	Node[] withStack;
	while(curelem !is null){
		if(auto dot = cast(NodeDot)curelem){
			withStack ~= new NodeId(dot.loc, dot.name);
			curelem = dot.e;
		}else if(auto idx = cast(NodeIndex)curelem){
			// `e` should be NodeDot or NodeId
			if(auto dot = cast(NodeDot)idx.e){
				auto x = new NodeIndex(idx.loc, new NodeId(dot.loc, dot.name));
				x.ei0 = idx.ei0;
				x.ei1 = idx.ei1;
				withStack ~= x;
				curelem = dot.e;
			}else if(auto id = cast(NodeId)idx.e){
				withStack ~= idx;
				curelem = null;
			}else{
				compileError(curelem.loc, "something strange is happened here(0)");
			}
		}else if(auto id = cast(NodeId)curelem){
			withStack ~= id;
			curelem = null;
		}else{
			compileError(curelem.loc, "something strange is happened here(1)");
		}
	}
	assert(withStack.length);
	/*
	foreach(immutable idx, Node n; withStack; reverse){
		import std.stdio;
		writeln(idx, ": ", n.toString);
	}
	*/
	if(cast(NodeIndex)withStack[$-1]) compileError(withStack[$-1].loc, "invalid indexing");
	// generate iterators(in reverse order) for [1..$]; [0] is field
	ubyte[] its;
	uint[] spc;
	uint[] jchain;
	foreach_reverse(Node n; withStack[1..$]){
		its ~= allocSlot(n.loc);
		auto xid = compileVarAccess(n, asCond: false, noLocalsCheck: true);
		freeSlot(xid);
		emit(Op.siter, its[$-1], xid); // start instance iterator; dest: iterid; op0: objid or instid; next is jump over loop
		jchain ~= emitJumpChain(0);
		spc ~= pc;
	}
	// trivial body of the inner `with`
	compileVarStore(withStack[0], assVal, noLocalsCheck: true);
	// unwind `with`
	foreach_reverse(immutable idx; 0..its.length){
		emit(Op.niter, its[idx]);
		emitJumpTo(spc[idx]);
		fixJumpChain(jchain[idx], pc);
		emit(Op.kiter, its[idx]);
		freeSlot(its[idx]);
	}
}


void compileStatement(Node nn){
	assert(nn !is null);
	nn.pcs = pc;
	scope(exit) nn.pce = pc;
	return selectNode!void(nn,
		(NodeVarDecl n){},
		(NodeBlock n){
			foreach(Node st; n.stats) compileStatement(st);
		},
		(NodeStatementEmpty n){},
		(NodeStatementAss n){
			// assignment
			// lower `a.b = 42;` to `with`
			// note that `a.b[42] = 666;` is `NodeIndex` which contains `NodeDot`
			if(cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null) compileError(n.loc, "assignment to rvalue");
			auto ksl = isKnownSlot(n.el);
			if(ksl >= 0){
				// this is known slot, just put value into it directly
				auto src = compileExpr(n.er, ksl);
				assert(src == ksl);
			}else{
				auto src = compileExpr(n.er);
				// known object?
				if(auto dot = cast(NodeDot)n.el){
					if(auto oid = cast(NodeId)dot){
						// we know object name directly
						if(oid.name == "self" || oid.name == "other" || oid.name == "global"){
							// well-known name
							auto fid = allocSlot(dot.loc);
							emit2Bytes(Op.xlit, fid, cast(short)allocateFieldId(dot.name));
							if(oid.name == "global"){
								auto oids = allocSlot(dot.loc);
								emit2Bytes(Op.ilit, oids, -666);
								freeSlot(oids);
								emit(Op.fstore, src, oids, fid);
							}else{
								emit(Op.fstore, src, (oid.name == "self" ? VM.Slot.Self: VM.Slot.Other), fid);
							}
							freeSlot(fid);
							freeSlot(src);
							return;
						}
					}
				}
				// dot access need lowering
				if(hasDotAccess(n.el)){
					lowerLeftAss(n.el, src);
				}else{
					compileVarStore(n.el, src);
				}
				freeSlot(src);
			}
		},
		(NodeStatementExpr n){ freeSlot(compileExpr(n.e)); },
		(NodeReturn n){
			if(n.e is null){
				emitPLit(n.loc, 0, 0);
				emit(Op.ret, 0);
			}else{
				auto dest = compileExpr(n.e);
				emit(Op.ret, dest);
				freeSlot(dest);
			}
		},
		(NodeWith n){
			auto obc = breakChain;
			auto occ = contChain;
			auto cca = contChainIsAddr;
			scope(exit){ breakChain = obc; contChain = occ; contChainIsAddr = cca; }
			// object
			auto iid = allocSlot(n.loc);
			auto obs = compileExpr(n.e);
			freeSlot(obs);
			// iteration start
			emit(Op.siter, iid, obs); // start instance iterator; dest: iterid; op0: objid or instid; next is jump over loop
			breakChain = emitJumpChain(0); // jump over the loop
			contChain = 0;
			contChainIsAddr = false;
			// loop body
			auto bpc = pc;
			compileStatement(n.ebody);
			// continue point
			fixJumpChain(contChain, pc);
			emit(Op.niter, iid);
			emitJumpTo(bpc);
			// end of loop, break point
			fixJumpChain(breakChain, pc);
			emit(Op.kiter, iid);
			freeSlot(iid);
		},
		(NodeIf n){
			auto cs = compileExpr(n.ec, asCond:true);
			freeSlot(cs); // yep, free it here
			emit(Op.xtrue, cs);
			uint jfc = 0;
			// simple optimization
			jfc = emitJumpChain(0, Op.jump);
			compileStatement(n.et);
			if(n.ef !is null){
				auto exc = emitJumpChain(0, Op.jump);
				fixJumpChain(jfc, pc);
				jfc = exc;
				compileStatement(n.ef);
			}
			fixJumpChain(jfc, pc);
		},
		(NodeStatementBreak n){
			breakChain = emitJumpChain(breakChain);
		},
		(NodeStatementContinue n){
			if(contChainIsAddr){
				emitJumpTo(contChain);
			}else{
				contChain = emitJumpChain(contChain);
			}
		},
		(NodeFor n){
			compileStatement(n.einit);
			// generate code like this:
			//   jump to "continue"
			//   body
			//  continue:
			//   cond
			//   jumptostart
			auto obc = breakChain;
			auto occ = contChain;
			auto cca = contChainIsAddr;
			scope(exit){ breakChain = obc; contChain = occ; contChainIsAddr = cca; }
			// jump to "continue"
			contChain = emitJumpChain(0); // start new chain
			contChainIsAddr = false;
			breakChain = 0; // start new chain
			auto stpc = pc;
			// increment
			compileStatement(n.enext);
			// body
			compileStatement(n.ebody);
			// fix "continue"
			fixJumpChain(contChain, pc);
			// condition
			auto dest = compileExpr(n.econd, asCond:true);
			freeSlot(dest); // yep, right here
			emit(Op.xfalse, dest); // skip jump on false
			emitJumpTo(stpc);
			// "break" is here
			fixJumpChain(breakChain, pc);
		},
		(NodeWhile n){
			// nothing fancy
			auto obc = breakChain;
			auto occ = contChain;
			auto cca = contChainIsAddr;
			scope(exit){ breakChain = obc; contChain = occ; contChainIsAddr = cca; }
			// new break chain
			breakChain = 0;
			// "continue" is here
			contChain = pc;
			contChainIsAddr = true;
			// condition
			auto dest = compileExpr(n.econd, asCond:true);
			freeSlot(dest); // yep, right here
			emit(Op.xfalse, dest); // skip jump on false
			breakChain = emitJumpChain(breakChain); // get out of here
			// body
			compileStatement(n.ebody);
			// and again
			emitJumpTo(contChain);
			// "break" is here
			fixJumpChain(breakChain, pc);
		},
		(NodeDoUntil n){
			// nothing fancy
			auto obc = breakChain;
			auto occ = contChain;
			auto cca = contChainIsAddr;
			scope(exit){ breakChain = obc; contChain = occ; contChainIsAddr = cca; }
			auto stpc = pc;
			// new break chain
			breakChain = 0;
			// new continue chain
			contChain = 0;
			contChainIsAddr = false;
			// body
			compileStatement(n.ebody);
			// "continue" is here
			fixJumpChain(contChain, pc);
			// condition
			auto dest = compileExpr(n.econd, asCond:true);
			freeSlot(dest); // yep, right here
			emit(Op.xfalse, dest); // skip jump on false
			// and again
			emitJumpTo(stpc);
			// "break" is here
			fixJumpChain(breakChain, pc);
		},
		(NodeRepeat n){ //TODO: don't allow object names here
			// allocate node for counter
			auto cnt = compileExpr(n.ecount);
			// allocate "1" constant(we will need it)
			auto one = allocSlot(n.loc);
			emit2Bytes(Op.ilit, one, cast(short)1);
			// alice in chains
			auto obc = breakChain;
			auto occ = contChain;
			auto cca = contChainIsAddr;
			scope(exit){ breakChain = obc; contChain = occ; contChainIsAddr = cca; }
			// new break chain
			breakChain = 0;
			// "continue" is here
			contChain = pc;
			contChainIsAddr = true;
			// check and decrement counter
			auto ck = allocSlot(n.ecount.loc);
			freeSlot(ck); // we don't need that slot anymore, allow body to reuse it
			emit(Op.ge, ck, cnt, one);
			emit(Op.xtrue, ck);
			breakChain = emitJumpChain(breakChain); // get out of here
			// decrement counter in-place
			emit(Op.sub, cnt, cnt, one);
			// body
			compileStatement(n.ebody);
			// and again
			emitJumpTo(contChain);
			// "break" is here
			fixJumpChain(breakChain, pc);
			// free used slots
			freeSlot(one);
			freeSlot(cnt);
		},
		(NodeSwitch n){
			// switch expression
			auto expr = compileExpr(n.e);
			if(n.cases.length){
				// has some cases
				uint defaultBodyAddr = 0; // address of "default" node body(even if it is empty)
				uint lastFalltrhuJump = 0; // this is the address of the Op.jump at the end of the previous case node
				uint lastCaseSkipJumpAddr = 0; // this is the address of the Op.jump at the failed condition of the previous case node
				// new "break" chain
				auto obc = breakChain;
				scope(exit) breakChain = obc;
				breakChain = 0;
				// now generate code for case nodes, skipping "default" by the way
				foreach(immutable idx, ref ci; n.cases){
					uint nodeSkipChain = 0;
					// check condition
					if(ci.e !is null){
						// jump here from the last failed condition
						fixJumpChain(lastCaseSkipJumpAddr, pc);
						auto cond = compileExpr(ci.e);
						// trick: reuse "cond" slot
						freeSlot(cond);
						emit(Op.eq, cond, cond, expr);
						emit(Op.xtrue, cond);
						// new skip chain
						lastCaseSkipJumpAddr = emitJumpChain(0);
					}else{
						// this is default node, jump over it
						nodeSkipChain = emitJumpChain(0);
						// and save info
						defaultBodyAddr = pc;
					}
					// fix fallthru jump
					fixJumpChain(lastFalltrhuJump, pc);
					// the body is here
					compileStatement(ci.st);
					// new fallthru chain
					lastFalltrhuJump = (idx < n.cases.length-1 ? emitJumpChain(0): 0);
					// fix "default skip" chain
					fixJumpChain(nodeSkipChain, pc);
				}
				// we can free expression slot right here
				freeSlot(expr);
				// do we have default node?
				if(defaultBodyAddr){
					// jump there from the last failed condition
					fixJumpChain(lastCaseSkipJumpAddr, defaultBodyAddr);
				}else{
					// jump here from the last failed condition
					fixJumpChain(lastCaseSkipJumpAddr, pc);
				}
				// fix last fallthru jump
				fixJumpChain(lastFalltrhuJump, pc);
				// fix "break" chain
				fixJumpChain(breakChain, pc);
			}else{
				freeSlot(expr);
			}
		},
		(){ assert(0, "unimplemented node: "~typeid(nn).name); },
	);
}

// ////////////////////////////////////////////////////////////////////////// //
void doCompileFunc(NodeFunc fn){
	assert(fn !is null);
	assert(fn.ebody !is null);
	assert(fn.name.length);
	curfn = fn;
	scope(exit){ curfn = null; setupSlots(); }

	setupSlots();

	// collect var declarations(gml is not properly scoped)
	visitNodes(curfn.ebody, (Node n){
		if(auto vd = cast(NodeVarDecl)n){
			foreach(immutable idx, string name; vd.names){
				if(name in locals){
					if(vd.asGlobal) compileError(vd.locs[idx], "conflicting variable '", name, "' declaration(previous at ", vdecls[name].toStringNoFile, ")");
				}else if(name in globals){
					if(!vd.asGlobal) compileError(vd.locs[idx], "conflicting variable '", name, "' declaration(previous at ", vdecls[name].toStringNoFile, ")");
				}
				vdecls[name] = vd.locs[idx];
				if(vd.asGlobal){
					globals[name] = 0;
				}else{
					// don't allocate slots for locals here, we can remove some locals due to arguments aliasing later
					//firstFreeSlot = allocSlot(vd.locs[idx]);
					//locals[name] = cast(ubyte)firstFreeSlot;
					//++firstFreeSlot;
					locals[name] = 42; // temporary value
				}
			}
		}
		return VisitRes.Continue;
	});

	findUninitialized();

	/* here we will do very simple analysis for code like
	 *   var m, n;
	 *   m = argument0;
	 *   n = argument1;
	 *   ...no `arument0` and `argument1` usage after this point
	 * we can just alias `m` to `arument0`, and `n` to `argument1` then
	 */

{
		uint firstBadStatement = 0;
		foreach(immutable idx, Node st; curfn.ebody.stats){
			if(cast(NodeStatementEmpty)st || cast(NodeStatementExpr)st || cast(NodeVarDecl)st){
				firstBadStatement = cast(uint)idx+1;
			}else{
				break;
			}
		}
		if(firstBadStatement > 0){
			bool[string] varsused;
			// scan statements, find assignments
			foreach(immutable idx, Node st; curfn.ebody.stats[0..firstBadStatement]){
				if(auto ass = cast(NodeStatementAss)st){
					// wow, assignment
					auto lv = cast(NodeId)ass.el;
					auto rv = cast(NodeId)ass.er;
					if(lv !is null && rv !is null){
						// "a = b"
					{ import std.stdio: stderr; stderr.writeln("found assignment: '", lv.name, "' = '", rv.name, "'"); }
						if(argvar(rv.name) >= 0 && argvar(lv.name) < 0){
							// "a = argumentx"
							if(lv.name in varsused || rv.name in varsused) continue; // no wai
							if(lv.name !in locals) continue; // not a local
							auto ai = argvar(rv.name);
							if(aaliases[ai].length && aaliases[ai] != lv.name) continue; // already have an alias(TODO)
							aaliases[ai] = lv.name; // possible alias
						}else{
							// check for reassignment
							if(lv.name !in varsused){
								// not used before, but used now; remove it from aliases
								foreach(ref an; aaliases) if(an == lv.name) an = null;
								varsused[lv.name] = true;
							}
						}
					}
				}
			}
			// now check if we have any assignment to aliased argument
			foreach(immutable idx, string an; aaliases){
				if(an.length == 0) continue;
				visitNodes(curfn.ebody, (Node n){
					if(auto ass = cast(NodeStatementAss)n){
						if(auto id = cast(NodeId)ass.el){
							auto ai = argvar(id.name);
							if(ai >= 0) aaliases[idx] = null;
							return VisitRes.Stop;
						}
					}
					return VisitRes.Continue;
				});
			}
			// remove aliases from locals(we don't need slots for 'em)
			foreach(immutable idx, string an; aaliases){
				if(an.length == 0) continue;
				locals.remove(an);
			}
			// dump aliases
		{
				import std.stdio: stderr;
				foreach(immutable idx, string an; aaliases){
					if(an.length) stderr.writeln("'argument", idx, "' is aliased to '", an, "'");
				}
			}
		}
	}

	// now assign slots to locals
	foreach(string name; locals.keys){
		firstFreeSlot = allocSlot(vdecls[name]);
		locals[name] = cast(ubyte)firstFreeSlot;
		++firstFreeSlot;
	}

	if(auto sid = curfn.name in VM.scripts){
		if(VM.scriptPCs[*sid] < 0) return; // can't override built-in function
	}

	uint sid = sid4name(curfn.name);
	/*debug(vm_exec)*/{ import std.stdio; writeln("compiling '", curfn.name, "' (", sid, ")..."); }
	auto startpc = emit(Op.enter);
	curfn.pcs = pc;
	compileStatement(curfn.ebody);
	emitPLit(curfn.loc, 0, 0);
	emit(Op.ret, 0);
	curfn.pce = pc;
	// patch enter
	VM.code[startpc] = cast(uint)( (locals.length<<24)|((maxUsedSlot+1)<<16)|(maxArgUsed<<8)|cast(ubyte)Op.enter );
	VM.scriptPCs[sid] = startpc;
	VM.scriptASTs[sid] = curfn;
}
