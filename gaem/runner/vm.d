/* GML runner
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
module gaem.runner.vm is aliced;

import std.stdio : File;
import std.traits;

import gaem.ungmk;
import gaem.parser;

import gaem.runner.strpool;
import gaem.runner.value;
import gaem.runner.opcodes;


// ////////////////////////////////////////////////////////////////////////// //
public static struct VM {
@disable this ();
@disable this (this);

public:
  enum Slot {
    Self,
    Other,
    Argument0,
    Argument1,
    Argument2,
    Argument3,
    Argument4,
    Argument5,
    Argument6,
    Argument7,
    Argument8,
    Argument9,
    Argument10,
    Argument11,
    Argument12,
    Argument13,
    Argument14,
    Argument15,
  }

private:
  alias PrimDg = Real delegate (uint pc, Real* bp, ubyte argc);

  package(gaem.runner):
  __gshared uint[] code; // [0] is reserved
  __gshared uint[string] scripts; // name -> number
  __gshared string[uint] scriptNum2Name;
  __gshared int[] scriptPCs; // by number; 0 is reserved; <0: primitive number
  __gshared NodeFunc[] scriptASTs; // by number
  __gshared PrimDg[] prims; // by number
  __gshared Real[] vpool; // pool of values
  __gshared Real[] globals;
  __gshared Gmk gmk;


  shared static this () {
    code.length = 1;
    VM.scriptPCs.length = 1;
    scriptASTs.length = 1;
    VM.prims.length = 1;
  }

static public:
  void setGmk (Gmk agmk) {
    assert(agmk !is null);
    assert(gmk is null);
    gmk = agmk;
  }

  int objId (string name) {
    if (gmk is null) return -1;
    if (auto o = gmk.objByName(name)) return o.idx;
    return -1;
  }

  void opIndexAssign(DG) (DG dg, string name) if (isCallable!DG) {
    assert(name.length > 0);
    uint sid;
    if (auto sptr = name in VM.scripts) {
      sid = *sptr;
    } else {
      sid = cast(uint)VM.scriptPCs.length;
      if (sid > 32767) assert(0, "too many scripts");
      assert(scriptASTs.length == sid);
      // reserve slots
      VM.scriptPCs ~= 0;
      scriptASTs ~= null;
      scriptNum2Name[sid] = name;
      VM.scripts[name] = sid;
    }
    auto pnum = cast(uint)VM.prims.length;
    assert(pnum);
    VM.scriptPCs[sid] = -cast(int)pnum;
    VM.prims ~= register(dg);
  }

  Real exec(A...) (string name, A args) {
    static assert(A.length < 16, "too many arguments");
    auto sid = VM.scripts[name];
    assert(curframe is null);
    // create frame
    if (stack.length < 65536) stack.length = 65536;
    curframe = &frames[0];
    curframe.bp = 0;
    curframe.script = sid;
    stack[0..VM.Slot.max+1] = 0;
    foreach (immutable idx, immutable a; args) {
      static if (is(typeof(a) : const(char)[])) {
        //FIXME
        assert(0);
      } else static if (is(typeof(a) : Real)) {
        stack[VM.Slot.Argument0+idx] = cast(Real)a;
      } else {
        static assert(0, "invalid argument type");
      }
    }
    //{ import std.stdio; writeln(VM.scriptPCs[sid]); }
    return doExec(VM.scriptPCs[sid]);
  }
}


// ////////////////////////////////////////////////////////////////////////// //
private:
static struct CallFrame {
  uint script; // script id
  uint bp; // base pointer (address of the current frame in stack)
  uint pc; // current pc; will be set on "call"; it is used by callee
  ubyte rval; // slot for return value; will be set on "call"; it is used by callee
  @disable this (this);
}


__gshared CallFrame[32768] frames;
__gshared CallFrame* curframe;
__gshared Real[] stack;

void runtimeError(A...) (uint pc, A args) {
  import std.stdio : stderr;
  stderr.writef("ERROR at %08X: ", pc);
  stderr.writeln(args);
  // try to build stack trace
  if (curframe !is null) {
    curframe.pc = pc;
    auto cf = curframe;
    for (;;) {
      stderr.writefln("%08X: %s", cf.pc, VM.scriptNum2Name[cf.script]);
      if (cf is frames.ptr) break; // it's not legal to compare pointers from different regions
      --cf;
    }
  }
  throw new Exception("fuuuuu");
}


// current frame must be properly initialized
Real doExec (uint pc) {
  enum BinOpMixin(string op, string ack="") =
    "auto dest = opx.opDest;\n"~
    "auto o0 = bp[opx.opOp0];\n"~
    "auto o1 = bp[opx.opOp1];\n"~
    ack~
    "if (!o0.isReal || !o1.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "bp[dest] = o0"~op~"o1;\n"~
    "break;";
  enum BinIOpMixin(string op, string ack="") =
    "auto dest = opx.opDest;\n"~
    "auto o0 = bp[opx.opOp0];\n"~
    "auto o1 = bp[opx.opOp1];\n"~
    ack~
    "if (!o0.isReal || !o1.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "bp[dest] = lrint(o0)"~op~"lrint(o1);\n"~
    "break;";

  enum BinCmpMixin(string op) =
    "auto dest = opx.opDest;\n"~
    "auto o0 = bp[opx.opOp0];\n"~
    "auto o1 = bp[opx.opOp1];\n"~
    "assert(!o0.isUndef && !o1.isUndef);\n"~
    "if (o0.isString) {\n"~
    "  if (!o1.isString) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "  string s0 = getDynStr(o0.getStrId);\n"~
    "  string s1 = getDynStr(o1.getStrId);\n"~
    "  bp[dest] = (s0 "~op~" s1 ? 1 : 0);\n"~
    "} else {\n"~
    "  assert(o0.isReal);\n"~
    "  if (!o1.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "  bp[dest] = (o0 "~op~" o1 ? 1 : 0);\n"~
    "}\n"~
    "break;";

  enum BinLogMixin(string op) =
    "auto dest = opx.opDest;\n"~
    "auto o0 = bp[opx.opOp0];\n"~
    "auto o1 = bp[opx.opOp1];\n"~
    "assert(!o0.isUndef && !o1.isUndef);\n"~
    "if (o0.isString) {\n"~
    "  if (!o1.isString) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "  string s0 = getDynStr(o0.getStrId);\n"~
    "  string s1 = getDynStr(o1.getStrId);\n"~
    "  bp[dest] = (s0.length "~op~" s1.length ? 1 : 0);\n"~
    "} else {\n"~
    "  assert(o0.isReal);\n"~
    "  if (!o1.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), `invalid type`);\n"~
    "  bp[dest] = (lrint(o0) "~op~" lrint(o1) ? 1 : 0);\n"~
    "}\n"~
    "break;";

  static if (is(Real == float)) {
    import core.stdc.math : lrint = lrintf;
  } else static if (is(Real == double)) {
    import core.stdc.math : lrint;
  } else {
    static assert(0, "wtf?!");
  }
  assert(curframe !is null);
  assert(pc > 0 && pc < VM.code.length);
  assert(VM.code[pc].opCode == Op.enter);
  assert(stack.length > 0);
  auto bp = &stack[curframe.bp];
  auto origcf = curframe;
  auto cptr = VM.code.ptr+pc;
  //if (stack.length < 65536) stack.length = 65536;
  debug(vm_exec) uint maxslots = VM.Slot.max+1;
  for (;;) {
    debug(vm_exec) {
      import std.stdio : stderr;
      foreach (immutable idx; 0..maxslots) stderr.writeln("  ", idx, ": ", bp[idx]);
      dumpInstr(stderr, cast(uint)(cptr-VM.code.ptr));
    }
    auto opx = *cptr++;
    switch (opx.opCode) {
      case Op.nop:
        break;

      case Op.copy: // copy regs; dest: dest reg; op0: first reg to copy; op1: number of regs to copy (0: no copy, lol)
        import core.stdc.string : memmove;
        auto dest = opx.opDest;
        auto first = opx.opOp0;
        auto count = opx.opOp1;
        if (count) memmove(bp+dest, bp+first, count*Real.sizeof);
        break;

      case Op.lnot: // lognot
        auto dest = opx.opDest;
        auto o0 = bp[opx.opOp0];
        assert(!o0.isUndef);
        if (o0.isString) {
          auto s0 = getDynStr(o0.getStrId);
          bp[dest] = (s0.length ? 0 : 1);
        } else {
          bp[dest] = (lrint(o0) ? 0 : 1);
        }
        break;
      case Op.neg:
        auto dest = opx.opDest;
        auto o0 = bp[opx.opOp0];
        if (!o0.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "invalid type");
        bp[dest] = -o0;
        break;
      case Op.bneg:
        auto dest = opx.opDest;
        auto o0 = bp[opx.opOp0];
        if (!o0.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "invalid type");
        bp[dest] = cast(int)(~(cast(int)lrint(o0)));
        break;

      case Op.add:
        auto dest = opx.opDest;
        auto o0 = bp[opx.opOp0];
        auto o1 = bp[opx.opOp1];
        assert(!o0.isUndef && !o1.isUndef);
        if (o0.isString) {
          if (!o1.isString) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "invalid type");
          string s0 = getDynStr(o0.getStrId);
          string s1 = getDynStr(o1.getStrId);
          //FIXME
          if (s0.length == 0) {
            bp[dest] = o1;
          } else if (s1.length == 0) {
            bp[dest] = o0;
          } else {
            bp[dest] = buildStrId(newDynStr(s0~s1));
          }
        } else {
          assert(o0.isReal);
          if (!o1.isReal) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "invalid type");
          bp[dest] = o0+o1;
        }
        break;
      case Op.sub: mixin(BinOpMixin!"-");
      case Op.mul: mixin(BinOpMixin!"*");
      case Op.mod: mixin(BinOpMixin!("%", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "division by zero"); }));
      case Op.div: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "division by zero"); }));
      case Op.rdiv: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "division by zero"); }));
      case Op.bor: mixin(BinIOpMixin!"|");
      case Op.bxor: mixin(BinIOpMixin!"^");
      case Op.band: mixin(BinIOpMixin!"&");
      case Op.shl: mixin(BinIOpMixin!"<<");
      case Op.shr: mixin(BinIOpMixin!">>");

      case Op.lt: mixin(BinCmpMixin!"<");
      case Op.le: mixin(BinCmpMixin!"<=");
      case Op.gt: mixin(BinCmpMixin!">");
      case Op.ge: mixin(BinCmpMixin!">=");
      case Op.eq: mixin(BinCmpMixin!"==");
      case Op.ne: mixin(BinCmpMixin!"!=");

      case Op.lor: mixin(BinLogMixin!"||");
      case Op.land: mixin(BinLogMixin!"&&");
      case Op.lxor: assert(0);

      case Op.plit: // dest becomes pool slot val (val: 2 bytes) -- load value from pool slot
        auto dest = opx.opDest;
        uint idx = cast(ushort)opx.op2Byte;
        if (idx == ushort.max) {
          assert((*cptr).opCode == Op.skip);
          idx = (*cptr++).op3Byte;
        }
        bp[dest] = VM.vpool.ptr[idx];
        break;
      case Op.ilit: // dest becomes ilit val (val: short) -- load small integer literal
      case Op.slit:
        auto dest = opx.opDest;
        bp[dest] = opx.opILit;
        break;
      case Op.xlit: // dest becomes integer(!) val (val: short) -- load small integer literal
        auto dest = opx.opDest;
        *cast(uint*)(bp+dest) = opx.opILit;
        break;

      case Op.jump: // addr: 3 bytes
        cptr = VM.code.ptr+opx.op3Byte;
        break;
      case Op.xtrue: // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)
        if (lrint(bp[opx.opDest]) != 0) ++cptr;
        break;
      case Op.xfalse: // dest is reg to check; skip next instruction if dest is "gml false" (i.e. fabs(v) >= 0.5`)
        if (lrint(bp[opx.opDest]) == 0) ++cptr;
        break;

      case Op.call: // dest is result; op0: call frame (see below); op1: number of args
            // call frame is:
            //   new function frame
            //   int scriptid (after op1+3 slots)
            // note that there should be no used registers after those (as that will be used as new function frame regs)
        auto sid = *cast(uint*)(bp+opx.opOp0+VM.Slot.Argument0+opx.opOp1);
        if (sid >= VM.scriptPCs.length) runtimeError(cast(uint)(cptr-VM.code.ptr-1), "invalid script id");
        pc = VM.scriptPCs.ptr[sid];
        if (pc < 1 || pc >= VM.code.length) {
          if (pc&0x8000_0000) {
            // this is primitive
            uint pid = -cast(int)pc;
            if (pid >= VM.prims.length) assert(0, "wtf?!");
            bp[opx.opDest] = VM.prims.ptr[pid](cast(uint)(cptr-VM.code.ptr-1), bp+opx.opOp0, opx.opOp1);
            break;
          } else {
            string scname;
            foreach (auto kv; VM.scripts.byKeyValue) if (kv.value == sid) { scname = kv.key; break; }
            runtimeError(cast(uint)(cptr-VM.code.ptr-1), "trying to execute undefined script '", scname, "'");
          }
        }
        debug(vm_exec) {
          import std.stdio : stderr;
          stderr.writeln("calling '", scriptNum2Name[sid], "'");
          foreach (immutable aidx; 0..opx.opOp1) stderr.writeln("  ", bp[opx.opOp0+VM.Slot.Argument0+aidx]);
        }
        // if this is tail call, just do it as tail call then
        // but don't optimize out top-level call, heh
        if (curframe !is origcf && (*cptr).opCode == Op.ret) {
          import core.stdc.string : memcpy;
          // yay, it is a tail call!
          // copy arguments (it's safe to use `memcpy()` here); `self` and `other` are automatically ok
          if (opx.opOp1) memcpy(bp+VM.Slot.Argument0, bp+opx.opOp0+VM.Slot.Argument0, Real.sizeof*opx.opOp1);
          // simply replace current frame with new one
        } else {
          bp[opx.opOp0..opx.opOp0+VM.Slot.Argument0] = bp[0..VM.Slot.Argument0]; // copy `self` and `other`
          curframe.pc = cast(uint)(cptr-VM.code.ptr);
          curframe.rval = opx.opDest;
          ++curframe;
          curframe.bp = curframe[-1].bp+opx.opOp0;
          bp = &stack[curframe.bp];
        }
        curframe.script = sid;
        cptr = VM.code.ptr+VM.scriptPCs.ptr[sid];
        //assert((*cptr).opCode == Op.enter);
        // clear unused arguments, if any
        // we know that first instruction is always Op.enter, use that fact
        auto aused = (*cptr).opDest+1;
        //{ import std.stdio; writeln("aused=", aused, "; op1=", opx.opOp1); }
        if (aused > opx.opOp1) bp[VM.Slot.Argument0+opx.opOp1..VM.Slot.Argument0+aused] = 0;
        break;

      case Op.enter: // dest: number of arguments used; op0: number of stack slots used (including result and args); op1: number of locals
        if (curframe.bp+opx.opOp0 > stack.length) {
          stack.length = curframe.bp+opx.opOp0;
          bp = &stack[curframe.bp];
        }
        //foreach (immutable idx; VM.Slot.max+1..VM.Slot.max+1+opx.opOp1) bp[idx] = 0; // clear locals
        if (opx.opOp1) bp[VM.Slot.max+1..VM.Slot.max+1+opx.opOp1] = 0; // clear locals
        debug(vm_exec) maxslots = opx.opOp0;
        debug(vm_exec) { import std.stdio : stderr; foreach (immutable idx; VM.Slot.Argument0..VM.Slot.Argument15+1) stderr.writeln("  :", bp[idx]); }
        break;

      case Op.ret: // dest is retvalue; it is copied to reg0; other stack items are discarded
        if (curframe is origcf) return bp[opx.opDest]; // done
        assert(cast(uint)curframe > cast(uint)origcf);
        --curframe;
        auto rv = bp[opx.opDest];
        // remove stack frame
        bp = &stack[curframe.bp];
        cptr = VM.code.ptr+curframe.pc;
        bp[curframe.rval] = rv;
        debug(vm_exec) { import std.stdio : stderr; stderr.writeln("RET(", curframe.rval, "): ", rv); }
        break;

      //as we are using refloads only in the last stage of assignment, they can create values
      /*
      case Op.lref: // load slot reference to dest
        *cast(int*)bp[opx.opDest] = opx.opOp0;
        break;
      case Op.fref: // load field reference; op0: obj id; op1: int! reg (field id); can create fields
        assert(0);
      case Op.fval: // load field value; op0: obj id; op1: int! reg (field id)
        assert(0);
      case Op.i1ref: // load indexed reference; op0: varref; op1: index; can create arrays
        assert(0);
      case Op.i2ref: // load indexed reference; op0: varref; op1: first index; (op1+1): second index; can create arrays
        assert(0);
      case Op.i1val: // load indexed value; op0: varref; op1: index
        assert(0);
      case Op.i2val: // load indexed value; op0: varref; op1: first index; (op1+1): second index
        assert(0);

      case Op.rstore: // store to op0-varref from op1
        auto x = *cast(int*)bp[opx.opOp0];
        assert(x >= 0 && x <= 255);
        bp[x] = bp[opx.opOp1];
        break;
      */
      //case Op.lstore: // store value *from* dest into local slot; op0: slot number
      //  bp[opx.opOp0] = bp[opx.opDest];
      //  break;
      /*
      case Op.fstore: // store value *from* dest into field; op0: obj id; op1: int! reg (field id); can create fields
        assert(0);
      case Op.i1store: // store value *from* dest into indexed reference; op0: varref; op1: index; can create arrays
        assert(0);
      case Op.i2store: // store value *from* dest into indexed reference; op0: varref; op1: first index; (op1+1): second index; can create arrays
        assert(0);
      */

      //case Op.oload: // load object field to dest; op0: int reg (obj id; -666: global object); op1: int reg (field id)
      //case Op.iload: // load indexed (as iref)
      //case Op.mload: // load indexed (as mref)
      default: assert(0);
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// create primitive delegate for D delegate/function
// D function can include special args like:
//   Real* -- bp
//   Real[] -- all args
//   string, integer, float
//   no ref args are supported, sorry
private VM.PrimDg register(DG) (DG dg) @trusted if (isCallable!DG) {
  import core.stdc.math : lrint;
  assert(dg !is null);
  // build call thunk
  return delegate (uint pc, Real* bp, ubyte argc) {
    // prepare arguments
    Parameters!DG arguments;
    alias rt = ReturnType!dg;
    // (Real* bp, ubyte argc)
    static if (arguments.length == 2 && is(typeof(arguments[1]) == Real*) && is(typeof(arguments[2]) : int)) {
      static if (is(rt == void)) {
        cast(void)dg(bp, cast(typeof(arguments[2]))argc);
        return cast(Real)0;
      } else {
        return Value(dg(bp, cast(typeof(arguments[2]))argc));
      }
    } else {
      foreach (immutable idx, ref arg; arguments) {
        // is last argument suitable for `withobj`?
        static if (is(typeof(arg) == Real[])) {
          arg = bp[0..VM.Slot.Argument0+argc];
        } else {
          static assert(idx < 16, "too many arguments required");
          static if (is(typeof(arg) == const(char)[]) || is(typeof(arg) == string)) {
            auto v = bp[VM.Slot.Argument0+idx];
            if (!v.isString) runtimeError(pc, "invalid argument type");
            arg = getDynStr(v.getStrId);
          } else static if (is(typeof(arg) == bool)) {
            auto v = bp[VM.Slot.Argument0+idx];
                 if (v.isString) arg = (v.getStrId != 0);
            else if (v.isReal) arg = (lrint(v) != 0);
            else runtimeError(pc, "invalid argument type");
          } else static if (is(typeof(arg) : long) || is(typeof(arg) : double)) {
            auto v = bp[VM.Slot.Argument0+idx];
            if (!v.isReal) runtimeError(pc, "invalid D argument type");
            arg = cast(typeof(arg))v;
          }
        }
      }
      static if (is(rt == void)) {
        cast(void)dg(arguments);
        return cast(Real)0;
      } else {
        return Value(dg(arguments));
      }
    }
  };
}
