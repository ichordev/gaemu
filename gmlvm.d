module gmlvm is aliced;

import gmlparser;
import std.stdio : File;


enum Op {
  nop,

  copy, // copy regs; dest: dest reg; op0: first reg to copy; op1: number of regs to copy (0: no copy, lol)

  lnot, //: lognot
  neg,
  bneg,

  add,
  sub,
  mul,
  mod,
  div,
  rdiv,
  bor,
  bxor,
  band,
  shl,
  shr,
  lt,
  le,
  gt,
  ge,
  eq,
  ne,
  lor,
  land,
  lxor,

  plit, // dest becomes pool slot val (val: 2 bytes) -- load value from pool slot
  ilit, // dest becomes ilit val (val: short) -- load small integer literal
  xlit, // dest becomes integer(!) val (val: short) -- load small integer literal

  jump, // addr: 3 bytes
  xtrue, // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)
  xfalse, // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)

  call, // dest is result; op0: call frame (see below); op1: number of args
        // call frame is:
        //   new function frame (starting with return value)
        //   int scriptid (after op1+3 slots)
        // note that there should be no used registers after those (as that will be used as new function frame regs)

  //tcall, // same as call, but does tail call

  prim, // call "primitive" (built-in function); dest is result; op0: call frame (see below); op1: number of args
        // call frame is:
        //   new function frame (starting with return value)
        //   int primid (after op1+3 slots)
        // note that there should be no used registers after those (as that will be used as new function frame regs)

  //tprim, // same as prim, but does tail call

  enter, // dest: number of stack slots used (including result and args)
         // any function will ALWAYS starts with this

  ret, // dest is retvalue; it is copied to reg0; other stack items are discarded

  //as we are using refloads only in the last stage of assignment, they can create values
  lref, // load var reference to dest
  oref, // load object reference to dest; op0: int reg (obj id; -666: global object)
  fref, // load field reference; op0: varref; op1: int reg (field id); can't create fields
  fcrf, // load field reference; op0: varref; op1: int reg (field id); can create field
  iref, // load indexed reference; op0: varref; op1: int reg (index)
  mref, // load indexed reference; op0: varref; op1: int reg (first index); (op1+1): int reg (second index)

  rload, // load from op0-varref to dest
  rstore, // store to op0-varref from op1

  oload, // load object field to dest; op0: int reg (obj id; -666: global object); op1: int reg (field id)
  iload, // load indexed (as iref)
  mload, // load indexed (as mref)


  //`with` is done by copying `self` to another reg, execute the code and restore `self`

  siter, // start instance iterator; dest: iterid; op0: objid or instid
         // this is special: it will skip next instruction if iteration has at least one item
         // next instruction is always jump, which skips the loop
  niter, // dest is iterreg; do jump (pc is the same as in jump) if iteration is NOT complete
  kiter, // kill iterator, should be called to prevent memory leaks

  // so return from `with` should call kiter for all created iterators first

  // possible iterator management: preallocate slots for each non-overlapped "with";
  // let VM to free all iterators from those slots on function exit

  lirint, // dest = lrint(op0): do lrint() (or another fast float->int conversion)
}


final class VM {
public:
  alias Real = float;

private:
  uint[] code; // [0] is reserved
  uint[string] scripts; // name -> number
  uint[] scriptPCs; // by number; 0 is reserved
  // fixuper will not remove fixup chains, so we can replace script with new one
  Real[] vpool; // pool of values
  string[] spool; // pool of strings
  Real[] globals;
  uint[string] fields; // known fields and their offsets in object (and in globals too)

public:
  // value manipulation
  static bool isReal (Real v) {
    import std.math;
    return !isNaN(v);
  }

  static bool isString (Real v) {
    import std.math;
    return isNaN(v);
  }

  static bool isUndef (Real v) {
    import std.math;
    return (isNaN(v) && getNaNPayload(v) < 0);
  }

  // creates "undefined" value
  static Real undefValue () {
    import std.math;
    return NaN(-666);
  }

  // for invalid strings it returns 0
  static int strId (Real v) {
    import std.math;
    if (isNaN(v)) {
      auto res = getNaNPayload(v);
      static if (is(Real == float)) {
        return (res < 0 ? 0 : cast(int)res);
      } else {
        return (res < 0 || res > int.max ? 0 : cast(int)res);
      }
    } else {
      return 0;
    }
  }

  Real buildStrId (int id) {
    import std.math;
    static if (is(Real == float)) {
      assert(id >= 0 && id <= 0x3F_FFFF);
    } else {
      assert(id >= 0);
    }
    return NaN(id);
  }

public:
  this () {
    code.length = 1;
    scriptPCs.length = 1;
  }

  void compile (NodeFunc fn) {
    import std.stdio : stdout;
    auto spc = code.length;
    doCompileFunc(fn);
    while (spc < code.length) spc += dumpInstr(stdout, spc);
  }

  bool isJump (uint pc) {
    if (pc < 1 || pc >= code.length) return false;
    switch (code[pc].opCode) {
      case Op.jump:
        return true;
      default: break;
    }
    return false;
  }

  // returns instruction size
  uint dumpInstr (File fo, uint pc) {
    fo.writef("%08X: ", pc);
    if (pc == 0 || pc >= code.length) {
      fo.writeln("<INVALID>");
      return 1;
    }
    auto atp = opargs[code[pc].opCode];
    if (atp == OpArgs.None) {
      fo.writefln("%s", cast(Op)code[pc].opCode);
      return 1;
    }
    fo.writef("%-8s", cast(Op)code[pc].opCode);
    switch (atp) with (OpArgs) {
      case Dest: fo.writefln("dest:%s", code[pc].opDest); break;
      case DestOp0: fo.writefln("dest:%s, op0:%s", code[pc].opDest, code[pc].opOp0); break;
      case DestOp0Op1: fo.writefln("dest:%s, op0:%s, op1:%s", code[pc].opDest, code[pc].opOp0, code[pc].opOp1); break;
      case Dest2Bytes: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].op2Byte); break;
      case Dest3Bytes: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].op3Byte); break;
      case DestInt: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].opILit); break;
      case DestJump: fo.writefln("0x%08x", code[pc].op3Byte); break;
      case DestCall: fo.writefln("dest:%s; frame:%s; args:%s", code[pc].opDest, code[pc].opOp0, code[pc].opOp1); break;
      default: assert(0);
    }
    return 1;
  }

private:
  void doCompileFunc (NodeFunc fn) {

    uint pc () { return cast(uint)code.length; }

    uint emit (Op op, ubyte dest=0, ubyte op0=0, ubyte op1=0) {
      auto res = cast(uint)code.length;
      code ~= (op1<<24)|(op0<<16)|(dest<<8)|cast(ubyte)op;
      return res;
    }

    uint emit3Bytes (Op op, uint val) {
      assert(val <= 0xffffff);
      auto res = cast(uint)code.length;
      code ~= (val<<8)|cast(ubyte)op;
      return res;
    }

    uint emit2Bytes (Op op, ubyte dest, short val) {
      auto res = cast(uint)code.length;
      code ~= (val<<16)|(dest<<8)|cast(ubyte)op;
      return res;
    }

    uint emitJumpTo (Op op, uint addr) {
      assert(addr <= 0xffffff);
      auto res = cast(uint)code.length;
      code ~= cast(uint)op|(addr<<8);
      return res;
    }

    // this starts "jump chain", return new chain id
    uint emitJumpChain (uint chain, Op op) {
      assert(chain <= 0xffffff);
      auto res = cast(uint)code.length;
      code ~= cast(uint)op|(chain<<8);
      return res;
    }

    void fixJumpChain (uint chain, uint addr) {
      assert(chain <= 0xffffff);
      assert(addr <= 0xffffff);
      while (chain) {
        auto nc = op3Byte(code[chain]);
        code[chain] = (code[chain]&0xff)|(addr<<8);
        chain = nc;
      }
    }

    enum Slot {
      RVal = 0,
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

    assert(fn !is null);
    assert(fn.ebody !is null);
    assert(fn.name.length);

    void compileError(A...) (Loc loc, A args) {
      if (fn.pp !is null) {
        fn.pp.error(loc, args);
      } else {
        import std.stdio : stderr;
        stderr.writeln("ERROR at ", loc, ": ", args);
        string msg;
        foreach (immutable a; args) {
          import std.string : format;
          msg ~= "%s".format(a);
        }
        throw new ErrorAt(loc, msg);
      }
    }

    bool[256] slots;
    foreach (immutable idx; 0..Slot.max+1) slots[idx] = true; // used
    uint firstFreeSlot = Slot.max+1;
    uint maxUsedSlot = firstFreeSlot-1;

    ubyte allocSlot (Loc loc, int ddest=-1) {
      if (ddest >= 0) {
        assert(ddest < slots.length);
        return cast(ubyte)ddest;
      }
      foreach (immutable idx; firstFreeSlot..slots.length) {
        if (!slots[idx]) {
          if (idx > maxUsedSlot) maxUsedSlot = cast(uint)idx;
          slots[idx] = true;
          return cast(ubyte)idx;
        }
      }
      compileError(loc, "out of free slots");
      assert(0);
    }

    ubyte reserveCallSlots (Loc loc, uint resnum) {
      foreach_reverse (immutable idx, bool v; slots) {
        if (v) {
          if (idx+resnum+1 > slots.length) compileError(loc, "out of free slots");
          return cast(ubyte)(idx+1);
        }
      }
      compileError(loc, "out of free slots");
      assert(0);
    }

    void freeSlot (ubyte num) {
      if (num >= firstFreeSlot) {
        assert(slots[num]);
        slots[num] = false;
      }
    }

    ubyte[string] locals;
    uint[string] globals;
    Loc[string] vdecls; // for error messages

    // collect var declarations (gml is not properly scoped)
    visitNodes(fn.ebody, (Node n) {
      if (auto vd = cast(NodeVarDecl)n) {
        foreach (immutable idx, string name; vd.names) {
          if (name in locals) {
            if (vd.asGlobal) compileError(vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
          } else if (name in globals) {
            if (!vd.asGlobal) compileError(vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
          }
          vdecls[name] = vd.locs[idx];
          if (vd.asGlobal) {
            globals[name] = 0;
          } else {
            firstFreeSlot = allocSlot(vd.locs[idx]);
            locals[name] = cast(ubyte)firstFreeSlot;
            ++firstFreeSlot;
          }
        }
      }
      return VisitRes.Continue;
    });

    ushort allocNumConst (Real v) {
      return 0;
    }

    ushort allocStrConst (string s) {
      return 0;
    }

    // returns dest slot
    // can put value in desired dest
    ubyte compileExpr (Node nn, int ddest=-1, bool wantref=false) {
      ubyte doBinOp (Op op, NodeBinary n) {
        auto dest = allocSlot(n.loc, ddest);
        auto o0 = compileExpr(n.el);
        auto o1 = compileExpr(n.er);
        emit(op, dest, o0, o1);
        freeSlot(o0);
        freeSlot(o1);
        return dest;
      }

      ubyte doUnOp (Op op, NodeUnary n) {
        auto dest = allocSlot(n.loc, ddest);
        auto o0 = compileExpr(n.e);
        emit(op, dest, o0);
        freeSlot(o0);
        return dest;
      }

      return selectNode!ubyte(nn,
        (NodeLiteralNum n) {
          auto dest = allocSlot(n.loc, ddest);
          emit2Bytes(Op.plit, dest, allocNumConst(n.val));
          return dest;
        },
        (NodeLiteralStr n) {
          auto dest = allocSlot(n.loc, ddest);
          emit2Bytes(Op.plit, dest, allocStrConst(n.val));
          return dest;
        },
        (NodeUnaryParens n) => compileExpr(n.e, ddest, wantref),
        (NodeUnaryNot n) => doUnOp(Op.lnot, n),
        (NodeUnaryNeg n) => doUnOp(Op.neg, n),
        (NodeUnaryBitNeg n) => doUnOp(Op.bneg, n),
        (NodeBinaryAss n) {
          if (cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null) compileError(n.loc, "assignment to rvalue");
          auto src = compileExpr(n.er);
          auto dest = compileExpr(n.el, true);
          emit(Op.rstore, dest, src);
          freeSlot(src);
          freeSlot(dest);
          return 0;
        },
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
        (NodeFCall n) {
          auto dest = allocSlot(n.loc, ddest);
          if (auto id = cast(NodeId)n.fe) {
          } else {
            compileError(n.loc, "invalid function call");
          }
          ubyte[16] slt;
          if (n.args.length > 16) compileError(n.loc, "too many arguments in function call");
          foreach (immutable idx, Node a; n.args) slt[idx] = compileExpr(a);
          auto fcs = reserveCallSlots(n.loc, cast(uint)n.args.length+Slot.Argument0+1);
          /*
          foreach (immutable idx; 0..n.args.length) {
            emit(Op.copy, cast(ubyte)(fcs+Slot.Argument0+idx), slt[idx], 1); //TODO: optimize
          }
          */
          {
            uint sidx = 0;
            while (sidx < n.args.length) {
              uint eidx = sidx+1;
              while (eidx < n.args.length && slt[eidx] == slt[eidx-1]+1) ++eidx;
              emit(Op.copy, cast(ubyte)(fcs+Slot.Argument0+sidx), slt[sidx], cast(ubyte)(eidx-sidx));
              sidx = eidx;
            }
          }
          foreach (immutable idx, Node a; n.args) freeSlot(slt[idx]);
          // put script id
          if (auto aptr = (cast(NodeId)n.fe).name in scripts) {
            // known script
            emit2Bytes(Op.ilit, cast(ubyte)(fcs+Slot.Argument0+n.args.length), cast(short)(*aptr));
          } else {
            auto snum = cast(uint)scriptPCs.length;
            if (snum > 32767) compileError(n.loc, "too many scripts");
            scriptPCs ~= 0;
            scripts[(cast(NodeId)n.fe).name] = snum;
            // unknown script
            emit2Bytes(Op.ilit, cast(ubyte)(fcs+Slot.Argument0+n.args.length), cast(short)snum);
          }
          // emit call
          emit(Op.call, dest, fcs, cast(ubyte)n.args.length);
          return dest;
        },
        (NodeId n) {
          switch (n.name) {
            case "argument0": return cast(ubyte)(Slot.Argument0+0);
            case "argument1": return cast(ubyte)(Slot.Argument0+1);
            case "argument2": return cast(ubyte)(Slot.Argument0+2);
            case "argument3": return cast(ubyte)(Slot.Argument0+3);
            case "argument4": return cast(ubyte)(Slot.Argument0+4);
            case "argument5": return cast(ubyte)(Slot.Argument0+5);
            case "argument6": return cast(ubyte)(Slot.Argument0+6);
            case "argument7": return cast(ubyte)(Slot.Argument0+7);
            case "argument8": return cast(ubyte)(Slot.Argument0+8);
            case "argument9": return cast(ubyte)(Slot.Argument0+9);
            case "argument10": return cast(ubyte)(Slot.Argument0+10);
            case "argument11": return cast(ubyte)(Slot.Argument0+11);
            case "argument12": return cast(ubyte)(Slot.Argument0+12);
            case "argument13": return cast(ubyte)(Slot.Argument0+13);
            case "argument14": return cast(ubyte)(Slot.Argument0+14);
            case "argument15": return cast(ubyte)(Slot.Argument0+15);
            case "self": return cast(ubyte)(Slot.Self);
            case "other": return cast(ubyte)(Slot.Other);
            default:
          }
          if (auto v = n.name in locals) return *v;
          return 0;
        },
        (NodeDot n) {
        },
        (NodeIndex n) {
          //if (auto r = visitNodes(n.ei0, dg)) return r;
          //if (auto r = visitNodes(n.ei1, dg)) return r;
          //return visitNodes(n.e, dg);
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }

    void compile (Node nn) {
      assert(nn !is null);
      return selectNode!void(nn,
        (NodeVarDecl n) {},
        (NodeBlock n) {
          foreach (Node st; n.stats) compile(st);
        },
        (NodeStatementEmpty n) {},
        (NodeStatementExpr n) {
          freeSlot(compileExpr(n.e));
        },
        (NodeReturn n) {
          if (n.e is null) {
            emit(Op.ret, Slot.RVal);
          } else {
            auto dest = compileExpr(n.e);
            emit(Op.ret, dest);
            freeSlot(dest);
          }
        },
        (NodeWith n) {
          assert(0);
        },
        (NodeIf n) {
          auto cs = compileExpr(n.ec);
          freeSlot(cs); // yep, free it here
          emit(Op.xtrue);
          uint jfc = 0;
          // simple optimization
          jfc = emitJumpChain(0, Op.jump);
          compile(n.et);
          if (n.ef !is null) {
            auto exc = emitJumpChain(0, Op.jump);
            fixJumpChain(jfc, pc);
            jfc = exc;
            compile(n.ef);
          }
          fixJumpChain(jfc, pc);
        },
        (NodeStatementBreak n) {
          assert(0);
        },
        (NodeStatementContinue n) {
          assert(0);
        },
        (NodeFor n) {
          /*
          if (auto r = visitNodes(n.einit, dg)) return r;
          if (auto r = visitNodes(n.econd, dg)) return r;
          if (auto r = visitNodes(n.enext, dg)) return r;
          return visitNodes(n.ebody, dg);
          */
          assert(0);
        },
        (NodeWhile n) {
          assert(0);
        },
        (NodeDoUntil n) {
          assert(0);
        },
        (NodeRepeat n) {
          assert(0);
        },
        (NodeSwitch n) {
          /*
          if (auto r = visitNodes(n.e, dg)) return r;
          foreach (ref ci; n.cases) {
            if (auto r = visitNodes(ci.e, dg)) return r;
            if (auto r = visitNodes(ci.st, dg)) return r;
          }
          return null;
          */
          assert(0);
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }

    auto startpc = emit(Op.enter);
    compile(fn.ebody);
    emit(Op.ret);
    // patch enter
    code[startpc] = (maxUsedSlot<<8)|cast(ubyte)Op.enter;
    if (fn.name !in scripts) {
      auto snum = cast(uint)scriptPCs.length;
      if (snum > 32767) compileError(fn.loc, "too many scripts");
      scriptPCs ~= startpc;
      scripts[fn.name] = snum;
    }
  }

static:
  enum OpArgs {
    None,
    Dest,
    DestOp0,
    DestOp0Op1,
    Dest2Bytes,
    Dest3Bytes,
    DestInt,
    DestJump,
    DestCall,
  }
  immutable OpArgs[ubyte] opargs;
  shared static this () {
    with(OpArgs) opargs = [
      Op.nop: None,
      Op.copy: DestOp0Op1,
      Op.lnot: DestOp0, //: lognot
      Op.neg: DestOp0,
      Op.bneg: DestOp0,

      Op.add: DestOp0Op1,
      Op.sub: DestOp0Op1,
      Op.mul: DestOp0Op1,
      Op.mod: DestOp0Op1,
      Op.div: DestOp0Op1,
      Op.rdiv: DestOp0Op1,
      Op.bor: DestOp0Op1,
      Op.bxor: DestOp0Op1,
      Op.band: DestOp0Op1,
      Op.shl: DestOp0Op1,
      Op.shr: DestOp0Op1,
      Op.lt: DestOp0Op1,
      Op.le: DestOp0Op1,
      Op.gt: DestOp0Op1,
      Op.ge: DestOp0Op1,
      Op.eq: DestOp0Op1,
      Op.ne: DestOp0Op1,
      Op.lor: DestOp0Op1,
      Op.land: DestOp0Op1,
      Op.lxor: DestOp0Op1,

      Op.plit: Dest2Bytes,
      Op.ilit: DestInt,
      Op.xlit: DestInt,

      Op.jump: DestJump,
      Op.xtrue: Dest,
      Op.xfalse: Dest,

      Op.call: DestCall,
      //Op.tcall: DestCall,

      Op.prim: DestCall,
      //Op.tprim: DestCall,

      Op.enter: Dest,

      Op.ret: Dest,

      Op.lref: DestOp0,
      Op.oref: DestOp0,
      Op.fref: DestOp0Op1,
      Op.fcrf: DestOp0Op1,
      Op.iref: DestOp0Op1,
      Op.mref: DestOp0Op1,

      Op.rload: DestOp0,
      Op.rstore: DestOp0,

      Op.oload: DestOp0Op1,
      Op.iload: DestOp0Op1,
      Op.mload: DestOp0Op1,


      Op.siter: DestOp0,
      Op.niter: DestJump,
      Op.kiter: Dest,

      Op.lirint: DestOp0, // dest = lrint(op0): do lrint() (or another fast float->int conversion)
    ];
  }
}


// ////////////////////////////////////////////////////////////////////////// //
private:
ubyte opCode (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op&0xff); }
ubyte opDest (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>8)&0xff); }
ubyte opOp0 (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>16)&0xff); }
ubyte opOp1 (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>24)&0xff); }
short opILit (uint op) pure nothrow @safe @nogc { pragma(inline, true); return cast(short)((op>>16)&0xffff); }
uint op3Byte (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op>>8); }
uint op2Byte (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op>>16); }

uint opMakeILit (ubyte op, byte dest, short val) pure nothrow @safe @nogc { pragma(inline, true); return ((val<<16)|((dest&0xff)<<8)|op); }
uint opMake3Byte (ubyte op, uint val) pure nothrow @safe @nogc { pragma(inline, true); assert(val <= 0xffffff); return (val<<8)|op; }
