module gmlvm is aliced;

import gmlparser;
import std.stdio : File;


// ////////////////////////////////////////////////////////////////////////// //
alias Real = float;


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
static int getStrId (Real v) {
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


// ////////////////////////////////////////////////////////////////////////// //
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
        //   new function frame
        //   int scriptid (after op1+3 slots)
        // note that there should be no used registers after those (as that will be used as new function frame regs)

  //tcall, // same as call, but does tail call

  prim, // call "primitive" (built-in function); dest is result; op0: call frame (see below); op1: number of args
        // call frame is:
        //   new function frame (starting with return value)
        //   int primid (after op1+3 slots)
        // note that there should be no used registers after those (as that will be used as new function frame regs)

  //tprim, // same as prim, but does tail call

  enter, // op0: number of stack slots used (including result and args); op1: number of locals
         // any function will ALWAYS starts with this

  ret, // dest is retvalue; it is copied to reg0; other stack items are discarded

  //as we are using refloads only in the last stage of assignment, they can create values
  lref, // load slot reference to dest
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


// ////////////////////////////////////////////////////////////////////////// //
final class VM {
public:

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
      case Op0Op1: fo.writefln("op0:%s, op1:%s", code[pc].opOp0, code[pc].opOp1); break;
      default: assert(0);
    }
    return 1;
  }

private:
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

    ushort allocNumConst (Real v, Loc loc) {
      //FIXME: speed it up!
      foreach (immutable idx, Real vp; vpool) {
        if (vp == v) return cast(ushort)idx;
      }
      auto vpi = cast(uint)vpool.length;
      if (vpi > ushort.max) compileError(loc, "too many constants");
      vpool ~= v;
      return cast(ushort)vpi;
    }

    ushort allocStrConst (string s, Loc loc) {
      //FIXME: speed it up!
      foreach (immutable idx, string vp; spool) {
        if (vp == s) return allocNumConst(buildStrId(cast(uint)idx), loc);
      }
      auto sidx = cast(uint)spool.length;
      spool ~= s;
      return allocNumConst(buildStrId(sidx), loc);
    }

    int varSlot (string name) {
      switch (name) {
        case "argument0": return Slot.Argument0+0;
        case "argument1": return Slot.Argument0+1;
        case "argument2": return Slot.Argument0+2;
        case "argument3": return Slot.Argument0+3;
        case "argument4": return Slot.Argument0+4;
        case "argument5": return Slot.Argument0+5;
        case "argument6": return Slot.Argument0+6;
        case "argument7": return Slot.Argument0+7;
        case "argument8": return Slot.Argument0+8;
        case "argument9": return Slot.Argument0+9;
        case "argument10": return Slot.Argument0+10;
        case "argument11": return Slot.Argument0+11;
        case "argument12": return Slot.Argument0+12;
        case "argument13": return Slot.Argument0+13;
        case "argument14": return Slot.Argument0+14;
        case "argument15": return Slot.Argument0+15;
        case "self": return Slot.Self;
        case "other": return Slot.Other;
        default:
      }
      if (auto v = name in locals) return *v;
      return -1;
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
          import core.stdc.math : lrint;
          auto dest = allocSlot(n.loc, ddest);
          if (lrint(n.val) == n.val && lrint(n.val) >= short.min && lrint(n.val) <= short.max) {
            emit2Bytes(Op.ilit, dest, cast(short)lrint(n.val));
          } else {
            emit2Bytes(Op.plit, dest, allocNumConst(n.val, n.loc));
          }
          return dest;
        },
        (NodeLiteralStr n) {
          auto dest = allocSlot(n.loc, ddest);
          emit2Bytes(Op.plit, dest, allocStrConst(n.val, n.loc));
          return dest;
        },
        (NodeUnaryParens n) => compileExpr(n.e, ddest, wantref),
        (NodeUnaryNot n) => doUnOp(Op.lnot, n),
        (NodeUnaryNeg n) => doUnOp(Op.neg, n),
        (NodeUnaryBitNeg n) => doUnOp(Op.bneg, n),
        (NodeBinaryAss n) {
          if (cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null) compileError(n.loc, "assignment to rvalue");
          if (auto did = cast(NodeId)n.el) {
            auto vdst = varSlot(did.name);
            assert(vdst >= 0);
            auto dest = compileExpr(n.er, ddest:vdst);
            freeSlot(dest);
          } else {
            auto src = compileExpr(n.er);
            auto dest = compileExpr(n.el, wantref:true);
            emit(Op.rstore, dest, src);
            freeSlot(src);
            freeSlot(dest);
          }
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
          if (cast(NodeId)n.fe is null) compileError(n.loc, "invalid function call");
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
            emit2Bytes(Op.xlit, cast(ubyte)(fcs+Slot.Argument0+n.args.length), cast(short)(*aptr));
          } else {
            auto snum = cast(uint)scriptPCs.length;
            if (snum > 32767) compileError(n.loc, "too many scripts");
            scriptPCs ~= 0;
            scripts[(cast(NodeId)n.fe).name] = snum;
            // unknown script
            emit2Bytes(Op.xlit, cast(ubyte)(fcs+Slot.Argument0+n.args.length), cast(short)snum);
          }
          // emit call
          emit(Op.call, dest, fcs, cast(ubyte)n.args.length);
          return dest;
        },
        (NodeId n) {
          if (wantref) {
            auto vsl = varSlot(n.name);
            assert(vsl >= 0);
            auto dest = allocSlot(n.loc, ddest);
            emit(Op.lref, dest, cast(ubyte)vsl);
            return dest;
          } else {
            auto vsl = varSlot(n.name);
            assert(vsl >= 0);
            auto dest = allocSlot(n.loc, ddest);
            if (dest == vsl) return dest;
            emit(Op.copy, dest, cast(ubyte)vsl, 1);
            return dest;
          }
          assert(0);
          //return 0;
        },
        (NodeDot n) {
          assert(0);
        },
        (NodeIndex n) {
          //if (auto r = visitNodes(n.ei0, dg)) return r;
          //if (auto r = visitNodes(n.ei1, dg)) return r;
          //return visitNodes(n.e, dg);
          assert(0);
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
            emit2Bytes(Op.ilit, 0, 0);
            emit(Op.ret, 0);
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
          emit(Op.xtrue, cs);
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
    code[startpc] = (locals.length<<24)|((maxUsedSlot+1)<<16)|cast(ubyte)Op.enter;
    if (auto sid = fn.name in scripts) {
      scriptPCs[*sid] = startpc;
    } else {
      auto snum = cast(uint)scriptPCs.length;
      if (snum > 32767) compileError(fn.loc, "too many scripts");
      scriptPCs ~= startpc;
      scripts[fn.name] = snum;
    }
  }

private:
  static struct CallFrame {
    uint script;
    uint bp; // base pointer (address of the current frame in stack)
    uint pc; // current pc; will be set on "call"
    ubyte rval; // slot for return value; will be set on "call"
    @disable this (this);
  }
  CallFrame[32768] frames;
  CallFrame* curframe;
  Real[] stack;

  void runtimeError(A...) (uint pc, A args) {
    import std.stdio : stderr;
    stderr.writef("ERROR at %08X: ", pc);
    stderr.writeln(args);
    throw new Exception("fuuuuu");
  }

  public Real exec(A...) (string name, A args) {
    static assert(A.length < 16, "too many arguments");
    auto sid = scripts[name];
    assert(curframe is null);
    // create frame
    if (stack.length < 65536) stack.length = 65536;
    curframe = &frames[0];
    curframe.bp = 0;
    stack[0..Slot.max+1] = 0;
    foreach (immutable idx, immutable a; args) {
      static if (is(typeof(a) : const(char)[])) {
        //FIXME
        assert(0);
      } else static if (is(typeof(a) : Real)) {
        stack[Slot.Argument0+idx] = cast(Real)a;
      } else {
        static assert(0, "invalid argument type");
      }
    }
    { import std.stdio; writeln(scriptPCs[sid]); }
    return doExec(scriptPCs[sid]);
  }

  // current frame must be properly initialized
  Real doExec (uint pc) {
    enum BinOpMixin(string op, string ack="") =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      ack~
      "if (!o0.isReal || !o1.isReal) runtimeError(pc-1, `invalid type`);\n"~
      "bp[dest] = o0"~op~"o1;\n"~
      "break;";
    enum BinIOpMixin(string op, string ack="") =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      ack~
      "if (!o0.isReal || !o1.isReal) runtimeError(pc-1, `invalid type`);\n"~
      "bp[dest] = lrint(o0)"~op~"lrint(o1);\n"~
      "break;";

    enum BinCmpMixin(string op) =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      "assert(!o0.isUndef && !o1.isUndef);\n"~
      "if (o0.isString) {\n"~
      "  if (!o1.isString) runtimeError(pc-1, `invalid type`);\n"~
      "  string s0 = spool[o0.getStrId];\n"~
      "  string s1 = spool[o1.getStrId];\n"~
      "  bp[dest] = (s0 "~op~" s1 ? 1 : 0);\n"~
      "} else {\n"~
      "  assert(o0.isReal);\n"~
      "  if (!o1.isReal) runtimeError(pc-1, `invalid type`);\n"~
      "  bp[dest] = (o0 "~op~" o1 ? 1 : 0);\n"~
      "}\n"~
      "break;";

    enum BinLogMixin(string op) =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      "assert(!o0.isUndef && !o1.isUndef);\n"~
      "if (o0.isString) {\n"~
      "  if (!o1.isString) runtimeError(pc-1, `invalid type`);\n"~
      "  string s0 = spool[o0.getStrId];\n"~
      "  string s1 = spool[o1.getStrId];\n"~
      "  bp[dest] = (s0.length "~op~" s1.length ? 1 : 0);\n"~
      "} else {\n"~
      "  assert(o0.isReal);\n"~
      "  if (!o1.isReal) runtimeError(pc-1, `invalid type`);\n"~
      "  bp[dest] = (lrint(o0) "~op~" lrint(o1) ? 1 : 0);\n"~
      "}\n"~
      "break;";

    static if (is(Real == float)) {
      import core.stdc.math : lrint = lrintf;
    } else {
      import core.stdc.math : lrint;
    }
    assert(curframe !is null);
    assert(pc > 0 && pc < code.length);
    assert(code[pc].opCode == Op.enter);
    assert(stack.length > 0);
    auto bp = &stack[curframe.bp];
    auto origcf = curframe;
    //if (stack.length < 65536) stack.length = 65536;
    debug(vm_exec) uint maxslots = Slot.max+1;
    for (;;) {
      assert(pc > 0);
      debug(vm_exec) {
        import std.stdio : stderr;
        foreach (immutable idx; 0..maxslots) stderr.writeln("  ", idx, ": ", bp[idx]);
        dumpInstr(stderr, pc);
      }
      auto opx = code.ptr[pc++];
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
            auto s0 = spool[o0.getStrId];
            bp[dest] = (s0.length ? 0 : 1);
          } else {
            bp[dest] = (lrint(o0) ? 0 : 1);
          }
          break;
        case Op.neg:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          if (!o0.isReal) runtimeError(pc-1, "invalid type");
          bp[dest] = -o0;
          break;
        case Op.bneg:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          if (!o0.isReal) runtimeError(pc-1, "invalid type");
          bp[dest] = cast(int)(~(cast(int)lrint(o0)));
          break;

        case Op.add:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          auto o1 = bp[opx.opOp1];
          assert(!o0.isUndef && !o1.isUndef);
          if (o0.isString) {
            if (!o1.isString) runtimeError(pc-1, "invalid type");
            string s0 = spool[o0.getStrId];
            string s1 = spool[o1.getStrId];
            //FIXME
            if (s0.length == 0) {
              bp[dest] = o1;
            } else if (s1.length == 0) {
              bp[dest] = o0;
            } else {
              auto sidx = cast(uint)spool.length;
              spool ~= s0~s1;
              bp[dest] = buildStrId(sidx);
            }
          } else {
            assert(o0.isReal);
            if (!o1.isReal) runtimeError(pc-1, "invalid type");
            bp[dest] = o0+o1;
          }
          break;
        case Op.sub: mixin(BinOpMixin!"-");
        case Op.mul: mixin(BinOpMixin!"*");
        case Op.mod: mixin(BinOpMixin!("%", q{ if (o1 == 0) runtimeError(pc-1, "division by zero"); }));
        case Op.div: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(pc-1, "division by zero"); }));
        case Op.rdiv: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(pc-1, "division by zero"); }));
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
          bp[dest] = vpool[opx.op2Byte];
          break;
        case Op.ilit: // dest becomes ilit val (val: short) -- load small integer literal
          auto dest = opx.opDest;
          bp[dest] = opx.opILit;
          break;
        case Op.xlit: // dest becomes integer(!) val (val: short) -- load small integer literal
          auto dest = opx.opDest;
          *cast(uint*)(bp+dest) = opx.opILit;
          break;

        case Op.jump: // addr: 3 bytes
          pc = opx.op3Byte;
          break;
        case Op.xtrue: // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)
          if (lrint(bp[opx.opDest]) != 0) ++pc;
          break;
        case Op.xfalse: // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)
          if (lrint(bp[opx.opDest]) == 0) ++pc;
          break;

        case Op.call: // dest is result; op0: call frame (see below); op1: number of args
              // call frame is:
              //   new function frame
              //   int scriptid (after op1+3 slots)
              // note that there should be no used registers after those (as that will be used as new function frame regs)
          auto sid = *cast(uint*)(bp+opx.opOp0+Slot.Argument0+opx.opOp1);
          debug(vm_exec) {
            import std.stdio : stderr;
            foreach (auto kv; scripts.byKeyValue) {
              if (kv.value == sid) {
                stderr.writeln("calling '", kv.key, "'");
                foreach (immutable aidx; 0..opx.opOp1) {
                  stderr.writeln("  ", bp[opx.opOp0+Slot.Argument0+aidx]);
                }
              }
            }
          }
          bp[opx.opOp0+Slot.Argument0+opx.opOp1] = 0; // just in case
          bp[opx.opOp0..opx.opOp0+Slot.Argument0] = bp[0..Slot.Argument0]; // copy `self` and `other`
          curframe.pc = pc;
          curframe.rval = opx.opDest;
          ++curframe;
          curframe.bp = curframe[-1].bp+opx.opOp0;
          curframe.script = sid;
          bp = &stack[curframe.bp];
          pc = scriptPCs[sid];
          assert(code[pc].opCode == Op.enter);
          if (opx.opOp1 < 16) bp[Slot.Argument0+opx.opOp1..Slot.Argument15+1] = 0;
          break;

        //tcall, // same as call, but does tail call

        //case Op.prim: // call "primitive" (built-in function); dest is result; op0: call frame (see below); op1: number of args
              // call frame is:
              //   new function frame (starting with return value)
              //   int primid (after op1+3 slots)
              // note that there should be no used registers after those (as that will be used as new function frame regs)

        //tprim, // same as prim, but does tail call

        case Op.enter: // op0: number of stack slots used (including result and args); op1: number of locals
          if (curframe.bp+opx.opOp0 > stack.length) {
            stack.length = curframe.bp+opx.opOp0;
            bp = &stack[curframe.bp];
          }
          //foreach (immutable idx; Slot.max+1..Slot.max+1+opx.opOp1) bp[idx] = 0; // clear locals
          if (opx.opOp1) bp[Slot.max+1..Slot.max+1+opx.opOp1] = 0; // clear locals
          debug(vm_exec) maxslots = opx.opOp0;
          debug(vm_exec) { import std.stdio : stderr; foreach (immutable idx; Slot.Argument0..Slot.Argument15+1) stderr.writeln("  :", bp[idx]); }
          break;

        case Op.ret: // dest is retvalue; it is copied to reg0; other stack items are discarded
          if (curframe is origcf) return bp[opx.opDest]; // done
          assert(cast(uint)curframe > cast(uint)origcf);
          --curframe;
          auto rv = bp[opx.opDest];
          // remove stack frame
          bp = &stack[curframe.bp];
          pc = curframe.pc;
          bp[curframe.rval] = rv;
          debug(vm_exec) { import std.stdio : stderr; stderr.writeln("RET(", curframe.rval, "): ", rv); }
          break;

        //as we are using refloads only in the last stage of assignment, they can create values
        case Op.lref: // load slot reference to dest
          *cast(int*)bp[opx.opDest] = opx.opOp0;
          break;
        //case Op.oref: // load object reference to dest; op0: int reg (obj id; -666: global object)
        //case Op.fref: // load field reference; op0: varref; op1: int reg (field id); can't create fields
        //case Op.fcrf: // load field reference; op0: varref; op1: int reg (field id); can create field
        //case Op.iref: // load indexed reference; op0: varref; op1: int reg (index)
        //case Op.mref: // load indexed reference; op0: varref; op1: int reg (first index); (op1+1): int reg (second index)

        //case Op.rload: // load from op0-varref to dest
        case Op.rstore: // store to op0-varref from op1
          auto x = *cast(int*)bp[opx.opOp0];
          assert(x >= 0 && x <= 255);
          bp[x] = bp[opx.opOp1];
          break;

        //case Op.oload: // load object field to dest; op0: int reg (obj id; -666: global object); op1: int reg (field id)
        //case Op.iload: // load indexed (as iref)
        //case Op.mload: // load indexed (as mref)
        default: assert(0);
      }
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
    Op0Op1,
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

      Op.enter: Op0Op1,

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
