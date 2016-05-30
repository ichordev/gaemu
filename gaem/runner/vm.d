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

import gaem.parser;


// ////////////////////////////////////////////////////////////////////////// //
//alias Real = float;
alias Real = double;


// value manipulation
bool isReal (Real v) {
  import std.math;
  return !isNaN(v);
}

bool isString (Real v) {
  import std.math;
  return isNaN(v);
}

bool isUndef (Real v) {
  import std.math;
  return (isNaN(v) && getNaNPayload(v) < 0);
}

// creates "undefined" value
Real undefValue () {
  import std.math;
  return NaN(-666);
}

// for invalid strings it returns 0
int getStrId (Real v) {
  import std.math;
  if (isNaN(v)) {
    auto res = getNaNPayload(v);
    static if (Real.sizeof == 4) {
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
  static if (Real.sizeof == 4) {
    assert(id >= 0 && id <= 0x3F_FFFF);
  } else {
    assert(id >= 0);
  }
  return NaN(id);
}


Real Value(T) (VM vm, T v) {
  pragma(inline, true);
       static if (is(T : const(char)[])) return buildStrId(vm.newDynStr(v));
  else static if (is(T : Real)) return cast(Real)v;
  else static assert(0, "invalid value type");
}


// ////////////////////////////////////////////////////////////////////////// //
enum Op {
  nop,
  skip, // skip current instruction; it usually has 3-byte payload

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

  plit, // dest becomes pool slot val (val: 2 bytes) -- load value from pool slot; if val is 0xffff, next instruction is skip
  ilit, // dest becomes ilit val (val: short) -- load small integer literal
  xlit, // dest becomes integer(!) val (val: short) -- load small integer literal

  jump, // addr: 3 bytes
  xtrue, // dest is reg to check; skip next instruction if dest is "gml true" (i.e. fabs(v) >= 0.5`)
  xfalse, // dest is reg to check; skip next instruction if dest is "gml false" (i.e. fabs(v) >= 0.5`)

  call, // dest is result; op0: call frame (see below); op1: number of args
        // call frame is:
        //   new function frame
        //   int scriptid (after op1+3 slots)
        // note that there should be no used registers after those (as that will be used as new function frame regs)

  enter, // dest: number of arguments used; op0: number of stack slots used (including result and args); op1: number of locals
         // any function will ALWAYS starts with this

  ret, // dest is retvalue; it is copied to reg0; other stack items are discarded

  // as we are using refloads only in the last stage of assignment, they can create values
  lref, // load slot reference to dest; op0: slot number
  fref, // load field reference; op0: obj id; op1: int! reg (field id); can create fields
  fval, // load field value; op0: obj id; op1: int! reg (field id)
  i1ref, // load indexed reference; op0: varref; op1: index; can create arrays
  i2ref, // load indexed reference; op0: varref; op1: first index; (op1+1): second index; can create arrays
  i1val, // load indexed value; op0: varref; op1: index
  i2val, // load indexed value; op0: varref; op1: first index; (op1+1): second index

  rstore, // store to op0-varref from op1

  // `with` is done by copying `self` to another reg, execute the code and restore `self`

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

  static struct Str {
    string val; // string value
    uint rc; // refcount; <0: persistent string; also serves as free list index with 31 bit set
  }

private:
  uint[] code; // [0] is reserved
  uint[string] scripts; // name -> number
  string[uint] scriptNum2Name;
  int[] scriptPCs; // by number; 0 is reserved; <0: primitive number
  NodeFunc[] scriptASTs; // by number
  PrimDg[] prims; // by number
  // fixuper will not remove fixup chains, so we can replace script with new one
  Real[] vpool; // pool of values
  Str[] spool; // pool of strings
  uint spoolFree = 0x8000_0000; // none
  Real[] globals;
  uint[string] fields; // known fields and their offsets in object (and in globals too)

private:
  short fieldIdByName (string name) {
    if (auto fpi = name in fields) return cast(short)*fpi;
    auto fid = cast(uint)fields.length;
    if (fid > short.max) assert(0, "too many fields");
    fields[name] = fid;
    return cast(short)fid;
  }

public:
  // has rc of 1
  uint newDynStr(T) (T str) if (is(T : const(char)[])) {
    if (str.length == 0) return 0;
    if (str.length == 1) return cast(uint)str.ptr[0]+1;
    static if (is(T == string)) alias sv = str; else auto sv = str.idup;
    if (spoolFree&0x7fff_ffff) {
      // reuse existing
      auto sid = spoolFree&0x7fff_ffff;
      auto ss = spool.ptr+sid;
      spoolFree = ss.rc;
      ss.val = sv;
      ss.rc = 1;
      return sid;
    } else {
      // allocate new
      auto sid = cast(uint)spool.length;
      if (sid > 0x3F_FFFF) assert(0, "too many dynamic strings");
      spool ~= Str(sv, 1);
      return sid;
    }
  }

  void dynStrIncRef (uint sid) {
    pragma(inline, true);
    if (sid < spool.length && spool.ptr[sid].rc > 0) {
      assert(spool.ptr[sid].rc < 0x8000_0000);
      ++spool.ptr[sid].rc;
    }
  }

  void dynStrDecRef (uint sid) {
    pragma(inline, true);
    if (sid < spool.length && spool.ptr[sid].rc > 0) {
      assert(spool.ptr[sid].rc < 0x8000_0000);
      if (--spool.ptr[sid].rc == 0) {
        spool.ptr[sid].rc = spoolFree;
        spoolFree = sid|0x8000_0000;
      }
    }
  }

  string getDynStr (uint sid) {
    pragma(inline, true);
    return (sid < spool.length && spool.ptr[sid].rc < 0x8000_0000 ? spool.ptr[sid].val : null);
  }

public:
  this () {
    code.length = 1;
    scriptPCs.length = 1;
    scriptASTs.length = 1;
    prims.length = 1;
    // preallocate small strings
    spool ~= Str("", 0);
    foreach (ubyte c; 0..256) spool ~= Str(""~cast(char)c, 0);
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
  void doCompileFunc (NodeFunc fn) {
    import std.bitmanip; // BitArray

    int argvar (string s) {
      switch (s) {
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

    uint sid4name (string name) {
      if (auto sptr = name in scripts) {
        return *sptr;
      } else {
        auto sid = cast(uint)scriptPCs.length;
        if (sid > 32767) compileError(fn.loc, "too many scripts");
        assert(scriptASTs.length == sid);
        // reserve slots
        scriptPCs ~= 0;
        scriptASTs ~= null;
        scriptNum2Name[sid] = name;
        scripts[name] = sid;
        return sid;
      }
    }

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

    uint emitJumpTo (uint addr, Op op=Op.jump) {
      assert(addr <= 0xffffff);
      auto res = cast(uint)code.length;
      code ~= cast(uint)op|(addr<<8);
      return res;
    }

    // this starts "jump chain", return new chain id
    uint emitJumpChain (uint chain, Op op=Op.jump) {
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

    ubyte allocSlots (Loc loc, int count) {
      assert(count > 0 && count < slots.length);
      foreach (immutable idx; firstFreeSlot..slots.length-count) {
        bool ok = true;
        foreach (immutable c; idx..idx+count) if (slots[c]) { ok = false; break; }
        if (ok) {
          if (idx+count-1 > maxUsedSlot) maxUsedSlot = cast(uint)idx+count-1;
          foreach (immutable c; idx..idx+count) slots[c] = true;
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
    ubyte maxArgUsed; // maximum `argumentX` we've seen

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

    void findUninitialized () {
      bool[string] inited;
      bool[string] used;

      void processExpr (Node n, bool asAss=false) {
        if (n is null) return;
        visitNodes(n, (Node nn) {
          if (auto n = cast(NodeBinaryAss)nn) {
            if (cast(NodeId)n.el is null && cast(NodeDot)n.el is null && cast(NodeIndex)n.el is null) {
              compileError(nn.loc, "assignment to rvalue");
              return VisitRes.SkipChildren;
            }
            processExpr(n.er); // it is calculated first
            if (auto did = cast(NodeId)n.el) {
              inited[did.name] = true;
              used[did.name] = true;
            } else {
              processExpr(n.el, asAss:true);
            }
            return VisitRes.SkipChildren;
          }
          if (auto id = cast(NodeId)nn) {
            if (argvar(id.name) < 0) {
              if (!asAss && id.name in locals && id.name !in inited) {
                compileError(nn.loc, "using uninitialized variable; declared at ", vdecls[id.name].toStringNoFile);
              }
            }
            inited[id.name] = true;
            used[id.name] = true;
            return VisitRes.SkipChildren;
          }
          if (auto n = cast(NodeFCall)nn) {
            if (cast(NodeId)n.fe is null) compileError(n.loc, "invalid function call");
            if (n.args.length > 16) compileError(n.loc, "too many arguments in function call");
            foreach (immutable idx, Node a; n.args) {
              // no assignments allowed there
              processExpr(a);
            }
            return VisitRes.SkipChildren;
          }
          return VisitRes.Continue;
        });
      }

      void processStatement (Node nn) {
        if (nn is null) return;
        return selectNode!void(nn,
          (NodeVarDecl n) {},
          (NodeBlock n) {
            foreach (Node st; n.stats) {
              if (cast(NodeStatementBreakCont)st !is null) break;
              processStatement(st);
              if (cast(NodeReturn)st !is null) break;
            }
          },
          (NodeStatementEmpty n) {},
          (NodeStatementExpr n) { processExpr(n.e); },
          (NodeReturn n) { processExpr(n.e); },
          (NodeWith n) {
            processExpr(n.e); // can't contain assignments
            // body can be executed zero times, so...
            auto before = inited.dup;
            processStatement(n.ebody);
            inited = before;
          },
          (NodeIf n) {
            processExpr(n.ec);
            auto before = inited.dup;
            processStatement(n.et);
            auto tset = inited.dup;
            inited = before.dup;
            processStatement(n.ef);
            // now copy to `before` all items that are set both in `tset` and in `inited`
            foreach (string name; inited.byKey) {
              if (name in tset) before[name] = true;
            }
            inited = before;
          },
          (NodeStatementBreakCont n) {},
          (NodeFor n) {
            processExpr(n.einit);
            // "next" and "cond" can't contain assignments, so it's safe here
            processExpr(n.econd);
            processExpr(n.enext);
            // yet body can be executed zero times, so...
            auto before = inited.dup;
            processStatement(n.ebody);
            inited = before;
          },
          (NodeWhile n) {
            // "cond" can't contain assignments, so it's safe here
            processExpr(n.econd);
            // yet body can be executed zero times, so...
            auto before = inited.dup;
            processStatement(n.ebody);
            inited = before;
          },
          (NodeDoUntil n) {
            // "cond" can't contain assignments, so it's safe here
            processExpr(n.econd);
            // body is guaranteed to execute at least one time
            processStatement(n.ebody);
          },
          (NodeRepeat n) {
            // "count" can't contain assignments, so it's safe here
            processExpr(n.ecount);
            // yet body can be executed zero times, so...
            auto before = inited.dup;
            processStatement(n.ebody);
            inited = before;
          },
          (NodeSwitch n) {
            // "expr" can't contain assignments, so it's safe here
            processExpr(n.e);
            auto before = inited.dup;
            foreach (ref ci; n.cases) {
              processExpr(ci.e); // can't contain assignments
              // and this one can
              if (ci.st !is null) {
                inited = before.dup;
                processStatement(ci.st);
              }
            }
            inited = before;
          },
          () { assert(0, "unimplemented node: "~typeid(nn).name); },
        );
      }

      processStatement(fn.ebody);

      // now show (and remove) unused locals
      //static struct Info { Loc loc; string name; }
      //Info[] unusedLocs;
      foreach (string name; locals.keys) {
        if (name !in used) {
          { import std.stdio; writeln("removing unused local '", name, "'"); }
          //unusedLocs ~= Info(vdecls[name], name);
          locals.remove(name);
        }
      }
      //import std.algorithm : sort;
      //unusedLocs.sort!((ref a, ref b) { if (a.loc.line < b.loc.line) return true; if (a.loc.line > b.loc.line) return false; return (a.loc.col < b.loc.col); });
      //foreach (ref nfo; unusedLocs) compileError(nfo.loc, "unused local '", nfo.name, "'");
    }

    findUninitialized();

    /* here we will do very simple analysis for code like
     *   var m, n;
     *   m = argument0;
     *   n = argument1;
     *   ...no `arument0` and `argument1` usage after this point
     * we can just alias `m` to `arument0`, and `n` to `argument1` then
     */

    string[16] aaliases; // argument aliases
    {
      uint firstBadStatement = 0;
      foreach (immutable idx, Node st; fn.ebody.stats) {
        if (cast(NodeStatementEmpty)st || cast(NodeStatementExpr)st || cast(NodeVarDecl)st) {
          firstBadStatement = cast(uint)idx+1;
        } else {
          break;
        }
      }
      if (firstBadStatement > 0) {
        bool[string] varsused;
        // scan statements, find assignments
        foreach (immutable idx, Node st; fn.ebody.stats[0..firstBadStatement]) {
          if (auto se = cast(NodeStatementExpr)st) {
            if (auto ass = cast(NodeBinaryAss)se.e) {
              // wow, assignment
              auto lv = cast(NodeId)ass.el;
              auto rv = cast(NodeId)ass.er;
              if (lv !is null && rv !is null) {
                // "a = b"
                { import std.stdio : stderr; stderr.writeln("found assignment: '", lv.name, "' = '", rv.name, "'"); }
                if (argvar(rv.name) >= 0 && argvar(lv.name) < 0) {
                  // "a = argumentx"
                  if (lv.name in varsused || rv.name in varsused) continue; // no wai
                  if (lv.name !in locals) continue; // not a local
                  auto ai = argvar(rv.name);
                  if (aaliases[ai].length && aaliases[ai] != lv.name) continue; // already have an alias (TODO)
                  aaliases[ai] = lv.name; // possible alias
                } else {
                  // check for reassignment
                  if (lv.name !in varsused) {
                    // not used before, but used now; remove it from aliases
                    foreach (ref an; aaliases) if (an == lv.name) an = null;
                    varsused[lv.name] = true;
                  }
                }
              }
            }
          }
        }
        // now check if we have any assignment to aliased argument
        foreach (immutable idx, string an; aaliases) {
          if (an.length == 0) continue;
          visitNodes(fn.ebody, (Node n) {
            if (auto ass = cast(NodeBinaryAss)n) {
              if (auto id = cast(NodeId)ass.el) {
                auto ai = argvar(id.name);
                if (ai >= 0) aaliases[idx] = null;
                return VisitRes.Stop;
              }
            }
            return VisitRes.Continue;
          });
        }
        // remove aliases from locals (we don't need slots for 'em)
        foreach (immutable idx, string an; aaliases) {
          if (an.length == 0) continue;
          locals.remove(an);
        }
        // dump aliases
        {
          import std.stdio : stderr;
          foreach (immutable idx, string an; aaliases) {
            if (an.length) stderr.writeln("'argument", idx, "' is aliased to '", an, "'");
          }
        }
      }
    }

    // now assign slots to locals
    foreach (string name; locals.keys) {
      firstFreeSlot = allocSlot(vdecls[name]);
      locals[name] = cast(ubyte)firstFreeSlot;
      ++firstFreeSlot;
    }

    void emitPLit (Loc loc, ubyte dest, Real v) {
      uint vpidx = uint.max;
      if (v.isReal) {
        // number
        import core.stdc.math : lrint;
        if (lrint(v) == v && lrint(v) >= short.min && lrint(v) <= short.max) {
          emit2Bytes(Op.ilit, dest, cast(short)lrint(v));
          return;
        }
        //FIXME: speed it up!
        foreach (immutable idx, Real vp; vpool) if (vp == v) { vpidx = cast(uint)idx; break; }
      } else if (v.isString) {
        // string
        //FIXME: speed it up!
        auto sid = v.getStrId;
        foreach (immutable idx, Real vp; vpool) if (vp.isString && vp.getStrId == sid) { vpidx = cast(uint)idx; break; }
      } else {
        assert(0, "wtf?!");
      }
      if (vpidx == uint.max) {
        vpidx = cast(uint)vpool.length;
        if (vpidx >= 0xffffff) compileError(loc, "too many constants");
        vpool ~= v;
      }
      if (vpidx < ushort.max) {
        emit2Bytes(Op.plit, dest, cast(ushort)vpidx);
      } else {
        // special form
        emit2Bytes(Op.plit, dest, cast(short)ushort.max);
        emit3Bytes(Op.skip, vpidx);
      }
    }

    uint allocStrConst (string s, Loc loc) {
      if (s.length == 0) return 0;
      //FIXME: speed it up!
      foreach (immutable idx, ref ds; spool) {
        if (ds.val == s) return cast(ushort)idx;
      }
      auto sidx = cast(uint)spool.length;
      if (sidx >= 0xffffff) compileError(loc, "too many strings");
      spool ~= Str(s, 0);
      return sidx;
    }

    int varSlot (string name) {
      auto avn = argvar(name);
      if (avn >= 0) return Slot.Argument0+avn;
      switch (name) {
        case "self": return Slot.Self;
        case "other": return Slot.Other;
        default:
      }
      // argument aliases
      foreach (immutable idx, string an; aaliases) if (an == name) return cast(int)Slot.Argument0+idx;
      // locals
      if (auto v = name in locals) return *v;
      return -1;
    }

    // options for expression
    static struct EOpts {
      int ddest = -1; // >=0: put result in this slot
      bool dna; // use `ddest` only if we don't need to allocate more slots
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

      nn.pcs = pc;
      scope(exit) nn.pce = pc;
      return selectNode!ubyte(nn,
        (NodeLiteralNum n) {
          auto dest = allocSlot(n.loc, ddest);
          emitPLit(n.loc, dest, n.val);
          return dest;
        },
        (NodeLiteralStr n) {
          auto dest = allocSlot(n.loc, ddest);
          auto sid = allocStrConst(n.val, n.loc);
          emitPLit(n.loc, dest, buildStrId(sid));
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
          if (cast(NodeId)n.fe is null) compileError(n.loc, "invalid function call");
          if (n.args.length > 16) compileError(n.loc, "too many arguments in function call");
          auto dest = allocSlot(n.loc, ddest);
          // preallocate frame
          // we can do this, as current slot allocation scheme guarantees
          // that we won't have used slots with higher numbert after compiling
          // argument expressions
          // `reserveCallSlots()` won't mark slots as used
          auto frameSize = cast(uint)n.args.length+Slot.Argument0;
          auto fcs = reserveCallSlots(n.loc, frameSize+1); // +1 for script id
          // put arguments where we want 'em to be
          foreach (immutable idx, Node a; n.args) {
            // reserve result slot, so it won't be overwritten
            assert(!slots[fcs+Slot.Argument0+idx]);
            slots[fcs+Slot.Argument0+idx] = true;
            auto dp = compileExpr(a, fcs+Slot.Argument0+idx);
            if (dp != fcs+Slot.Argument0+idx) assert(0, "internal compiler error");
          }
          // now free result slots
          foreach (immutable idx; 0..n.args.length) freeSlot(cast(ubyte)(fcs+Slot.Argument0+idx));
          // make sure that our invariant holds
          if (reserveCallSlots(n.loc, 1) != fcs) assert(0, "internal compiler error");
          // put script id
          // emit call
          uint sid = sid4name((cast(NodeId)n.fe).name);
          emit2Bytes(Op.xlit, cast(ubyte)(fcs+Slot.Argument0+n.args.length), cast(short)sid);
          emit(Op.call, dest, fcs, cast(ubyte)n.args.length);
          return dest;
        },
        (NodeId n) {
          // keep track of maximum argument we've seen
          if (maxArgUsed < 15) {
            if (auto ai = argvar(n.name)) {
              if (ai > maxArgUsed) maxArgUsed = cast(ubyte)ai;
            }
          }
          if (wantref) {
            // load reference
            auto dest = allocSlot(n.loc, ddest);
            auto vsl = varSlot(n.name);
            if (vsl >= 0) {
              // this is local variable
              emit(Op.lref, dest, cast(ubyte)vsl);
            } else {
              // this is `self` field
              auto fid = allocSlot(n.loc);
              emit2Bytes(Op.ilit, fid, cast(short)fieldIdByName(n.name));
              freeSlot(fid);
              emit(Op.fref, dest, Slot.Self, fid);
            }
            return dest;
          } else {
            // load value
            auto vsl = varSlot(n.name);
            if (vsl >= 0) {
              // this is local variable
              if (ddest < 0) return vsl; // just use this slot directly
              auto dest = allocSlot(n.loc, ddest);
              if (dest == vsl) return dest;
              emit(Op.copy, dest, cast(ubyte)vsl, 1);
              return dest;
            } else {
              // this is `self` field
              auto dest = allocSlot(n.loc, ddest);
              auto fid = allocSlot(n.loc);
              emit2Bytes(Op.ilit, fid, cast(short)fieldIdByName(n.name));
              freeSlot(fid);
              emit(Op.fval, dest, Slot.Self, fid);
              return dest;
            }
          }
          assert(0);
        },
        (NodeDot n) {
          // field access
          auto aop = (wantref ? Op.fref : Op.fval);
          auto dest = allocSlot(n.loc, ddest);
          if (auto oid = cast(NodeId)n.e) {
            // we know object name directly
            if (oid.name == "self" || oid.name == "other" || oid.name == "global") {
              // well-known name
              auto fid = allocSlot(n.loc);
              emit2Bytes(Op.ilit, fid, cast(short)fieldIdByName(n.name));
              if (oid.name == "global") {
                auto oids = allocSlot(n.loc);
                emit2Bytes(Op.ilit, oids, -666);
                freeSlot(oids);
                emit(aop, dest, oids, fid);
              } else {
                emit(aop, dest, (oid.name == "self" ? Slot.Self : Slot.Other), fid);
              }
              freeSlot(fid);
              return dest;
            }
          }
          // this is some complex expression
          auto fid = allocSlot(n.loc);
          emit2Bytes(Op.ilit, fid, cast(short)fieldIdByName(n.name));
          auto oids = compileExpr(n.e);
          freeSlot(oids);
          emit(aop, dest, oids, fid);
          return dest;
        },
        (NodeIndex n) {
          assert(n.ei0 !is null);
          auto dest = allocSlot(n.loc, ddest);
          if (n.ei1 is null) {
            // one index
            if (auto id = cast(NodeId)n.e) {
              auto vid = varSlot(id.name);
              if (vid >= 0) {
                // this is local variable
                compileError(n.loc, "indexing locals is not supported yet");
                assert(0);
              }
            }
            // not a local
            auto i0 = compileExpr(n.ei0);
            auto refs = compileExpr(n.e, wantref:true);
            emit((wantref ? Op.i1ref : Op.i1val), dest, refs, i0);
            freeSlot(refs);
            freeSlot(i0);
          } else {
            // two indexes
            auto islots = allocSlots(n.loc, 2);
            auto i0 = compileExpr(n.ei0, islots);
            assert(i0 == islots);
            auto i1 = compileExpr(n.ei1, islots+1);
            assert(i0 == islots+1);
            auto refs = compileExpr(n.e, wantref:true);
            emit((wantref ? Op.i2ref : Op.i2val), dest, refs, islots);
            freeSlot(refs);
            freeSlot(i0);
            freeSlot(i1);
          }
          return dest;
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }

    uint breakChain; // current jump chain for `break`
    uint contChain; // current jump chain for `continue`
    bool contChainIsAddr; // is `contChain` an address, not a chain?

    void compile (Node nn) {
      assert(nn !is null);
      nn.pcs = pc;
      scope(exit) nn.pce = pc;
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
          breakChain = emitJumpChain(breakChain);
        },
        (NodeStatementContinue n) {
          if (contChainIsAddr) {
            emitJumpTo(contChain);
          } else {
            contChain = emitJumpChain(contChain);
          }
        },
        (NodeFor n) {
          freeSlot(compileExpr(n.einit));
          // generate code like this:
          //   jump to "continue"
          //   body
          //  continue:
          //   cond
          //   jumptostart
          auto obc = breakChain;
          auto occ = contChain;
          auto cca = contChainIsAddr;
          scope(exit) { breakChain = obc; contChain = occ; contChainIsAddr = cca; }
          // jump to "continue"
          contChain = emitJumpChain(0); // start new chain
          contChainIsAddr = false;
          breakChain = 0; // start new chain
          auto stpc = pc;
          // increment
          freeSlot(compileExpr(n.enext));
          // body
          compile(n.ebody);
          // fix "continue"
          fixJumpChain(contChain, pc);
          // condition
          auto dest = compileExpr(n.econd);
          freeSlot(dest); // yep, right here
          emit(Op.xfalse, dest); // skip jump on false
          emitJumpTo(stpc);
          // "break" is here
          fixJumpChain(breakChain, pc);
        },
        (NodeWhile n) {
          // nothing fancy
          auto obc = breakChain;
          auto occ = contChain;
          auto cca = contChainIsAddr;
          scope(exit) { breakChain = obc; contChain = occ; contChainIsAddr = cca; }
          // new break chain
          breakChain = 0;
          // "continue" is here
          contChain = pc;
          contChainIsAddr = true;
          // condition
          auto dest = compileExpr(n.econd);
          freeSlot(dest); // yep, right here
          emit(Op.xfalse, dest); // skip jump on false
          breakChain = emitJumpChain(breakChain); // get out of here
          // body
          compile(n.ebody);
          // and again
          emitJumpTo(contChain);
          // "break" is here
          fixJumpChain(breakChain, pc);
        },
        (NodeDoUntil n) {
          // nothing fancy
          auto obc = breakChain;
          auto occ = contChain;
          auto cca = contChainIsAddr;
          scope(exit) { breakChain = obc; contChain = occ; contChainIsAddr = cca; }
          auto stpc = pc;
          // new break chain
          breakChain = 0;
          // new continue chain
          contChain = 0;
          contChainIsAddr = false;
          // body
          compile(n.ebody);
          // "continue" is here
          fixJumpChain(contChain, pc);
          // condition
          auto dest = compileExpr(n.econd);
          freeSlot(dest); // yep, right here
          emit(Op.xfalse, dest); // skip jump on false
          // and again
          emitJumpTo(stpc);
          // "break" is here
          fixJumpChain(breakChain, pc);
        },
        (NodeRepeat n) {
          // allocate node for counter
          auto cnt = compileExpr(n.ecount);
          // allocate "1" constant (we will need it)
          auto one = allocSlot(n.loc);
          emit2Bytes(Op.ilit, one, cast(short)1);
          // alice in chains
          auto obc = breakChain;
          auto occ = contChain;
          auto cca = contChainIsAddr;
          scope(exit) { breakChain = obc; contChain = occ; contChainIsAddr = cca; }
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
          compile(n.ebody);
          // and again
          emitJumpTo(contChain);
          // "break" is here
          fixJumpChain(breakChain, pc);
          // free used slots
          freeSlot(one);
          freeSlot(cnt);
        },
        (NodeSwitch n) {
          // switch expression
          auto expr = compileExpr(n.e);
          if (n.cases.length) {
            // has some cases
            uint defaultBodyAddr = 0; // address of "default" node body (even if it is empty)
            uint lastFalltrhuJump = 0; // this is the address of the Op.jump at the end of the previous case node
            uint lastCaseSkipJumpAddr = 0; // this is the address of the Op.jump at the failed condition of the previous case node
            // new "break" chain
            auto obc = breakChain;
            scope(exit) breakChain = obc;
            breakChain = 0;
            // now generate code for case nodes, skipping "default" by the way
            foreach (immutable idx, ref ci; n.cases) {
              uint nodeSkipChain = 0;
              // check condition
              if (ci.e !is null) {
                // jump here from the last failed condition
                fixJumpChain(lastCaseSkipJumpAddr, pc);
                auto cond = compileExpr(ci.e);
                // trick: reuse "cond" slot
                freeSlot(cond);
                emit(Op.eq, cond, cond, expr);
                emit(Op.xtrue, cond);
                // new skip chain
                lastCaseSkipJumpAddr = emitJumpChain(0);
              } else {
                // this is default node, jump over it
                nodeSkipChain = emitJumpChain(0);
                // and save info
                defaultBodyAddr = pc;
              }
              // fix fallthru jump
              fixJumpChain(lastFalltrhuJump, pc);
              // the body is here
              compile(ci.st);
              // new fallthru chain
              lastFalltrhuJump = (idx < n.cases.length-1 ? emitJumpChain(0) : 0);
              // fix "default skip" chain
              fixJumpChain(nodeSkipChain, pc);
            }
            // we can free expression slot right here
            freeSlot(expr);
            // do we have default node?
            if (defaultBodyAddr) {
              // jump there from the last failed condition
              fixJumpChain(lastCaseSkipJumpAddr, defaultBodyAddr);
            } else {
              // jump here from the last failed condition
              fixJumpChain(lastCaseSkipJumpAddr, pc);
            }
            // fix last fallthru jump
            fixJumpChain(lastFalltrhuJump, pc);
            // fix "break" chain
            fixJumpChain(breakChain, pc);
          } else {
            freeSlot(expr);
          }
        },
        () { assert(0, "unimplemented node: "~typeid(nn).name); },
      );
    }

    if (auto sid = fn.name in scripts) {
      if (scriptPCs[*sid] < 0) return; // can't override built-in function
    }

    uint sid = sid4name(fn.name);
    /*debug(vm_exec)*/ { import std.stdio; writeln("compiling '", fn.name, "' (", sid, ")..."); }
    auto startpc = emit(Op.enter);
    fn.pcs = pc;
    compile(fn.ebody);
    emit(Op.ret);
    fn.pce = pc;
    // patch enter
    code[startpc] = (locals.length<<24)|((maxUsedSlot+1)<<16)|(maxArgUsed<<8)|cast(ubyte)Op.enter;
    scriptPCs[sid] = startpc;
    scriptASTs[sid] = fn;
  }

private:
  static struct CallFrame {
    uint script; // script id
    uint bp; // base pointer (address of the current frame in stack)
    uint pc; // current pc; will be set on "call"; it is used by callee
    ubyte rval; // slot for return value; will be set on "call"; it is used by callee
    @disable this (this);
  }
  CallFrame[32768] frames;
  CallFrame* curframe;
  Real[] stack;

  void runtimeError(A...) (uint pc, A args) {
    import std.stdio : stderr;
    stderr.writef("ERROR at %08X: ", pc);
    stderr.writeln(args);
    // try to build stack trace
    if (curframe !is null) {
      curframe.pc = pc;
      auto cf = curframe;
      for (;;) {
        stderr.writefln("%08X: %s", cf.pc, scriptNum2Name[cf.script]);
        if (cf is frames.ptr) break; // it's not legal to compare pointers from different regions
        --cf;
      }
    }
    throw new Exception("fuuuuu");
  }

  public void opIndexAssign(DG) (DG dg, string name) if (isCallable!DG) {
    assert(name.length > 0);
    uint sid;
    if (auto sptr = name in scripts) {
      sid = *sptr;
    } else {
      sid = cast(uint)scriptPCs.length;
      if (sid > 32767) assert(0, "too many scripts");
      assert(scriptASTs.length == sid);
      // reserve slots
      scriptPCs ~= 0;
      scriptASTs ~= null;
      scriptNum2Name[sid] = name;
      scripts[name] = sid;
    }
    auto pnum = cast(uint)prims.length;
    assert(pnum);
    scriptPCs[sid] = -cast(int)pnum;
    prims ~= register(dg);
  }

  public Real exec(A...) (string name, A args) {
    static assert(A.length < 16, "too many arguments");
    auto sid = scripts[name];
    assert(curframe is null);
    // create frame
    if (stack.length < 65536) stack.length = 65536;
    curframe = &frames[0];
    curframe.bp = 0;
    curframe.script = sid;
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
    //{ import std.stdio; writeln(scriptPCs[sid]); }
    return doExec(scriptPCs[sid]);
  }

  // current frame must be properly initialized
  Real doExec (uint pc) {
    enum BinOpMixin(string op, string ack="") =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      ack~
      "if (!o0.isReal || !o1.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
      "bp[dest] = o0"~op~"o1;\n"~
      "break;";
    enum BinIOpMixin(string op, string ack="") =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      ack~
      "if (!o0.isReal || !o1.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
      "bp[dest] = lrint(o0)"~op~"lrint(o1);\n"~
      "break;";

    enum BinCmpMixin(string op) =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      "assert(!o0.isUndef && !o1.isUndef);\n"~
      "if (o0.isString) {\n"~
      "  if (!o1.isString) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
      "  string s0 = spool[o0.getStrId].val;\n"~
      "  string s1 = spool[o1.getStrId].val;\n"~
      "  bp[dest] = (s0 "~op~" s1 ? 1 : 0);\n"~
      "} else {\n"~
      "  assert(o0.isReal);\n"~
      "  if (!o1.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
      "  bp[dest] = (o0 "~op~" o1 ? 1 : 0);\n"~
      "}\n"~
      "break;";

    enum BinLogMixin(string op) =
      "auto dest = opx.opDest;\n"~
      "auto o0 = bp[opx.opOp0];\n"~
      "auto o1 = bp[opx.opOp1];\n"~
      "assert(!o0.isUndef && !o1.isUndef);\n"~
      "if (o0.isString) {\n"~
      "  if (!o1.isString) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
      "  string s0 = spool[o0.getStrId].val;\n"~
      "  string s1 = spool[o1.getStrId].val;\n"~
      "  bp[dest] = (s0.length "~op~" s1.length ? 1 : 0);\n"~
      "} else {\n"~
      "  assert(o0.isReal);\n"~
      "  if (!o1.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), `invalid type`);\n"~
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
    assert(pc > 0 && pc < code.length);
    assert(code[pc].opCode == Op.enter);
    assert(stack.length > 0);
    auto bp = &stack[curframe.bp];
    auto origcf = curframe;
    auto cptr = code.ptr+pc;
    //if (stack.length < 65536) stack.length = 65536;
    debug(vm_exec) uint maxslots = Slot.max+1;
    for (;;) {
      debug(vm_exec) {
        import std.stdio : stderr;
        foreach (immutable idx; 0..maxslots) stderr.writeln("  ", idx, ": ", bp[idx]);
        dumpInstr(stderr, cast(uint)(cptr-code.ptr));
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
            auto s0 = spool[o0.getStrId].val;
            bp[dest] = (s0.length ? 0 : 1);
          } else {
            bp[dest] = (lrint(o0) ? 0 : 1);
          }
          break;
        case Op.neg:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          if (!o0.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), "invalid type");
          bp[dest] = -o0;
          break;
        case Op.bneg:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          if (!o0.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), "invalid type");
          bp[dest] = cast(int)(~(cast(int)lrint(o0)));
          break;

        case Op.add:
          auto dest = opx.opDest;
          auto o0 = bp[opx.opOp0];
          auto o1 = bp[opx.opOp1];
          assert(!o0.isUndef && !o1.isUndef);
          if (o0.isString) {
            if (!o1.isString) runtimeError(cast(uint)(cptr-code.ptr-1), "invalid type");
            string s0 = spool[o0.getStrId].val;
            string s1 = spool[o1.getStrId].val;
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
            if (!o1.isReal) runtimeError(cast(uint)(cptr-code.ptr-1), "invalid type");
            bp[dest] = o0+o1;
          }
          break;
        case Op.sub: mixin(BinOpMixin!"-");
        case Op.mul: mixin(BinOpMixin!"*");
        case Op.mod: mixin(BinOpMixin!("%", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-code.ptr-1), "division by zero"); }));
        case Op.div: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-code.ptr-1), "division by zero"); }));
        case Op.rdiv: mixin(BinOpMixin!("/", q{ if (o1 == 0) runtimeError(cast(uint)(cptr-code.ptr-1), "division by zero"); }));
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
          bp[dest] = vpool.ptr[idx];
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
          cptr = code.ptr+opx.op3Byte;
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
          auto sid = *cast(uint*)(bp+opx.opOp0+Slot.Argument0+opx.opOp1);
          if (sid >= scriptPCs.length) runtimeError(cast(uint)(cptr-code.ptr-1), "invalid script id");
          pc = scriptPCs.ptr[sid];
          if (pc < 1 || pc >= code.length) {
            if (pc&0x8000_0000) {
              // this is primitive
              uint pid = -cast(int)pc;
              if (pid >= prims.length) assert(0, "wtf?!");
              bp[opx.opDest] = prims.ptr[pid](cast(uint)(cptr-code.ptr-1), bp+opx.opOp0, opx.opOp1);
              break;
            } else {
              string scname;
              foreach (auto kv; scripts.byKeyValue) if (kv.value == sid) { scname = kv.key; break; }
              runtimeError(cast(uint)(cptr-code.ptr-1), "trying to execute undefined script '", scname, "'");
            }
          }
          debug(vm_exec) {
            import std.stdio : stderr;
            stderr.writeln("calling '", scriptNum2Name[sid], "'");
            foreach (immutable aidx; 0..opx.opOp1) stderr.writeln("  ", bp[opx.opOp0+Slot.Argument0+aidx]);
          }
          // if this is tail call, just do it as tail call then
          // but don't optimize out top-level call, heh
          if (curframe !is origcf && (*cptr).opCode == Op.ret) {
            import core.stdc.string : memcpy;
            // yay, it is a tail call!
            // copy arguments (it's safe to use `memcpy()` here); `self` and `other` are automatically ok
            if (opx.opOp1) memcpy(bp+Slot.Argument0, bp+opx.opOp0+Slot.Argument0, Real.sizeof*opx.opOp1);
            // simply replace current frame with new one
          } else {
            bp[opx.opOp0..opx.opOp0+Slot.Argument0] = bp[0..Slot.Argument0]; // copy `self` and `other`
            curframe.pc = cast(uint)(cptr-code.ptr);
            curframe.rval = opx.opDest;
            ++curframe;
            curframe.bp = curframe[-1].bp+opx.opOp0;
            bp = &stack[curframe.bp];
          }
          curframe.script = sid;
          cptr = code.ptr+scriptPCs.ptr[sid];
          //assert((*cptr).opCode == Op.enter);
          // clear unused arguments, if any
          // we know that first instruction is always Op.enter, use that fact
          auto aused = (*cptr).opDest+1;
          //{ import std.stdio; writeln("aused=", aused, "; op1=", opx.opOp1); }
          if (aused > opx.opOp1) bp[Slot.Argument0+opx.opOp1..Slot.Argument0+aused] = 0;
          break;

        case Op.enter: // dest: number of arguments used; op0: number of stack slots used (including result and args); op1: number of locals
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
          cptr = code.ptr+curframe.pc;
          bp[curframe.rval] = rv;
          debug(vm_exec) { import std.stdio : stderr; stderr.writeln("RET(", curframe.rval, "): ", rv); }
          break;

        //as we are using refloads only in the last stage of assignment, they can create values
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

        //case Op.oload: // load object field to dest; op0: int reg (obj id; -666: global object); op1: int reg (field id)
        //case Op.iload: // load indexed (as iref)
        //case Op.mload: // load indexed (as mref)
        default: assert(0);
      }
    }
  }

private:
  // create primitive delegate for D delegate/function
  // D function can include special args like:
  //   Real* -- bp
  //   VM -- vm instance (should be at the end)
  //   Real -- unmodified argument value
  //   one or two args after VM: `self` and `other`
  //   string, integer, float
  //   no ref args are supported, sorry
  private PrimDg register(DG) (DG dg) @trusted if (isCallable!DG) {
    import core.stdc.math : lrint;
    assert(dg !is null);
    // build call thunk
    return delegate (uint pc, Real* bp, ubyte argc) {
      // prepare arguments
      Parameters!DG arguments;
      alias rt = ReturnType!dg;
      // (VM self, Real* bp, ubyte argc)
      static if (arguments.length == 3 &&
                 is(typeof(arguments[0]) : VM) &&
                 is(typeof(arguments[1]) == Real*) &&
                 is(typeof(arguments[2]) : int))
      {
        static if (is(rt == void)) {
          cast(void)dg(this, bp, cast(typeof(arguments[2]))argc);
          return cast(Real)0;
        } else {
          return Value(this, dg(this, bp, cast(typeof(arguments[2]))argc));
        }
      } else {
        foreach (immutable idx, ref arg; arguments) {
          // is last argument suitable for `withobj`?
          static if (is(typeof(arg) : VM)) {
            arg = this;
            static if (idx+1 < arguments.length) {
              static assert(is(typeof(arguments[idx+1]) == Real), "invalid 'self' argument type");
              arguments[idx+1] = bp[Slot.Self];
              static if (idx+2 < arguments.length) {
                static assert(is(typeof(arguments[idx+2]) == Real), "invalid 'other' argument type");
                arguments[idx+2] = bp[Slot.Other];
                static assert(idx+3 == arguments.length, "too many extra arguments");
              }
            }
          } else {
            static assert(idx < 16, "too many arguments required");
            static if (is(typeof(arg) == const(char)[]) || is(typeof(arg) == string)) {
              auto v = bp[Slot.Argument0+idx];
              if (!v.isString) runtimeError(pc, "invalid argument type");
              arg = getDynStr(v.getStrId);
            } else static if (is(typeof(arg) == bool)) {
              auto v = bp[Slot.Argument0+idx];
                   if (v.isString) arg = (v.getStrId != 0);
              else if (v.isReal) arg = (lrint(v) != 0);
              else runtimeError(pc, "invalid argument type");
            } else static if (is(typeof(arg) : long) || is(typeof(arg) : double)) {
              auto v = bp[Slot.Argument0+idx];
              if (!v.isReal) runtimeError(pc, "invalid D argument type");
              arg = cast(typeof(arg))v;
            }
          }
        }
        static if (is(rt == void)) {
          cast(void)dg(arguments);
          return cast(Real)0;
        } else {
          return Value(this, dg(arguments));
        }
      }
    };
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
      Op.skip: None,
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

      Op.enter: DestOp0Op1,

      Op.ret: Dest,

      Op.lref: DestOp0,
      Op.fref: DestOp0Op1,
      Op.fval: DestOp0Op1,
      Op.i1ref: DestOp0Op1,
      Op.i1val: DestOp0Op1,
      Op.i2ref: DestOp0Op1,
      Op.i2val: DestOp0Op1,

      Op.rstore: DestOp0,

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
