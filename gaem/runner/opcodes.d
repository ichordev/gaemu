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
module gaem.runner.opcodes is aliced;


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
  slit, // dest becomes ilit val (val: short) -- load small integer literal as string index
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

  oval, // load object value to dest; 2byte: object index
        // this is used for `if (oPlayer1)` and such conditions
        // "object" is any object here, including sprites, background, sounds...

  fval, // load field value; op0: obj id; op1: int! reg (field id)
  i1fval, // load indexed value; op0: obj id; op1: xslots (int! field id, first index)
  i2fval, // load indexed value; op0: obj id; op1: xslots (int! field id, first index, second index)

  // ref+store will be replaced with this
  fstore, // store value *from* dest into field; op0: obj id; op1: int! reg (field id); can create fields

  i1fstore, // store value *from* dest into indexed reference; op0: obj id; op1: xslots (int! field id, first index)
  i2fstore, // store value *from* dest into indexed reference; op0: obj id; op1: xslots (int! field id, first index, second index)

  // `with` is done by copying `self` to another reg, execute the code and restore `self`
  siter, // start instance iterator; dest: iterid; op0: objid or instid
         // this is special: it will skip next instruction if iteration has at least one item
         // next instruction is always jump, which skips the loop
  niter, // op0: is iterreg; next instruction is always jump, which continutes the loop
  kiter, // kill iterator, should be called to prevent memory leaks

  // so return from `with` should call kiter for all created iterators first

  // possible iterator management: preallocate slots for each non-overlapped "with";
  // let VM to free all iterators from those slots on function exit

  lirint, // dest = lrint(op0): do lrint() (or another fast float->int conversion)
}


// ////////////////////////////////////////////////////////////////////////// //
ubyte opCode (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op&0xff); }
ubyte opDest (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>8)&0xff); }
ubyte opOp0 (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>16)&0xff); }
ubyte opOp1 (uint op) pure nothrow @safe @nogc { pragma(inline, true); return ((op>>24)&0xff); }
short opILit (uint op) pure nothrow @safe @nogc { pragma(inline, true); return cast(short)((op>>16)&0xffff); }
uint op3Byte (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op>>8); }
uint op2Byte (uint op) pure nothrow @safe @nogc { pragma(inline, true); return (op>>16); }

uint opMakeILit (ubyte op, byte dest, short val) pure nothrow @safe @nogc { pragma(inline, true); return ((val<<16)|((dest&0xff)<<8)|op); }
uint opMake3Byte (ubyte op, uint val) pure nothrow @safe @nogc { pragma(inline, true); assert(val <= 0xffffff); return (val<<8)|op; }


// ////////////////////////////////////////////////////////////////////////// //
private import std.stdio : File;

// returns instruction size
uint dumpInstr (File fo, uint pc, const(uint)[] code) {
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
  fo.writef("%-9s", cast(Op)code[pc].opCode);
  switch (atp) with (OpArgs) {
    case Dest: fo.writefln("dest:%s", code[pc].opDest); break;
    case DestOp0: fo.writefln("dest:%s, op0:%s", code[pc].opDest, code[pc].opOp0); break;
    case DestOp0Op1: fo.writefln("dest:%s, op0:%s, op1:%s", code[pc].opDest, code[pc].opOp0, code[pc].opOp1); break;
    case Dest2Bytes: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].op2Byte); break;
    case Dest3Bytes: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].op3Byte); break;
    case DestInt: fo.writefln("dest:%s; val:%s", code[pc].opDest, code[pc].opILit); break;
    case DestJump: fo.writefln("0x%08X", code[pc].op3Byte); break;
    case DestCall: fo.writefln("dest:%s; frame:%s; args:%s", code[pc].opDest, code[pc].opOp0, code[pc].opOp1); break;
    case Op0Op1: fo.writefln("op0:%s, op1:%s", code[pc].opOp0, code[pc].opOp1); break;
    case Op0: fo.writefln("op0:%s", code[pc].opOp0); break;
    default: assert(0);
  }
  return 1;
}


// ////////////////////////////////////////////////////////////////////////// //
private:

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
  Op0,
}

__gshared immutable OpArgs[ubyte] opargs;


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
    Op.slit: DestInt,
    Op.xlit: DestInt,

    Op.jump: DestJump,
    Op.xtrue: Dest,
    Op.xfalse: Dest,

    Op.call: DestCall,

    Op.enter: DestOp0Op1,

    Op.ret: Dest,

    Op.oval: Dest2Bytes,

    Op.fval: DestOp0Op1,
    Op.i1fval: DestOp0Op1,
    Op.i2fval: DestOp0Op1,

    Op.fstore: DestOp0Op1,

    Op.i1fstore: DestOp0Op1,
    Op.i2fstore: DestOp0Op1,

    Op.siter: DestOp0,
    Op.niter: Op0,
    Op.kiter: Dest,

    Op.lirint: DestOp0, // dest = lrint(op0): do lrint() (or another fast float->int conversion)
  ];
}
