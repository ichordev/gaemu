/* GML parser
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
module gmlparser.ast is aliced;

import gmlparser.lexer;
import gmlparser.tokens;
import gmlparser.utils;


// ////////////////////////////////////////////////////////////////////////// //
auto selectNode(RetType=void, A...) (Node node, scope A args) => selector!RetType(node, args);


// ////////////////////////////////////////////////////////////////////////// //
/*
private string killEB (string r) {
  if (r.length < 2 || r[0] != '(' || r[$-1] != ')') return r;
  int depth = 1;
  usize pos = 1;
  while (pos < r.length-1) {
    if (r.ptr[pos] == '(') {
      ++depth;
    } else if (r.ptr[pos] == ')') {
      if (--depth == 0) return r;
    }
    ++pos;
  }
  return r[1..$-1];
}
*/


// ////////////////////////////////////////////////////////////////////////// //
class Node {
  Loc loc;
  bool textual; // used for unary and binary nodes where, for example, "and" is used instead of "&&"

  this () {}
  this (Node n) { if (n !is null) loc = n.loc; }
  this (Loc aloc) { loc = aloc; }

  override string toString () const => "<invalid node:"~typeof(this).stringof~">";
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeLiteral : Node {
  this () {}
  this (Loc aloc) { super(aloc); }
}

class NodeLiteralString : NodeLiteral {
  string val;

  this () {}
  this (Loc aloc, string aval) { val = aval; super(aloc); }

  override string toString () const {
    import std.array : appender;
    import std.format : formatElement, FormatSpec;
    auto res = appender!string();
    FormatSpec!char fspc; // defaults to 's'
    formatElement(res, val, fspc);
    return res.data;
  }
}

class NodeLiteralNum : NodeLiteral {
  float val;

  this () {}
  this (Loc aloc, float aval) { val = aval; super(aloc); }

  override string toString () const { import std.string : format; return "%s".format(val); }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeExpr : Node {
  string name; // various purposes

  this () {}
  this (string aname) { name = aname; super(); }
  this (Node ae, string aname) { name = aname; super(ae); }
  this (Loc aloc, string aname) { name = aname; super(aloc); }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeUnary : NodeExpr {
  Node e;

  this () {}
  this (Node ae, string aname) { e = ae; super(ae, aname); }
  this (Loc aloc, Node ae, string aname) { e = ae; super(aloc, aname); }

  override string toString () const => name~e.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// "()" expression
class NodeUnaryParens : NodeUnary {
  this () {}
  this (Node ae) { super(ae, "()"); }
  this (Loc aloc, Node ae) { super(aloc, e, "()"); }

  override string toString () const => "("~e.toString~")";
}


// ////////////////////////////////////////////////////////////////////////// //
private enum UnaryOpMixin(string name, string opstr) =
  "class NodeUnary"~name~" : NodeUnary {\n"~
  "   this () {}\n"~
  "   this (Node ae) { super(ae, \""~opstr~"\"); }\n"~
  "   this (Loc aloc, Node ae) { super(loc, ae, \""~opstr~"\"); }\n"~
  "}";

mixin(UnaryOpMixin!("Not", "!"));
mixin(UnaryOpMixin!("Neg", "-"));
mixin(UnaryOpMixin!("BitNeg", "~"));


// ////////////////////////////////////////////////////////////////////////// //
class NodeBinary : NodeExpr {
  Node el, er;

  this () {}
  this (Node ael, Node aer, string aname) { el = ael; er = aer; super((ael !is null ? ael : aer), aname); }
  this (Loc aloc, Node ael, Node aer, string aname) { el = ael; er = aer; super(aloc, aname); }

  override string toString () const {
    if (name.length > 1 && name != "<<" && name != ">>") return el.toString~" "~name~" "~er.toString;
    // to correctly emit "a- -1"
    if (name == "-") {
      if (auto l = cast(NodeLiteralNum)er) return el.toString~(l.val < 0 ? " " : "")~name~(l.val < 0 ? " " : "")~er.toString;
    }
    return el.toString~name~er.toString;
  }
}

class NodeBinaryCmp : NodeBinary {
  this () {}
  this (Node ael, Node aer, string aname) { super(ael, aer, aname); }
  this (Loc aloc, Node ael, Node aer, string aname) { super(aloc, ael, aer, aname); }

  override string toString () const => el.toString~" "~name~" "~er.toString;
}

class NodeBinaryLogic : NodeBinary {
  this () {}
  this (Node ael, Node aer, string aname) { super(ael, aer, aname); }
  this (Loc aloc, Node ael, Node aer, string aname) { super(aloc, ael, aer, aname); }

  override string toString () const => el.toString~" "~name~" "~er.toString;
}

private enum BinaryOpMixin(string name, string opstr, string base="") =
  "class NodeBinary"~name~" : NodeBinary"~base~" {\n"~
  "   this () {}\n"~
  "   this (Node ael, Node aer) { super(ael, aer, \""~opstr~"\"); }\n"~
  "   this (Loc aloc, Node ael, Node aer) { super(aloc, ael, aer, \""~opstr~"\"); }\n"~
  "}";

mixin(BinaryOpMixin!("Add", "+"));
mixin(BinaryOpMixin!("Sub", "-"));
mixin(BinaryOpMixin!("Mul", "*"));
mixin(BinaryOpMixin!("RDiv", "/"));
mixin(BinaryOpMixin!("Div", "div"));
mixin(BinaryOpMixin!("Mod", "mod"));
mixin(BinaryOpMixin!("BitOr", "|"));
mixin(BinaryOpMixin!("BitAnd", "&"));
mixin(BinaryOpMixin!("BitXor", "^"));
mixin(BinaryOpMixin!("LShift", "<<"));
mixin(BinaryOpMixin!("RShift", ">>"));

mixin(BinaryOpMixin!("Less", "<", "Cmp"));
mixin(BinaryOpMixin!("Great", ">", "Cmp"));
mixin(BinaryOpMixin!("LessEqu", "<=", "Cmp"));
mixin(BinaryOpMixin!("GreatEqu", ">=", "Cmp"));
mixin(BinaryOpMixin!("Equ", "==", "Cmp"));
mixin(BinaryOpMixin!("NotEqu", "!=", "Cmp"));

mixin(BinaryOpMixin!("LogOr", "||", "Logic"));
mixin(BinaryOpMixin!("LogAnd", "&&", "Logic"));
mixin(BinaryOpMixin!("LogXor", "^^", "Logic"));

mixin(BinaryOpMixin!("Ass", "=")); // assign ;-)

// these nodes will never end up in AST, they are here for parser needs
mixin(BinaryOpMixin!("And", "and", "Logic"));
mixin(BinaryOpMixin!("Or", "or", "Logic"));
mixin(BinaryOpMixin!("Xor", "xor", "Logic"));


// ////////////////////////////////////////////////////////////////////////// //
// variable access (as lvalue and as rvalue)
class NodeId : NodeExpr {
  this () {}
  this (string aname) { super(aname); }
  this (Loc aloc, string aname) { super(aloc, aname); }

  override string toString () const => name;
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeDot : NodeExpr {
  Node e; // base

  this () {}
  this (Node ae, string name) { e = ae; super(ae, name); }
  this (Loc aloc, Node ae, string name) { e = ae; super(aloc, name); }

  override string toString () const => e.toString~"."~name;
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeIndex : NodeExpr {
  Node e; // base
  Node ei0, ei1; // indicies, `ei1` can be `null`

  this () {}
  this (Node ae) { e = ae; super(ae, "index"); }
  this (Loc aloc, Node ae) { e = ae; super(aloc, "index"); }

  override string toString () const {
    string res = e.toString~"["~ei0.toString;
    if (ei1 !is null) res ~= ", "~ei1.toString;
    return res~"]";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// function call
class NodeFCall : NodeExpr {
  Node fe; // function expression
  Node[] args;

  this () {}
  this (Loc aloc, Node afe) { fe = afe; super(aloc, "fcall"); }

  override string toString () const {
    string res = fe.toString~"(";
    foreach (immutable idx, const Node a; args) {
      if (idx != 0) res ~= ", ";
      res ~= a.toString;
    }
    return res~")";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeStatement : Node {
  this () {}
  this (Node ae) { super(ae); }
  this (Loc aloc) { super(aloc); }
}


// ////////////////////////////////////////////////////////////////////////// //
// local var
class NodeVarDecl : NodeStatement {
  string[] names;
  Loc[] locs; // for names
  bool asGlobal;

  this () {}
  this (Loc aloc) { super(aloc); }

  bool hasVar (const(char)[] name) {
    foreach (string n; names) if (n == name) return true;
    return false;
  }

  override string toString () const {
    string res = (asGlobal ? "globalvar " : "var ");
    foreach (immutable idx, string n; names) {
      if (idx != 0) res ~= ", ";
      res ~= n;
    }
    return res~";";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// statement block
class NodeBlock : NodeStatement {
  Node[] stats;

  this () {}
  this (Loc aloc) { loc = aloc; }

  void addStatement (Node n) {
    if (n is null) return;
    stats ~= n;
  }

  override string toString () const {
    string res = "{";
    foreach (const n; stats) res ~= "\n"~n.toString;
    res ~= "\n}";
    return res;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeStatementEmpty : NodeStatement {
  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const => "{}";
}


// ////////////////////////////////////////////////////////////////////////// //
// `expression` operator
class NodeStatementExpr : NodeStatement {
  Node e; // expression

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae) { e = ae; super(aloc); }

  override string toString () const => e.toString~";";
}


// ////////////////////////////////////////////////////////////////////////// //
// `return` and `exit` operators
class NodeReturn : NodeStatement {
  Node e; // return expression (if any); can be `null`

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae=null) { e = ae; super(aloc); }

  override string toString () const => (e !is null ? "return "~e.toString~";" : "exit;");
}


// ////////////////////////////////////////////////////////////////////////// //
// `with` operator
class NodeWith : NodeStatement {
  Node e;
  Node ebody;

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae) { e = ae; super(aloc); }

  override string toString () const => "with ("~e.toString~") "~ebody.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// `if` operator
class NodeIf : NodeStatement {
  Node ec, et, ef;

  this () {}
  this (Node aec, Node aet, Node aef) { ec = aec; et = aet; ef = aef; super((aec !is null ? aec : aet ! is null ? aet : aef)); }
  this (Loc aloc, Node aec, Node aet, Node aef) { ec = aec; et = aet; ef = aef; super(aloc); }

  override string toString () const => "if ("~ec.toString~") "~et.toString~(ef !is null && !cast(NodeStatementEmpty)ef ? " else "~ef.toString : "");
}


// ////////////////////////////////////////////////////////////////////////// //
// `break` and `continue` operators
class NodeStatementBreakCont : NodeStatement {
  Node ewhich; // loop/switch node

  this () {}
  this (Loc aloc, Node awhich) { ewhich = awhich; super(aloc); }
}

// `break` operator
class NodeStatementBreak : NodeStatementBreakCont {
  this (Loc aloc, Node awhich) { super(aloc, awhich); }

  override string toString () const => "break;";
}

// `continue` operator
class NodeStatementContinue : NodeStatementBreakCont {
  this (Loc aloc, Node awhich) { super(aloc, awhich); }

  override string toString () const => "continue;";
}


// ////////////////////////////////////////////////////////////////////////// //
// `for` operator
class NodeFor : NodeStatement {
  Node einit, econd, enext;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toString () const => "for ("~einit.toString~"; "~econd.toString~"; "~enext.toString~") "~ebody.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// `while` operator
class NodeWhile : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toString () const => "while ("~econd.toString~") "~ebody.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// `do/until` operator
class NodeDoUntil : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toString () const => "do "~ebody.toString~" until ("~econd.toString~");";
}


// ////////////////////////////////////////////////////////////////////////// //
// `repeat` operator
class NodeRepeat : NodeStatement {
  Node ecount;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toString () const => "repeat ("~ecount.toString~") "~ebody.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// `switch` operator
class NodeSwitch : NodeStatement {
  static struct Case {
    Node e; // condition; `null` means "default"
    Node st; // can be `null`
  }
  Node e; // switch expression
  Case[] cases; // never mutate directly!

  this () {}
  this (Loc aloc) { super(aloc); }

  void appendCase (Node ae, Node ast) {
    if (ae is null) {
      foreach (ref cc; cases) {
        if (cc.e is null) throw new ErrorAt(loc, "duplicate `default`");
      }
    }
    cases ~= Case(ae, ast);
  }

  override string toString () const {
    string res = "switch ("~e.toString~") {";
    foreach (ref c; cases) {
      if (c.e !is null) {
        res ~= "\ncase "~c.e.toString~":";
      } else {
        res ~= "\ndefault:";
      }
      res ~= (c.st !is null ? " "~c.st.toString : "");
    }
    return res~"\n}";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// function with body
class NodeFunc : Node {
  string name;
  NodeBlock ebody;

  this () {}
  this (Loc aloc, string aname) { name = aname; super(aloc); }

  override string toString () const => "function "~name~" "~ebody.toString;
}
