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


// ////////////////////////////////////////////////////////////////////////// //
class Node {
  enum DontVisitMe; // mark nodes that should not be visited with this
  enum CompoundVisitor; // mark struct/class fields that should be processed by visitor with this
  enum SkipChildren = int.min;

  Loc loc;
  bool textual; // used for unary and binary nodes where, for example, "and" is used instead of "&&"

  this (Node n=null) { if (n !is null) loc = n.loc; }
  this (Loc aloc) { loc = aloc; }

  override string toString () const { return "<invalid node:"~typeof(this).stringof~">"; }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeLiteral : Node {
  this (Node n=null) { super(n); }
  this (Loc aloc) { super(aloc); }
}

class NodeLiteralString : NodeLiteral {
  string val;

  //this (Node n, string aval) { val = aval; super(n); }
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

  //this (float aval, Node n=null) { val = aval; super(n); }
  this (Loc aloc, float aval) { val = aval; super(aloc); }

  override string toString () const { import std.string : format; return "%s".format(val); }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeExpr : Node {
  string name;

  this (string aname) { name = aname; super(); }
  this (Node ae, string aname) { name = aname; super(ae); }
  this (Loc aloc, string aname) { name = aname; super(aloc); }
  this (Node ae, Loc aloc, string aname) { name = aname; super(aloc); }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeUnary : NodeExpr {
  Node e;

  this (Node ae, string aname) { e = ae; super(ae, aname); }
  this (Node ae, Loc aloc, string aname) { e = ae; super(ae, aloc, aname); }

  override string toString () const { return name~e.toString; }
}

private enum UnaryOpMixin(string name, string opstr) =
  "class NodeUnary"~name~" : NodeUnary {\n"~
  "   this (Node ae) { super(ae, \""~opstr~"\"); }\n"~
  "   this (Node ae, Loc aloc) { super(ae, loc, \""~opstr~"\"); }\n"~
  "}";

mixin(UnaryOpMixin!("Not", "!"));
mixin(UnaryOpMixin!("Neg", "-"));
mixin(UnaryOpMixin!("BitNeg", "~"));


// ////////////////////////////////////////////////////////////////////////// //
class NodeBinary : NodeExpr {
  Node el, er;

  this (Node ael, Node aer, string aname) { el = ael; er = aer; super(ael, aname); }
  this (Node ael, Node aer, Loc aloc, string aname) { el = ael; er = aer; super(ael, aloc, aname); }

  override string toString () const {
    if (name == "=") return el.toString~" = "~killEB(er.toString);
    string spc = " ";
    switch (name) {
      case "+":
      case "-":
      case "*":
      case "/":
      case "|":
      case "&":
      case "^":
        spc = "";
        break;
      default:
    }
    return "("~el.toString~spc~name~spc~er.toString~")";
  }
}

private enum BinaryOpMixin(string name, string opstr) =
  "class NodeBinary"~name~" : NodeBinary {\n"~
  "   this (Node ael, Node aer) { super(ael, aer, \""~opstr~"\"); }\n"~
  "   this (Node ael, Node aer, Loc aloc) { super(ael, aer, aloc, \""~opstr~"\"); }\n"~
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

mixin(BinaryOpMixin!("Less", "<"));
mixin(BinaryOpMixin!("Great", ">"));
mixin(BinaryOpMixin!("LessEqu", "<="));
mixin(BinaryOpMixin!("GreatEqu", ">="));
mixin(BinaryOpMixin!("Equ", "=="));
mixin(BinaryOpMixin!("NotEqu", "!="));

mixin(BinaryOpMixin!("LogOr", "||"));
mixin(BinaryOpMixin!("LogAnd", "&&"));
mixin(BinaryOpMixin!("LogXor", "^^"));

mixin(BinaryOpMixin!("Ass", "=")); // assign ;-)

// these nodes will never end up in AST, it's for parser
mixin(BinaryOpMixin!("And", "and"));
mixin(BinaryOpMixin!("Or", "or"));
mixin(BinaryOpMixin!("Xor", "xor"));


// ////////////////////////////////////////////////////////////////////////// //
// variable access (as lvalue and as rvalue)
class NodeId : NodeExpr {
  //string name; // for semantic

  this (string aname) { super(aname); }
  this (Loc aloc, string aname) { super(aloc, aname); }

  override string toString () const { return name; }
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeDot : NodeExpr {
  Node e; // base

  this (Node ae, string name) { e = ae; super(ae, name); }
  this (Node ae, Loc aloc, string name) { e = ae; super(ae, aloc, name); }

  override string toString () const {
    if (cast(NodeId)e) return e.toString~"."~name;
    return "("~e.toString~")."~name;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeIndex : NodeExpr {
  Node e; // base
  Node ei0, ei1; // indicies, `ei1` can be `null`

  this (Node ae) { e = ae; super(ae, "index"); }
  this (Node ae, Loc aloc) { e = ae; super(ae, aloc, "index"); }

  override string toString () const {
    string res;
    if (cast(NodeId)e) res = e.toString; else res = "("~e.toString~")";
    res ~= "["~ei0.toString;
    if (ei1 !is null) res ~= ", "~ei1.toString;
    return res~"]";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// function call
class NodeFCall : NodeExpr {
  Node fe; // function expression
  Node[] args;

  //this (Loc aloc, string aname) { fe = new NodeId(aname, aloc); super(aloc); }
  this (Loc aloc, Node afe) { fe = afe; super(aloc, "fcall"); }

  override string toString () const {
    string res = fe.toString~"(";
    foreach (immutable idx, const Node a; args) {
      if (idx != 0) res ~= ", ";
      res ~= killEB(a.toString);
    }
    return res~")";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeStatement : Node {
  this () { super(); }
  this (Node ae) { super(ae); }
  this (Loc aloc) { super(aloc); }
}


// ////////////////////////////////////////////////////////////////////////// //
// local var
class NodeVarDecl : NodeStatement {
  string[] names;
  Loc[] locs; // for names
  bool asGlobal;

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
    foreach (const n; stats) res ~= "\n"~killEB(n.toString);
    res ~= "\n}";
    return res;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeStatementEmpty : NodeStatement {
  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const { return "{}"; }
}


// ////////////////////////////////////////////////////////////////////////// //
// `expression` operator
class NodeStatementExpr : NodeStatement {
  Node e; // expression

  this (Node ae) { e = ae; super(ae); if (ae !is null) loc = ae.loc; }
  this (Node ae, Loc aloc) { e = ae; loc = aloc; }

  override string toString () const {
    return killEB(e.toString)~";";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// `return` and `exit` operators
class NodeReturn : NodeStatement {
  Node e; // return expression (if any); can be `null`

  this (Node ae) { e = ae; if (ae !is null) loc = ae.loc; }
  this (Node ae, Loc aloc) { e = ae; loc = aloc; }

  override string toString () const { return (e !is null ? "return "~e.toString~";" : "exit;"); }
}


// ////////////////////////////////////////////////////////////////////////// //
// `with` operator
class NodeWith : NodeStatement {
  Node e;
  Node ebody;

  this (Node ae) { e = ae; if (ae !is null) loc = ae.loc; }
  this (Node ae, Loc aloc) { e = ae; loc = aloc; }

  override string toString () const { return "with ("~killEB(e.toString)~") "~ebody.toString; }
}


// ////////////////////////////////////////////////////////////////////////// //
// `if` operator
class NodeIf : NodeStatement {
  Node ec, et, ef;

  this (Node aec, Node aet, Node aef) { ec = aec; et = aet; ef = aef; super(aec); }
  this (Node aec, Node aet, Node aef, Loc aloc) { ec = aec; et = aet; ef = aef; loc = aloc; }

  override string toString () const {
    return "if ("~killEB(ec.toString)~") "~et.toString~(ef !is null && !cast(NodeStatementEmpty)ef ? " else "~ef.toString : "");
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// `break` and `continue` operators
class NodeStatementBreakCont : NodeStatement {
  Node ewhich; // loop/switch node

  this (Loc aloc, Node awhich) { ewhich = awhich; super(aloc); }
}

// `break` operator
class NodeStatementBreak : NodeStatementBreakCont {
  this (Loc aloc, Node awhich) { super(aloc, awhich); }

  override string toString () const { return "break;"; }
}

// `continue` operator
class NodeStatementContinue : NodeStatementBreakCont {
  this (Loc aloc, Node awhich) { super(aloc, awhich); }

  override string toString () const { return "continue;"; }
}


// ////////////////////////////////////////////////////////////////////////// //
// `for` operator
class NodeFor : NodeStatement {
  Node einit, econd, enext;
  Node ebody;

  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const {
    if (cast(NodeBlock)ebody) {
      return "for ("~killEB(einit.toString)~"; "~killEB(econd.toString)~"; "~killEB(enext.toString)~") "~killEB(ebody.toString);
    } else {
      return "for ("~killEB(einit.toString)~"; "~killEB(econd.toString)~"; "~killEB(enext.toString)~") {\n"~killEB(ebody.toString)~"\n}";
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// `while` operator
class NodeWhile : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const {
    if (cast(NodeBlock)ebody) {
      return "while ("~killEB(econd.toString)~" "~killEB(ebody.toString);
    } else {
      return "while ("~killEB(econd.toString)~" {\n"~ebody.toString~"\n}";
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// `do/until` operator
class NodeDoUntil : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const {
    if (cast(NodeBlock)ebody) {
      return "do "~ebody.toString~"\n while ("~killEB(econd.toString)~");";
    } else {
      return "do {\n"~killEB(ebody.toString)~"\n} while ("~killEB(econd.toString)~");";
    }
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// `repeat` operator
class NodeRepeat : NodeStatement {
  Node ecount;
  Node ebody;

  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toString () const {
    if (cast(NodeBlock)ebody) {
      return "repeat("~killEB(ecount.toString)~") "~ebody.toString;
    } else {
      return "repeat("~killEB(ecount.toString)~") {\n"~killEB(ebody.toString)~"\n}";
    }
  }
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
  this (Loc aloc) { loc = aloc; }

  void appendCase (Node ae, Node ast) {
    if (ae is null) {
      foreach (ref cc; cases) {
        if (cc.e is null) throw new ErrorAt(loc, "duplicate `default`");
      }
    }
    cases ~= Case(ae, ast);
  }

  override string toString () const {
    string res = "switch ("~killEB(e.toString)~") {";
    foreach (ref c; cases) {
      if (c.e !is null) {
        res ~= "\ncase "~killEB(c.e.toString)~":";
      } else {
        res ~= "\ndefault:";
      }
      res ~= (c.st !is null ? " "~killEB(c.st.toString) : "");
    }
    return res~"\n}";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// function with body
class NodeFunc : Node {
  string name;
  NodeBlock ebody;

  this (string aname, Loc aloc) { name = aname; loc = aloc; }

  override string toString () const {
    string res = "function "~name~" ";
    if (cast(NodeBlock)ebody) {
      res ~= ebody.toString;
    } else {
      res ~= "{\n"~killEB(ebody.toString)~"\n}";
    }
    return res;
  }
}
