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
class Node {
  protected import std.string : stripLeft, stripRight;

  Loc loc;
  bool textual; // used for unary and binary nodes where, for example, "and" is used instead of "&&"

  this () {}
  this (Node n) { if (n !is null) loc = n.loc; }
  this (Loc aloc) { loc = aloc; }

  string toStringInd (int indent) const => indentStr(indent)~"<invalid node:"~typeof(this).stringof~">";

  override string toString () const => toStringInd(0);

  // add outer "()" if there is none
  string asCondStr () const => (cast(NodeUnaryParens)this ? this.toString : "("~this.toString~")");

  static string indentStr (int indent) {
    if (indent < 1) return null;
    auto s = new char[](indent);
    s[] = ' ';
    return cast(string)s; // it is safe here
  }
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

  override string toStringInd (int indent) const {
    import std.array : appender;
    import std.format : formatElement, FormatSpec;
    auto res = appender!string();
    res.put(indentStr(indent));
    FormatSpec!char fspc; // defaults to 's'
    formatElement(res, val, fspc);
    return res.data;
  }
}

class NodeLiteralNum : NodeLiteral {
  float val;

  this () {}
  this (Loc aloc, float aval) { val = aval; super(aloc); }

  override string toStringInd (int indent) const { import std.string : format; return "%s%s".format(indentStr(indent), val); }
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

  override string toStringInd (int indent) const => indentStr(indent)~name~e.toString;
}


// ////////////////////////////////////////////////////////////////////////// //
// "()" expression
class NodeUnaryParens : NodeUnary {
  this () {}
  this (Node ae) { super(ae, "()"); }
  this (Loc aloc, Node ae) { super(aloc, ae, "()"); }

  override string toStringInd (int indent) const => indentStr(indent)~"("~e.toString~")";
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

  override string toStringInd (int indent) const {
    if (name.length > 1 && name != "<<" && name != ">>") return indentStr(indent)~el.toString~" "~name~" "~er.toString;
    // to correctly emit "a- -1"
    if (name == "-") {
      if (auto l = cast(NodeLiteralNum)er) return indentStr(indent)~el.toString~(l.val < 0 ? " " : "")~name~(l.val < 0 ? " " : "")~er.toString;
    }
    return indentStr(indent)~el.toString~name~er.toString;
  }
}

class NodeBinaryCmp : NodeBinary {
  this () {}
  this (Node ael, Node aer, string aname) { super(ael, aer, aname); }
  this (Loc aloc, Node ael, Node aer, string aname) { super(aloc, ael, aer, aname); }

  override string toStringInd (int indent) const => indentStr(indent)~el.toString~" "~name~" "~er.toString;
}

class NodeBinaryLogic : NodeBinary {
  this () {}
  this (Node ael, Node aer, string aname) { super(ael, aer, aname); }
  this (Loc aloc, Node ael, Node aer, string aname) { super(aloc, ael, aer, aname); }

  override string toStringInd (int indent) const => indentStr(indent)~el.toString~" "~name~" "~er.toString;
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

class NodeBinaryAss : NodeBinary {
  bool expanded; // this is expanded opOpAssign

  this () {}
  this (Node ael, Node aer) { super(ael, aer, "="); }
  this (Loc aloc, Node ael, Node aer) { super(aloc, ael, aer, "="); }
  this (Loc aloc, Node ael, Node aer, bool aexpanded) { expanded = aexpanded; super(aloc, ael, aer, "="); }

  override string toStringInd (int indent) const {
    string res = indentStr(indent)~el.toString~" ";
    if (expanded) {
      if (auto x = cast(NodeBinary)er) {
        res ~= x.name~"= "~x.er.toString;
      } else {
        assert(0, "wtf?!");
      }
    } else {
      res ~= "= "~er.toString;
    }
    return res;
  }
}

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

  override string toStringInd (int indent) const => indentStr(indent)~name;
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeDot : NodeExpr {
  Node e; // base

  this () {}
  this (Node ae, string name) { e = ae; super(ae, name); }
  this (Loc aloc, Node ae, string name) { e = ae; super(aloc, name); }

  override string toStringInd (int indent) const => indentStr(indent)~e.toString~"."~name;
}


// ////////////////////////////////////////////////////////////////////////// //
// field access (as lvalue and as rvalue)
class NodeIndex : NodeExpr {
  Node e; // base
  Node ei0, ei1; // indicies, `ei1` can be `null`

  this () {}
  this (Node ae) { e = ae; super(ae, "index"); }
  this (Loc aloc, Node ae) { e = ae; super(aloc, "index"); }

  override string toStringInd (int indent) const {
    string res = indentStr(indent)~e.toString~"["~ei0.toString;
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

  override string toStringInd (int indent) const {
    string res = indentStr(indent)~fe.toString~"(";
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

  override string toStringInd (int indent) const {
    string res = indentStr(indent)~(asGlobal ? "globalvar " : "var ");
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

  override string toStringInd (int indent) const {
    string res = "{";
    foreach (const n; stats) res ~= "\n"~n.toStringInd(indent+2);
    res ~= "\n"~indentStr(indent)~"}";
    return res;
  }
}


// ////////////////////////////////////////////////////////////////////////// //
class NodeStatementEmpty : NodeStatement {
  this () {}
  this (Loc aloc) { loc = aloc; }

  override string toStringInd (int indent) const => indentStr(indent)~"{}";
}


// ////////////////////////////////////////////////////////////////////////// //
// `expression` operator
class NodeStatementExpr : NodeStatement {
  Node e; // expression

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae) { e = ae; super(aloc); }

  override string toStringInd (int indent) const => e.toStringInd(indent)~";";
}


// ////////////////////////////////////////////////////////////////////////// //
// `return` and `exit` operators
class NodeReturn : NodeStatement {
  Node e; // return expression (if any); can be `null`

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae=null) { e = ae; super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~(e !is null ? "return "~e.toString~";" : "exit;");
}


// ////////////////////////////////////////////////////////////////////////// //
// `with` operator
class NodeWith : NodeStatement {
  Node e;
  Node ebody;

  this () {}
  this (Node ae) { e = ae; super(ae); }
  this (Loc aloc, Node ae) { e = ae; super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"with "~e.asCondStr~" "~ebody.toStringInd(indent).stripLeft;
}


// ////////////////////////////////////////////////////////////////////////// //
// `if` operator
class NodeIf : NodeStatement {
  Node ec;
  NodeStatement et, ef;

  this () {}
  this (Node aec, NodeStatement aet, NodeStatement aef) { ec = aec; et = aet; ef = aef; super((aec !is null ? aec : aet ! is null ? aet : aef)); }
  this (Loc aloc, Node aec, NodeStatement aet, NodeStatement aef) { ec = aec; et = aet; ef = aef; super(aloc); }

  override string toStringInd (int indent) const {
    //{ import std.stdio : stderr; stderr.writeln(loc); }
    assert(et !is null);
    string res = indentStr(indent)~"if "~ec.asCondStr~" "~et.toStringInd(indent).stripLeft;
    if (ef is null) return res;
    if (cast(NodeBlock)et || cast(NodeBlock)ef) return res~" else "~ef.toStringInd(indent).stripLeft;
    if (cast(NodeIf)et) return res~" else "~ef.toStringInd(indent).stripLeft;
    if (cast(NodeIf)ef) return res~"\n"~indentStr(indent)~"else "~ef.toStringInd(indent).stripLeft;
    string st = et.toString;
    string sf = ef.toString;
    if (st.length+sf.length <= 68) return res~" else "~sf;
    return res~"\n"~indentStr(indent)~"else "~ef.toStringInd(indent).stripLeft;
  }
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

  override string toStringInd (int indent) const => indentStr(indent)~"break;";
}

// `continue` operator
class NodeStatementContinue : NodeStatementBreakCont {
  this (Loc aloc, Node awhich) { super(aloc, awhich); }

  override string toStringInd (int indent) const => indentStr(indent)~"continue;";
}


// ////////////////////////////////////////////////////////////////////////// //
// `for` operator
class NodeFor : NodeStatement {
  Node einit, econd, enext;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"for ("~einit.toString~"; "~econd.toString~"; "~enext.toString~") "~ebody.toStringInd(indent).stripLeft;
}


// ////////////////////////////////////////////////////////////////////////// //
// `while` operator
class NodeWhile : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"while "~econd.asCondStr~" "~ebody.toStringInd(indent).stripLeft;
}


// ////////////////////////////////////////////////////////////////////////// //
// `do/until` operator
class NodeDoUntil : NodeStatement {
  Node econd;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"do "~ebody.toStringInd(indent).stripLeft~" until "~econd.asCondStr~";";
}


// ////////////////////////////////////////////////////////////////////////// //
// `repeat` operator
class NodeRepeat : NodeStatement {
  Node ecount;
  Node ebody;

  this () {}
  this (Loc aloc) { super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"repeat "~ecount.asCondStr~" "~ebody.toStringInd(indent).stripLeft;
}


// ////////////////////////////////////////////////////////////////////////// //
// `switch` operator
class NodeSwitch : NodeStatement {
  static struct Case {
    Node e; // condition; `null` means "default"
    NodeBlock st; // can be `null`
  }
  Node e; // switch expression
  Case[] cases; // never mutate directly!

  this () {}
  this (Loc aloc) { super(aloc); }

  void appendCase (Node ae, NodeBlock ast) {
    if (ae is null) {
      foreach (ref cc; cases) {
        if (cc.e is null) throw new ErrorAt(loc, "duplicate `default`");
      }
    }
    cases ~= Case(ae, ast);
  }

  override string toStringInd (int indent) const {
    string res = indentStr(indent)~"switch "~e.asCondStr~" {";
    indent += 2;
    foreach (immutable idx, ref c; cases) {
      if (c.e !is null) {
        res ~= "\n"~indentStr(indent)~"case "~c.e.toString~":";
      } else {
        res ~= "\n"~indentStr(indent)~"default:";
      }
      if (c.st !is null && c.st.stats.length > 0) {
        /*
        if (auto blk = cast(NodeBlock)c.st) {
          import std.string : stripRight;
          res ~= " "~c.st.toStringInd(indent).stripLeft.stripRight[1..$-1].stripLeft.stripRight;
        } else {
          res ~= " "~c.st.toStringInd(indent).stripLeft;
        }
        */
        if (c.st.stats.length == 1) {
          res ~= " "~c.st.toStringInd(indent).stripLeft.stripRight[1..$-1].stripLeft.stripRight;
        } else if (c.st.stats.length == 2 && (idx == 0 || cases[idx-1].st !is null)) {
          string stx = c.st.stats[0].toString~" "~c.st.stats[1].toString;
          if (stx.length <= 69) {
            res ~= " "~stx;
          } else {
            res ~= "\n"~indentStr(indent+2)~c.st.toStringInd(indent).stripLeft.stripRight[1..$-1].stripLeft.stripRight;
          }
        } else {
          res ~= "\n"~indentStr(indent+2)~c.st.toStringInd(indent).stripLeft.stripRight[1..$-1].stripLeft.stripRight;
        }
      }
    }
    indent -= 2;
    return res~"\n"~indentStr(indent)~"}";
  }
}


// ////////////////////////////////////////////////////////////////////////// //
// function with body
class NodeFunc : Node {
  string name;
  NodeBlock ebody;

  this () {}
  this (Loc aloc, string aname) { name = aname; super(aloc); }

  override string toStringInd (int indent) const => indentStr(indent)~"function "~name~" "~ebody.toStringInd(indent).stripLeft;
}
