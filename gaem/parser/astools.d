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
module gaem.parser.astools is aliced;

import gaem.parser.lexer;
import gaem.parser.tokens;
import gaem.parser.utils;
import gaem.parser.ast;


// ////////////////////////////////////////////////////////////////////////// //
enum VisitRes { Continue, Stop, SkipChildren }

private final class NodeSkipChildren : Node {}
private __gshared NodeSkipChildren nnc;
shared static this () { nnc = new NodeSkipChildren(); }


Node visitNodes (Node nn, VisitRes delegate (Node n) dg) {
  if (nn is null) return null;
  if (auto r = dg(nn)) return (r == VisitRes.Stop ? nn : null);
  Node res;
  if (cast(NodeStatement)nn) {
    res = selectNode!(Node)(nn,
      (NodeVarDecl n) => null,
      (NodeBlock n) {
        foreach (Node st; n.stats) if (auto r = visitNodes(st, dg)) return r;
        return null;
      },
      (NodeStatementEmpty n) => null,
      (NodeStatementExpr n) => visitNodes(n.e, dg),
      (NodeReturn n) => visitNodes(n.e, dg),
      (NodeWith n) {
        if (auto r = visitNodes(n.e, dg)) return r;
        return visitNodes(n.ebody, dg);
      },
      (NodeIf n) {
        if (auto r = visitNodes(n.ec, dg)) return r;
        if (auto r = visitNodes(n.et, dg)) return r;
        return visitNodes(n.ef, dg);
      },
      (NodeStatementBreak n) => null,
      (NodeStatementContinue n) => null,
      (NodeFor n) {
        if (auto r = visitNodes(n.einit, dg)) return r;
        if (auto r = visitNodes(n.econd, dg)) return r;
        if (auto r = visitNodes(n.enext, dg)) return r;
        return visitNodes(n.ebody, dg);
      },
      (NodeWhile n) {
        if (auto r = visitNodes(n.econd, dg)) return r;
        return visitNodes(n.ebody, dg);
      },
      (NodeDoUntil n) {
        if (auto r = visitNodes(n.econd, dg)) return r;
        return visitNodes(n.ebody, dg);
      },
      (NodeRepeat n) {
        if (auto r = visitNodes(n.ecount, dg)) return r;
        return visitNodes(n.ebody, dg);
      },
      (NodeSwitch n) {
        if (auto r = visitNodes(n.e, dg)) return r;
        foreach (ref ci; n.cases) {
          if (auto r = visitNodes(ci.e, dg)) return r;
          if (auto r = visitNodes(ci.st, dg)) return r;
        }
        return null;
      },
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  } else {
    res = selectNode!(Node)(nn,
      (NodeLiteral n) => null,
      (NodeUnary n) => visitNodes(n.e, dg),
      (NodeBinary n) {
        if (auto r = visitNodes(n.el, dg)) return r;
        return visitNodes(n.er, dg);
      },
      (NodeFCall n) {
        if (auto r = visitNodes(n.fe, dg)) return r;
        foreach (immutable idx, Node a; n.args) if (auto r = visitNodes(a, dg)) return r;
        return null;
      },
      (NodeId n) => null,
      (NodeDot n) => visitNodes(n.e, dg),
      (NodeIndex n) {
        if (auto r = visitNodes(n.ei0, dg)) return r;
        if (auto r = visitNodes(n.ei1, dg)) return r;
        return visitNodes(n.e, dg);
      },
      (NodeFunc n) => visitNodes(n.ebody, dg),
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  }
  if (res is nnc) res = null;
  return res;
}


// ////////////////////////////////////////////////////////////////////////// //
bool isEmpty (Node nn) {
  if (nn is null) return true;
  if (auto n = cast(NodeFunc)nn) return isEmpty(n.ebody);
  if (cast(NodeStatement)nn) {
    if (cast(NodeStatementEmpty)nn) return true;
    if (auto n = cast(NodeBlock)nn) {
      foreach (Node st; n.stats) if (!isEmpty(st)) return false;
      return true;
    }
  }
  return false;
}


// ////////////////////////////////////////////////////////////////////////// //
bool hasReturn (Node nn) {
  if (nn is null) return false;
  if (cast(NodeStatement)nn) {
    return selectNode!(bool)(nn,
      (NodeVarDecl n) => false,
      (NodeBlock n) {
        foreach (Node st; n.stats) if (hasReturn(st)) return true;
        return false;
      },
      (NodeStatementEmpty n) => false,
      (NodeStatementExpr n) => hasReturn(n.e),
      (NodeReturn n) => true,
      (NodeWith n) => (hasReturn(n.e) || hasReturn(n.ebody)),
      (NodeIf n) => (hasReturn(n.ec) || hasReturn(n.et) || hasReturn(n.ef)),
      (NodeStatementBreak n) => false,
      (NodeStatementContinue n) => false,
      (NodeFor n) => (hasReturn(n.einit) || hasReturn(n.econd) || hasReturn(n.enext) || hasReturn(n.ebody)),
      (NodeWhile n) => (hasReturn(n.econd) || hasReturn(n.ebody)),
      (NodeDoUntil n) => (hasReturn(n.econd) || hasReturn(n.ebody)),
      (NodeRepeat n) => (hasReturn(n.ecount) || hasReturn(n.ebody)),
      (NodeSwitch n) {
        if (hasReturn(n.e)) return true;
        foreach (ref ci; n.cases) if (hasReturn(ci.e) || hasReturn(ci.st)) return true;
        return false;
      },
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  } else {
    return selectNode!(bool)(nn,
      (NodeLiteral n) => false,
      (NodeUnary n) => hasReturn(n.e),
      (NodeBinary n) => hasReturn(n.el) || hasReturn(n.er),
      (NodeFCall n) {
        if (hasReturn(n.fe)) return true;
        foreach (immutable idx, Node a; n.args) if (hasReturn(a)) return true;
        return false;
      },
      (NodeId n) => false,
      (NodeDot n) => hasReturn(n.e),
      (NodeIndex n) => (hasReturn(n.e) || hasReturn(n.ei0) || hasReturn(n.ei1)),
      (NodeFunc n) => hasReturn(n.ebody),
      () { assert(0, "unimplemented node: "~typeid(nn).name); },
    );
  }
}


// ////////////////////////////////////////////////////////////////////////// //
//x > view_xview[0]-16 && x < view_xview[0]+view_wview[0]+16 &&
//y > view_yview[0]-16 && y < view_yview[0]+view_hview[0]+16
struct FCInfo {
  Node n;
  bool isNotFrozen;
}

void findFrozenChecks (ref Node[] cn, Node nn) {
  import std.stdio;
  import std.string : replace;

  static bool isViewAccess (Node nn, char what) {
    if (auto n = cast(NodeIndex)nn) {
      // has second index?
      if (n.ei1 !is null) return false;
      // id
      if (auto id = cast(NodeId)n.e) {
        if (id.name.length != 10) return false;
        if (id.name.ptr[5] != what) return false;
        if (id.name[0..5] != "view_") return false;
        if (id.name[6..$] != "view") return false;
      } else {
        return false;
      }
      // [0]
      if (auto l = cast(NodeLiteralNum)n.ei0) {
        if (l.val != 0) return false;
      } else {
        return false;
      }
      return true;
    }
    return false;
  }

  static bool isXYStartExpr (Node nn, char what) {
    if (auto n = cast(NodeBinary)nn) {
      if (!isViewAccess(n.el, what)) return false;
      if (cast(NodeBinaryAdd)nn is null && cast(NodeBinarySub)nn is null) return false;
      if (cast(NodeLiteralNum)n.er is null) return false;
      return true;
    } else if (isViewAccess(nn, what)) {
      return true;
    }
    return false;
  }

  static bool isXYEndExpr (Node nn, char what) {
    if (auto n = cast(NodeBinary)nn) {
      //TODO: check for "+"
      if (isViewAccess(n.el, (what == 'w' ? 'x' : 'y')) && isViewAccess(n.er, what)) return true;
    }
    if (auto n = cast(NodeBinaryAdd)nn) {
      //writeln("x00(", what, "): ", nn.toString.replace("\n", " "), " : ", typeid(n.el).name);
      //writeln("x01(", what, "): ", n.el.toString.replace("\n", " "), " : ", typeid(n.el).name);
      if (cast(NodeBinaryAdd)n.el is null && cast(NodeBinarySub)n.el is null) return false;
      //writeln("x02(", what, "): ", n.el.toString.replace("\n", " "), " : ", typeid(n.el).name);
      auto x = cast(NodeBinary)n.el;
      assert(x !is null);
      if (!isViewAccess(x.el, (what == 'w' ? 'x' : 'y'))) return false;
      //writeln("x03(", what, "): ", n.el.toString.replace("\n", " "), " : ", typeid(n.el).name);
      if (!isViewAccess(x.er, what)) return false;
      //writeln("x04(", what, "): ", n.el.toString.replace("\n", " "), " : ", typeid(n.el).name);
      return true;
    }
    return false;
  }

  static bool isCompare(alias exprcheck) (Node nn, char what) {
    if (auto n = cast(NodeBinaryCmp)nn) {
      if (auto id = cast(NodeId)n.el) {
        if (id.name != "x" && id.name != "y") return false;
        //writeln("cmp(", what, "): ", n.er.toString.replace("\n", " "));
        return exprcheck(n.er, what);
      } else if (auto id = cast(NodeId)n.er) {
        if (id.name != "x" && id.name != "y") return false;
        return exprcheck(n.el, what);
      }
    }
    return false;
  }

  static bool isFrozenExpr3 (Node nn) {
    if (auto a3 = cast(NodeBinaryLogAnd)nn) {
      //writeln("fe3: ", nn.toString.replace("\n", " "));
      //writeln("fe3.l: ", a3.el.toString.replace("\n", " "));
      //writeln("fe3.r: ", a3.er.toString.replace("\n", " "));
      //writeln("     : ", isCompare!isXYStartExpr(a3.el, 'x'));
      //writeln("     : ", isCompare!isXYEndExpr(a3.er, 'w'));
      return
        isCompare!isXYStartExpr(a3.el, 'x') &&
        isCompare!isXYEndExpr(a3.er, 'w');
    }
    return false;
  }

  static bool isFrozenExpr2 (Node nn) {
    if (auto a2 = cast(NodeBinaryLogAnd)nn) {
      //writeln("fe2: ", nn.toString.replace("\n", " "));
      //writeln("fe2.l: ", a2.el.toString.replace("\n", " "));
      //writeln("fe2.r: ", a2.er.toString.replace("\n", " "));
      //writeln("     : ", isCompare!isXYStartExpr(a2.er, 'y'));
      return
        isFrozenExpr3(a2.el) &&
        isCompare!isXYStartExpr(a2.er, 'y');
    }
    return false;
  }

  static bool isFrozenExpr1 (Node nn) {
    if (auto a1 = cast(NodeBinaryLogAnd)nn) {
      //writeln("fe1: ", nn.toString.replace("\n", " "));
      //writeln("fe1.l: ", a1.el.toString.replace("\n", " "));
      //writeln("fe1.r: ", a1.er.toString.replace("\n", " "));
      //writeln("     : ", isCompare!isXYEndExpr(a1.er, 'y'));
      return
        isFrozenExpr2(a1.el) &&
        isCompare!isXYEndExpr(a1.er, 'h');
    }
    return false;
  }

  bool isFrozenExpr (Node nn) {
    auto res = visitNodes(nn, (n) {
      if (isFrozenExpr1(n)) return VisitRes.Stop;
      if (auto id = cast(NodeId)n) {
        if (id.name == "isNotFrozen") return VisitRes.Stop;
      }
      return VisitRes.Continue;
    });
    return (res !is null);
  }

  visitNodes(nn, (nn) {
    if (auto n = cast(NodeIf)nn) {
      if (isFrozenExpr(n.ec)) cn ~= n;
    }
    return VisitRes.Continue;
  });
}
