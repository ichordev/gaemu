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
module gmlparser.astools is aliced;

import gmlparser.lexer;
import gmlparser.tokens;
import gmlparser.utils;
import gmlparser.ast;


// ////////////////////////////////////////////////////////////////////////// //
enum VisitRes { Continue, Stop, NoChildren }

private final class NodeNoChildren : Node {}
private __gshared NodeNoChildren nnc;
shared static this () { nnc = new NodeNoChildren(); }


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
