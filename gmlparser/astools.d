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
      (NodeReturn n) => hasReturn(n.e),
      (NodeWith n) => (hasReturn(n.e) || hasReturn(n.ebody)),
      (NodeWithObject n) => (hasReturn(n.e) || hasReturn(n.ebody)),
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
