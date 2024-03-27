/* GML analyzer
 * coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module gaem.anal.ass;

import gaem.parser;
import gaem.anal.utils;


// ////////////////////////////////////////////////////////////////////////// //
void analAss(NodeFunc fn){
	void anal(Node nn, Loc wasCmp=Loc.init){
		if(nn is null) return;
		if(cast(NodeStatement)nn){
			selectNode!(void)(nn,
				(NodeVarDecl n){},
				(NodeBlock n){ foreach(Node st; n.stats) anal(st); },
				(NodeStatementEmpty n){},
				(NodeStatementAss n){ anal(n.el, n.loc/*hack*/); anal(n.er, n.loc/*hack*/); },
				(NodeStatementExpr n){ anal(n.e); },
				(NodeReturn n){ anal(n.e); },
				(NodeWith n){ anal(n.e); anal(n.ebody); },
				(NodeIf n){ anal(n.ec, wasCmp); anal(n.et); anal(n.ef); },
				(NodeStatementBreak n){},
				(NodeStatementContinue n){},
				(NodeFor n){ anal(n.einit); anal(n.econd); anal(n.enext); anal(n.ebody); },
				(NodeWhile n){ anal(n.econd); anal(n.ebody); },
				(NodeDoUntil n){ anal(n.econd); anal(n.ebody); },
				(NodeRepeat n){ anal(n.ecount); anal(n.ebody); },
				(NodeSwitch n){
					anal(n.e);
					foreach(ref ci; n.cases){ anal(ci.e); anal(ci.st); }
				},
				(){ assert(0, "unimplemented node: "~typeid(nn).name); },
			);
		}else{
			selectNode!(void)(nn,
				(NodeLiteral n){},
				(NodeUnaryParens n){ anal(n.e); },
				(NodeUnary n){ anal(n.e, wasCmp); },
				(NodeBinaryCmp n){
					if(wasCmp.valid) message(fn, n.loc, ": double logic op in expression, previous was at ", wasCmp.toStringNoFile);
					anal(n.el, n.loc); anal(n.er, n.loc);
				},
				(NodeBinary n){ anal(n.el, wasCmp); anal(n.er, wasCmp); },
				(NodeFCall n){
					anal(n.fe, wasCmp);
					foreach(immutable idx, Node a; n.args) anal(a);
				},
				(NodeId n){},
				(NodeDot n){ anal(n.e); },
				(NodeIndex n){ anal(n.ei0); anal(n.ei1); anal(n.e); },
				(NodeFunc n){ anal(n.ebody); },
				(){ assert(0, "unimplemented node: "~typeid(nn).name); },
			);
		}
	}
	anal(fn);
}
