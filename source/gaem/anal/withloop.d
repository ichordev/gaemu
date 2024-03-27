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
module gaem.anal.withloop;

import gaem.parser;
import gaem.anal.utils;


// ////////////////////////////////////////////////////////////////////////// //
void analWith(NodeFunc fn){
	
	void anal(Node nn){
		if(nn is null) return;
		if(cast(NodeStatement)nn){
			selectNode!(void)(nn,
				(NodeVarDecl n){},
				(NodeBlock n){ foreach(Node st; n.stats) anal(st); },
				(NodeStatementEmpty n){},
				(NodeStatementAss n){ anal(n.el); anal(n.er); },
				(NodeStatementExpr n){ anal(n.e); },
				(NodeReturn n){ anal(n.e); },
				(NodeWith n){
					anal(n.e);
					if(auto blk = cast(NodeBlock)n.ebody){
						if(blk.stats.length == 0){
							message(fn, n.loc, ": ???");
						}else if(blk.stats.length == 1){
							if(cast(NodeStatementExpr)blk.stats[0] || cast(NodeReturn)blk.stats[0]){
								message(fn, n.loc, ": possible excessive '{}' in 'with'");
								return;
							}
							if(cast(NodeStatementEmpty)blk.stats[0]){
								message(fn, n.loc, ": empty statement in empty block in 'with'");
								return;
							}
							if(cast(NodeStatementBreak)blk.stats[0]){
								message(fn, n.loc, ": single 'break' in 'with', noop");
								return;
							}
							if(cast(NodeStatementContinue)blk.stats[0]){
								message(fn, n.loc, ": single 'continue' in 'with', noop");
								return;
							}
						}
					}
					anal(n.ebody);
				},
				(NodeIf n){ anal(n.ec); anal(n.et); anal(n.ef); },
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
				(NodeUnary n){ anal(n.e); },
				(NodeStatementAss n){ anal(n.el); anal(n.er); },
				(NodeBinary n){ anal(n.el); anal(n.er); },
				(NodeFCall n){
					anal(n.fe);
					foreach(immutable idx, Node a; n.args) anal(a);
				},
				(NodeId n){},
				(NodeDot n){ anal(n.e); },
				(NodeIndex n){
					anal(n.ei0);
					anal(n.ei1);
					anal(n.e);
				},
				(NodeFunc n){ anal(n.ebody); },
				(){ assert(0, "unimplemented node: "~typeid(nn).name); },
			);
		}
	}
	
	anal(fn);
}
