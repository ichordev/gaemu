/* GML analyzer
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
module gaem.anal.uninit is aliced;

import gaem.parser;
import gaem.anal.utils;


// ////////////////////////////////////////////////////////////////////////// //
void analUninited (NodeFunc fn) {
  bool[string] locals;
  bool[string] globals;
  Loc[string] vdecls; // for error messages

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
      case "self": return 16;
      case "other": return 17;
      default:
    }
    return -1;
  }

  void warning(A...) (Loc loc, A args) {
    import std.stdio;
    writeln(loc, ": ", args);
    if (fn.pp !is null) fn.pp.printCaret(loc);
  }

  // collect var declarations (gml is not properly scoped)
  visitNodes(fn.ebody, (Node n) {
    if (auto vd = cast(NodeVarDecl)n) {
      foreach (immutable idx, string name; vd.names) {
        if (name in locals) {
          if (vd.asGlobal) message(fn, vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
        } else if (name in globals) {
          if (!vd.asGlobal) message(fn, vd.locs[idx], "conflicting variable '", name, "' declaration (previous at ", vdecls[name].toStringNoFile, ")");
        }
        vdecls[name] = vd.locs[idx];
        if (vd.asGlobal) {
          globals[name] = true;
        } else {
          locals[name] = true;
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
            message(fn, nn.loc, "assignment to rvalue");
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
              message(fn, nn.loc, "using uninitialized variable; declared at ", vdecls[id.name].toStringNoFile);
            }
          }
          inited[id.name] = true;
          used[id.name] = true;
          return VisitRes.SkipChildren;
        }
        if (auto n = cast(NodeFCall)nn) {
          if (cast(NodeId)n.fe is null) message(fn, n.loc, "invalid function call");
          if (n.args.length > 16) message(fn, n.loc, "too many arguments in function call");
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

    // now show unused locals
    static struct Info { Loc loc; string name; }
    Info[] unusedLocs;
    foreach (string name; locals.keys) {
      if (name !in used) {
        //message(fn, vdecls[name], "unused local '", name, "'");
        unusedLocs ~= Info(vdecls[name], name);
        locals.remove(name);
      }
    }
    import std.algorithm : sort;
    unusedLocs.sort!((ref a, ref b) { if (a.loc.line < b.loc.line) return true; if (a.loc.line > b.loc.line) return false; return (a.loc.col < b.loc.col); });
    foreach (ref nfo; unusedLocs) message(fn, nfo.loc, "unused local '", nfo.name, "'");
  }

  findUninitialized();
}
