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
module gmlparser.parser is aliced;

import gmlparser.ast;
import gmlparser.lexer;
import gmlparser.tokens;


final class Parser {
  Lexer lex;
  Node curbreak, curcont; // current nodes for `break` and `continue`
  bool showCaret = true;
  bool warnings = true;

  this (Lexer alex) {
    lex = alex;
  }

  this(T) (const(char)[] atext, T afname) if (is(T : const(char)[])) {
    lex = new Lexer(atext, afname);
  }

  // ////////////////////////////////////////////////////////////////////// //
  private import std.stdio : File, stdout;

  void printCaret (Loc loc, File ofile=stdout) {
    auto line = lex.line(loc.line);
    if (line.length == 0) return;
    ofile.writeln(line);
    foreach (immutable _; 1..loc.col) ofile.write(' ');
    ofile.writeln('^');
  }

  void warning(A...) (Loc loc, A args) {
    import std.stdio : stderr;
    stderr.writeln("WARNING at ", loc, ": ", args);
    if (showCaret) printCaret(loc, stderr);
  }

  void error(A...) (Loc loc, A args) {
    import std.stdio : stderr;
    stderr.writeln("ERROR at ", loc, ": ", args);
    if (showCaret) printCaret(loc, stderr);
    string msg;
    foreach (immutable a; args) {
      import std.string : format;
      msg ~= "%s".format(a);
    }
    throw new ErrorAt(loc, msg);
  }

  // ////////////////////////////////////////////////////////////////////// //
  // expression parser helpers
  Node parseExprBinOp(string me, string upfunc, T...) (bool stopOnAss) {
    auto e = mixin(upfunc~"(stopOnAss)");
    assert(e !is null);
    mainloop: while (lex.isKw) {
      foreach (immutable idx, auto _; T) {
        static if (idx%2 == 0) {
          if (lex == T[idx]) {
            static if (T[idx] == Keyword.Ass || T[idx] == Keyword.Equ) {
              if (stopOnAss) break mainloop;
            }
            auto loc = lex.loc;
            lex.popFront();
            auto r = mixin(me~"(stopOnAss)");
            assert(r !is null);
            // hacks
                 static if (T[idx] == Keyword.Ass) { enum textual = true; alias tp = NodeBinaryEqu; }
            else static if (T[idx] == Keyword.And) { enum textual = true; alias tp = NodeBinaryLogAnd; }
            else static if (T[idx] == Keyword.Or) { enum textual = true; alias tp = NodeBinaryLogOr; }
            else static if (T[idx] == Keyword.Xor) { enum textual = true; alias tp = NodeBinaryLogXor; }
            else { enum textual = false; alias tp = T[idx+1]; }
            e = new tp(e, r);
            e.textual = textual;
            e.loc = loc;
            assert(e !is null);
            continue mainloop;
          }
        }
      }
      break;
    }
    return e;
  }

  mixin template BuildExprBinOp(string name, string upfunc, T...) {
    static private template BuildOps(T...) {
      static if (T.length == 0)
        enum BuildOps = "";
      else
        enum BuildOps = "Keyword."~T[0]~", NodeBinary"~T[0]~", "~BuildOps!(T[1..$]);
    }
    mixin(
      "Node parseExpr"~name~" (bool stopOnAss) {"~
      "  return parseExprBinOp!(\"parseExpr"~upfunc~"\", \"parseExpr"~upfunc~"\", "~BuildOps!T~")(stopOnAss);"~
      "}");
  }

  // ////////////////////////////////////////////////////////////////////// //
  // expression parser

  // lparen eaten; returns fc
  Node parseFCallArgs (NodeFCall fc) {
    while (lex != Keyword.RParen) {
      fc.args ~= parseExpr();
      if (lex.eatKw(Keyword.Comma)) continue;
      break;
    }
    lex.expect(Keyword.RParen);
    return fc;
  }

  Node parseExprPrimary () {
    auto loc = lex.loc;

    // literals and id
    switch (lex.front.type) {
      case Token.Type.Num: auto n = lex.front.num; lex.popFront(); return new NodeLiteralNum(loc, n);
      case Token.Type.Str: auto n = lex.front.istr; lex.popFront(); return new NodeLiteralStr(loc, n);
      case Token.Type.Id: return new NodeId(loc, lex.expectId);
      default: break;
    }

    // "(...)"
    if (lex.eatKw(Keyword.LParen)) {
      auto res = parseExpr();
      if (lex != Keyword.RParen) error(lex.loc, "`)` expected for `(` at ", loc.toStringNoFile);
      lex.expect(Keyword.RParen);
      return new NodeUnaryParens(loc, res);
    }

    // `true`, `false`, and other funny keywords
    if (lex.eatKw(Keyword.True)) return new NodeLiteralNum(loc, 1);
    if (lex.eatKw(Keyword.False)) return new NodeLiteralNum(loc, 0);
    if (lex.eatKw(Keyword.All)) return new NodeId(loc, "all");
    if (lex.eatKw(Keyword.Noone)) return new NodeId(loc, "noone");
    if (lex.eatKw(Keyword.Self)) return new NodeId(loc, "self");
    if (lex.eatKw(Keyword.Other)) return new NodeId(loc, "other");
    if (lex.eatKw(Keyword.Global)) return new NodeId(loc, "global");
    if (lex.eatKw(Keyword.Pi)) { import std.math : PI; return new NodeLiteralNum(loc, PI); }

    // global scope
    if (lex.eatKw(Keyword.Dot)) error(loc, "no global scope access is supported yet");

    if (lex.isKw(Keyword.PlusPlus)) error(loc, "GML doesn't have '++'");
    if (lex.isKw(Keyword.MinusMinus)) error(loc, "GML doesn't have '--'");

    error(loc, "primary expression expected");
    assert(0);
  }

  Node parseIndexing (Node n) {
    auto loc = lex.loc;
    //lex.expect(Keyword.LBracket); // eaten
    auto res = new NodeIndex(loc, n);
    res.ei0 = parseExpr();
    if (lex.eatKw(Keyword.Comma)) {
      res.ei1 = parseExpr();
    }
    lex.expect(Keyword.RBracket);
    return res;
  }

  Node parseExprPostfix (Node n) {
    for (;;) {
      auto nn = lex.select!(Node, "pop-nondefault")(
        Keyword.Dot, (Loc aloc) => new NodeDot(aloc, n, lex.expectId),
        Keyword.LParen, (Loc aloc) => parseFCallArgs(new NodeFCall(aloc, n)),
        Keyword.LBracket, (Loc aloc) => parseIndexing(n),
        () => null, // special
      );
      if (nn is null) return n;
      n = nn;
    }
  }

  Node parseExprUnary (bool stopOnAss=false) {
    auto loc = lex.loc;

    if (lex.eatKw(Keyword.Add)) return parseExprUnary();
    if (lex.eatKw(Keyword.Sub)) return new NodeUnaryNeg(loc, parseExprUnary());
    if (lex.eatKw(Keyword.LogNot)) return new NodeUnaryNot(loc, parseExprUnary());
    if (lex.eatKw(Keyword.Not)) { auto res = new NodeUnaryNot(loc, parseExprUnary()); res.textual = true; return res; }
    if (lex.eatKw(Keyword.BitNeg)) return new NodeUnaryBitNeg(loc, parseExprUnary());

    auto res = parseExprPrimary();
    return parseExprPostfix(res);
  }

  //                     name      upfunc     tokens
  mixin BuildExprBinOp!("Mul",    "Unary",   "Mul", "Div", "RDiv", "Mod");
  mixin BuildExprBinOp!("Add",    "Mul",     "Add", "Sub"); // binop `~` is here too, but we don't have it
  mixin BuildExprBinOp!("Shift",  "Add",     "LShift", "RShift");
  mixin BuildExprBinOp!("Cmp",    "Shift",   "Less", "Great", "Equ", "NotEqu", "LessEqu", "GreatEqu", "Ass"); // `a is b`, `a in b` are here too
  mixin BuildExprBinOp!("BitAnd", "Cmp",     "BitAnd");
  mixin BuildExprBinOp!("BitOr",  "BitAnd",  "BitOr", "BitXor");
  mixin BuildExprBinOp!("LogAnd", "BitOr",   "LogAnd", "And");
  mixin BuildExprBinOp!("LogOr",  "LogAnd",  "LogOr", "LogXor", "Or", "Xor");

  Node parseExpr () {
    return parseExprLogOr(false);
  }

  // this can be assign expression, check it
  Node parseAssExpr () {
    auto e = parseExprLogOr(true); // stop on assign
    auto loc = lex.loc;
    if (lex.eatKw(Keyword.Ass)) return new NodeBinaryAss(loc, e, parseExpr());
    if (lex.eatKw(Keyword.AssAdd)) return new NodeBinaryAss(loc, e, new NodeBinaryAdd(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssSub)) return new NodeBinaryAss(loc, e, new NodeBinarySub(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssMul)) return new NodeBinaryAss(loc, e, new NodeBinaryMul(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssDiv)) return new NodeBinaryAss(loc, e, new NodeBinaryRDiv(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssBitAnd)) return new NodeBinaryAss(loc, e, new NodeBinaryBitAnd(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssBitOr)) return new NodeBinaryAss(loc, e, new NodeBinaryBitOr(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssBitXor)) return new NodeBinaryAss(loc, e, new NodeBinaryBitXor(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssLShift)) return new NodeBinaryAss(loc, e, new NodeBinaryLShift(lex.loc, e, parseExpr()), true);
    if (lex.eatKw(Keyword.AssRShift)) return new NodeBinaryAss(loc, e, new NodeBinaryRShift(lex.loc, e, parseExpr()), true);
    return e;
  }

  // ////////////////////////////////////////////////////////////////////// //
  void endOfStatement () {
    if (!lex.eatKw(Keyword.Semi)) {
      warning(lex.peloc, "';' missing");
    } else {
      if (lex.isKw(Keyword.Semi)) warning(lex.loc, "extra ';'");
      while (lex.eatKw(Keyword.Semi)) {}
    }
  }

  Node exprInParens () {
    auto ec = parseExpr();
    if (cast(NodeUnaryParens)ec is null) warning(ec.loc, "'(' missing");
    return ec;
  }

  // ////////////////////////////////////////////////////////////////////// //
  // higher-level parsers
  // can create new block
  NodeStatement parseCodeBlock () {
    auto loc = lex.loc;
    lex.expect(Keyword.LCurly);
    auto blk = new NodeBlock(loc);
    // "{}" is just an empty statement, but we'll still create empty code block
    while (!lex.isKw(Keyword.RCurly)) blk.addStatement(parseStatement());
    lex.expect(Keyword.RCurly);
    return blk;
  }

  NodeStatement parseReturn () {
    auto loc = lex.loc;
    lex.expect(Keyword.Return);
    auto res = new NodeReturn(loc, parseExpr());
    endOfStatement();
    return res;
  }

  NodeStatement parseExit () {
    auto loc = lex.loc;
    lex.expect(Keyword.Exit);
    auto res = new NodeReturn(loc);
    endOfStatement();
    return res;
  }

  NodeStatement parseIf () {
    auto loc = lex.loc;
    lex.expect(Keyword.If);
    auto ec = exprInParens();
    auto et = parseStatement();
    auto ef = (lex.eatKw(Keyword.Else) ? parseStatement() : null);
    return new NodeIf(loc, ec, et, ef);
  }

  NodeStatement parseWhile () {
    auto res = new NodeWhile(lex.loc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = res;
    lex.expect(Keyword.While);
    res.econd = exprInParens();
    res.ebody = parseStatement();
    return res;
  }

  NodeStatement parseDoUntil () {
    auto res = new NodeDoUntil(lex.loc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = res;
    lex.expect(Keyword.Do);
    res.ebody = parseStatement();
    lex.expect(Keyword.Until);
    res.econd = exprInParens();
    endOfStatement();
    return res;
  }

  NodeStatement parseRepeat () {
    auto res = new NodeRepeat(lex.loc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = res;
    lex.expect(Keyword.Repeat);
    res.ecount = exprInParens();
    res.ebody = parseStatement();
    return res;
  }

  NodeStatement parseWith () {
    auto loc = lex.loc;
    lex.expect(Keyword.With);
    auto wc = exprInParens();
    auto res = new NodeWith(loc, wc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = res;
    res.ebody = parseStatement();
    return res;
  }

  NodeStatement parseVar () {
    auto loc = lex.loc;
    bool gvar = false;
    if (lex.eatKw(Keyword.Globalvar)) {
      gvar = true;
    } else {
      lex.expect(Keyword.Var);
    }
    if (!lex.isId) error(lex.loc, "identifier expected");
    auto vd = new NodeVarDecl(loc);
    vd.asGlobal = gvar;
    while (lex.isId) {
      vd.locs ~= lex.loc;
      vd.names ~= lex.expectId;
      if (lex.isKw(Keyword.Ass)) error(vd.locs[$-1], "GML doesn't support variable initialization");
      if (!lex.eatKw(Keyword.Comma)) break;
    }
    endOfStatement();
    return vd;
  }

  NodeStatement parseBreak () {
    if (curbreak is null) error(lex.loc, "`break` without loop/switch");
    auto loc = lex.loc;
    lex.expect(Keyword.Break);
    auto res = new NodeStatementBreak(loc, curbreak);
    endOfStatement();
    return res;
  }

  NodeStatement parseCont () {
    if (curcont is null) error(lex.loc, "`continue` without loop/switch");
    auto loc = lex.loc;
    lex.expect(Keyword.Continue);
    auto res = new NodeStatementContinue(loc, curcont);
    endOfStatement();
    return res;
  }

  NodeStatement parseFor () {
    auto forn = new NodeFor(lex.loc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = forn;
    lex.expect(Keyword.For);
    lex.expect(Keyword.LParen);
    // init
    forn.einit = parseAssExpr();
    lex.expect(Keyword.Semi);
    // condition
    forn.econd = parseExpr();
    lex.expect(Keyword.Semi);
    // next
    forn.enext = parseAssExpr();
    lex.expect(Keyword.RParen);
    forn.ebody = parseStatement();
    return forn;
  }

  NodeStatement parseSwitch () {
    auto sw = new NodeSwitch(lex.loc);
    auto oldbreak = curbreak;
    scope(exit) { curbreak = oldbreak; }
    curbreak = sw;
    lex.expect(Keyword.Switch);
    sw.e = exprInParens();
    lex.expect(Keyword.LCurly);
    // parse case nodes; i won't support insane things like Duff's device here
    while (lex != Keyword.RCurly) {
      Node e;
      if (lex.eatKw(Keyword.Default)) {
        // do nothing here
      } else if (lex.eatKw(Keyword.Case)) {
        e = parseExpr();
      } else {
        lex.expect(Keyword.Case);
      }
      lex.expect(Keyword.Colon);
      // `case` without body
      if (lex != Keyword.Case && lex != Keyword.Default && lex != Keyword.RCurly) {
        auto blk = new NodeBlock(lex.loc);
        while (lex != Keyword.Case && lex != Keyword.Default && lex != Keyword.RCurly) {
          auto st = parseStatement();
          // remove excessive blocks
          while (cast(NodeBlock)st) {
            auto bk = cast(NodeBlock)st;
            if (bk.stats.length != 1) break;
            if (auto b = cast(NodeStatement)bk.stats[0]) st = b; else break;
          }
          blk.addStatement(st);
        }
        // remove excessive blocks
        while (blk.stats.length == 1) {
          if (auto b = cast(NodeBlock)blk.stats[0]) blk = b; else break;
        }
        sw.appendCase(e, blk);
      } else {
        sw.appendCase(e, null);
      }
    }
    lex.expect(Keyword.RCurly);
    return sw;
  }

  // can create new block
  NodeStatement parseStatement () {
    // var declaration
    auto loc = lex.loc;
    // empty statement
    if (lex.eatKw(Keyword.Semi)) {
      warning(loc, "use '{}' instead of ';' for empty statement");
      return new NodeStatementEmpty(loc);
    }
    // block statement
    if (lex.isKw(Keyword.LCurly)) return parseCodeBlock();
    // operators and other keyworded things
    if (lex.isKw) {
      // some keyword
      switch (lex.front.kw) {
        case Keyword.If: return parseIf();
        case Keyword.Return: return parseReturn();
        case Keyword.Exit: return parseExit();
        case Keyword.For: return parseFor();
        case Keyword.While: return parseWhile();
        case Keyword.Do: return parseDoUntil();
        case Keyword.Repeat: return parseRepeat();
        case Keyword.Break: return parseBreak();
        case Keyword.Continue: return parseCont();
        case Keyword.Switch: return parseSwitch();
        case Keyword.Var: return parseVar();
        case Keyword.Globalvar: return parseVar();
        case Keyword.With: return parseWith();
        case Keyword.Case: error(loc, "you cannot use `case` here"); return null;
        case Keyword.Default: error(loc, "you cannot use `default` here"); return null;
        case Keyword.LParen:
        case Keyword.Add:
        case Keyword.Sub:
        case Keyword.True:
        case Keyword.False:
        case Keyword.All:
        case Keyword.Noone:
        case Keyword.Self:
        case Keyword.Other:
        case Keyword.Global:
          goto estat;
        default:
      }
      error(loc, "unexpected keyword: `"~keywordtext(lex.front.kw)~"`");
      return null;
    }
    // should be an expression
   estat:
    auto res = new NodeStatementExpr(parseAssExpr());
    endOfStatement();
    return res;
  }

  // whole input
  NodeFunc parseFunctionBody (NodeFunc fn) {
    fn.ebody = new NodeBlock(lex.loc);
    while (!lex.empty) fn.ebody.addStatement(parseStatement());
    return fn;
  }

  // whole input
  NodeFunc parseFunctionBody (string name) {
    auto fn = new NodeFunc(lex.loc, name);
    fn.ebody = new NodeBlock(lex.loc);
    while (!lex.empty) fn.ebody.addStatement(parseStatement());
    return fn;
  }

  // from `function` keyword
  NodeFunc parseFunction () {
    auto loc = lex.loc;
    lex.expect(Keyword.Function);
    if (!lex.isId) error(lex.loc, "function name expected");
    string name = lex.expectId;
    auto fn = new NodeFunc(loc, name);
    fn.ebody = cast(NodeBlock)parseCodeBlock();
    assert(fn.ebody !is null);
    return fn;
  }
}
