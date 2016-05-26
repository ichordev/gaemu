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
  bool strict = false;
  bool warnings = false;
  bool showCaret = true;

  Lexer lex;
  Node curbreak, curcont; // current nodes for `break` and `continue`

  this (Lexer alex, bool astrict=false) {
    lex = alex;
    strict = astrict;
  }

  this(T) (const(char)[] atext, T afname, bool astrict=false) if (is(T : const(char)[])) {
    lex = new Lexer(atext, afname);
    strict = astrict;
  }

  static void errorAt (Loc loc, string msg, string file=__FILE__, usize line=__LINE__) {
    throw new ErrorAt(loc, msg, null, file, line);
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
            static if (T[idx] == Keyword.Ass) {
              if (stopOnAss) break mainloop;
              warning(lex.loc, "'=' instead of '=='");
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
      case Token.Type.Str: auto n = lex.front.istr; lex.popFront(); return new NodeLiteralString(loc, n);
      case Token.Type.Id: return new NodeId(loc, lex.expectId);
      default: break;
    }

    // "(...)"
    if (lex.eatKw(Keyword.LParen)) {
      auto res = parseExpr();
      if (lex != Keyword.RParen) errorAt(lex.loc, "`)` expected for `(` at "~loc.toStringNoFile);
      lex.expect(Keyword.RParen);
      return res;
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
    if (lex.eatKw(Keyword.Dot)) errorAt(loc, "no global scope access is supported yet");

    if (lex.isKw(Keyword.PlusPlus)) errorAt(loc, "GML doesn't have '++'");
    if (lex.isKw(Keyword.MinusMinus)) errorAt(loc, "GML doesn't have '--'");

    errorAt(loc, "primary expression expected");
    assert(0);
  }

  Node parseIndexing (Node n) {
    auto loc = lex.loc;
    //lex.expect(Keyword.LBracket); // eaten
    auto res = new NodeIndex(n, loc);
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
        Keyword.Dot, (Loc aloc) => new NodeDot(n, aloc, lex.expectId),
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
    if (lex.eatKw(Keyword.Sub)) return new NodeUnaryNeg(parseExprUnary(), loc);
    if (lex.eatKw(Keyword.LogNot)) return new NodeUnaryNot(parseExprUnary(), loc);
    if (lex.eatKw(Keyword.Not)) { auto res = new NodeUnaryNot(parseExprUnary(), loc); res.textual = true; return res; }
    if (lex.eatKw(Keyword.BitNeg)) return new NodeUnaryBitNeg(parseExprUnary(), loc);

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
    if (lex.eatKw(Keyword.Ass)) return new NodeBinaryAss(e, parseExpr(), loc);
    if (lex.eatKw(Keyword.AssAdd)) return new NodeBinaryAss(e, new NodeBinaryAdd(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssSub)) return new NodeBinaryAss(e, new NodeBinarySub(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssMul)) return new NodeBinaryAss(e, new NodeBinaryMul(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssDiv)) return new NodeBinaryAss(e, new NodeBinaryRDiv(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssBitAnd)) return new NodeBinaryAss(e, new NodeBinaryBitAnd(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssBitOr)) return new NodeBinaryAss(e, new NodeBinaryBitOr(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssBitXor)) return new NodeBinaryAss(e, new NodeBinaryBitXor(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssLShift)) return new NodeBinaryAss(e, new NodeBinaryLShift(e, parseExpr(), lex.loc), loc);
    if (lex.eatKw(Keyword.AssRShift)) return new NodeBinaryAss(e, new NodeBinaryRShift(e, parseExpr(), lex.loc), loc);
    return e;
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
    if (warnings) {
      import std.stdio : stderr;
      stderr.writeln("WARNING at ", loc, ": ", args);
      if (showCaret) printCaret(loc, stderr);
    }
  }

  void endOfStatement () {
    if (!lex.eatKw(Keyword.Semi)) {
      if (strict) {
        lex.expect(Keyword.Semi);
      } else {
        warning(lex.peloc, "';' missing");
      }
    }
  }

  Node exprInParens () {
    if (strict) {
      lex.expect(Keyword.LParen);
      auto ec = parseExpr();
      lex.expect(Keyword.RParen);
      return ec;
    } else {
      if (!lex.isKw(Keyword.LParen)) warning(lex.loc, "'(' missing");
      return parseExpr();
    }
  }

  // higher-level parsers
  // can create new block
  Node parseCodeBlock () {
    auto loc = lex.loc;
    lex.expect(Keyword.LCurly);
    auto blk = new NodeBlock(loc);
    // "{}" is just an empty statement, but we'll still create empty code block
    while (!lex.isKw(Keyword.RCurly)) blk.addStatement(parseStatement());
    lex.expect(Keyword.RCurly);
    return blk;
  }

  Node parseReturn () {
    auto loc = lex.loc;
    lex.expect(Keyword.Return);
    auto res = new NodeReturn(parseExpr(), loc);
    endOfStatement();
    return res;
  }

  Node parseExit () {
    auto loc = lex.loc;
    lex.expect(Keyword.Exit);
    auto res = new NodeReturn(null, loc);
    endOfStatement();
    return res;
  }

  Node parseIf () {
    auto loc = lex.loc;
    lex.expect(Keyword.If);
    auto ec = exprInParens();
    auto et = parseStatement();
    if (!strict && lex.isKw(Keyword.Semi)) {
      int pos = 1;
      while (lex.peek(pos).isKw(Keyword.Semi)) ++pos;
      if (lex.peek(pos).isKw(Keyword.Else)) {
        if (strict) throw new ErrorAt(lex.peek(pos-1).loc, "unexpected ';'");
        warning(lex.loc, "extra ';'");
        while (lex.eatKw(Keyword.Semi)) {}
      }
    }
    auto ef = (lex.eatKw(Keyword.Else) ? parseStatement() : null);
    return new NodeIf(ec, et, ef, loc);
  }

  Node parseWhile () {
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

  Node parseDoUntil () {
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

  Node parseRepeat () {
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

  Node parseWith () {
    auto loc = lex.loc;
    lex.expect(Keyword.With);
    auto wc = exprInParens();
    auto res = new NodeWith(wc, loc);
    auto oldbreak = curbreak;
    auto oldcont = curcont;
    scope(exit) { curbreak = oldbreak; curcont = oldcont; }
    curbreak = curcont = res;
    res.ebody = parseStatement();
    return res;
  }

  Node parseVar () {
    auto loc = lex.loc;
    bool gvar = false;
    if (lex.eatKw(Keyword.Globalvar)) {
      gvar = true;
    } else {
      lex.expect(Keyword.Var);
    }
    if (!lex.isId) lex.error("identifier expected");
    auto vd = new NodeVarDecl(loc);
    vd.asGlobal = gvar;
    while (lex.isId) {
      //if (vd.hasVar(lex.front.tstr)) lex.error("duplicate variable name '"~lex.front.istr~"'");
      vd.names ~= lex.expectId;
      if (lex.isKw(Keyword.Ass)) lex.error("GML doesn't support variable initialization");
      if (!lex.eatKw(Keyword.Comma)) break;
    }
    endOfStatement();
    return vd;
  }

  Node parseBreak () {
    if (curbreak is null) {
      if (strict) lex.error("`break` without loop/switch");
      warning(lex.loc, "`break` without loop/switch");
    }
    auto loc = lex.loc;
    lex.expect(Keyword.Break);
    auto res = new NodeStatementBreak(loc, curbreak);
    endOfStatement();
    return res;
  }

  Node parseCont () {
    if (curcont is null) {
      if (strict) lex.error("`continue` without loop/switch");
      warning(lex.loc, "`continue` without loop/switch");
    }
    auto loc = lex.loc;
    lex.expect(Keyword.Continue);
    auto res = new NodeStatementContinue(loc, curcont);
    endOfStatement();
    return res;
  }

  Node parseFor () {
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

  Node parseSwitch () {
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
          blk.addStatement(parseStatement());
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
  Node parseStatement () {
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
        case Keyword.Case: lex.error("you cannot use `case` here"); return null;
        case Keyword.Default: lex.error("you cannot use `default` here"); return null;
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
      lex.error("unexpected keyword: `"~keywordtext(lex.front.kw)~"`");
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
    auto fn = new NodeFunc(name, lex.loc);
    fn.ebody = new NodeBlock(lex.loc);
    while (!lex.empty) fn.ebody.addStatement(parseStatement());
    return fn;
  }

  // from `function` keyword
  NodeFunc parseFunction () {
    auto loc = lex.loc;
    lex.expect(Keyword.Function);
    if (!lex.isId) lex.error("function name expected");
    string name = lex.expectId;
    auto fn = new NodeFunc(name, lex.loc);
    fn.ebody = cast(NodeBlock)parseCodeBlock();
    assert(fn.ebody !is null);
    return fn;
  }
}
