/* GML parser
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
module gaem.parser.lexer is aliced;

import gaem.parser.tokens;


// ////////////////////////////////////////////////////////////////////////// //
public struct Loc {
  string file;
  int line, col;
  uint tpos;

  string toString () const { import std.string : format; return "%s (%s,%s)".format(file, line, col); }
  string toStringNoFile () const { import std.string : format; return "(%s,%s)".format(line, col); }

  @property bool valid () const pure nothrow @safe @nogc { pragma(inline, true); return (line > 0 && col > 0); }
}


// ////////////////////////////////////////////////////////////////////////// //
public class ErrorAt : Exception {
  Loc loc;

  this (string msg, Throwable next=null, string file=__FILE__, usize line=__LINE__) pure nothrow @safe @nogc { super(msg, file, line, next); }
  this (in Loc aloc, string msg, Throwable next=null, string file=__FILE__, usize line=__LINE__) pure nothrow @safe @nogc { loc = aloc; super(msg, file, line, next); }
}


// ////////////////////////////////////////////////////////////////////////// //
public struct Token {
public:
  enum Type {
    EOF = -1,
    Kw,
    Id,
    Str,
    Num,
    Spec,
  }

private:
  const(char)[] tkstr;

public:
  Loc loc, eloc; // token start, token end (after last char)
  Type type = Type.EOF; // token type
  union {
    Keyword kw;
    float num;
  }

@safe:
  void mustbeType (Token.Type tp, string msg="identifier expected", string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    if (type != tp) throw new ErrorAt(loc, msg, null, file, line);
  }
  void mustbeId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Id, msg, file, line); }
  void mustbeStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Str, msg, file, line); }
  void mustbeNum (string msg="number expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); mustbeType(Type.Num, msg, file, line); }

  string toString () const @trusted {
    import std.string : format;
    final switch (type) with (Type) {
      case EOF: return "(%s,%d): <EOF>".format(loc.line, loc.col);
      case Kw: return "(%s,%d): kw.%s <%s>".format(loc.line, loc.col, kw, tkstr);
      case Id: return "(%s,%d): Id:%s".format(loc.line, loc.col, tkstr);
      case Str: return "(%s,%d): Str:%s".format(loc.line, loc.col, Lexer.quote(tkstr));
      case Num: return "(%s,%d): Num:%s".format(loc.line, loc.col, num);
      case Spec: return "(%s,%d): Spec:<%s>".format(loc.line, loc.col, tkstr);
    }
    assert(0);
  }

nothrow:
  // get immutable string
  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  @property string istr () { pragma(inline, true); return (tkstr.length ? tkstr.idup : null); }

const pure nothrow @nogc:
  bool opEquals (Keyword akw) { pragma(inline, true); return (type == Type.Kw && kw == akw); }
  bool isKw (Keyword akw) { pragma(inline, true); return (type == Type.Kw && kw == akw); }
  bool isKw () { pragma(inline, true); return (type == Type.Kw); }

@property:
  const(char)[] str () { pragma(inline, true); return tkstr; }
  Keyword Kw () { pragma(inline, true); return (type == Type.Kw ? kw : Keyword.NoKW); }
  bool isId () { pragma(inline, true); return (type == Type.Id); }
  bool isStr () { pragma(inline, true); return (type == Type.Str); }
  bool isNum () { pragma(inline, true); return (type == Type.Num); }
  bool isSpec () { pragma(inline, true); return (type == Type.Spec); }
  bool isEOF () { pragma(inline, true); return (type == Type.EOF); }
}


// ////////////////////////////////////////////////////////////////////////// //
public final class Lexer {
private:
  const(char)[] text;
  uint tpos;
  Loc cpos; // position for last `getChar()`
  Loc pend; // end of previous token, for better error messages
  bool eof;
  bool lastWasEOL = true;
  Token[] lookup;
  Token tokeof; // will be fixed by `nextToken()`

public:
  this(T) (const(char)[] atext, T afname=null) if (is(T : const(char)[])) {
    text = atext;
    if (afname.length > 0) { static if (is(T == string)) cpos.file = afname; else cpos.file = afname.idup; }
    tokeof.loc.file = cpos.file;
    nextToken();
    pend.line = 1;
    pend.col = 1;
    pend.tpos = 0;
  }

  void error (string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt((lookup.length == 0 ? loc : lookup[0].loc), msg, null, file, line);
  }

  static private void error (in ref Token tk, string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt(tk.loc, msg, null, file, line);
  }

  static private void error() (in auto ref Loc loc, string msg, string file=__FILE__, usize line=__LINE__) {
    pragma(inline, true);
    throw new ErrorAt(loc, msg, null, file, line);
  }

  const(char)[] line (uint idx) {
    if (idx == 0) ++idx;
    uint pos = 0;
    while (--idx > 0) {
      while (pos < text.length && text.ptr[pos] != '\n') ++pos;
      ++pos;
    }
    if (pos >= text.length) return null;
    uint epos = pos;
    while (epos < text.length && text.ptr[epos] != '\n') ++epos;
    while (epos > pos && text.ptr[epos-1] <= ' ') --epos;
    return text[pos..epos];
  }

  void popFront () {
    if (lookup.length > 0) {
      pend = lookup.ptr[0].eloc;
      ++pend.col; // for better error messages
      ++pend.tpos; // to be consistent
      foreach (immutable idx; 1..lookup.length) lookup.ptr[idx-1] = lookup.ptr[idx];
      lookup.length -= 1;
      lookup.assumeSafeAppend;
    }
    nextToken();
  }

  @property pure nothrow @safe @nogc {
    bool empty () const { pragma(inline, true); return (lookup.length == 0); }
    ref inout(Token) front () inout { pragma(inline, true); return (lookup.length ? lookup.ptr[0] : tokeof); }
    // current token's loc
    auto loc () const { pragma(inline, true); return front.loc; }
    auto eloc () const { pragma(inline, true); return front.eloc; }
    auto peloc () const { pragma(inline, true); return pend; }

    bool isId () const { pragma(inline, true); return front.isId; }
    bool isStr () const { pragma(inline, true); return front.isStr; }
    bool isNum () const { pragma(inline, true); return front.isNum; }
    bool isSpec () const { pragma(inline, true); return front.isSpec; }
  }

  bool isKw (Keyword kw) const pure nothrow @safe @nogc { pragma(inline, true); return front.isKw(kw); }
  bool isKw () const pure nothrow @safe @nogc { pragma(inline, true); return front.isKw(); }

  bool opEquals (Keyword kw) const pure nothrow @safe @nogc { pragma(inline, true); return (front == kw); }

  // this eats keyword
  void expect (Keyword kw, string file=__FILE__, usize line=__LINE__) {
    if (!front.isKw(kw)) error(loc, "`"~keywordtext(kw)~"` expected", file, line);
    popFront();
  }

  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  string expectId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) {
    mustbeId(msg, file, line);
    auto res = lookup[0].istr;
    popFront();
    return res;
  }

  // this converts id to `string` via `.idup`, use with caution!
  // `.idup` is used to not anchor the whole source string
  string expectStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) {
    //pragma(inline, true);
    mustbeStr(msg, file, line);
    auto res = lookup[0].istr;
    popFront();
    return res;
  }

  // `mustbe` doesn't eat token
  void mustbeType (Token.Type tp, string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeType(tp, msg, file, line); }
  void mustbeId (string msg="identifier expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeId(msg, file, line); }
  void mustbeStr (string msg="string expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeStr(msg, file, line); }
  void mustbeNum (string msg="number expected", string file=__FILE__, usize line=__LINE__) { pragma(inline, true); return front.mustbeNum(msg, file, line); }

  bool eatKw (Keyword kw) {
    if (!isKw(kw)) return false;
    popFront();
    return true;
  }

  ref Token peek (uint dist) {
    while (!eof && lookup.length <= dist) nextToken();
    return (dist < lookup.length ? lookup.ptr[dist] : tokeof);
  }

  ref Token opIndex (usize dist) { pragma(inline, true); return peek(dist); }

  // return loc for next `getChar()`
  Loc nextLoc () nothrow @safe @nogc {
    Loc res = cpos;
    if (lastWasEOL) { ++res.line; res.col = 1; } else ++res.col;
    return res;
  }

  char peekChar (uint dist=0) nothrow @trusted @nogc {
    pragma(inline, true);
    return (tpos+dist >= text.length ? '\0' : (text.ptr[tpos+dist] ? text.ptr[tpos+dist] : ' '));
  }

  // return char or 0
  char getChar () nothrow @trusted @nogc {
    if (tpos >= text.length) { tpos = text.length; eof = true; }
    if (eof) return '\0';
    cpos.tpos = tpos;
    char ch = text.ptr[tpos++];
    if (ch == '\0') ch = ' ';
    if (lastWasEOL) { ++cpos.line; cpos.col = 1; } else ++cpos.col;
    lastWasEOL = (ch == '\n');
    return ch;
  }

  // skip blanks and comments
  //TODO: make special "comment" token(s)?
  void skipBlanks () @safe {
    mainloop: for (;;) {
      char ch = peekChar;
      if (ch == '/') {
        switch (peekChar(1)) {
          case '/': // single-line comment
            do { ch = getChar(); } while (ch != 0 && ch != '\n');
            continue mainloop;
          case '*': // multiline comment
            getChar(); // skip slash
            auto lc = cpos;
            getChar(); // skip star
            char pch = ' ';
            ch = ' '; // we need this
            for (;;) {
              pch = ch;
              ch = getChar();
              if (ch == 0) error(lc, "unterminated comment");
              if (ch == '/' && pch == '*') break;
            }
            continue mainloop;
          default:
        }
      }
      if (ch == 0 || ch > 32) return;
      getChar();
    }
  }

  private void nextToken () {
    if (eof) return;

    skipBlanks();
    if (peekChar == '\0') {
      eof = true;
      tokeof.loc = cpos;
      tokeof.eloc = cpos;
      //++tokeof.eloc.col; // for better error messages
      //++tokeof.eloc.tpos; // to be consistent
      return;
    }

    Token tk;
    auto tkspos = tpos;
    char ch = getChar();
    tk.loc = cpos;

    // quoted string
    if (ch == '"' || ch == '\'') {
      char ech = ch;
      tk.type = Token.Type.Str;
      ++tkspos; // skip quote
      for (;;) {
        ch = getChar();
        if (ch == 0) error(tk, "unterminated string");
        if (ch == ech) break;
      }
      tk.tkstr = text[tkspos..tpos-1]; // -1 due to eaten quote
      tk.eloc = cpos;
      //++tk.eloc.col; // for better error messages
      //++tk.eloc.tpos; // to be consistent
      lookup ~= tk;
      return;
    }

    // hex number
    if (ch == '$') {
      float n = 0;
      tk.type = Token.Type.Num;
      getChar(); // skip dollar
      int dv = digitValue(peekChar);
      if (dv < 0 || dv > 15) error(tk, "hex number expected");
      for (;;) {
        dv = digitValue(peekChar);
        if (dv < 0 || dv > 15) break;
        n = n*16+dv;
        getChar();
      }
      ch = peekChar;
      if (isIdChar(ch) || ch == '.') error(tk, "hex number expected");
      tk.num = n;
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      //++tk.eloc.col; // for better error messages
      //++tk.eloc.tpos; // to be consistent
      lookup ~= tk;
      return;
    }

    // number
    if (isDigit(ch) || (ch == '.' && isDigit(peekChar))) {
      float n = 0;
      tk.type = Token.Type.Num;
      if (ch != '.') n = ch-'0';
      if (ch != '.') {
        // integral part
        for (;;) {
          if (!isDigit(peekChar)) break;
          ch = getChar();
          n = n*10+ch-'0';
        }
        if (peekChar == '.') ch = getChar();
      }
      if (ch == '.') {
        // fractional part
        if (!isDigit(peekChar)) error(tk, "real number expected");
        float div = 1;
        for (;;) {
          if (!isDigit(peekChar)) break;
          ch = getChar();
          div /= 10;
          n += div*(ch-'0');
        }
      }
      if (peekChar == 'e' || peekChar == 'E') {
        // exponent
        getChar();
        bool neg = false;
        if (peekChar == '+') getChar(); else if (peekChar == '-') { getChar(); neg = true; }
        if (!isDigit(peekChar)) error(tk, "invalid number");
        int e = 0;
        while (isDigit(peekChar)) {
          ch = getChar();
          e = e*10+(ch-'0');
          if (e < 0) error(tk, "invalid number (exponent overflow)");
        }
        //{ import std.conv : to; assert(0, to!string(e)); }
        if (neg) {
          while (e-- > 0) n = n/10;
        } else {
          while (e-- > 0) n = n*10;
        }
      }
      tk.num = n;
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      //++tk.eloc.col; // for better error messages
      //++tk.eloc.tpos; // to be consistent
      ch = peekChar;
      if (isIdChar(ch) || ch == '.') error(tk, "invalid number");
      lookup ~= tk;
      return;
    }

    // identifier
    if (isIdStart(ch)) {
      tk.type = Token.Type.Id;
      while (isIdChar(peekChar)) getChar();
      tk.tkstr = text[tkspos..tpos];
      tk.eloc = cpos;
      //++tk.eloc.col; // for better error messages
      //++tk.eloc.tpos; // to be consistent
      if (auto kw = tk.tkstr in keywords) {
        tk.type = Token.Type.Kw;
        tk.kw = *kw;
      }
      lookup ~= tk;
      return;
    }

    // delimiter
    char[5] dbuf;
    dbuf[0] = ch;
    if (auto xkw = dbuf[0..1] in keywords) {
      tk.type = Token.Type.Kw;
      tk.kw = *xkw;
      foreach (uint dpos; 1..dbuf.length) {
        dbuf[dpos] = peekChar;
        if (auto kw = dbuf[0..dpos+1] in keywords) {
          tk.type = Token.Type.Kw;
          tk.kw = *kw;
          getChar(); // eat token char
        } else {
          break;
        }
      }
    } else {
      tk.type = Token.Type.Spec;
    }
    tk.tkstr = text[tkspos..tpos];
    tk.eloc = cpos;
    //++tk.eloc.col; // for better error messages
    //++tk.eloc.tpos; // to be consistent
    lookup ~= tk;
  }

  auto select(RetType, string mode="peek", A...) (scope A args) { pragma(inline, true); return selectN!(RetType, mode)(0, args); }

  auto selectN(RetType, string mode="peek", A...) (usize n, scope A args) {
    import std.traits : ReturnType;

    static assert(mode == "peek" || mode == "pop" || mode == "pop-nondefault", "selectN: invalid mode: '"~mode~"'");

    template isGoodDg(usize idx, T) {
      private import std.traits;
      static if (idx < A.length && isCallable!(A[idx]) && arity!(args[idx]) == 1) {
        enum isGoodDg = is(Parameters!(A[idx])[0] == T);
      } else {
        enum isGoodDg = false;
      }
    }

    template isGoodArglessDg(usize idx) {
      private import std.traits;
      static if (idx < A.length && isCallable!(A[idx]) && arity!(args[idx]) == 0) {
        enum isGoodArglessDg = true;
      } else {
        enum isGoodArglessDg = false;
      }
    }

    // sorry, but this has to be string mixin, due to possible empty `arg`
    enum DoCallDg(string arg) =
      "static if (!is(ReturnType!(A[xidx]) == void)) return cast(RetType)(args[xidx]("~arg~")); else { args[xidx]("~arg~"); return RetType.init; }";

    // we can't have inner mixin templates, so... sorry, it's string again
    enum CallDg = q{
           static if (isGoodDg!(xidx, Token)) { mixin(DoCallDg!"tk"); }
      else static if (isGoodDg!(xidx, Loc)) { mixin(DoCallDg!"tk.loc"); }
      else static if (isGoodDg!(xidx, Token.Type)) { mixin(DoCallDg!"tk.type"); }
      else static if (isGoodDg!(xidx, Keyword)) { mixin(DoCallDg!"tk.Kw"); }
      else static if (isGoodArglessDg!(xidx)) { mixin(DoCallDg!""); }
      else static assert(0, "selectN: invalid delegate #"~xidx.stringof);
    };

    auto tk = peek(n);
    bool found = false;
    foreach (immutable aidx, auto arg; args) {
      static if (aidx%2 == 0) {
        static if (is(typeof(arg) == Keyword) || is(typeof(arg) == Token.Type)) {
               static if (is(typeof(arg) == Keyword)) found = (tk == arg);
          else static if (is(typeof(arg) == Token.Type)) found = (tk.type == arg);
          else static assert(0, "wtf?!");
          if (found) {
            // process `mode`
            static if (mode != "peek") popFront();
            // call delegate
            enum xidx = aidx+1;
            mixin(CallDg);
          }
        } else {
          // default
          // process `mode`
          static if (mode == "pop") popFront();
          // call delegate
          enum xidx = aidx;
          mixin(CallDg);
        }
      }
    }
    error(tk, "selectN is out of nodes");
    assert(0);
  }

static:
  private immutable byte[256] digitValues = {
    byte[256] res = -1;
    foreach (ubyte idx; '0'..'9'+1) res[idx] = cast(byte)(idx-'0');
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = cast(byte)(idx-'A'+10);
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = cast(byte)(idx-'a'+10);
    return res;
  }();

  private immutable bool[256] idStartChars = {
    bool[256] res = false;
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = true;
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = true;
    res['_'] = true;
    return res;
  }();

  private immutable bool[256] idChars = {
    bool[256] res = false;
    foreach (ubyte idx; '0'..'9'+1) res[idx] = true;
    foreach (ubyte idx; 'A'..'Z'+1) res[idx] = true;
    foreach (ubyte idx; 'a'..'z'+1) res[idx] = true;
    res['_'] = true;
    return res;
  }();

  bool isDigit() (char ch) { pragma(inline, true); return (ch >= '0' && ch <= '9'); }
  int digitValue() (char ch) { pragma(inline, true); return digitValues.ptr[cast(ubyte)ch]; }
  bool isIdStart() (char ch) { pragma(inline, true); return idStartChars.ptr[cast(ubyte)ch]; }
  bool isIdChar() (char ch) { pragma(inline, true); return idChars.ptr[cast(ubyte)ch]; }

  string gmlQuote (const(char)[] s) {
    import std.array : appender;
    auto res = appender!string();
    enum Prev { Nothing, Char, Spec }
    Prev prev = Prev.Nothing;
    foreach (char ch; s) {
      if (ch < ' ' || ch == 127 || ch == '"') {
        import std.conv : to;
        final switch (prev) with (Prev) {
          case Nothing: break;
          case Char: res.put(`"+`); break;
          case Spec: res.put(`+`); break;
        }
        prev = Prev.Spec;
        res.put("chr(");
        res.put(to!string(cast(uint)ch));
        res.put(")");
      } else {
        final switch (prev) with (Prev) {
          case Nothing: res.put('"'); break;
          case Char: break;
          case Spec: res.put(`+"`); break;
        }
        prev = Prev.Char;
        res.put(ch);
      }
    }
    if (prev == Prev.Nothing) return `""`;
    if (prev == Prev.Char) res.put('"');
    return res.data;
  }

  /// quote string: append double quotes, screen all special chars;
  /// so quoted string forms valid D string literal.
  /// allocates.
  string quote (const(char)[] s) {
    import std.array : appender;
    import std.format : formatElement, FormatSpec;
    auto res = appender!string();
    FormatSpec!char fspc; // defaults to 's'
    formatElement(res, s, fspc);
    return res.data;
  }
}


version(gml_lexer_test) unittest {
  import std.file;
  import std.stdio;
  auto s = readText("scrDrawHUD.gml");
  auto lex = new Lexer(s, "scrDrawHUD.gml");
  try {
    while (!lex.empty) {
      //if (lex == Keyword.RCurly) writeln("*******************");
      auto v = lex.select!(int, "pop")(
        Keyword.LCurly, (ref Token tk) => 1,
        Keyword.RCurly, (Keyword kw) => 2,
        Keyword.Semi, () => 6,
        Keyword.Sub, (Loc loc) => 99,
        Token.Type.Num, (ref Token tk) => 3,
        (ref Token tk) => writeln(tk),
      );
      if (v) writeln("*** ", v);
      //writeln(v, ": ", lex.front);
      //lex.popFront();
    }
  } catch (ErrorAt e) {
    writeln("PARSE ERROR: ", e.line);
    writeln(e.loc);
  }
}
