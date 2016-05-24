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
module gmlparser.gentokens is aliced;

string[] tokens = [
  "false",
  "true",

  "all",
  "noone",
  "self",
  "other",
  "global",

  "not",
  "and",
  "or",
  "xor",

  "break",
  "continue",

  "switch",
  "case",
  "default",

  "div",
  "mod",

  "do",
  "until",
  "repeat",
  "while",

  "if",
  "else",

  "for",

  "return",
  "exit",

  "var",
  "globalvar",

  "with",

  "pi",

  "function",
  //"with_object",
];


struct SpTk {
  string text;
  string name;
}

SpTk[] sptk = [
  SpTk("+", "Add"),
  SpTk("-", "Sub"),
  SpTk("*", "Mul"),
  SpTk("/", "RDiv"),
  SpTk("&", "BitAnd"),
  SpTk("|", "BitOr"),
  SpTk("^", "BitXor"),
  SpTk("~", "BitNeg"),
  SpTk("&&", "LogAnd"),
  SpTk("||", "LogOr"),
  SpTk("^^", "LogXor"),
  SpTk("!", "LogNot"),
  SpTk("<", "Less"),
  SpTk(">", "Great"),
  SpTk("<=", "LessEqu"),
  SpTk(">=", "GreatEqu"),
  SpTk("==", "Equ"),
  SpTk("!=", "NotEqu"),
  SpTk("=", "Ass"),
  SpTk("+=", "AssAdd"),
  SpTk("-=", "AssSub"),
  SpTk("*=", "AssMul"),
  SpTk("/=", "AssDiv"),
  SpTk("&=", "AssBitAnd"),
  SpTk("|=", "AssBitOr"),
  SpTk("^=", "AssBitXor"),
  SpTk("<<=", "AssLShift"),
  SpTk(">>=", "AssRShift"),
  SpTk(";", "Semi"),
  SpTk(":", "Colon"),
  SpTk(",", "Comma"),
  SpTk(".", "Dot"),
  SpTk("{", "LCurly"),
  SpTk("}", "RCurly"),
  SpTk("(", "LParen"),
  SpTk(")", "RParen"),
  SpTk("[", "LBracket"),
  SpTk("]", "RBracket"),
  SpTk("<<", "LShift"),
  SpTk(">>", "RShift"),
  SpTk("++", "PlusPlus"),
  SpTk("--", "MinusMinus"),
];


void main () {
  import std.algorithm;
  import std.array;
  import std.stdio;
  tokens = tokens.sort!((a, b) => a < b).array;

  {
    bool[string] tk;
    foreach (string s; tokens) {
      if (s in tk) assert(0, "duplicate token: "~s);
      tk[s] = true;
    }
    foreach (ref st; sptk) {
      if (st.text in tk) assert(0, "duplicate token: "~st.text);
      tk[st.text] = true;
    }
  }

  bool[string] tnm;

  auto fo = File("tokens.d", "w");
fo.writeln(`/* GML parser
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
 */`);
  fo.writeln("module gmlparser.tokens;");

  // fo.write enum
  fo.writeln();
  fo.writeln();
  fo.writeln("enum Keyword {");
  fo.writeln("  NoKW,");
  foreach (string n; tokens) {
    import std.uni : toLower, toUpper;
    string tename = n[0..1].toUpper~n[1..$].toLower;
    if (tename in tnm) assert(0, "duplicate token name: "~tename);
    tnm[tename] = true;
    fo.writeln("  ", tename, ",");
  }
  foreach (ref ti; sptk) {
    if (ti.name in tnm) assert(0, "duplicate token name: "~ti.name);
    tnm[ti.name] = true;
    fo.writeln("  ", ti.name, ",");
  }
  fo.writeln("}");

  fo.writeln();
  fo.writeln();
  fo.writeln("__gshared immutable Keyword[string] keywords;");
  fo.writeln("__gshared immutable string[int] keywordstx;");

  fo.writeln();
  fo.writeln();
  fo.writeln("shared static this () {");
  fo.writeln("  keywords = [");
  foreach (string n; tokens) {
    import std.uni : toLower, toUpper;
    fo.writeln("    \"", n, "\": Keyword.", n[0..1].toUpper, n[1..$].toLower, ",");
  }
  foreach (ref ti; sptk) fo.writeln("    \"", ti.text, "\": Keyword.", ti.name, ",");
  fo.writeln("  ];");
  fo.writeln("  keywordstx = [");
  foreach (string n; tokens) {
    import std.uni : toLower, toUpper;
    fo.writeln("    Keyword.", n[0..1].toUpper, n[1..$].toLower, ": \"", n, "\",");
  }
  foreach (ref ti; sptk) fo.writeln("    Keyword.", ti.name, ": \"", ti.text, "\",");
  fo.writeln("  ];");
  fo.writeln("}");

  fo.writeln();
  fo.writeln();
  fo.writeln("static string keywordtext (uint id) {");
  fo.writeln("  if (auto kw = id in keywordstx) return *kw;");
  fo.writeln("  return \"<unknown>\";");
  fo.writeln("}");
}
