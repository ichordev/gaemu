module checker is aliced;

import gmlparser;


void loadScript (string name, bool warnings=false) {
  import std.algorithm : startsWith;
  import std.file : readText;
  import std.path : baseName, extension;
  import std.stdio;
  import std.string : replace;

  bool asXml = false;
  auto s = readText(name);
  s = s.replace("\r\n", "\n").replace("\r", "\n");
  if (s.startsWith("<?xml")) {
    import std.string : indexOf;
    auto pos = s.indexOf(`<argument kind="STRING">`);
    if (pos < 0) assert(0, "wtf?!");
    s = s[pos+24..$];
    pos = s.indexOf(`</argument>`);
    if (pos < 0) assert(0, "wtfx?!");
    s = s[0..pos];
    asXml = true;
  }

  auto lex = new Lexer(s, name);
  lex.xmlMode = asXml;
  auto parser = new Parser(lex);
  parser.strict = false;
  parser.warnings = warnings;
  bool asGmx = (name.extension == ".gmx");
  try {
    if (asGmx) {
      while (lex.isKw(Keyword.Function)) {
        auto loc = lex.loc;
        lex.expect(Keyword.Function);
        string fname = lex.expectId;
        auto fnode = new NodeFunc(fname, loc);
        Node st;
        if (lex.isKw(Keyword.LCurly)) {
          st = parser.parseCodeBlock();
        } else {
          st = parser.parseStatement();
        }
        if (auto b = cast(NodeBlock)st) {
          fnode.ebody = b;
        } else {
          auto blk = new NodeBlock(st.loc);
          blk.addStatement(st);
          fnode.ebody = blk;
        }
        //if (exec.hasFunction(fnode.name)) throw new Exception("duplicate script '"~fnode.name~"' (from file '"~name~"')");
        exec[fnode.name] = fnode;
      }
    } else {
      string scname = name.baseName(".gml");
      auto n = parser.parseFunctionBody(new NodeFunc(scname, lex.loc));
      //writeln(n.toString);
      //if (exec.hasFunction(n.name)) throw new Exception("duplicate script '"~n.name~"'");
      exec[n.name] = n;
    }
    if (!lex.empty) throw new Exception("script '"~name~"' has some extra code");
    return;
  } catch (ErrorAt e) {
    import std.stdio;
    writeln("ERROR at ", e.loc, ": ", e.msg);
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    assert(0);
  } catch (Exception e) {
    import std.stdio;
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    assert(0);
  }
}


NodeFunc parseScript (string code, string scname, bool strict=true) {
  auto lex = new Lexer(code, scname);
  lex.xmlMode = false;
  auto parser = new Parser(lex, strict);
  //parser.warnings = true;
  try {
    return parser.parseFunctionBody(new NodeFunc(scname, lex.loc));
  } catch (ErrorAt e) {
    import std.stdio;
    writeln("ERROR at ", e.loc, ": ", e.msg);
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    writeln(code);
    throw e;
  } catch (Exception e) {
    import std.stdio;
    writeln(typeid(e).name, "@", e.file, "(", e.line, "): ", e.msg);
    throw e;
  }
  assert(0);
}
