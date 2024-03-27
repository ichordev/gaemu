/* Invisible Vector Library
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
// some string operations: quoting, `indexOf()` for non-utf8
module iv.strex /*is aliced*/;

import iv.alice;

/// quote string: append double quotes, screen all special chars;
/// so quoted string forms valid D string literal.
/// allocates.
string quote (const(char)[] s) {
	import std.array : appender;
	import std.format : formatValue, FormatSpec;
	auto res = appender!string();
	FormatSpec!char fspc; // defaults to 's'
	formatValue(res, s, fspc);
	return res.data;
}

/// convert integral number to number with commas
char[] intWithCommas(T) (char[] dest, T nn, char comma=',') if (__traits(isIntegral, T)) {
	static if (__traits(isUnsigned, T)) {
		enum neg = false;
		//alias n = nn;
		static if (T.sizeof < 8) {
			uint n = nn;
		} else {
			ulong n = nn;
		}
	} else {
		bool neg = (nn < 0);
		static if (T.sizeof < 8) {
			long n = nn;
			if (neg) n = -n;
			if (n < 0) n = T.max;
		} else {
			//alias n = nn;
			long n = nn;
			if (neg) n = -n;
			if (n < 0) n = T.max; //FIXME
		}
	}
	char[256] buf = void;
	int bpos = cast(int)buf.length;
	int leftToComma = 3;
	do {
		if (leftToComma-- == 0) { buf[--bpos] = comma; leftToComma = 2; }
		buf[--bpos] = cast(char)('0'+n%10);
	} while ((n /= 10) != 0);
	if (neg) buf[--bpos] = '-';
	auto len = buf.length-bpos;
	if (dest is null) dest = new char[](len);
	if (len > dest.length) len = dest.length;
	dest[0..len] = buf[bpos..bpos+len];
	return dest[0..len];
}

char[] intWithCommas(T) (T nn, char comma=',') if (__traits(isIntegral, T)) { return intWithCommas(null, nn, comma); }


//char tolower (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return (ch >= 'A' && ch <= 'Z' ? cast(char)(ch-'A'+'a') : ch); }
//char toupper (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return (ch >= 'a' && ch <= 'z' ? cast(char)(ch-'a'+'A') : ch); }
char tolower (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return cast(char)(ch+((ch >= 'A' && ch <= 'Z')<<5)); }
char toupper (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return cast(char)(ch-((ch >= 'a' && ch <= 'z')<<5)); }

bool islower (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return (ch >= 'a' && ch <= 'z'); }
bool isupper (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return (ch >= 'A' && ch <= 'Z'); }

bool isalpha (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')); }
bool isdigit (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return (ch >= '0' && ch <= '9'); }
bool isalnum (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9')); }
bool isxdigit (char ch) pure nothrow @trusted @nogc { pragma(inline, true); return ((ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f') || (ch >= '0' && ch <= '9')); }

/// case-insensitive char compare for ASCII
bool charEquCI (const char c0, const char c1) pure nothrow @trusted @nogc {
	pragma(inline, true);
	// (c0 |= 0x20) is lowercase-conversion for ASCII
	// the good thing is that only uppercase letters will become lowercase letters,
	// other things will become a garbage
	// also, let's hope that any decent compiler is able to perform CSE here
	return
		c0 == c1 || // try the easiest case first
		((c0|0x20) >= 'a' && (c0|0x20) <= 'z' && // it wasn't a letter, no need to check the second char
		 (c0|0x20) == (c1|0x20)); // c1 will become a lowercase ascii only if it was uppercase/lowercase ascii
}

int digitInBase (const char ch, const int base=10) pure nothrow @trusted @nogc {
	pragma(inline, true);
	return
		ch >= '0' && ch <= '9' && ch-'0' < base ? ch-'0' :
		base > 10 && ch >= 'A' && ch < 'Z' && ch-'A'+10 < base ? ch-'A'+10 :
		base > 10 && ch >= 'a' && ch < 'z' && ch-'a'+10 < base ? ch-'a'+10 :
		-1;
}


alias atof = atofd!float; /// very simple atof/atod converter. accepts exponents. returns NaN on error.
alias atod = atofd!double; /// very simple atof/atod converter. accepts exponents. returns NaN on error.

/// very simple atof/atod converter. accepts exponents.
/// returns NaN on error.
T atofd(T) (const(char)[] str) pure nothrow @trusted @nogc if (is(T == float) || is(T == double)) {
	if (str.length == 0) return T.nan; // oops

	const(char)[] s = str;
	double res = 0.0, sign = 1.0;
	bool hasIntPart = false, hasFracPart = false;

	char peekChar () nothrow @trusted @nogc { pragma(inline, true); return (s.length ? s.ptr[0] : '\0'); }
	void skipChar () nothrow @trusted @nogc { pragma(inline, true); if (s.length > 0) s = s[1..$]; }
	char getChar () nothrow @trusted @nogc { char ch = 0; if (s.length > 0) { ch = s.ptr[0]; s = s[1..$]; } return ch; }

	// optional sign
	switch (peekChar) {
		case '-': sign = -1; goto case;
		case '+': skipChar(); break;
		default: break;
	}

	// integer part
	if (isdigit(peekChar)) {
		hasIntPart = true;
		while (isdigit(peekChar)) res = res*10.0+(getChar()-'0');
	}

	// fractional part.
	if (peekChar == '.') {
		skipChar(); // skip '.'
		if (isdigit(peekChar)) {
			hasFracPart = true;
			int divisor = 1;
			long num = 0;
			while (isdigit(peekChar)) {
				divisor *= 10;
				num = num*10+(getChar()-'0');
			}
			res += cast(double)num/divisor;
		}
	}

	// valid number should have integer or fractional part
	if (!hasIntPart && !hasFracPart) return T.nan;

	// optional exponent
	if (peekChar == 'e' || peekChar == 'E') {
		skipChar(); // skip 'E'
		// optional sign
		bool epositive = true;
		switch (peekChar) {
			case '-': epositive = false; goto case;
			case '+': skipChar(); break;
			default: break;
		}
		int expPart = 0;
		while (isdigit(peekChar)) expPart = expPart*10+(getChar()-'0');
		if (epositive) {
			foreach (immutable _; 0..expPart) res *= 10.0;
		} else {
			foreach (immutable _; 0..expPart) res /= 10.0;
		}
	}

	return cast(T)(res*sign);
}


// ascii only
bool strEquCI (const(char)[] s0, const(char)[] s1) pure nothrow @trusted @nogc {
	if (s0.length != s1.length) return false;
	if (s0.ptr == s1.ptr) return true;
	foreach (immutable idx, char c0; s0) {
		// try the easiest case first
		if (__ctfe) {
			if (c0 == s1[idx]) continue;
		} else {
			if (c0 == s1.ptr[idx]) continue;
		}
		c0 |= 0x20; // convert to ascii lowercase
		if (c0 < 'a' || c0 > 'z') return false; // it wasn't a letter, no need to check the second char
		// c0 is guaranteed to be a lowercase ascii here
		if (__ctfe) {
			if (c0 != (s1[idx]|0x20)) return false; // c1 will become a lowercase ascii only if it was uppercase/lowercase ascii
		} else {
			if (c0 != (s1.ptr[idx]|0x20)) return false; // c1 will become a lowercase ascii only if it was uppercase/lowercase ascii
		}
	}
	return true;
}


version(test_strex) unittest {
	assert(strEquCI("Alice", "alice"));
	assert(strEquCI("alice", "Alice"));
	assert(strEquCI("alice", "alice"));
}


// ascii only
int strCmpCI (const(char)[] s0, const(char)[] s1) pure nothrow @trusted @nogc {
	auto slen = s0.length;
	if (s1.length == slen && s0.ptr == s1.ptr) return 0;
	if (slen > s1.length) slen = s1.length;
	char c1;
	foreach (immutable idx, char c0; s0[0..slen]) {
		c0 = c0.tolower;
		if (__ctfe) {
			c1 = s1[idx].tolower;
		} else {
			c1 = s1.ptr[idx].tolower;
		}
		if (c0 < c1) return -1;
		if (c0 > c1) return 1;
	}
	if (s0.length < s1.length) return -1;
	if (s0.length > s1.length) return +1;
	return 0;
}


inout(char)[] xstrip (inout(char)[] s) pure nothrow @trusted @nogc {
	if (__ctfe) {
		while (s.length && s[0] <= ' ') s = s[1..$];
	} else {
		while (s.length && s.ptr[0] <= ' ') s = s[1..$];
	}
	while (s.length && s[$-1] <= ' ') s = s[0..$-1];
	return s;
}


inout(char)[] xstripleft (inout(char)[] s) pure nothrow @trusted @nogc {
	if (__ctfe) {
		while (s.length && s[0] <= ' ') s = s[1..$];
	} else {
		while (s.length && s.ptr[0] <= ' ') s = s[1..$];
	}
	return s;
}


inout(char)[] xstripright (inout(char)[] s) pure nothrow @trusted @nogc {
	while (s.length && s[$-1] <= ' ') s = s[0..$-1];
	return s;
}


bool startsWith (const(char)[] str, const(char)[] pat) pure nothrow @trusted @nogc {
	if (pat.length > str.length) return false;
	return (str[0..pat.length] == pat);
}


bool endsWith (const(char)[] str, const(char)[] pat) pure nothrow @trusted @nogc {
	if (pat.length > str.length) return false;
	return (str[$-pat.length..$] == pat);
}


// ascii only
bool startsWithCI (const(char)[] str, const(char)[] pat) pure nothrow @trusted @nogc {
	if (pat.length > str.length) return false;
	return strEquCI(str[0..pat.length], pat);
}


// ascii only
bool endsWithCI (const(char)[] str, const(char)[] pat) pure nothrow @trusted @nogc {
	if (pat.length > str.length) return false;
	return strEquCI(str[$-pat.length..$], pat);
}


ptrdiff_t indexOf (const(char)[] hay, const(char)[] need, size_t stIdx=0) pure nothrow @trusted @nogc {
	if (hay.length <= stIdx || need.length == 0 || need.length > hay.length-stIdx) {
		return -1;
	} else {
		if (need.length == 1) {
			if (__ctfe) {
				return indexOf(hay, need[0], stIdx);
			} else {
				return indexOf(hay, need.ptr[0], stIdx);
			}
		} else {
			if (__ctfe) {
				foreach (immutable idx; stIdx..hay.length-need.length+1) {
					if (hay[idx..idx+need.length] == need) return idx;
				}
				return -1;
			} else {
				auto res = cast(const(char)*)memmem(hay.ptr+stIdx, hay.length-stIdx, need.ptr, need.length);
				return (res !is null ? cast(ptrdiff_t)(res-hay.ptr) : -1);
			}
		}
	}
}

ptrdiff_t indexOf (const(char)[] hay, char ch, size_t stIdx=0) pure nothrow @trusted @nogc {
	if (hay.length <= stIdx) {
		return -1;
	} else {
		if (__ctfe) {
			foreach (immutable idx; stIdx..hay.length) {
				if (hay[idx] == ch) return idx;
			}
			return -1;
		} else {
			import core.stdc.string : memchr;
			auto res = cast(const(char)*)memchr(hay.ptr+stIdx, ch, hay.length-stIdx);
			return (res !is null ? cast(ptrdiff_t)(res-hay.ptr) : -1);
		}
	}
}


ptrdiff_t lastIndexOf (const(char)[] hay, const(char)[] need, size_t stIdx=0) pure nothrow @trusted @nogc {
	if (hay.length <= stIdx || need.length == 0 || need.length > hay.length-stIdx) {
		return -1;
	} else {
		if (hay.length == 1) {
			if (__ctfe) {
				return lastIndexOf(hay, need[0], stIdx);
			} else {
				return lastIndexOf(hay, need.ptr[0], stIdx);
			}
		} else {
			if (__ctfe) {
				foreach_reverse (immutable idx; stIdx..hay.length-need.length+1) {
					if (hay[idx..idx+need.length] == need) return idx;
				}
				return -1;
			} else {
				auto res = cast(char*)memrmem(hay.ptr+stIdx, hay.length-stIdx, need.ptr, need.length);
				return (res !is null ? cast(ptrdiff_t)(res-hay.ptr) : -1);
			}
		}
	}
}

ptrdiff_t lastIndexOf (const(char)[] hay, char ch, size_t stIdx=0) pure nothrow @trusted @nogc {
	if (hay.length <= stIdx) {
		return -1;
	} else {
		if (__ctfe) {
			foreach_reverse (immutable idx; stIdx..hay.length) {
				if (hay[idx] == ch) return idx;
			}
			return -1;
		} else {
			auto res = cast(const(char)*)memrchr(hay.ptr+stIdx, ch, hay.length-stIdx);
			return (res !is null ? cast(ptrdiff_t)(res-hay.ptr) : -1);
		}
	}
}


version(test_strex) unittest {
	assert(indexOf("Alice & Miriel", " & ") == 5);
	assert(indexOf("Alice & Miriel", " &!") == -1);
	assert(indexOf("Alice & Miriel", "Alice & Miriel was here!") == -1);
	assert(indexOf("Alice & Miriel", '&') == 6);
	char ch = ' ';
	assert(indexOf("Alice & Miriel", ch) == 5);

	assert(indexOf("Alice & Miriel", "i") == 2);
	assert(indexOf("Alice & Miriel", "i", 6) == 9);
	assert(indexOf("Alice & Miriel", "i", 12) == -1);

	assert(indexOf("Alice & Miriel", "Miriel", 8) == 8);
	assert(indexOf("Alice & Miriel", "Miriel", 9) == -1);

	assert(lastIndexOf("Alice & Miriel", "i") == 11);
	assert(lastIndexOf("Alice & Miriel", "i", 6) == 11);
	assert(lastIndexOf("Alice & Miriel", "i", 11) == 11);
	assert(lastIndexOf("Alice & Miriel", "i", 12) == -1);

	assert(lastIndexOf("iiii", "ii") == 2);
}


string detab (const(char)[] s, uint tabSize=8) {
	assert(tabSize > 0);

	import std.array : appender;
	auto res = appender!string();
	uint col = 0;

	foreach (char ch; s) {
		if (ch == '\n' || ch == '\r') {
			col = 0;
		} else if (ch == '\t') {
			auto spins = tabSize-col%tabSize;
			col += spins;
			while (spins-- > 1) res.put(' ');
			ch = ' ';
		} else {
			++col;
		}
		res.put(ch);
	}

	return res.data;
}


version(test_strex) unittest {
	assert(detab(" \n\tx", 9) == " \n         x");
	assert(detab("  ab\t asdf ") == "  ab     asdf ");
}


auto byLine(T) (T s) if (is(T:const(char)[])) {
	static struct Range(T) {
	nothrow @safe @nogc:
	private:
		T s;
		size_t llen, npos;
		this (T as) { s = as; popFront(); }
	public:
		@property bool empty () const { pragma(inline, true); return (s.length == 0); }
		@property T front () const { pragma(inline, true); return cast(T)s[0..llen]; } // fuckin' const!
		auto save () const @trusted { Range!T res = void; res.s = s; res.llen = llen; res.npos = npos; return res; }
		void popFront () @trusted {
			s = s[npos..$];
			llen = npos = 0;
			while (npos < s.length) {
				if (s.ptr[npos] == '\r') {
					llen = npos;
					if (s.length-npos > 1 && s.ptr[npos+1] == '\n') ++npos;
					++npos;
					return;
				}
				if (s.ptr[npos] == '\n') {
					llen = npos;
					++npos;
					return;
				}
				++npos;
			}
			llen = npos;
		}
	}
	return Range!T(s);
}

/*
version(test_strex) unittest {
	enum s = q{
			 import std.stdio;
			 void main() {
					 writeln("Hello");
			 }
		};
		enum ugly = q{
import std.stdio;
void main() {
		writeln("Hello");
}
};

	foreach (/+auto+/ line; s.byLine) {
		import std.stdio;
		writeln("LN: [", line, "]");
	}

	foreach (/+auto+/ line; ugly.byLine) {
		import std.stdio;
		writeln("LN: [", line, "]");
	}
}
*/

// string should be detabbed!
string outdentAll (const(char)[] s) {
	import std.array : appender;
	// first calculate maximum indent spaces
	uint maxspc = uint.max;
	foreach (/*auto*/ line; s.byLine) {
		uint col = 0;
		while (col < line.length && line.ptr[col] <= ' ') {
			if (line.ptr[col] == '\t') assert(0, "can't outdent shit with tabs");
			++col;
		}
		if (col >= line.length) continue; // empty line, don't care
		if (col < maxspc) maxspc = col;
		if (col == 0) break; // nothing to do anymore
	}

	auto res = appender!string();
	foreach (/*auto*/ line; s.byLine) {
		uint col = 0;
		while (col < line.length && line.ptr[col] <= ' ') ++col;
		if (col < line.length) {
			// non-empty line
			res.put(line[maxspc..$]);
		}
		res.put('\n');
	}

	return res.data;
}


version(test_strex) unittest {
		enum pretty = q{
			 import std.stdio;
			 void main() {
					 writeln("Hello");
			 }
		}.outdentAll;

		enum ugly = q{
import std.stdio;
void main() {
		writeln("Hello");
}

};

	import std.stdio;
	assert(pretty == ugly);
}


//From: Yahoo Groups <confirm-s2-2ny0qbq23nljzefbilh5vpjrg1pik5hf-ketmar=ketmar.no-ip.org@yahoogroups.com>
private bool isValidEmailNameChar (char ch) pure nothrow @safe @nogc {
	pragma(inline, true);
	if (ch <= 32) return false;
	if (ch >= '0' && ch <= '9') return true;
	if (ch >= 'a' && ch <= 'z') ch -= 32; // poor man's tolower
	if (ch >= 'A' && ch <= 'Z') return true;
	if (ch == '_' || ch == '+' || ch == '-' || ch == '=' || ch == '.' || ch == '$') return true;
	if (ch >= 128) return true; // why not?
	// why not?
	if (ch == '!' || ch == '%' || ch == '^' || ch == '&' || ch == '(' || ch == ')') return true;
	if (ch == '?') return true;
	return false;
}


private bool isValidEmailHostChar (char ch) pure nothrow @safe @nogc {
	pragma(inline, true);
	if (ch <= 32 || ch >= 127) return false;
	if (ch >= '0' && ch <= '9') return true;
	if (ch >= 'a' && ch <= 'z') ch -= 32; // poor man's tolower
	if (ch >= 'A' && ch <= 'Z') return true;
	if (ch == '-' || ch == '.') return true;
	return false;
}


bool isGoodEmail (const(char)[] s) pure nothrow @trusted @nogc {
	if (s.length == 0 || s.ptr[0] == '@') return false;
	// parse part until '@'
	while (s.length) {
		char ch = s.ptr[0];
		if (ch == '@') break;
		if (!isValidEmailNameChar(ch)) return false;
		s = s[1..$];
	}
	if (!s.length) return false; // no doggy
	assert(s.ptr[0] == '@');
	s = s[1..$];
	if (s.length == 0) return false;
	while (s.length) {
		char ch = s.ptr[0];
		if (!isValidEmailHostChar(ch)) return false;
		s = s[1..$];
	}
	return true;
}


/// backslash in ranges is used to escaping; '<' and '>' matching word start and end
bool globmatch(bool casesens=true) (const(char)[] str, const(char)[] pat) pure nothrow @trusted @nogc {
	static bool globIsWordChar (const char ch) pure nothrow @safe @nogc {
		pragma(inline, true);
		return
			(ch >= 'A' && ch <= 'Z') ||
			(ch >= 'a' && ch <= 'z') ||
			(ch >= '0' && ch <= '9') ||
			ch == '_' || ch >= 128;
	}

	// empty pattern cannot match non-empty string
	if (pat.length == 0) return (str.length == 0);
	// start matching
	const(char)* realstart = str.ptr;
	bool star = false;
	usize patpos = void;
loopStart:
	patpos = 0;
	foreach (usize i; 0..str.length) {
		static if (casesens) {
			immutable char sch = str.ptr[i];
		} else {
			immutable char sch = tolower(str.ptr[i]);
		}
	matchAgain:
		if (patpos >= pat.length) goto starCheck;
		switch (pat.ptr[patpos++]) {
			case '?': // match anything
				break;
			case '*':
				star = true;
				str = str[i..$];
				pat = pat[patpos..$];
				// skip excessive stars
				while (pat.length && pat.ptr[0] == '*') pat = pat[1..$];
				if (pat.length == 0) return true;
				goto loopStart;
			case '[':
				{
					bool hasMatch = false;
					bool inverted = (pat.ptr[patpos] == '^');
					if (inverted) ++patpos;
					if (patpos >= pat.length) return false; // malformed pattern
					do {
						char c0 = pat.ptr[patpos++];
						if (c0 == '\\' && patpos < pat.length) c0 = pat.ptr[patpos++];
						if (patpos >= pat.length) return false; // malformed pattern
						static if (!casesens) c0 = tolower(c0);
						char c1 = c0;
						if (pat.ptr[patpos] == '-') {
							// char range
							++patpos; // skip '-'
							if (patpos >= pat.length) return false; // malformed pattern
							c1 = pat.ptr[patpos++];
							if (c1 == '\\' && patpos < pat.length) c1 = pat.ptr[patpos++];
							static if (!casesens) c1 = tolower(c1);
							if (patpos >= pat.length) return false; // malformed pattern
						}
						hasMatch = (!hasMatch && sch >= c0 && sch <= c1);
					} while (patpos < pat.length && pat.ptr[patpos] != ']');
					if (patpos >= pat.length || pat.ptr[patpos] != ']') return false; // malformed pattern
					++patpos;
					if (inverted) hasMatch = !hasMatch;
					if (!hasMatch) goto starCheck;
					break;
				}
			case '<': // word boundary (start)
				// current char must be a word char
				if (!globIsWordChar(sch)) goto starCheck;
				{
					const usize realpos = cast(ptrdiff_t)(str.ptr-realstart)+i;
					// previous char must not be a word char
					if (realpos > 0 && globIsWordChar(realstart[realpos-1u])) goto starCheck;
				}
				goto matchAgain;
			case '>': // word boundary (end)
				// current char must not be a word char
				if (globIsWordChar(sch)) goto starCheck;
				{
					const usize realpos = cast(ptrdiff_t)(str.ptr-realstart)+i;
					// previous char must be a word char
					if (realpos > 0 && !globIsWordChar(realstart[realpos-1u])) goto starCheck;
				}
				goto matchAgain;
			case '\\':
				++patpos;
				if (patpos >= pat.length) return false; // malformed pattern
				goto default;
			default:
				static if (casesens) {
					if (sch != pat.ptr[patpos-1]) goto starCheck;
				} else {
					if (sch != tolower(pat.ptr[patpos-1])) goto starCheck;
				}
				break;
		}
	}
	pat = pat[patpos..$];
	// pattern may end with stars, skip them
	// also skip word boundaries check (they are always true here)
	while (pat.length && (pat.ptr[0] == '*' || pat.ptr[0] == '<' || pat.ptr[0] == '>')) pat = pat[1..$];
	return (pat.length == 0);

starCheck:
	if (!star) return false;
	if (str.length) str = str[1..$];
	goto loopStart;
}

/// ditto.
alias globmatchCI = globmatch!false;


void detectUrl (const(char)[] s, usize stpos, scope bool delegate (const(char)[] url, usize spos, usize epos) dg) {
	static bool strCheckPrev (const(char)[] s, usize pos, const(char)[] pfx) {
		if (pfx.length == 0 || s.length == 0) return false;
		if (pos < pfx.length) return false;
		if (pos > s.length) return false;
		pos -= pfx.length;
		foreach (immutable c; 0..pfx.length) {
			import iv.strex : tolower;
			if (s[pos+c].tolower != pfx[c].tolower) return false;
		}
		return true;
	}

	static int urlepos (const(char)[] s, int spos) {
		import iv.strex : indexOf, isalnum;
		if (spos < 0 || spos >= s.length) return -1;
		assert(spos < s.length);
		auto sposx = s.indexOf("://", spos);
		assert(sposx >= 0);
		sposx += 3;
		if (sposx >= s.length) return -1;
		spos = cast(int)sposx;
		// host
		while (spos < s.length) {
			char ch = s[spos];
			if (ch == '/') break;
			if (ch <= ' ') return spos;
			if (ch != '.' && ch != '-' && !ch.isalnum) return spos;
			++spos;
		}
		if (spos >= s.length) return spos;
		// path
		assert(s[spos] == '/');
		char[16] brcmap;
		usize brlevel = 0;
		bool wasSharp = false;
		for (; spos < s.length; ++spos) {
			import iv.strex : isalnum;
			char ch = s[spos];
			//writeln("spos=", spos, "; ch=", ch);
			if (ch <= ' ' || ch == '<' || ch == '>' || ch == '"' || ch == '\'' || ch >= 127) return spos;
			// hash
			if (ch == '#') {
				if (wasSharp) return spos;
				wasSharp = true;
				brlevel = 0;
				continue;
			}
			// path delimiter
			if (ch == '/') {
				brlevel = 0;
				continue;
			}
			// opening bracket
			if (ch == '(' || ch == '{' || ch == '[') {
				if (brlevel >= brcmap.length) return spos; // too nested
				if (s.length-spos < 2) return spos; // no more chars, ignore
				if (!isalnum(s[spos+1]) && s[spos+1] != '_' && s[spos+1] != '%') return spos; // ignore
				// looks like URL part
				final switch (ch) {
					case '(': ch = ')'; break;
					case '[': ch = ']'; break;
					case '{': ch = '}'; break;
				}
				brcmap[brlevel++] = ch;
				continue;
			}
			// closing bracket
			if (ch == ')' || ch == '}' || ch == ']') {
				//writeln("spos=", spos, "; brlevel=", brlevel);
				if (brlevel == 0 || ch != brcmap[brlevel-1]) return spos; // oops
				--brlevel;
				continue;
			}
			// other punctuation
			if (brlevel == 0 && !isalnum(ch) && ch != '_' && ch != '%') {
				// other special chars
				if (s.length-spos < 2) return spos; // no more chars, ignore
				if (!isalnum(s[spos+1]) && s[spos+1] != '_' && s[spos+1] != '%') {
					if (ch == '.' || ch == '!' || ch == ';' || ch == ',' || s[spos+1] != ch) return spos; // ignore
				}
			}
		}
		if (spos >= s.length) spos = cast(int)s.length;
		return spos;
	}


	if (dg is null) return;
	if (stpos >= s.length) return;
	s = s[stpos..$];

	import iv.strex;
	while (s.length) {
		auto np = s.indexOf("://");
		if (np <= 0) break;
		int spos = -1;
				 if (strCheckPrev(s, np, "https")) spos = cast(int)np-5;
		else if (strCheckPrev(s, np, "http")) spos = cast(int)np-4;
		else if (strCheckPrev(s, np, "ftp")) spos = cast(int)np-3;
		if (spos < 0) {
			stpos += np+3;
			s = s[np+3..$];
			continue;
		}
		int epos = urlepos(s, spos);
		if (epos > cast(int)s.length) epos = cast(int)s.length;
		if (epos <= spos) {
			if (np >= s.length) break;
			if (s.length-np < 4) break;
			stpos += np+3;
			s = s[np+3..$];
			continue;
		}
		// get url
		int eepos = epos;
		while (eepos > spos && s[eepos-1] == '.') --eepos;
		if (eepos > spos) {
			const(char)[] url = s[spos..eepos];
			if (!dg(url, stpos+spos, stpos+eepos)) break;
		}
		stpos += epos;
		s = s[epos..$];
	}
}


pure nothrow @system @nogc:
version(linux) {
	extern(C) inout(void)* memmem (inout(void)* haystack, size_t haystacklen, inout(void)* needle, size_t needlelen);
	extern(C) inout(void)* memrchr (inout(void)* s, int ch, size_t slen);
} else {
	inout(void)* memmem (inout(void)* haystack, size_t haystacklen, inout(void)* needle, size_t needlelen) {
		// size_t is unsigned
		if (needlelen > haystacklen || needlelen == 0) return null;
		auto h = cast(const(ubyte)*)haystack;
		auto n = cast(const(ubyte)*)needle;
		foreach (immutable i; 0..haystacklen-needlelen+1) {
			import core.stdc.string : memcmp;
			if (memcmp(h+i, n, needlelen) == 0) return cast(typeof(return))(h+i);
		}
		return null;
	}

	inout(void)* memrchr (inout(void)* haystack, int ch, size_t haystacklen) {
		// size_t is unsigned
		if (haystacklen == 0) return null;
		auto h = cast(const(ubyte)*)haystack;
		ch &= 0xff;
		foreach_reverse (immutable idx, ubyte v; h[0..haystacklen]) {
			if (v == ch) return cast(typeof(return))(h+idx);
		}
		return null;
	}
}

inout(void)* memrmem (inout(void)* haystack, size_t haystacklen, inout(void)* needle, size_t needlelen) {
	if (needlelen > haystacklen) return null;
	auto h = cast(const(ubyte)*)haystack;
	const(ubyte)* res = null;
	// size_t is unsigned
	if (needlelen > haystacklen || needlelen == 0) return null;
	version(none) {
		size_t pos = 0;
		while (pos < haystacklen-needlelen+1) {
			auto ff = memmem(haystack+pos, haystacklen-pos, needle, needlelen);
			if (ff is null) break;
			res = cast(const(ubyte)*)ff;
			pos = cast(size_t)(res-haystack)+1;
		}
		return cast(void*)res;
	} else {
		auto n = cast(const(ubyte)*)needle;
		size_t len = haystacklen-needlelen+1;
		while (len > 0) {
			import core.stdc.string : memcmp;
			auto ff = cast(const(ubyte)*)memrchr(haystack, *n, len);
			if (ff is null) break;
			if (memcmp(ff, needle, needlelen) == 0) return cast(void*)ff;
			//if (ff is h) break;
			len = cast(size_t)(ff-cast(ubyte*)haystack);
		}
		return null;
	}
}
