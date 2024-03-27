/* GML runner
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
module gaem.runner.value;

import gaem.runner.strpool;


// ////////////////////////////////////////////////////////////////////////// //
alias Real = double;


// value manipulation
bool isReal(Real v) nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	return !isNaN(v);
}


bool isString(Real v) nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	return isNaN(v);
}


bool isUndef(Real v) nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	return(isNaN(v) && getNaNPayload(v) < 0);
}


// creates "undefined" value
Real undefValue() nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	return NaN(-666);
}


// for invalid strings it returns 0
int getStrId(Real v) nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	if(isNaN(v)){
		auto res = getNaNPayload(v);
		static if(Real.sizeof == 4){
			return(res < 0 ? 0: cast(int)res);
		}else{
			return(res < 0 || res > int.max ? 0: cast(int)res);
		}
	}else{
		return 0;
	}
}


Real buildStrId(int id) nothrow @safe @nogc{
	pragma(inline, true);
	import std.math;
	static if(Real.sizeof == 4){
		assert(id >= 0 && id <= 0x3F_FFFF);
	}else{
		assert(id >= 0);
	}
	return NaN(id);
}


Real Value() nothrow @safe @nogc{ pragma(inline, true); return undefValue(); }

Real Value(T)(T v){
	pragma(inline, true);
			 static if(is(T: const(char)[])) return buildStrId(newDynStr(v));
	else static if(is(T: Real)) return cast(Real)v;
	else static assert(0, "invalid value type");
}
