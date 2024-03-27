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
module iv.alice;

version(aliced) {} else {

// ////////////////////////////////////////////////////////////////////////// //
static if (!is(typeof(usize))) public alias usize = size_t;
static if (!is(typeof(sptrdiff))) public alias sptrdiff = ptrdiff_t;
static if (!is(typeof(ssizediff))) public alias ssizediff = ptrdiff_t;

static if (!is(typeof(ssize))) {
       static if (usize.sizeof == 8) public alias ssize = long; //k8
  else static if (usize.sizeof == 4) public alias ssize = int; //k8
  else static assert(0, "invalid usize size"); //k8
}


// ////////////////////////////////////////////////////////////////////////// //
static if (!is(typeof(NamedExceptionBase))) {
  mixin template ExceptionCtorMixinTpl() {
    this (string msg, string file=__FILE__, size_t line=__LINE__, Throwable next=null) @safe pure nothrow @nogc {
      super(msg, file, line, next);
    }
  }

  // usage:
  //   mixin(NewExceptionClass!"MyEx");
  //   mixin(NewExceptionClass!("MyEx1", "MyEx"));
  enum NewExceptionClass(string name, string base="Exception") = `class `~name~` : `~base~` { mixin ExceptionCtorMixinTpl; }`;


  mixin(NewExceptionClass!("NamedExceptionBase", "Exception"));
  mixin(NewExceptionClass!("NamedException(string name)", "NamedExceptionBase"));
}


// ////////////////////////////////////////////////////////////////////////// //
template Imp(string mod) { mixin("import Imp="~mod~";"); }
// usage: auto boo(T)(Imp!"std.datetime".SysTime tm) if (Imp!"std.traits".isCallable!T) { return tm; }


// ////////////////////////////////////////////////////////////////////////// //
}


/*
 * Named method and struct literal arguments
 *
 * License:
 *   This Source Code Form is subject to the terms of
 *   the Mozilla Public License, v. 2.0. If a copy of
 *   the MPL was not distributed with this file, You
 *   can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Authors:
 *   Vladimir Panteleev <vladimir@thecybershadow.net>
 */
//module namedargs /*is aliced*/;

//import iv.alice;
//import std.traits;

// Inspired by
// http://forum.dlang.org/post/awjuoemsnmxbfgzhgkgx@forum.dlang.org

// Simulates named arguments for function calls.
// Accepts arguments as lambdas (name => value) on the template parameter list,
// and positional arguments on the runtime parameter list (see examples below).
template args(alias fun, dgs...) if (is(typeof(fun) == function)) {
  auto args(PosArgs...) (auto ref PosArgs posArgs) {
    import std.traits;
    ParameterTypeTuple!fun args;
    enum names = ParameterIdentifierTuple!fun;

    foreach (immutable i, ref arg; posArgs) args[i] = posArgs[i];
    foreach (immutable i, immutable arg; ParameterDefaults!fun) {
      static if (i >= posArgs.length) args[i] = ParameterDefaults!fun[i];
    }

    // anything works here, but use a custom type to avoid user errors
    static struct DummyType {}
    foreach (immutable dg; dgs) {
      alias fun = dg!DummyType;
      static if (is(FunctionTypeOf!fun PT == __parameters)) {
        enum name = __traits(identifier, PT);
        foreach (immutable i, string argName; names) static if (name == argName) args[i] = fun(DummyType.init);
      } else {
        static assert(false, "Failed to extract parameter name from " ~ fun.stringof);
      }
    }
    return fun(args);
  }
}

//
version(iv_named_args_test) unittest {
  static int fun (int a=1, int b=2, int c=3, int d=4, int e=5) {
    return a+b+c+d+e;
  }

  assert(args!(fun) == 15);
  assert(args!(fun, b=>3) == 16);
  assert(args!(fun, b=>3, d=>3) == 15);
  { import core.stdc.stdio; printf("named args test 00 complete.\n"); }
}

// Mixing named and positional arguments
version(iv_named_args_test) unittest {
  static int fun(int a, int b=2, int c=3, int d=4, int e=5) {
    return a+b+c+d+e;
  }

  assert(args!(fun)(1) == 15);
  assert(args!(fun, b=>3)(1) == 16);
  { import core.stdc.stdio; printf("named args test 01 complete.\n"); }
}

// Simulates named arguments for struct literals.
template args(S, dgs...) if (is(S == struct)) {
  @property S args () {
    import std.traits;
    S s;
    // anything works here, but use a custom type to avoid user errors
    static struct DummyType {}
    foreach (immutable dg; dgs) {
      alias fun = dg!DummyType;
      static if (is(FunctionTypeOf!fun PT == __parameters)) {
        enum name = __traits(identifier, PT);
        foreach (immutable i, immutable field; s.tupleof) static if (__traits(identifier, S.tupleof[i]) == name) s.tupleof[i] = fun(DummyType.init);
      } else {
        static assert(false, "Failed to extract parameter name from " ~ fun.stringof);
      }
    }
    return s;
  }
}

version(iv_named_args_test) unittest {
  static struct S {
    int a = 1, b = 2, c = 3, d = 4, e = 5;
    @property int sum () { return a+b+c+d+e; }
  }

  assert(args!(S).sum == 15);
  assert(args!(S, b=>3).sum == 16);
  assert(args!(S, b=>3, d=>3).sum == 15);

  static assert(!is(typeof(args!(S, b=>b))));
  { import core.stdc.stdio; printf("named args test 02 complete.\n"); }
}
