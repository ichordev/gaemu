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
/**
 * wrap any low-level (or high-level) stream into refcounted struct.
 * this struct can be used instead of `std.stdio.File` when you need
 * a concrete type instead of working with generic stream templates.
 * wrapped stream is thread-safe (i.e. reads, writes, etc), but
 * wrapper itself isn't.
 */
module iv.vfs.vfile /*is aliced*/;

//version = vfs_add_std_stdio_wrappers;
//version = vfs_debug_name_set;

private:
static import core.stdc.stdio;
static import core.sys.posix.stdio;
static import core.sys.posix.unistd;
version(vfs_add_std_stdio_wrappers) static import std.stdio;

// we need this to simulate `synchronized`
extern (C) void _d_monitorenter (Object h) nothrow;
extern (C) void _d_monitorexit (Object h) nothrow;

import iv.alice;
import iv.vfs.types : Seek, VFSHiddenPointerHelper;
import iv.vfs.config;
import iv.vfs.error;
import iv.vfs.pred;

version(LDC) {}
else {
	version(vfs_add_std_stdio_wrappers) version = vfs_stdio_wrapper;
}

// uncomment to use zlib instead of internal inflater
//version = vfs_use_zlib_unpacker;

/// mark struct fields with this for VFile.readStruct
public enum IVVFSIgnore;


// ////////////////////////////////////////////////////////////////////////// //
/// wrapper structure for various streams. kinda like `std.stdio.File`,
/// but with less features. not thread-safe for assigns and such, but
/// thread-safe for i/o. i.e. you'd better not share this struct between
/// threads, but can safely use struct copies in different threads.
public struct VFile {
private:
	/*WrappedStreamRC*/usize wstp; // yep, the whole struct size: one pointer

	package @property WrappedStreamRC wst () const pure nothrow @trusted @nogc { pragma(inline, true); return *cast(WrappedStreamRC*)&wstp; }

	static bool doDecRef (WrappedStreamRC st) {
		if (st !is null) {
			debug(vfs_rc) { import core.stdc.stdio : printf; printf("DO DECREF FOR 0x%08x\n", cast(void*)st); }
			if (st.decRef) {
				// free `wst` itself
				import core.memory : GC;
				import core.stdc.stdlib : free;
				debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("REMOVING WRAPPER 0x%08x\n", st); }
				if (st.gcUnregister !is null) {
					debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("CALLING GC CLEANUP DELEGATE FOR WRAPPER 0x%08x\n", st); }
					st.gcUnregister(cast(void*)st);
				}
				free(cast(void*)st);
				return true;
			}
		}
		return false;
	}

	this (void* wptr) { wstp = cast(usize)wptr; }
	//this (usize wptr) { wstp = wptr; }

public:
	this (const VFile fl) {
		wstp = fl.wstp;
		if (wstp) wst.incRef();
	}

	version(vfs_stdio_wrapper)
	this (std.stdio.File fl, const(char)[] fname=null) {
		try {
			wstp = WrapStdioFile(fl, fname);
		} catch (Exception e) {
			// chain exception
			throw new VFSException("can't open file", __FILE__, __LINE__, e);
		}
	}

	/// this will throw if `fl` is `null`; `fl` is (not) owned by VFile now
	this (core.stdc.stdio.FILE* fl, bool own=true) {
		if (fl is null) throw new VFSException("can't open file");
		if (own) wstp = WrapLibcFile!true(fl, null); else wstp = WrapLibcFile!false(fl, null);
	}

	/// this will throw if `fl` is `null`; `fl` is (not) owned by VFile now
	this (core.stdc.stdio.FILE* fl, const(char)[] fname, bool own=true) {
		if (fl is null) throw new VFSException("can't open file");
		if (own) wstp = WrapLibcFile!true(fl, fname); else wstp = WrapLibcFile!false(fl, fname);
	}

	/// this will throw if `fl` is `null`; `fl` is (not) owned by VFile now
	static import etc.c.zlib;
	package(iv.vfs) static VFile OpenGZ (etc.c.zlib.gzFile fl, bool own=true) {
		if (fl is null) throw new VFSException("can't open file");
		VFile fres;
		if (own) fres.wstp = WrapGZFile!true(fl, null); else fres.wstp = WrapGZFile!false(fl, null);
		return fres;
	}

	/// this will throw if `fl` is `null`; `fl` is (not) owned by VFile now
	package(iv.vfs) static VFile OpenGZ (etc.c.zlib.gzFile fl, const(char)[] afname, bool own=true) {
		if (fl is null) throw new VFSException("can't open file");
		VFile fres;
		if (own) fres.wstp = WrapGZFile!true(fl, afname); else fres.wstp = WrapGZFile!false(fl, afname);
		return fres;
	}

	/// wrap file descriptor; `fd` is owned by VFile now; can throw
	static if (VFS_NORMAL_OS) this (int fd, bool own=true) {
		if (fd < 0) throw new VFSException("can't open file");
		if (own) wstp = WrapFD!true(fd, null); else wstp = WrapFD!true(fd, null);
	}

	/// wrap file descriptor; `fd` is owned by VFile now; can throw
	static if (VFS_NORMAL_OS) this (int fd, const(char)[] fname, bool own=true) {
		if (fd < 0) throw new VFSException("can't open file");
		if (own) wstp = WrapFD!true(fd, fname); else wstp = WrapFD!true(fd, fname);
	}

	/// open named file with VFS engine; start with "/" or "./" to use only disk files
	this(T:const(char)[]) (T fname, const(char)[] mode=null) {
		import iv.vfs.main : vfsOpenFile;
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("CTOR:STR(%.*s)\n", cast(uint)fname.length, fname.ptr); }
		auto fl = vfsOpenFile(fname, mode);
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("CTOR(0x%08x)\n", cast(void*)fl.wstp); }
		wstp = fl.wstp;
		fl.wstp = 0;
		//this = fl;
	}

	this (this) nothrow @trusted @nogc {
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("POSTBLIT(0x%08x)\n", cast(void*)wstp); }
		debug(vfs_rc_trace) {
			try { throw new Exception("stack trace"); } catch (Exception e) { import core.stdc.stdio; string es = e.toString; printf("*** %.*s", cast(uint)es.length, es.ptr); }
		}
		if (wst !is null) wst.incRef();
	}

	// WARNING: dtor hides exceptions!
	~this () nothrow {
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("DTOR(0x%08x)\n", cast(void*)wstp); }
		debug(vfs_rc_trace) {
			try { throw new Exception("stack trace"); } catch (Exception e) { import core.stdc.stdio; string es = e.toString; printf("*** %.*s", cast(uint)es.length, es.ptr); }
		}
		try {
			doDecRef(wst);
		} catch (Exception e) {
		}
	}

	@property bool opCast(T) () const nothrow @safe @nogc if (is(T == bool)) { return this.isOpen; }

	@property const(char)[] name () const nothrow @safe @nogc {
		if (!wstp) return null;
		version(aliced) {
			synchronized(wst) return wst.name;
		} else {
			// vanilla sux!
			return wst.name;
		}
	}

	@property bool isOpen () const nothrow @safe @nogc {
		if (!wstp) return false;
		version(aliced) {
			synchronized(wst) return wst.isOpen;
		} else {
			// vanilla sux!
			return wst.isOpen;
		}
	}

	void close () {
		try {
			auto oldo = wst;
			wstp = 0;
			doDecRef(oldo);
		} catch (Exception e) {
			// chain exception
			throw new VFSException("read error", __FILE__, __LINE__, e);
		}
	}

	@property bool eof () { return (!wstp || wst.eof); }

	T[] rawRead(T) (T[] buf) if (!is(T == const) && !is(T == immutable)) {
		if (!isOpen) throw new VFSException("can't read from closed stream");
		if (buf.length > 0) {
			ssize res;
			try {
				synchronized(wst) res = wst.read(buf.ptr, buf.length*T.sizeof);
			} catch (Exception e) {
				// chain exception
				throw new VFSException("read error", __FILE__, __LINE__, e);
			}
			if (res == -1 || res%T.sizeof != 0) throw new VFSException("read error");
			return buf[0..res/T.sizeof];
		} else {
			return buf[0..0];
		}
	}

	private T[] rawReadNoLock(T) (T[] buf) if (!is(T == const) && !is(T == immutable)) {
		if (!isOpen) throw new VFSException("can't read from closed stream");
		if (buf.length > 0) {
			ssize res;
			try {
				res = wst.read(buf.ptr, buf.length*T.sizeof);
			} catch (Exception e) {
				// chain exception
				throw new VFSException("read error", __FILE__, __LINE__, e);
			}
			if (res == -1 || res%T.sizeof != 0) throw new VFSException("read error");
			return buf[0..res/T.sizeof];
		} else {
			return buf[0..0];
		}
	}

	/// read exact size or throw error
	T[] rawReadExact(T) (T[] buf) if (!is(T == const) && !is(T == immutable)) {
		if (buf.length == 0) return buf;
		auto left = buf.length*T.sizeof;
		auto dp = cast(ubyte*)buf.ptr;
		synchronized(wst) {
			try {
				while (left > 0) {
					ssize res = wst.read(dp, left);
					if (res <= 0) throw new VFSException("read error");
					dp += res;
					left -= res;
				}
			} catch (Exception e) {
				// chain exception
				throw new VFSException("read error", __FILE__, __LINE__, e);
			}
		}
		return buf;
	}

	private T[] rawReadExactNoLock(T) (T[] buf) if (!is(T == const) && !is(T == immutable)) {
		if (buf.length == 0) return buf;
		auto left = buf.length*T.sizeof;
		auto dp = cast(ubyte*)buf.ptr;
		try {
			while (left > 0) {
				ssize res = wst.read(dp, left);
				if (res <= 0) throw new VFSException("read error");
				dp += res;
				left -= res;
			}
		} catch (Exception e) {
			// chain exception
			throw new VFSException("read error", __FILE__, __LINE__, e);
		}
		return buf;
	}

	void rawWrite(T) (in T[] buf) {
		if (!isOpen) throw new VFSException("can't write to closed stream");
		if (buf.length > 0) {
			ssize res;
			try {
				synchronized(wst) res = wst.write(buf.ptr, buf.length*T.sizeof);
			} catch (Exception e) {
				// chain exception
				throw new VFSException("read error", __FILE__, __LINE__, e);
			}
			if (res == -1 || res%T.sizeof != 0) throw new VFSException("write error");
		}
	}

	private void rawWriteNoLock(T) (in T[] buf) {
		if (!isOpen) throw new VFSException("can't write to closed stream");
		if (buf.length > 0) {
			ssize res;
			try {
				res = wst.write(buf.ptr, buf.length*T.sizeof);
			} catch (Exception e) {
				// chain exception
				throw new VFSException("read error", __FILE__, __LINE__, e);
			}
			if (res == -1 || res%T.sizeof != 0) throw new VFSException("write error");
		}
	}

	alias rawWriteExact = rawWrite; // for convenience

	long seek (long offset, int origin=Seek.Set) {
		if (!isOpen) throw new VFSException("can't seek in closed stream");
		long p;
		try {
			synchronized(wst) p = wst.lseek(offset, origin);
		} catch (Exception e) {
			// chain exception
			throw new VFSException("seek error", __FILE__, __LINE__, e);
		}
		if (p == -1) throw new VFSException("seek error");
		return p;
	}

	@property long tell () {
		if (!isOpen) throw new VFSException("can't get position in closed stream");
		long p;
		try {
			synchronized(wst) p = wst.lseek(0, Seek.Cur);
		} catch (Exception e) {
			// chain exception
			throw new VFSException("tell error", __FILE__, __LINE__, e);
		}
		if (p == -1) throw new VFSException("tell error");
		return p;
	}

	@property bool hasSize () {
		if (!isOpen) throw new VFSException("can't query closed stream");
		synchronized(wst) return wst.hasSize;
	}

	@property long size () {
		if (!isOpen) throw new VFSException("can't get size of closed stream");
		bool noChain = false;
		long p;
		try {
			synchronized(wst) {
				if (wst.hasSize) {
					p = wst.getsize;
					if (p == -1) { noChain = true; throw new VFSException("size error"); }
				} else {
					auto opos = wst.lseek(0, Seek.Cur);
					if (opos == -1) { noChain = true; throw new VFSException("size error"); }
					p = wst.lseek(0, Seek.End);
					if (p == -1) { noChain = true; throw new VFSException("size error"); }
					if (wst.lseek(opos, Seek.Set) == -1) { noChain = true; throw new VFSException("size error"); }
				}
			}
		} catch (Exception e) {
			// chain exception
			if (noChain) throw e;
			throw new VFSException("size error", __FILE__, __LINE__, e);
		}
		return p;
	}

	void flush () {
		if (!isOpen) throw new VFSException("can't get size of closed stream");
		bool noChain = false;
		try {
			if (!wst.flush) {
				noChain = true;
				throw new VFSException("flush error");
			}
		} catch (Exception e) {
			// chain exception
			if (noChain) throw e;
			throw new VFSException("flush error", __FILE__, __LINE__, e);
		}
	}

	void opAssign (VFile src) nothrow {
		if (!wstp && !src.wstp) return;
		try {
			debug(vfs_rc) { import core.stdc.stdio : printf; printf("***OPASSIGN(0x%08x -> 0x%08x)\n", cast(void*)src.wstp, cast(void*)wstp); }
			if (wstp) {
				// assigning to opened stream
				if (src.wstp) {
					// both streams are active
					if (wstp == src.wstp) return; // nothing to do
					auto oldo = wst;
					auto newo = src.wst;
					newo.incRef();
					// replace stream object
					wstp = src.wstp;
					// release old stream
					doDecRef(oldo);
				} else {
					// just close this one
					auto oldo = wst;
					wstp = 0;
					doDecRef(oldo);
				}
			} else if (src.wstp) {
				// this stream is closed, but other is active; easy deal
				wstp = src.wstp;
				wst.incRef();
			}
		} catch (Exception e) {
			// chain exception
			//throw new VFSException("read error", __FILE__, __LINE__, e);
		}
	}

	usize toHash () const pure nothrow @safe @nogc { return wstp; } // yeah, so simple
	bool opEquals() (auto ref VFile s) const { return (wstp == s.wstp); }

	// make this output stream
	void put (const(char)[] s...) { rawWrite(s); }
	//void put (const(wchar)[] s...) { rawWrite(s); }
	//void put (const(dchar)[] s...) { rawWrite(s); }

	static struct LockedWriterImpl {
		private VFile fl;

		private this (VFile afl) nothrow {
			if (afl.wstp) {
				import core.atomic;
				fl = afl;
				if (atomicOp!"+="(fl.wst.wrrc, 1) == 1) {
					//{ import core.stdc.stdio; printf("LockedWriterImpl(0x%08x): lock!\n", cast(uint)fl.wstp); }
					_d_monitorenter(fl.wst); // emulate `synchronized(fl.wst)` enter
				}
			}
		}

		this (this) {
			if (fl.wstp) {
				import core.atomic;
				atomicOp!"+="(fl.wst.wrrc, 1);
			}
		}

		~this () {
			if (fl.wstp) {
				import core.atomic;
				if (atomicOp!"-="(fl.wst.wrrc, 1) == 0) {
					//{ import core.stdc.stdio; printf("LockedWriterImpl(0x%08x): unlock!\n", cast(uint)fl.wstp); }
					_d_monitorexit(fl.wst); // emulate `synchronized(fl.wst)` exit
				}
				fl = VFile.init; // just in case
			}
		}

		void put (const(char)[] s...) { fl.rawWriteNoLock(s); }
	}

	@property LockedWriterImpl lockedWriter () { return LockedWriterImpl(this); }

	static struct UnlockedWriterImpl {
		private VFile fl;

		private this (VFile afl) nothrow {
			if (afl.wstp) fl = afl;
		}

		~this () { fl = VFile.init; /* just in case */ }

		void put (const(char)[] s...) { fl.rawWriteNoLock(s); }
	}

	@property UnlockedWriterImpl unlockedWriter () { return UnlockedWriterImpl(this); }

	// stream i/o functions
	version(LittleEndian) {
		private enum MyEHi = "LE";
		private enum MyELo = "le";
		private enum ItEHi = "BE";
		private enum ItELo = "be";
	} else {
		private enum MyEHi = "BE";
		private enum MyELo = "be";
		private enum ItEHi = "LE";
		private enum ItELo = "le";
	}

	public enum MyEndianness = MyEHi;

	// ////////////////////////////////////////////////////////////////////// //
	/// write integer value of the given type, with the given endianness (default: little-endian)
	/// usage: st.writeNum!ubyte(10)
	void writeNum(T, string es="LE") (T n) if (__traits(isIntegral, T)) {
		static assert(T.sizeof <= 8); // just in case
		static if (es == MyEHi || es == MyELo || T.sizeof == 1) {
			rawWrite((&n)[0..1]);
		} else static if (es == ItEHi || es == ItELo) {
			ubyte[T.sizeof] b = void;
			version(LittleEndian) {
				// convert to big-endian
				foreach_reverse (ref x; b) { x = n&0xff; n >>= 8; }
			} else {
				// convert to little-endian
				foreach (ref x; b) { x = n&0xff; n >>= 8; }
			}
			rawWrite(b[]);
		} else {
			static assert(0, "invalid endianness: '"~es~"'");
		}
	}

	/// read integer value of the given type, with the given endianness (default: little-endian)
	/// usage: auto v = st.readNum!ubyte
	T readNum(T, string es="LE") () if (__traits(isIntegral, T)) {
		static assert(T.sizeof <= 8); // just in case
		static if (es == MyEHi || es == MyELo || T.sizeof == 1) {
			T v = void;
			rawReadExact((&v)[0..1]);
			return v;
		} else static if (es == ItEHi || es == ItELo) {
			ubyte[T.sizeof] b = void;
			rawReadExact(b[]);
			T v = 0;
			version(LittleEndian) {
				// convert from big-endian
				foreach (ubyte x; b) { v <<= 8; v |= x; }
			} else {
				// conver from little-endian
				foreach_reverse (ubyte x; b) { v <<= 8; v |= x; }
			}
			return v;
		} else {
			static assert(0, "invalid endianness: '"~es~"'");
		}
	}

	private enum reverseBytesMixin = "
		foreach (idx; 0..b.length/2) {
			ubyte t = b[idx];
			b[idx] = b[$-idx-1];
			b[$-idx-1] = t;
		}
	";

	/// write floating value of the given type, with the given endianness (default: little-endian)
	/// usage: st.writeNum!float(10)
	void writeNum(T, string es="LE") (T n) if (__traits(isFloating, T)) {
		static assert(T.sizeof <= 8); // just in case
		static if (es == MyEHi || es == MyELo) {
			rawWrite((&n)[0..1]);
		} else static if (es == ItEHi || es == ItELo) {
			import core.stdc.string : memcpy;
			ubyte[T.sizeof] b = void;
			memcpy(b.ptr, &v, T.sizeof);
			mixin(reverseBytesMixin);
			rawWrite(b[]);
		} else {
			static assert(0, "invalid endianness: '"~es~"'");
		}
	}

	/// read floating value of the given type, with the given endianness (default: little-endian)
	/// usage: auto v = st.readNum!float
	T readNum(T, string es="LE") () if (__traits(isFloating, T)) {
		static assert(T.sizeof <= 8); // just in case
		T v = void;
		static if (es == MyEHi || es == MyELo) {
			rawReadExact((&v)[0..1]);
		} else static if (es == ItEHi || es == ItELo) {
			import core.stdc.string : memcpy;
			ubyte[T.sizeof] b = void;
			rawReadExact(b[]);
			mixin(reverseBytesMixin);
			memcpy(&v, b.ptr, T.sizeof);
		} else {
			static assert(0, "invalid endianness: '"~es~"'");
		}
		return v;
	}

	/// write btc-style integer
	void writeVarUNum(T, string es="LE") (T n) if (__traits(isIntegral, T) && __traits(isUnsigned, T)) {
		static assert(T.sizeof <= 8); // just in case
		if (n < 253) {
			writeNum!(ubyte, es)(cast(ubyte)n);
		} else if (n <= 255) {
			writeNum!(ubyte, es)(253);
			writeNum!(ushort, es)(cast(ushort)n);
		} else {
			// ushort?
			static if (T.sizeof == 2) {
				writeNum!(ubyte, es)(253);
				writeNum!(ushort, es)(cast(ushort)n);
			} else {
				// fits into ushort?
				if (n <= ushort.max) {
					writeNum!(ubyte, es)(253);
					writeNum!(ushort, es)(cast(ushort)n);
				} else {
					// uint?
					static if (T.sizeof == 4) {
						writeNum!(ubyte, es)(254);
						writeNum!(uint, es)(cast(uint)n);
					} else {
						// fits into uint?
						if (n <= uint.max) {
							writeNum!(ubyte, es)(254);
							writeNum!(uint, es)(cast(uint)n);
						} else {
							// ulong
							writeNum!(ubyte, es)(255);
							writeNum!(ulong, es)(cast(ulong)n);
						}
					}
				}
			}
		}
	}

	/// read btc-style integer
	T readVarUNum(T, string es="LE") () if (__traits(isIntegral, T) && __traits(isUnsigned, T)) {
		ubyte b = readNum!(ubyte, es);
		if (b < 253) return cast(T)b;
		switch (b) {
			case 253: // ushort
				ushort v = readNum!(ushort, es);
				if (v < 253) throw new VFSException("invalid varnum");
				// check overflow
				static if (T.sizeof == 1) {
					import std.conv : ConvOverflowException;
					if (v > ubyte.max) throw new ConvOverflowException("varnum overflow");
				}
				return cast(T)v;
			case 254: // uint
				uint v = readNum!(uint, es);
				if (v < 253) throw new VFSException("invalid varnum");
				// check overflow
				static if (T.sizeof < uint.sizeof) {
					import std.conv : ConvOverflowException;
					if (v > T.max) throw new ConvOverflowException("varnum overflow");
				}
				return cast(T)v;
			case 255: // ulong
				ulong v = readNum!(ulong, es);
				if (v < 253) throw new VFSException("invalid varnum");
				// check overflow
				static if (T.sizeof < ulong.sizeof) {
					import std.conv : ConvOverflowException;
					if (v > T.max) throw new ConvOverflowException("varnum overflow");
				}
				return cast(T)v;
			default: break;
		}
		return cast(T)b;
	}


	// ////////////////////////////////////////////////////////////////////////// //
	// first byte: bit 7 is sign; bit 6 is "has more bytes" mark; bits 0..5: first number bits
	// next bytes: bit 7 is "has more bytes" mark; bits 0..6: next number bits
	void writeXInt(T:ulong) (T vv) {
		ubyte[16] buf = void; // actually, 10 is enough ;-)
				 static if (T.sizeof == ulong.sizeof) ulong v = cast(ulong)vv;
		else static if (!__traits(isUnsigned, T)) ulong v = cast(ulong)cast(long)vv; // extend sign bits
		else ulong v = cast(ulong)vv;
		uint len = 1; // at least
		// now write as signed
		if (v == 0x8000_0000_0000_0000UL) {
			// special (negative zero)
			buf.ptr[0] = 0x80;
		} else {
			if (v&0x8000_0000_0000_0000UL) {
				v = (v^~0uL)+1; // negate v
				buf.ptr[0] = 0x80; // sign bit
			} else {
				buf.ptr[0] = 0;
			}
			buf.ptr[0] |= v&0x3f;
			v >>= 6;
			if (v != 0) buf.ptr[0] |= 0x40; // has more
			while (v != 0) {
				buf.ptr[len] = v&0x7f;
				v >>= 7;
				if (v > 0) buf.ptr[len] |= 0x80; // has more
				++len;
			}
		}
		rawWrite(buf.ptr[0..len]);
	}

	T readXInt(T:ulong) () {
		import std.conv : ConvOverflowException;
		ulong v = 0;
		ubyte c = void;
		// first byte contains sign flag
		rawReadExact((&c)[0..1]);
		if (c == 0x80) {
			// special (negative zero)
			v = 0x8000_0000_0000_0000UL;
		} else {
			bool neg = ((c&0x80) != 0);
			v = c&0x3f;
			c <<= 1;
			// 63/7 == 9, so we can shift at most 56==(7*8) bits
			ubyte shift = 6;
			while (c&0x80) {
				if (shift > 62) throw new ConvOverflowException("readXInt overflow");
				rawReadExact((&c)[0..1]);
				ulong n = c&0x7f;
				if (shift == 62 && n > 1) throw new ConvOverflowException("readXInt overflow");
				n <<= shift;
				v |= n;
				shift += 7;
			}
			if (neg) v = (v^~0uL)+1; // negate v
		}
		// now convert to output
		static if (T.sizeof == v.sizeof) {
			return v;
		} else static if (!__traits(isUnsigned, T)) {
			auto l = cast(long)v;
			if (v < T.min) throw new ConvOverflowException("readXInt underflow");
			if (v > T.max) throw new ConvOverflowException("readXInt overflow");
			return cast(T)l;
		} else {
			if (v > T.max) throw new ConvOverflowException("readXInt overflow");
			return cast(T)v;
		}
	}

	// ////////////////////////////////////////////////////////////////////// //
	void readStruct(string es="LE", SS) (ref SS st) if (is(SS == struct)) {
		void unserData(T) (ref T v) {
			import std.traits : Unqual;
			alias UT = Unqual!T;
			static if (is(T : V[], V)) {
				// array
				static if (__traits(isStaticArray, T)) {
					foreach (ref it; v) unserData(it);
				} else static if (is(UT == char)) {
					// special case: dynamic `char[]` array will be loaded as asciiz string
					char c;
					for (;;) {
						if (rawRead((&c)[0..1]).length == 0) break; // don't require trailing zero on eof
						if (c == 0) break;
						v ~= c;
					}
				} else {
					assert(0, "cannot load dynamic arrays yet");
				}
			} else static if (is(T : V[K], K, V)) {
				assert(0, "cannot load associative arrays yet");
			} else static if (__traits(isIntegral, UT) || __traits(isFloating, UT)) {
				// this takes care of `*char` and `bool` too
				v = cast(UT)readNum!(UT, es);
			} else static if (is(T == struct)) {
				// struct
				import std.traits : FieldNameTuple, hasUDA;
				foreach (string fldname; FieldNameTuple!T) {
					static if (!hasUDA!(__traits(getMember, T, fldname), IVVFSIgnore)) {
						unserData(__traits(getMember, v, fldname));
					}
				}
			}
		}

		unserData(st);
	}
}


// ////////////////////////////////////////////////////////////////////////// //
// base refcounted class for wrapped stream
package class WrappedStreamRC {
protected:
	shared uint rc = 1;
	shared uint wrrc = 0; // locked writer rc
	bool eofhit;
	//string fname;
	char[512] fnamebuf=0;
	usize fnameptr;
	usize fnamelen;

	this (const(char)[] aname) nothrow @trusted @nogc { setFileName(aname); }

	final void setFileName (const(char)[] aname) nothrow @trusted @nogc {
		if (aname.length) {
			if (aname.length <= fnamebuf.length) {
				if (fnameptr) { import core.stdc.stdlib : free; free(cast(void*)fnameptr); fnameptr = 0; }
				fnamebuf[0..aname.length] = aname;
				fnamelen = aname.length;
				version(Windows) foreach (ref char ch; fnamebuf[0..fnamelen]) if (ch == '\\') ch = '/';
			} else {
				import core.stdc.stdlib : realloc;
				auto nb = cast(char*)realloc(cast(void*)fnameptr, aname.length);
				if (nb !is null) {
					nb[0..aname.length] = aname[];
					fnameptr = cast(usize)nb;
					fnamelen = aname.length;
					version(Windows) foreach (ref char ch; nb[0..fnamelen]) if (ch == '\\') ch = '/';
				} else {
					fnamelen = 0;
				}
			}
		} else {
			if (fnameptr) { import core.stdc.stdlib : free; free(cast(void*)fnameptr); fnameptr = 0; }
			fnamelen = 0;
		}
		version(vfs_debug_name_set) {
			import core.stdc.stdio;
			stderr.fprintf("WrappedStreamRC: setFileName: aname");
			if (aname is null) stderr.fprintf(" IS NULL"); else stderr.fprintf("=<%.*s>", cast(uint)aname.length, aname.ptr);
			stderr.fprintf("; set name=<%.*s>\n", cast(uint)name.length, name.ptr);
		}
	}

	// this shouldn't be called, ever
	~this () nothrow @trusted {
		assert(0); // why we are here?!
		//if (gcUnregister !is null) gcUnregister(cast(void*)this);
	}

	final void incRef () nothrow @trusted @nogc {
		import core.atomic;
		if (atomicOp!"+="(rc, 1) == 0) assert(0); // hey, this is definitely a bug!
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("INCREF(0x%08x): %u (was %u)...\n", cast(void*)this, rc, rc-1); }
	}

	// return true if this class is dead
	final bool decRef () {
		// no need to protect this code with `synchronized`, as only one thread can reach zero rc anyway
		import core.atomic;
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("DECREF(0x%08x): %u (will be %u)...\n", cast(void*)this, rc, rc-1); }
		auto xrc = atomicOp!"-="(rc, 1);
		debug(vfs_rc) { import core.stdc.stdio : printf; printf("  DECREF(0x%08x): %u %u...\n", cast(void*)this, rc, xrc); }
		if (xrc == rc.max) assert(0); // hey, this is definitely a bug!
		if (xrc == 0) {
			import core.memory : GC;
			import core.stdc.stdlib : free;
			synchronized(this) { setFileName(null); close(); } // finalize stream; should be synchronized right here
			return true;
		} else {
			return false;
		}
	}

protected:
	void function (void* self) nothrow gcUnregister;

protected:
	final bool hasName () const pure nothrow @safe @nogc { return (fnamelen != 0); }
	@property const(char)[] name () const nothrow @trusted @nogc { return (fnamelen ? (fnameptr ? (cast(const(char)*)fnameptr)[0..fnamelen] : fnamebuf.ptr[0..fnamelen]) : ""); }
	@property bool eof () { return eofhit; }
	abstract @property bool isOpen () const nothrow @safe @nogc;
	abstract void close ();
	ssize read (void* buf, usize count) { return -1; }
	ssize write (in void* buf, usize count) { return -1; }
	long lseek (long offset, int origin) { return -1; }
	// override this if your stream has `flush()`
	bool flush () { return true; }
	// override this if your stream has dedicated `size`
	@property bool hasSize () { return false; }
	long getsize () { return -1; } // so it won't conflict with `iv.pred.size`
}


// ////////////////////////////////////////////////////////////////////////// //
usize newWS (CT, A...) (A args) if (is(CT : WrappedStreamRC)) {
	import core.exception : onOutOfMemoryErrorNoGC;
	import core.memory : GC;
	import core.stdc.stdlib : malloc;
	import core.stdc.string : memset;
	import std.conv : emplace;
	enum instSize = __traits(classInstanceSize, CT);
	// let's hope that malloc() aligns returned memory right
	auto mem = malloc(instSize);
	if (mem is null) onOutOfMemoryErrorNoGC(); // oops
	memset(mem, 0, instSize);
	emplace!CT(mem[0..instSize], args);
	bool createUnregister = false;
	{
		debug(vfs_vfile_gc) import core.stdc.stdio : printf;
		auto pbm = __traits(getPointerBitmap, CT);
		debug(vfs_vfile_gc) printf("[%.*s]: size=%u (%u) (%u)\n", cast(uint)CT.stringof.length, CT.stringof.ptr, cast(uint)pbm[0], cast(uint)instSize, cast(uint)(pbm[0]/usize.sizeof));
		immutable(ubyte)* p = cast(immutable(ubyte)*)(pbm.ptr+1);
		usize bitnum = 0;
		immutable end = pbm[0]/usize.sizeof;
		while (bitnum < end) {
			if (p[bitnum/8]&(1U<<(bitnum%8))) {
				usize len = 1;
				while (bitnum+len < end && (p[(bitnum+len)/8]&(1U<<((bitnum+len)%8))) != 0) ++len;
				debug(vfs_vfile_gc) printf("  #%u (%u)\n", cast(uint)(bitnum*usize.sizeof), cast(uint)len);
				GC.addRange((cast(usize*)mem)+bitnum, usize.sizeof*len);
				createUnregister = true;
				bitnum += len;
			} else {
				++bitnum;
			}
		}
	}
	if (createUnregister) {
		debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("REGISTERING CG CLEANUP DELEGATE FOR WRAPPER 0x%08x\n", cast(uint)mem); }
		(*cast(CT*)&mem).gcUnregister = function (void* self) {
			debug(vfs_vfile_gc) import core.stdc.stdio : printf;
			debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("DESTROYING WRAPPER 0x%08x\n", cast(uint)self); }
			auto pbm = __traits(getPointerBitmap, CT);
			debug(vfs_vfile_gc) printf("[%.*s]: size=%u (%u) (%u)\n", cast(uint)CT.stringof.length, CT.stringof.ptr, cast(uint)pbm[0], cast(uint)instSize, cast(uint)(pbm[0]/usize.sizeof));
			immutable(ubyte)* p = cast(immutable(ubyte)*)(pbm.ptr+1);
			usize bitnum = 0;
			immutable end = pbm[0]/usize.sizeof;
			while (bitnum < end) {
				if (p[bitnum/8]&(1U<<(bitnum%8))) {
					usize len = 1;
					while (bitnum+len < end && (p[(bitnum+len)/8]&(1U<<((bitnum+len)%8))) != 0) ++len;
					debug(vfs_vfile_gc) printf("  #%u (%u)\n", cast(uint)(bitnum*usize.sizeof), cast(uint)len);
					GC.removeRange((cast(usize*)self)+bitnum);
					bitnum += len;
				} else {
					++bitnum;
				}
			}
		};
	}
	debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("CREATED WRAPPER 0x%08x\n", mem); }
	return cast(usize)mem;
}


// ////////////////////////////////////////////////////////////////////////// //
version(VFS_NORMAL_OS) enum VFSSigRepeatCount = 2;

// ////////////////////////////////////////////////////////////////////////// //
version(vfs_stdio_wrapper)
final class WrappedStreamStdioFile : WrappedStreamRC {
private:
	std.stdio.File fl;

	public this (std.stdio.File afl, const(char)[] afname) { fl = afl; super(afname); } // fuck! emplace needs it

protected:
	override @property const(char)[] name () { return (hasName ? super.name : fl.name); }
	override @property bool isOpen () const nothrow @safe @nogc { return fl.isOpen; }
	override @property bool eof () { return fl.eof; }

	override void close () { if (fl.isOpen) fl.close(); }

	override ssize read (void* buf, usize count) {
		if (count == 0) return 0;
		return fl.rawRead(buf[0..count]).length;
	}

	override ssize write (in void* buf, usize count) {
		if (count == 0) return 0;
		fl.rawWrite(buf[0..count]);
		return count;
	}

	override long lseek (long offset, int origin) { fl.seek(offset, origin); return fl.tell; }

	override bool flush () { fl.flush(); return true; }

	override @property bool hasSize () { return true; }
	long getsize () { return fl.size; }
}


version(vfs_stdio_wrapper)
usize WrapStdioFile (std.stdio.File fl, const(char)[] fname=null) {
	return newWS!WrappedStreamStdioFile(fl, fname);
}


// ////////////////////////////////////////////////////////////////////////// //
private import core.stdc.errno;

final class WrappedStreamLibcFile(bool ownfl=true) : WrappedStreamRC {
private:
	//core.stdc.stdio.FILE* fl;
	usize flp; // hide from GC
	final @property core.stdc.stdio.FILE* fl () const pure nothrow @trusted @nogc { return cast(core.stdc.stdio.FILE*)flp; }
	final @property void fl (core.stdc.stdio.FILE* afl) pure nothrow @trusted @nogc { flp = cast(usize)afl; }

	public this (core.stdc.stdio.FILE* afl, const(char)[] afname) { fl = afl; super(afname); } // fuck! emplace needs it

protected:
	override @property bool isOpen () const nothrow @safe @nogc { return (flp != 0); }
	override @property bool eof () { return (flp == 0 || core.stdc.stdio.feof(fl) != 0); }

	override void close () {
		if (fl !is null) {
			static if (ownfl) {
				import std.exception : ErrnoException;
				auto res = core.stdc.stdio.fclose(fl);
				fl = null;
				if (res != 0) throw new ErrnoException("can't close file", __FILE__, __LINE__);
			} else {
				fl = null;
			}
		}
	}

	override ssize read (void* buf, usize count) {
		if (fl is null || core.stdc.stdio.ferror(fl)) return -1;
		if (count == 0) return 0;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = core.stdc.stdio.fread(buf, 1, count, fl);
			if (res == 0) return (core.stdc.stdio.ferror(fl) ? -1 : 0);
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { core.stdc.stdio.clearerr(fl); continue; } }
				}
			}
			return res;
		}
	}

	override ssize write (in void* buf, usize count) {
		if (fl is null || core.stdc.stdio.ferror(fl)) return -1;
		if (count == 0) return 0;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = core.stdc.stdio.fwrite(buf, 1, count, fl);
			if (res == 0) return (core.stdc.stdio.ferror(fl) ? -1 : 0);
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { core.stdc.stdio.clearerr(fl); continue; } }
				}
			}
			return res;
		}
	}

	override long lseek (long offset, int origin) {
		if (fl is null) return -1;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			static if (VFS_NORMAL_OS) {
				auto res = core.sys.posix.stdio.fseeko(fl, offset, origin);
			} else {
				// windoze sux
				if (offset < int.min || offset > int.max) return -1;
				auto res = core.stdc.stdio.fseek(fl, cast(int)offset, origin);
			}
			if (res != -1) {
				core.stdc.stdio.clearerr(fl);
				break;
			} else {
				version(VFS_NORMAL_OS) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { core.stdc.stdio.clearerr(fl); continue; } }
				}
				return res;
			}
		}
		static if (VFS_NORMAL_OS) {
			return core.sys.posix.stdio.ftello(fl);
		} else {
			return core.stdc.stdio.ftell(fl);
		}
	}

	override bool flush () {
		if (fl is null) return false;
		if (core.stdc.stdio.fflush(fl) == 0) return true;
		// check for special file
		import core.stdc.errno;
		if (errno == EROFS || errno == EINVAL) return true; // this is pipe, fifo, socket, etc., assume success
		return false;
	}
}


usize WrapLibcFile(bool ownfl=true) (core.stdc.stdio.FILE* fl, const(char)[] fname=null) {
	return newWS!(WrappedStreamLibcFile!ownfl)(fl, fname);
}


// ////////////////////////////////////////////////////////////////////////// //
final class WrappedStreamGZFile(bool ownfl=true) : WrappedStreamRC {
private import etc.c.zlib;
private:
	usize flp; // hide from GC
	final @property gzFile fl () const pure nothrow @trusted @nogc { return cast(gzFile)flp; }
	final @property void fl (gzFile afl) pure nothrow @trusted @nogc { flp = cast(usize)afl; }

	ulong cachedSize = ulong.max;
	ulong newpos = 0;

	int err () nothrow @trusted {
		int res = 0;
		if (flp != 0) gzerror(fl, &res);
		return res;
	}

	public this (gzFile afl, const(char)[] afname) { fl = afl; super(afname); } // fuck! emplace needs it

	// this shit tries to workaround "convenient features" of gzio
	// i really should rewrite the whole gz stuff and got rid of zlib
	bool fixPosition () {
		if (newpos >= int.max) return false; // alas
		auto cpos = gztell(fl);
		if (cpos == -1) return false; // something is VERY wrong
		if (cpos == newpos) return true;
		// position changed
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		if (newpos < cpos) {
			// have to rewind
			for (;;) {
				if (gzrewind(fl) < 0) {
					version(VFS_NORMAL_OS) {
						import core.stdc.errno;
						if (errno == EINTR) { if (sigsleft-- > 0) { gzclearerr(fl); continue; } }
					}
					return false;
				} else {
					break;
				}
			}
			cpos = gztell(fl);
			if (cpos == -1) return false; // something is VERY wrong
		}
		if (newpos < cpos) return false; // why?!
		version(VFS_NORMAL_OS) sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = gzseek(fl, cast(int)newpos, 0); // fuck you, phobos!
			if (res != -1) {
				gzclearerr(fl);
			} else {
				version(VFS_NORMAL_OS) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { gzclearerr(fl); continue; } }
				}
				return false;
			}
			break;
		}
		if (gztell(fl) != newpos) return false; // something is VERY wrong
		return true;
	}

protected:
	override @property bool isOpen () const nothrow @safe @nogc { return (flp != 0); }
	override @property bool eof () { return (flp == 0 || gzeof(fl) != 0); }

	override void close () {
		if (fl !is null) {
			static if (ownfl) {
				auto res = gzclose(fl);
				fl = null;
				if (res != Z_BUF_ERROR && res != Z_OK) throw new VFSException("can't close file", __FILE__, __LINE__);
			} else {
				fl = null;
			}
		}
	}

	override ssize read (void* buf, usize count) {
		if (fl is null || err()) return -1;
		if (count == 0) return 0;
		if (!fixPosition) return -1;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			static if (is(typeof(&gzfread))) {
				auto res = gzfread(buf, 1, count, fl);
			} else {
				static if (count.sizeof > uint.sizeof) { if (count >= int.max) return -1; }
				auto res = gzread(fl, buf, cast(uint)count);
			}
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { gzclearerr(fl); continue; } }
				}
			}
			if (res == 0) {
				//{ import core.stdc.stdio; printf("res=0; pos=%u; err=%d\n", cast(uint)newpos, err()); }
				return (err() ? -1 : 0);
			}
			newpos += res;
			return res;
		}
	}

	override ssize write (in void* buf, usize count) {
		if (fl is null || err()) return -1;
		if (count == 0) return 0;
		if (!fixPosition) return -1;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			static if (is(typeof(&gzfwrite))) {
				auto res = gzfwrite(cast(void*)buf, 1, count, fl); // fuck you, phobos!
			} else {
				static if (count.sizeof > uint.sizeof) { if (count >= int.max) return -1; }
				auto res = gzwrite(fl, cast(void*)buf, cast(uint)count);
			}
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { gzclearerr(fl); continue; } }
				}
			}
			if (res == 0) return (err() ? -1 : 0);
			newpos += res;
			if (cachedSize == ulong.max || newpos > cachedSize) cachedSize = newpos; // fix cached file size
			return res;
		}
	}

	override long lseek (long offset, int origin) {
		if (fl is null) return -1;
		//{ import core.stdc.stdio; printf("ofs=%d; orig=%d\n", cast(int)offset, origin); }
		// size query, and we have cached size?
		if (origin == 2 && offset == 0 && cachedSize != ulong.max) {
			// don't do anything
			newpos = cachedSize;
			return cachedSize;
		}
		// ok, "normal" seek
		static if (offset.sizeof > int.sizeof) {
			if (offset < int.min || offset > int.max) return -1;
		}
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = gzseek(fl, cast(int)offset, origin); // fuck you, phobos!
			if (res != -1) {
				gzclearerr(fl);
			} else {
				version(VFS_NORMAL_OS) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) { gzclearerr(fl); continue; } }
				}
				// ok, gzio sux and cannot seek, fallback to file reading if this is size query
				//{ import core.stdc.stdio; printf("ERR: ofs=%d; orig=%d\n", cast(int)offset, origin); }
				if (origin == 2 && offset == 0 && cachedSize == ulong.max) {
					char[512] buf = void;
					newpos = gztell(fl); // just in case
					for (;;) {
						auto rd = read(buf.ptr, buf.length);
						if (rd < 0) return -1; // alas
						if (rd == 0) break; // done
					}
					// no more bytes; assume that it is file size
					//{ import core.stdc.stdio; printf("np0: %u\n", cast(uint)newpos); }
					newpos = gztell(fl); // just in case
					//{ import core.stdc.stdio; printf("np1: %u\n", cast(uint)newpos); }
					cachedSize = newpos;
					return newpos;
				}
				return res;
			}
			//{ import core.stdc.stdio; printf("%d\n", cast(int)gztell(fl)); }
			newpos = gztell(fl);
			return newpos;
		}
	}

	override bool flush () {
		if (fl is null) return false;
		return (gzflush(fl, Z_FINISH) == 0);
	}
}

static import etc.c.zlib;

usize WrapGZFile(bool ownfl=true) (etc.c.zlib.gzFile fl, const(char)[] fname=null) {
	return newWS!(WrappedStreamGZFile!ownfl)(fl, fname);
}


// ////////////////////////////////////////////////////////////////////////// //
static if (VFS_NORMAL_OS) final class WrappedStreamFD(bool own) : WrappedStreamRC {
private:
	int fd;

	public this (int afd, const(char)[] afname) { fd = afd; eofhit = (afd < 0); super(afname); } // fuck! emplace needs it

protected:
	override @property bool isOpen () const nothrow @safe @nogc { return (fd >= 0); }

	override void close () {
		if (fd >= 0) {
			import std.exception : ErrnoException;
			static if (own) {
				debug(vfs_rc) { import core.stdc.stdio : printf; printf("******** CLOSING FD %u\n", cast(uint)fd); }
			} else {
				debug(vfs_rc) { import core.stdc.stdio : printf; printf("******** RELEASING FD %u\n", cast(uint)fd); }
			}
			static if (own) auto res = core.sys.posix.unistd.close(fd);
			fd = -1;
			eofhit = true;
			static if (own) if (res < 0) throw new ErrnoException("can't close file", __FILE__, __LINE__);
		} else {
			fd = -1;
			eofhit = true;
		}
	}

	override ssize read (void* buf, usize count) {
		if (fd < 0) return -1;
		if (count == 0) return 0;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = core.sys.posix.unistd.read(fd, buf, count);
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) continue; }
				}
			}
			if (res != count) eofhit = true;
			return res;
		}
	}

	override ssize write (in void* buf, usize count) {
		if (fd < 0) return -1;
		if (count == 0) return 0;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = core.sys.posix.unistd.write(fd, buf, count);
			version(VFS_NORMAL_OS) {
				if (res == -1) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) continue; }
				}
			}
			if (res != count) eofhit = true;
			return res;
		}
	}

	override long lseek (long offset, int origin) {
		if (fd < 0) return -1;
		version(VFS_NORMAL_OS) int sigsleft = VFSSigRepeatCount;
		for (;;) {
			auto res = core.sys.posix.unistd.lseek(fd, offset, origin);
			if (res != -1) {
				eofhit = false;
			} else {
				version(VFS_NORMAL_OS) {
					import core.stdc.errno;
					if (errno == EINTR) { if (sigsleft-- > 0) continue; }
				}
			}
			return res;
		}
	}

	override bool flush () {
		//import core.sys.posix.unistd : fdatasync;
		if (fd < 0) return false;
		//if (fdatasync(fd) == 0) return true;
		// check for special file
		import core.stdc.errno;
		if (errno == EROFS || errno == EINVAL) return true; // this is pipe, fifo, socket, etc., assume success
		return false;
	}
}


static if (VFS_NORMAL_OS) usize WrapFD(bool own) (int fd, const(char)[] fname=null) {
	return newWS!(WrappedStreamFD!own)(fd, fname);
}


// ////////////////////////////////////////////////////////////////////////// //
final class WrappedStreamAny(ST) : WrappedStreamRC {
private:
	ST st;
	bool closed;

	// fuck! emplace needs it
	public this() (auto ref ST ast, const(char)[] afname) {
		version(vfs_debug_name_set) {
			import core.stdc.stdio;
			stderr.fprintf("WrappedStreamAny(%s).ctor: afname", ST.stringof.ptr);
			if (afname is null) stderr.fprintf(" IS NULL\n"); else stderr.fprintf("=<%.*s>\n", cast(uint)afname.length, afname.ptr);
		}
		st = ast;
		super(afname);
		static if (streamHasIsOpen!ST) {
			closed = !st.isOpen;
		} else {
			closed = false;
		}
	}

protected:
	// prefer passed name, if it is not null
	override @property const(char)[] name () const nothrow @trusted @nogc {
		if (fnamelen && !closed) {
			return (fnameptr ? (cast(const(char)*)fnameptr)[0..fnamelen] : fnamebuf.ptr[0..fnamelen]);
		} else {
			static if (streamHasName!ST) {
				return (closed ? null : (hasName ? super.name : st.name));
			} else {
				return (fnameptr ? "" : null);
			}
		}
	}

	override @property bool isOpen () const nothrow @safe @nogc {
		static if (streamHasIsOpen!ST) {
			if (closed) return true;
			return st.isOpen;
		} else {
			return !closed;
		}
	}

	override @property bool eof () {
		if (closed) return true;
		static if (streamHasEof!ST) {
			return st.eof;
		} else {
			return eofhit;
		}
	}

	override void close () {
		if (!closed) {
			closed = true;
			eofhit = true;
			static if (streamHasClose!ST) st.close();
			st = ST.init;
		}
	}

	override ssize read (void* buf, usize count) {
		if (closed) return -1;
		if (count == 0) return 0;
		static if (isLowLevelStreamR!ST) {
			auto res = st.read(buf, count);
			if (res != count) eofhit = true;
			return res;
		} else static if (isReadableStream!ST) {
			return st.rawRead(buf[0..count]).length;
		} else {
			return -1;
		}
	}

	override ssize write (in void* buf, usize count) {
		if (closed) return -1;
		if (count == 0) return 0;
		static if (isLowLevelStreamW!ST) {
			auto res = st.write(buf, count);
			if (res != count) eofhit = true;
			return res;
		} else static if (isWriteableStream!ST) {
			st.rawWrite(buf[0..count]);
			return count;
		} else {
			return -1;
		}
	}

	override long lseek (long offset, int origin) {
		if (origin != Seek.Set && origin != Seek.Cur && origin != Seek.End) return -1;
		static if (isLowLevelStreamS!ST) {
			// has low-level seek
			if (closed) return -1;
			auto res = st.lseek(offset, origin);
			if (res != -1) eofhit = false;
			return res;
		} else static if (streamHasSeek!ST) {
			// has high-level seek
			if (closed) return -1;
			st.seek(offset, origin);
			eofhit = false;
			return st.tell;
		} else {
			// no seek at all
			return -1;
		}
	}

	override bool flush () {
		static if (streamHasFlush!ST) {
					 static if (is(typeof(st.flush()) == bool)) return st.flush();
			else static if (is(typeof(st.flush()) : long)) return (st.flush() == 0);
			else { st.flush(); return true; }
		} else {
			return true;
		}
	}

	override @property bool hasSize () { static if (streamHasSizeLowLevel!ST) return true; else return false; }
	override long getsize () { static if (streamHasSizeLowLevel!ST) return st.getsize; else return -1; }
}


/// wrap `std.stdio.File` into `VFile`
version(vfs_stdio_wrapper)
public VFile wrapStream (std.stdio.File st, const(char)[] fname=null) { return VFile(st, fname); }

/// wrap another `VFile` into `VFile` (a perfectly idiotic action)
public VFile wrapStream (VFile st) { return st; }

/// wrap libc `FILE*` into `VFile`
public VFile wrapStream (core.stdc.stdio.FILE* st, const(char)[] fname=null) { return VFile(st, fname); }

static if (VFS_NORMAL_OS) {
/// wrap file descriptor into `VFile`
public VFile wrapStream (int fd, const(char)[] fname=null) { return VFile(fd, fname); }
}

/** wrap any valid i/o stream into `VFile`.
 * "valid" stream should emplement one of two interfaces described below.
 * only one thread can call stream operations at a time, it's guaranteed by `VFile`.
 * note that any function is free to throw, `VFile` will take care of that.
 *
 * low-level interface:
 *
 * [mandatory] `ssize read (void* buf, usize count);`
 *
 *   read bytes; should read up to `count` bytes and return number of bytes read.
 *   should return -1 on error. can't be called with `count == 0`.
 *
 * [mandatory] `ssize write (in void* buf, usize count);`
 *
 *   write bytes; should write exactly `count` bytes and return number of bytes written.
 *   should return -1 on error. can't be called with `count == 0`. note that if you
 *   will return something that is not equal to `count` (i.e. will write less bytes than
 *   requested), `VFile` will throw.
 *
 * [mandatory] `long lseek (long offset, int origin);`
 *
 *   seek into stream. `origin` is one of `Seek.Set`, `Seek.Cur`, or `Seek.End` (can't be
 *   called with another values). should return resulting offset from stream start or -1
 *   on error. note that this method will be used to implement `tell()` and `size()`
 *   VFile APIs, so make it as fast as you can.
 *
 *   should return stream name, or throw on error. can return empty name.
 *
 * or high-level interface:
 *
 * [mandatory] `void[] rawRead (void[] buf);`
 *
 *   read bytes; should read up to `buf.length` bytes and return slice with read bytes.
 *   should throw on error. can't be called with empty buf.
 *
 * [mandatory] `void rawWrite (in void[] buf);`
 *
 *   write bytes; should write exactly `buf.length` bytes.
 *   should throw on error (note that if it wrote less bytes than requested, it is an
 *   error too).
 *
 * [mandatory] `void seek (long offset, int origin);`
 *
 *   seek into stream. `origin` is one of `Seek.Set`, `Seek.Cur`, or `Seek.End`.
 *   should throw on error (including invalid `origin`).
 *
 * [mandatory] `@property long tell ();`
 *
 *   should return current position in stream. should throw on error.
 *
 * [mandatory] `@property long getsize ();`
 *
 *   should return stream size. should throw on error.
 *
 * common interface, optional:
 *
 * [optional] `@property const(char)[] name ();`
 *
 *   should return stream name, or throw on error. can return empty name.
 *
 * [optional] `@property bool isOpen ();`
 *
 *   should return `true` if the stream is opened, or throw on error.
 *
 * [optional] `@property bool eof ();`
 *
 *   should return `true` if end of stream is reached, or throw on error.
 *   note that EOF flag may be set in i/o methods, so you can be at EOF,
 *   but this method can still return `false`. i.e. it is unreliable.
 *
 * [optional] `void close ();`
 *
 *   should close stream, or throw on error. VFile won't call that on
 *   streams that returns `false` from `isOpen()`, but you'd better
 *   handle this situation yourself.
 *
 * [optional] `bool flush ();`
 *
 *   flush unwritten data (if your stream supports writing).
 *   return `true` on success.
 *
 */
public VFile wrapStream(ST) (auto ref ST st, const(char)[] fname=null)
if (isReadableStream!ST || isWriteableStream!ST || isLowLevelStreamR!ST || isLowLevelStreamW!ST)
{
	return VFile(cast(void*)newWS!(WrappedStreamAny!ST)(st, fname));
}


// ////////////////////////////////////////////////////////////////////////// //
private struct PartialLowLevelRO {
	VFile zfl; // original file
	long stpos; // starting position
	long size; // unpacked size
	long pos; // current file position
	bool eofhit;

	this (VFile fl, long astpos, long asize) {
		stpos = astpos;
		size = asize;
		zfl = fl;
	}

	@property bool isOpen () const nothrow @safe @nogc { return zfl.isOpen; }
	@property bool eof () { return eofhit; }

	void close () {
		eofhit = true;
		if (zfl.isOpen) zfl.close();
	}

	ssize read (void* buf, usize count) {
		if (buf is null) return -1;
		if (count == 0 || size == 0) return 0;
		if (!isOpen) return -1; // read error
		if (pos >= size) { eofhit = true; return 0; } // EOF
		if (size-pos < count) { eofhit = true; count = cast(usize)(size-pos); }
		zfl.seek(stpos+pos);
		auto rd = zfl.rawRead(buf[0..count]);
		pos += rd.length;
		return rd.length;
	}

	ssize write (in void* buf, usize count) { return -1; }

	long lseek (long ofs, int origin) {
		if (!isOpen) return -1;
		//TODO: overflow checks
		switch (origin) {
			case Seek.Set: break;
			case Seek.Cur: ofs += pos; break;
			case Seek.End:
				if (ofs > 0) ofs = 0;
				ofs += size;
				break;
			default:
				return -1;
		}
		if (ofs < 0) return -1;
		eofhit = false;
		if (ofs > size) ofs = size;
		pos = ofs;
		return pos;
	}
}


/// wrap VFile into read-only stream, with given offset and length.
/// if `len` == -1, wrap from starting position to file end.
public VFile wrapStreamRO (VFile st, long stpos=0, long len=-1, const(char)[] fname=null) {
	if (stpos < 0) throw new VFSException("invalid starting position");
	if (len == -1) len = st.size-stpos;
	if (len < 0) throw new VFSException("invalid length");
	//return wrapStream(PartialLowLevelRO(st, stpos, len), fname);
	return VFile(cast(void*)newWS!(WrappedStreamAny!PartialLowLevelRO)(PartialLowLevelRO(st, stpos, len), (fname !is null ? fname : st.name)));
}


// ////////////////////////////////////////////////////////////////////////// //
public enum VFSZLibMode {
	Raw,
	ZLib,
	Zip, // special mode for zip archives
}


// ////////////////////////////////////////////////////////////////////////// //
version(vfs_use_zlib_unpacker) {
	struct ZLibLowLevelRO {
		private import etc.c.zlib;

		enum ibsize = 32768;

		VFile zfl; // archive file
		VFSZLibMode mode;
		long stpos; // starting position
		long size; // unpacked size
		long pksize; // packed size
		long pos; // current file position
		long prpos; // previous file position
		long pkpos; // current position in DAT
		ubyte[] pkb; // packed data
		z_stream zs;
		bool eoz;
		bool eofhit;
		// reading one byte from zlib fuckin' fails. shit.
		ubyte[65536] updata;
		uint uppos, upused;
		bool upeoz;

		this (VFile fl, VFSZLibMode amode, long aupsize, long astpos, long asize) {
			if (amode == VFSZLibMode.Raw && aupsize < 0) aupsize = asize;
			zfl = fl;
			stpos = astpos;
			size = aupsize;
			pksize = asize;
			mode = amode;
			uppos = upused = 0;
			upeoz = false;
		}

		@property bool isOpen () const nothrow @safe @nogc { return zfl.isOpen; }
		@property bool eof () { return eofhit; }

		void close () {
			import core.stdc.stdlib : free;
			eofhit = true;
			if (pkb.length) {
				inflateEnd(&zs);
				free(pkb.ptr);
				pkb = null;
			}
			uppos = upused = 0;
			upeoz = true;
			eoz = true;
			if (zfl.isOpen) zfl.close();
		}

		private bool initZStream (bool reinit=false) {
			import core.stdc.stdlib : malloc, free;
			if (mode == VFSZLibMode.Raw || (!reinit && pkb.ptr !is null)) return true;
			// allocate buffer for packed data
			if (pkb.ptr is null) {
				auto pb = cast(ubyte*)malloc(ibsize);
				if (pb is null) return false;
				pkb = pb[0..ibsize];
			}
			zs.avail_in = 0;
			zs.avail_out = 0;
			// initialize unpacker
			// -15 is a magic value used to decompress zip files:
			// it has the effect of not requiring the 2 byte header and 4 byte trailer
			if (inflateInit2(&zs, (mode == VFSZLibMode.Zip ? -15 : 15)) != Z_OK) {
				free(pkb.ptr);
				pkb = null;
				return false;
			}
			eoz = false;
			// we are ready
			return true;
		}

		private bool readPackedChunk () {
			if (zs.avail_in > 0) return true;
			if (pkpos >= pksize) return false;
			zs.next_in = cast(typeof(zs.next_in))pkb.ptr;
			zs.avail_in = cast(uint)(pksize-pkpos > ibsize ? ibsize : pksize-pkpos);
			zfl.seek(stpos+pkpos);
			auto rd = zfl.rawRead(pkb[0..zs.avail_in]);
			if (rd.length == 0) return false;
			zs.avail_in = cast(int)rd.length;
			pkpos += zs.avail_in;
			return true;
		}

		private bool unpackNextChunk () {
			while (zs.avail_out > 0) {
				if (eoz) return (size < 0); // `false` for known size, `true` for unknown size
				if (uppos >= upused) {
					if (upeoz) { eoz = true; continue; }
					if (!readPackedChunk()) return false;
					auto sv0 = zs.avail_out;
					auto sv1 = zs.next_out;
					uppos = 0;
					zs.avail_out = cast(uint)updata.length;
					zs.next_out = cast(ubyte*)updata.ptr;
					auto err = inflate(&zs, Z_SYNC_FLUSH);
					upused = cast(uint)(updata.length-zs.avail_out);
					zs.avail_out = sv0;
					zs.next_out = sv1;
					//if (err == Z_BUF_ERROR) { import iv.writer; writeln("*** OUT OF BUFFER!"); }
					if (err != Z_STREAM_END && err != Z_OK) return false;
					if (err == Z_STREAM_END) upeoz = true;
				} else {
					auto ptr = cast(ubyte*)zs.next_out;
					*ptr = updata.ptr[uppos++];
					--zs.avail_out;
					++zs.next_out;
				}
			}
			return true;
		}

		bool findUnpackedSize () {
			ubyte[1024] tbuf = void;
			//size = pos; // current size
			//{ import core.stdc.stdio; printf("findUnpackedSize: starting...\n"); }
			scope(exit) { import core.stdc.stdio; printf("findUnpackedSize: done...\n"); }
			for (;;) {
				uint rd = cast(uint)tbuf.length;
				zs.next_out = cast(typeof(zs.next_out))tbuf.ptr;
				zs.avail_out = rd;
				//{ import core.stdc.stdio; printf("findUnpackedSize: reading %u bytes...\n", rd); }
				if (!unpackNextChunk()) return false;
				rd -= zs.avail_out;
				//{ import core.stdc.stdio; printf("findUnpackedSize: read %u bytes...\n", rd); }
				if (pos+rd < 0) return false; // file too big
				prpos = (pos += rd);
				//{ import core.stdc.stdio; printf("findUnpackedSize: prpos=%u\n", cast(uint)prpos); }
				if (zs.avail_out != 0) break;
			}
			size = pos;
			return true;
		}

		ssize read (void* buf, usize count) {
			if (buf is null) return -1;
			if (count == 0 || size == 0) return 0;
			if (!isOpen) return -1; // read error
			if (size >= 0 && pos >= size) { eofhit = true; return 0; } // EOF
			if (mode == VFSZLibMode.Raw) {
				if (size-pos < count) { eofhit = true; count = cast(usize)(size-pos); }
				zfl.seek(stpos+pos);
				auto rd = zfl.rawRead(buf[0..count]);
				pos += rd.length;
				return rd.length;
			} else {
				if (pkb.ptr is null && !initZStream()) return -1;
				// do we want to seek backward?
				if (prpos > pos) {
					// yes, rewind
					inflateEnd(&zs);
					uppos = upused = 0;
					upeoz = false;
					zs = zs.init;
					pkpos = 0;
					if (!initZStream(true)) return -1;
					prpos = 0;
				}
				// do we need to seek forward?
				if (prpos < pos) {
					// yes, skip data
					ubyte[1024] tbuf = void;
					auto skp = pos-prpos;
					while (skp > 0) {
						uint rd = cast(uint)(skp > tbuf.length ? tbuf.length : skp);
						zs.next_out = cast(typeof(zs.next_out))tbuf.ptr;
						zs.avail_out = rd;
						if (!unpackNextChunk()) return -1;
						skp -= rd;
					}
					prpos = pos;
				}
				// unpack data
				if (size >= 0 && size-pos < count) { eofhit = true; count = cast(usize)(size-pos); }
				zs.next_out = cast(typeof(zs.next_out))buf;
				zs.avail_out = cast(uint)count;
				if (!unpackNextChunk()) return -1;
				if (size < 0 && zs.avail_out > 0) {
					eofhit = true;
					count -= zs.avail_out;
					size = pos+count;
				}
				prpos = (pos += count);
				return count;
			}
		}

		ssize write (in void* buf, usize count) { return -1; }

		long lseek (long ofs, int origin) {
			if (!isOpen) return -1;
			//TODO: overflow checks
			switch (origin) {
				case Seek.Set: break;
				case Seek.Cur: ofs += pos; break;
				case Seek.End:
					if (ofs > 0) ofs = 0;
					if (size < 0) {
						if (pkb.ptr is null && !initZStream()) return -1;
						if (!findUnpackedSize) return -1;
					}
					ofs += size;
					break;
				default:
					return -1;
			}
			if (ofs < 0) return -1;
			if (size >= 0 && ofs > size) ofs = size;
			pos = ofs;
			eofhit = false;
			return pos;
		}
	}
} else {
	// inflate
	import iv.vfs.inflate;

	struct ZLibLowLevelRO {
		VFile zfl; // archive file
		VFSZLibMode mode;
		InfStream* ifs;
		long stpos; // starting position
		long size; // unpacked size
		long pksize; // packed size
		long pkpos; // current position in packed data
		long pos; // current file position (number of unpacked bytes read)
		long prpos; // previous file position (seek is done when reading)
		bool eofhit; // did we hit EOF on last read?

		int readBuf (ubyte[] buf) {
			assert(buf.length > 0);
			assert(buf.length < int.max/2);
			//{ import core.stdc.stdio; printf("inf: reading %u bytes (pkpos=%d; pksize=%d)\n", cast(uint)buf.length, cast(int)pkpos, cast(int)pksize); }
			if (pkpos >= pksize) return 0; // eof
			int toread = cast(int)buf.length;
			if (toread > pksize-pkpos) toread = cast(int)(pksize-pkpos);
			assert(toread > 0);
			zfl.seek(stpos+pkpos);
			auto rd = zfl.rawRead(buf[0..toread]);
			if (rd.length == 0) { pkpos = pksize; return 0; } // eof
			pkpos += cast(int)rd.length;
			return cast(int)rd.length;
		}

		this (VFile fl, VFSZLibMode amode, long aupsize, long astpos, long asize) {
			//{ import core.stdc.stdio; printf("inf: aupsize=%d; astpos=%d; asize=%d\n", cast(int)aupsize, cast(int)astpos, cast(int)asize); }
			if (amode == VFSZLibMode.Raw && aupsize < 0) aupsize = asize;
			zfl = fl;
			stpos = astpos;
			size = aupsize;
			pksize = asize;
			pkpos = 0;
			mode = amode;
		}

		@property bool isOpen () const nothrow @safe @nogc { return zfl.isOpen; }
		@property bool eof () { return eofhit; }

		void inflateInit () {
			if (mode == VFSZLibMode.Raw) return;
			if (ifs is null) {
				import core.stdc.stdlib : malloc;
				import core.stdc.string : memset;
				ifs = cast(InfStream*)malloc(InfStream.sizeof);
				if (ifs is null) throw new Exception("out of memory");
				memset(ifs, 0, InfStream.sizeof);
			}
			ifs.reinit(mode == VFSZLibMode.ZLib ? InfStream.Mode.ZLib : InfStream.Mode.Deflate);
		}

		void close () {
			if (ifs !is null) {
				import core.stdc.stdlib : free;
				free(ifs);
				ifs = null;
			}
			eofhit = true;
			if (zfl.isOpen) zfl.close();
		}

		bool findUnpackedSize () {
			ubyte[1024] tbuf = void;
			if (ifs is null) inflateInit(); // here, 'cause struct can be copied
			//{ import core.stdc.stdio; printf("findUnpackedSize: starting...\n"); }
			//scope(exit) { import core.stdc.stdio; printf("findUnpackedSize: done...\n"); }
			for (;;) {
				//{ import core.stdc.stdio; printf("findUnpackedSize: reading %u bytes...\n", cast(uint)tbuf.length); }
				auto rd = ifs.rawRead(&readBuf, tbuf[]);
				//{ import core.stdc.stdio; printf("findUnpackedSize: read %u bytes...\n", cast(uint)rd.length); }
				if (rd.length == 0) break;
				prpos += rd.length;
				//{ import core.stdc.stdio; printf("findUnpackedSize: prpos=%u\n", cast(uint)prpos); }
			}
			size = pos = prpos;
			return true;
		}

		ssize read (void* buf, usize count) {
			if (buf is null) return -1;
			if (count == 0 || size == 0) return 0;
			if (!isOpen) return -1; // read error
			if (size >= 0 && pos >= size) { eofhit = true; return 0; } // EOF
			if (mode == VFSZLibMode.Raw) {
				// raw mode
				if (size-pos < count) { eofhit = true; count = cast(usize)(size-pos); }
				zfl.seek(stpos+pos);
				auto rd = zfl.rawRead(buf[0..count]);
				if (rd.length == 0) eofhit = true; // just in case
				pos += rd.length;
				return rd.length;
			} else {
				// unpack file part
				// do we want to seek backward?
				if (prpos > pos) {
					// yes, rewind
					pkpos = 0;
					inflateInit();
					prpos = 0;
				} else {
					if (ifs is null) inflateInit(); // here, 'cause struct can be copied
				}
				// do we need to seek forward?
				if (prpos < pos) {
					// yes, skip data
					ubyte[1024] tbuf = void;
					auto skp = pos-prpos;
					//{ import core.stdc.stdio; printf("00: skp=%d; prpos=%d; pos=%d\n", cast(int)skp, cast(int)prpos, cast(int)pos); }
					while (skp > 0) {
						uint rd = cast(uint)(skp <= tbuf.length ? skp : tbuf.length);
						auto b = ifs.rawRead(&readBuf, tbuf[0..rd]);
						if (b.length == 0) { eofhit = true; return -1; }
						prpos += b.length;
						skp -= b.length;
					}
					//{ import core.stdc.stdio; printf("01: prpos=%d; pos=%d\n", cast(int)prpos, cast(int)pos); }
				}
				assert(pos == prpos);
				// unpack data
				if (size >= 0 && size-pos < count) { eofhit = true; count = cast(usize)(size-pos); }
				auto rdb = ifs.rawRead(&readBuf, buf[0..count]);
				if (rdb.length == 0) { eofhit = true; return 0; }
				count = rdb.length;
				prpos = (pos += count);
				return count;
			}
		}

		ssize write (in void* buf, usize count) { return -1; }

		long lseek (long ofs, int origin) {
			if (!isOpen) return -1;
			//TODO: overflow checks
			switch (origin) {
				case Seek.Set: break;
				case Seek.Cur: ofs += pos; break;
				case Seek.End:
					if (ofs > 0) ofs = 0;
					if (size < 0) {
						if (mode == VFSZLibMode.Raw) return -1;
						if (!findUnpackedSize()) return -1;
					}
					ofs += size;
					break;
				default:
					return -1;
			}
			if (ofs < 0) return -1;
			if (size >= 0 && ofs > size) ofs = size;
			pos = ofs;
			eofhit = (pos >= size);
			return pos;
		}
	}
} // version


/*
void foo () {
	auto zro = ZLibLowLevelRO(VFile("a"), VFSZLibMode.ZLib, 10, 0, 10, "foo");
}
*/


/// wrap VFile into read-only zlib-packed stream, with given offset and length.
/// if `len` == -1, wrap from starting position to file end.
/// `upsize`: size of unpacked file (-1: size unknown)
public VFile wrapZLibStreamRO (VFile st, VFSZLibMode mode, long upsize, long stpos=0, long len=-1, const(char)[] fname=null) {
	if (stpos < 0) throw new VFSException("invalid starting position");
	if (upsize < 0 && upsize != -1) throw new VFSException("invalid unpacked size");
	if (len == -1) len = st.size-stpos;
	if (len < 0) throw new VFSException("invalid length");
	//return wrapStream(ZLibLowLevelRO(st, mode, upsize, stpos, len), fname);
	//{ import core.stdc.stdio; if (fname is null) stderr.fprintf("FNAME IS NULL\n"); else stderr.fprintf("FNAME: <%.*s>\n", cast(uint)fname.length, fname.ptr); }
	return VFile(cast(void*)newWS!(WrappedStreamAny!ZLibLowLevelRO)(ZLibLowLevelRO(st, mode, upsize, stpos, len), (fname !is null ? fname : st.name)));
}

/// the same as previous function, but using VFSZLibMode.ZLib, as most people is using it
public VFile wrapZLibStreamRO (VFile st, long upsize, long stpos=0, long len=-1, const(char)[] fname=null) {
	return wrapZLibStreamRO(st, VFSZLibMode.ZLib, upsize, stpos, len, fname);
}


// ////////////////////////////////////////////////////////////////////////// //
struct ZLibLowLevelWO {
	private import etc.c.zlib;

	enum obsize = 32768;

	VFile zfl; // destination file
	VFSZLibMode mode;
	long stpos; // starting position
	long pos; // current file position (from stpos)
	long prpos; // previous file position (from stpos)
	ubyte[] pkb; // packed data
	z_stream zs;
	bool eofhit;
	int complevel;

	this (VFile fl, VFSZLibMode amode, int acomplevel=-1) {
		zfl = fl;
		stpos = fl.tell;
		mode = amode;
		if (acomplevel < 0) acomplevel = 6;
		if (acomplevel > 9) acomplevel = 9;
		complevel = 9;
	}

	@property bool isOpen () const nothrow @safe @nogc { return (!eofhit && zfl.isOpen); }
	@property bool eof () { return isOpen; }

	void close () {
		scope(exit) {
			import core.stdc.stdlib : free;
			eofhit = true;
			if (pkb !is null) free(pkb.ptr);
			pkb = null;
		}
		//{ import core.stdc.stdio : printf; printf("CLOSING...\n"); }
		if (!eofhit) {
			scope(exit) zfl.close();
			if (zfl.isOpen && pkb !is null) {
				int err;
				// do leftovers
				//{ import core.stdc.stdio : printf; printf("writing %u bytes; avail_out: %u bytes\n", cast(uint)zs.avail_in, cast(uint)zs.avail_out); }
				for (;;) {
					zs.avail_in = 0;
					err = deflate(&zs, Z_FINISH);
					if (err != Z_OK && err != Z_STREAM_END && err != Z_BUF_ERROR) {
						//{ import core.stdc.stdio; printf("cerr: %d\n", err); }
						throw new VFSException("zlib compression error");
					}
					if (zs.avail_out < obsize) {
						//{ import core.stdc.stdio : printf; printf("flushing %u bytes (avail_out: %u bytes)\n", cast(uint)(obsize-zs.avail_out), cast(uint)zs.avail_out); }
						if (prpos != pos) throw new VFSException("zlib compression seek error");
						zfl.seek(stpos+pos);
						zfl.rawWriteExact(pkb[0..obsize-zs.avail_out]);
						pos += obsize-zs.avail_out;
						prpos = pos;
						zs.next_out = pkb.ptr;
						zs.avail_out = obsize;
					}
					if (err != Z_OK && err != Z_BUF_ERROR) break;
				}
				// succesfully flushed?
				if (err != Z_STREAM_END) throw new VFSException("zlib compression error");
			}
			deflateEnd(&zs);
		}
	}

	private bool initZStream () {
		import core.stdc.stdlib : malloc, free;
		if (mode == VFSZLibMode.Raw || pkb.ptr !is null) return true;
		// allocate buffer for packed data
		if (pkb.ptr is null) {
			auto pb = cast(ubyte*)malloc(obsize);
			if (pb is null) return false;
			pkb = pb[0..obsize];
		}
		zs.next_out = pkb.ptr;
		zs.avail_out = obsize;
		zs.next_in = null;
		zs.avail_in = 0;
		// initialize packer
		// -15 is a magic value used to decompress zip files:
		// it has the effect of not requiring the 2 byte header and 4 byte trailer
		if (deflateInit2(&zs, Z_BEST_COMPRESSION, Z_DEFLATED, (mode == VFSZLibMode.Zip ? -15 : 15), complevel, 0) != Z_OK) {
			free(pkb.ptr);
			pkb = null;
			eofhit = true;
			return false;
		}
		zs.next_out = pkb.ptr;
		zs.avail_out = obsize;
		zs.next_in = null;
		zs.avail_in = 0;
		// we are ready
		return true;
	}

	ssize read (void* buf, usize count) { return -1; }

	ssize write (in void* buf, usize count) {
		if (buf is null) return -1;
		if (count == 0) return 0;
		if (mode == VFSZLibMode.Raw) {
			if (prpos != pos) return -1;
			zfl.seek(stpos+prpos);
			zfl.rawWriteExact(buf[0..count]);
			prpos += count;
			pos = prpos;
		} else {
			if (!initZStream()) return -1;
			auto css = count;
			auto bp = cast(const(ubyte)*)buf;
			while (css > 0) {
				zs.next_in = cast(typeof(zs.next_in))bp;
				zs.avail_in = (css > 0x3fff_ffff ? 0x3fff_ffff : cast(uint)css);
				bp += zs.avail_in;
				css -= zs.avail_in;
				// now process the whole input
				while (zs.avail_in > 0) {
					// write buffer
					//{ import core.stdc.stdio : printf; printf("writing %u bytes; avail_out: %u bytes\n", cast(uint)zs.avail_in, cast(uint)zs.avail_out); }
					if (zs.avail_out == 0) {
						if (prpos != pos) return -1;
						zfl.seek(stpos+prpos);
						zfl.rawWriteExact(pkb[0..obsize]);
						prpos += obsize;
						pos = prpos;
						zs.next_out = pkb.ptr;
						zs.avail_out = obsize;
					}
					auto err = deflate(&zs, Z_NO_FLUSH);
					if (err != Z_OK) return -1;
				}
			}
		}
		return count;
	}

	long lseek (long ofs, int origin) {
		if (!isOpen) return -1;
		//TODO: overflow checks
		switch (origin) {
			case Seek.Set: break;
			case Seek.Cur: ofs += prpos; break;
			case Seek.End: if (ofs > 0) ofs = 0; ofs += pos; break;
			default: return -1;
		}
		if (ofs < 0) return -1;
		if (ofs > pos) ofs = pos;
		prpos = ofs;
		return pos;
	}
}


/// wrap VFile into write-only zlib-packing stream.
/// default compression mode is 9.
public VFile wrapZLibStreamWO (VFile st, VFSZLibMode mode, int complevel=9, const(char)[] fname=null) {
	//return wrapStream(ZLibLowLevelWO(st, mode, complevel), fname);
	return VFile(cast(void*)newWS!(WrappedStreamAny!ZLibLowLevelWO)(ZLibLowLevelWO(st, mode, complevel), (fname !is null ? fname : st.name)));
}

/// the same as previous function, but using VFSZLibMode.ZLib, as most people is using it
public VFile wrapZLibStreamWO (VFile st, int complevel=9, const(char)[] fname=null) {
	return wrapZLibStreamWO(st, VFSZLibMode.ZLib, complevel, (fname !is null ? fname : st.name));
}


// ////////////////////////////////////////////////////////////////////////// //
// WARNING! RW streams will set NO_INTERIOR!
public alias MemoryStreamRW = MemoryStreamImpl!(true, false);
public alias MemoryStreamRWRef = MemoryStreamImpl!(true, true);
public alias MemoryStreamRO = MemoryStreamImpl!(false, false);


// ////////////////////////////////////////////////////////////////////////// //
// not thread-safe
struct MemoryStreamImpl(bool rw, bool asref) {
private:
	static if (rw) {
		static if (asref)
			ubyte[]* data;
		else
			ubyte[] data;
	} else {
		static assert(!asref, "wtf?!");
		const(ubyte)[] data;
	}
	usize curpos;
	bool eofhit;
	bool closed = false;

public:
	static if (usize.sizeof == 4) {
		enum MaxSize = 0x7fff_ffffU;
	} else {
		enum MaxSize = 0x7fff_ffff_ffff_ffffUL;
	}

public:
	static if (rw) {
		static if (asref) {
			this (ref ubyte[] adata) @trusted {
				if (adata.length > MaxSize) throw new VFSException("buffer too big");
				data = &adata;
				eofhit = (adata.length == 0);
			}
			@property ubyte[]* bytes () pure nothrow @safe @nogc { return data; }
		} else {
			this (const(ubyte)[] adata) @trusted {
				if (adata.length > MaxSize) throw new VFSException("buffer too big");
				data = cast(typeof(data))(adata.dup);
				eofhit = (adata.length == 0);
			}
			@property const(ubyte)[] bytes () pure nothrow @safe @nogc { return data; }
		}
	} else {
		this (const(void)[] adata) @trusted {
			if (adata.length > MaxSize) throw new VFSException("buffer too big");
			data = cast(typeof(data))(adata);
			eofhit = (adata.length == 0);
		}
		@property const(ubyte)[] bytes () pure nothrow @safe @nogc { return data; }
	}

	@property const pure nothrow @safe @nogc {
		long getsize () { return data.length; }
		long tell () { return curpos; }
		bool eof () { return eofhit; }
		bool isOpen () { return !closed; }
	}

	void seek (long offset, int origin=Seek.Set) @trusted {
		if (closed) throw new VFSException("can't seek in closed stream");
		switch (origin) {
			case Seek.Set:
				if (offset < 0 || offset > MaxSize) throw new VFSException("invalid offset");
				curpos = cast(usize)offset;
				break;
			case Seek.Cur:
				if (offset < -cast(long)curpos || offset > MaxSize-curpos) throw new VFSException("invalid offset");
				curpos += offset;
				break;
			case Seek.End:
				if (offset < -cast(long)data.length || offset > MaxSize-data.length) throw new VFSException("invalid offset");
				curpos = cast(usize)(cast(long)data.length+offset);
				break;
			default: throw new VFSException("invalid offset origin");
		}
		eofhit = false;
	}

	ssize read (void* buf, usize count) {
		if (closed) return -1;
		if (curpos >= data.length) { eofhit = true; return 0; }
		if (count > 0) {
			import core.stdc.string : memcpy;
			usize rlen = data.length-curpos;
			if (rlen >= count) rlen = count; else eofhit = true;
			assert(rlen != 0);
			memcpy(buf, data.ptr+curpos, rlen);
			curpos += rlen;
			return cast(ssize)rlen;
		} else {
			return 0;
		}
	}

	ssize write (in void* buf, usize count) {
		static if (rw) {
			import core.stdc.string : memcpy;
			if (closed) return -1;
			if (count == 0) return 0;
			if (count > MaxSize-curpos) return -1;
			if (data.length < curpos+count) {
				auto optr = data.ptr;
				data.length = curpos+count;
				if (data.ptr !is optr) {
					import core.memory : GC;
					optr = data.ptr;
					if (optr is GC.addrOf(optr)) GC.setAttr(optr, GC.BlkAttr.NO_INTERIOR);
				}
			}
			memcpy(data.ptr+curpos, buf, count);
			curpos += count;
			return count;
		} else {
			return -1;
		}
	}

	void close () pure nothrow @safe @nogc { curpos = 0; data = null; eofhit = true; closed = true; }
}


// ////////////////////////////////////////////////////////////////////////// //
version(vfs_test_stream) {
	import std.stdio : File, stdout;

	private void dump (const(ubyte)[] data, File fl=stdout) @trusted {
		for (usize ofs = 0; ofs < data.length; ofs += 16) {
			fl.writef("%04X:", ofs);
			foreach (immutable i; 0..16) {
				if (i == 8) fl.write(' ');
				if (ofs+i < data.length) fl.writef(" %02X", data[ofs+i]); else fl.write("   ");
			}
			fl.write(" ");
			foreach (immutable i; 0..16) {
				if (ofs+i >= data.length) break;
				if (i == 8) fl.write(' ');
				ubyte b = data[ofs+i];
				if (b <= 32 || b >= 127) fl.write('.'); else fl.writef("%c", cast(char)b);
			}
			fl.writeln();
		}
	}

	static assert(isReadableStream!MemoryStreamRO);
	static assert(!isWriteableStream!MemoryStreamRO);
	static assert(!isRWStream!MemoryStreamRO);
	static assert(isSeekableStream!MemoryStreamRO);
	static assert(streamHasClose!MemoryStreamRO);
	static assert(streamHasEof!MemoryStreamRO);
	static assert(streamHasSeek!MemoryStreamRO);
	static assert(streamHasTell!MemoryStreamRO);
	static assert(streamHasSize!MemoryStreamRO);

	unittest {
		{
			auto ms = MemoryStreamRW();
			ms.rawWrite("hello");
			assert(!ms.eof);
			assert(ms.data == cast(ubyte[])"hello");
			//dump(ms.data);
			ushort[3] d;
			ms.seek(0);
			assert(!ms.eof);
			assert(ms.rawRead(d[0..2]).length == 2);
			assert(!ms.eof);
			assert(d == [0x6568, 0x6c6c, 0]);
			ms.seek(1);
			assert(ms.rawRead(d[0..2]).length == 2);
			assert(d == [0x6c65, 0x6f6c, 0]);
			assert(!ms.eof);
			//dump(cast(ubyte[])d);
		}
		{
			auto ms = new MemoryStreamRW();
			wchar[] a = ['\u0401', '\u0280', '\u089e'];
			ms.rawWrite(a);
			assert(ms.bytes == cast(const(ubyte)[])x"01 04 80 02 9E 08");
			//dump(ms.data);
		}
		{
			auto ms = MemoryStreamRO("hello");
			assert(ms.data == cast(const(ubyte)[])"hello");
		}
	}
}

/// wrap read-only memory buffer into VFile
public VFile wrapMemoryRO (const(void)[] buf, const(char)[] fname=null) {
	//return wrapStream(MemoryStreamRO(buf), fname);
	return VFile(cast(void*)newWS!(WrappedStreamAny!MemoryStreamRO)(MemoryStreamRO(buf), fname));
}

/// wrap read-write memory buffer into VFile; duplicates data
public VFile wrapMemoryRW (const(ubyte)[] buf, const(char)[] fname=null) {
	//return wrapStream(MemoryStreamRW(buf), fname);
	return VFile(cast(void*)newWS!(WrappedStreamAny!MemoryStreamRW)(MemoryStreamRW(buf), fname));
}


// ////////////////////////////////////////////////////////////////////////// //
/// wrap libc stdout
public VFile wrapStdout () {
	static if (VFS_NORMAL_OS) {
		return VFile(1, false); // don't own
	} else {
		import core.stdc.stdio : stdout;
		if (stdout !is null) return VFile(stdout, "<stdout>", false); // don't own
		return VFile.init;
	}
}

/// wrap libc stderr
public VFile wrapStderr () {
	static if (VFS_NORMAL_OS) {
		return VFile(2, false); // don't own
	} else {
		import core.stdc.stdio : stderr;
		if (stderr !is null) return VFile(stderr, "<stderr>", false); // don't own
		return VFile.init;
	}
}

/// wrap libc stdin
public VFile wrapStdin () {
	static if (VFS_NORMAL_OS) {
		return VFile(0, false); // don't own
	} else {
		import core.stdc.stdio : stdin;
		if (stdin !is null) return VFile(stdin, "<stdin>", false); // don't own
		return VFile.init;
	}
}


// ////////////////////////////////////////////////////////////////////////// //
/* handy building block for various unpackers/decoders
 * XPS API (XPS should be a struct):
 *
 * XPS.InitUpkBufSize = initial size of intermediate "unpack buffer", which will be used
 *                      to store unpacked data chunks
 *
 *   -1 means "allocate as much as necessary" (with -upkbufsize as initial size)
 *    0 means "allocate as much as necessary" (with no initial allocation)
 *   >0 means "this is exact size, we will never need more than that"
 *
 * void setup (VFile fl, ulong agflags, long apos, long apksize, long aupksize);
 *   initialize decoder
 *     fl = input file; store it somewhere
 *     agflags = flags, decoder should know what they are for
 *     apos = encoded data offset in fl
 *     apksize = encoded data size
 *     aupksize = decoded data size
 *
 * void reset ();
 *   reset decoder; will be called when engine wants to start decoding again from apos
 *   should reset all necessary vars, etc.
 *
 * void close ();
 *   close fl, shutdown decoder, free memory, etc. nothing else will be called after this
 *
 * bool unpackChunk (scope VStreamDecoderLowLevelROPutBytesDg pdg);
 *   decode chunk of arbitrary size, use `putUnpackedBytes()` to put unpacked bytes into
 *   buffer. return `false` if EOF was hit, otherwise try to emit at least one byte.
 *   note that `unpackChunk()` can emit no bytes and return `true` to indicate that it
 *   can be called again to get more data.
 *     pdg = delegate that should be called to emit decoded bytes
 */
public alias VStreamDecoderLowLevelROPutBytesDg = void delegate (const(ubyte)[] bts...) @trusted;

public struct VStreamDecoderLowLevelRO(XPS) {
private:
	static assert(XPS.InitUpkBufSize != int.min, "are you insane?");
	XPS epl;
	long size; // unpacked size
	long pos; // current file position
	long prpos; // previous file position
	bool eofhit;
	bool epleof;
	bool closed;
	static if (XPS.InitUpkBufSize <= 0) {
		mixin VFSHiddenPointerHelper!(ubyte, "upkbuf");
		uint upkbufsize;
	} else {
		ubyte[XPS.InitUpkBufSize] upkbuf;
		enum upkbufsize = cast(uint)XPS.InitUpkBufSize;
	}
	uint upkbufused, upkbufpos;

public:
	this (VFile fl, ulong agflags, long aupsize, long astpos, long asize) {
		//debug(vfs_vfile_gc) { import core.stdc.stdio : printf; printf("upkbuf ofs=%u\n", (cast(uint)&upkbuf)-(cast(uint)&this)); }
		if (aupsize > uint.max) aupsize = uint.max;
		if (asize > uint.max) asize = uint.max;
		epl.setup(fl, agflags, astpos, cast(uint)asize, cast(uint)aupsize);
		epleof = eofhit = (aupsize == 0);
		size = aupsize;
		closed = !fl.isOpen;
		static if (XPS.InitUpkBufSize < 0) {
			if (!closed) {
				import core.stdc.stdlib : malloc;
				upkbuf = cast(ubyte*)malloc(-XPS.InitUpkBufSize);
				if (upkbuf is null) throw new VFSException("out of memory");
				upkbufsize = -XPS.InitUpkBufSize;
			}
		}
	}

	@property bool isOpen () const pure nothrow @safe @nogc { return !closed; }
	@property bool eof () const pure nothrow @safe @nogc { return eofhit; }

	void close () {
		if (!closed) {
			//{ import core.stdc.stdio : printf; printf("CLOSED!\n"); }
			epleof = eofhit = true;
			closed = true;
			static if (XPS.InitUpkBufSize <= 0) {
				import core.stdc.stdlib : free;
				if (upkbuf !is null) free(upkbuf);
				upkbuf = null;
				upkbufsize = 0;
			}
			upkbufused = upkbufpos = 0;
			epl.close();
		}
	}

	private void reset () {
		upkbufused = 0;
		epl.reset();
	}

	private ssize doRealRead (void* buf, usize count) {
		if (count == 0) return 0; // the thing that should not be
		if (closed) return -1;
		auto dest = cast(ubyte*)buf;
		auto left = count;
		while (left > 0 && !closed) {
			// use the data that left from the unpacked chunk
			if (upkbufpos < upkbufused) {
				auto pkx = upkbufused-upkbufpos;
				if (pkx > left) pkx = cast(uint)left;
				static if (XPS.InitUpkBufSize <= 0) {
					dest[0..pkx] = upkbuf[upkbufpos..upkbufpos+pkx];
				} else {
					dest[0..pkx] = upkbuf.ptr[upkbufpos..upkbufpos+pkx];
				}
				dest += pkx;
				upkbufpos += pkx;
				left -= pkx;
			} else {
				if (epleof) break;
				// no data in unpacked chunk, request new
				upkbufused = upkbufpos = 0;
				while (upkbufused == 0 && !closed && !epleof) {
					if (!epl.unpackChunk(&putUnpackedByte)) epleof = true;
				}
			}
		}
		return count-left;
	}

	ssize read (void* buf, usize count) {
		if (buf is null) return -1;
		if (count == 0 || size == 0) return 0;
		if (!isOpen) return -1; // read error
		if (count > ssize.max) count = ssize.max;
		if (size >= 0 && pos >= size) { eofhit = true; return 0; } // EOF
		// do we want to seek backward?
		if (prpos > pos) {
			// yes, rewind
			reset();
			epleof = eofhit = (size == 0);
			prpos = 0;
		}
		// do we need to seek forward?
		if (prpos < pos) {
			// yes, skip data
			ubyte[512] tmp = 0;
			while (prpos < pos) {
				auto xrd = pos-prpos;
				auto rd = doRealRead(tmp.ptr, cast(uint)(xrd > tmp.length ? tmp.length : xrd));
				if (rd <= 0) return -1;
				prpos += rd;
			}
			if (prpos != pos) return -1;
		}
		assert(prpos == pos);
		// unpack data
		if (size >= 0 && size-pos < count) {
			count = cast(usize)(size-pos);
			if (count == 0) return 0;
			if (count > ssize.max) count = ssize.max;
		}
		auto rd = doRealRead(buf, count);
		pos = (prpos += rd);
		return rd;
	}

	ssize write (in void* buf, usize count) { return -1; }

	long lseek (long ofs, int origin) {
		if (!isOpen) return -1;
		//TODO: overflow checks
		switch (origin) {
			case Seek.Set: break;
			case Seek.Cur: ofs += pos; break;
			case Seek.End:
				if (ofs > 0) ofs = 0;
				ofs += size;
				break;
			default:
				return -1;
		}
		if (ofs < 0) return -1;
		eofhit = (size >= 0 && ofs >= size);
		pos = ofs;
		return pos;
	}

private:
	void putUnpackedByte (const(ubyte)[] bts...) @trusted {
		//{ import core.stdc.stdio : printf; printf("putUnpackedByte: %u; upkbufused=%u; upkbufsize=%u\n", cast(uint)bts.length, upkbufused, upkbufsize); }
		if (closed) return; // just in case
		foreach (ubyte b; bts[]) {
			static if (XPS.InitUpkBufSize <= 0) {
				if (upkbufused >= upkbufsize) {
					// allocate more memory for unpacked data buffer
					import core.stdc.stdlib : realloc;
					auto newsz = (upkbufsize ? upkbufsize*2 : 256*1024);
					if (newsz <= upkbufsize) throw new Exception("out of memory");
					auto nbuf = cast(ubyte*)realloc(upkbuf, newsz);
					if (!nbuf) throw new Exception("out of memory");
					upkbuf = nbuf;
					upkbufsize = newsz;
					//{ import core.stdc.stdio : printf; printf("  grow: upkbufsize=%u\n", upkbufsize); }
				}
				//assert(upkbufused < upkbufsize);
				//assert(upkbuf);
				upkbuf[upkbufused++] = b;
			} else {
				if (upkbufused >= upkbuf.length) throw new Exception("out of unpack buffer");
				upkbuf.ptr[upkbufused++] = b;
			}
		}
	}
}
