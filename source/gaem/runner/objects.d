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
module gaem.runner.objects;

import gaem.ungmk;
import gaem.runner.strpool;
import gaem.runner.value;
import gaem.runner.sprites;


// ////////////////////////////////////////////////////////////////////////// //
// each instance is registered in all it's parent objects instance lists

private __gshared uint[string] fields;


package(gaem.runner) short allocateFieldId(string name){
	assert(name.length > 0);
	if(auto fpi = name in fields) return cast(short)*fpi;
	auto fid = cast(uint)fields.length;
	if(fid > short.max) assert(0, "too many fields");
	fields[name] = fid;
	return cast(short)fid;
}


private enum PredefinedFields = [
	"object_index", // int
	"id", // int
	"sprite_index", // int
	"sprite_width", // int
	"sprite_height", // int
	"sprite_xoffset", // int
	"sprite_yoffset", // int
	"image_index", // Real
	"image_speed", // Real
	"image_xscale", // Real
	"image_yscale", // Real
	"image_angle", // Real
	"image_alpha", // Real
	"image_blend", // int
	"mask_index", // int
	"depth", // Real
	"x", // Real
	"y", // Real
	"xstart", // Real
	"ystart", // Real
	"xprevious", // Real
	"yprevious", // Real
	"direction", // Real
	"speed", // Real
	"hspeed", // Real
	"vspeed", // Real
	"friction", // Real
	"gravity_direction", // Real
	"gravity", // Real
	"bbox_left", // int
	"bbox_right", // int
	"bbox_top", // int
	"bbox_bottom", // int
	"visible", // int
	"solid", // int
	"persistent", // int
	"alarm0", // int
	"alarm1", // int
	"alarm2", // int
	"alarm3", // int
	"alarm4", // int
	"alarm5", // int
	"alarm6", // int
	"alarm7", // int
	"alarm8", // int
	"alarm9", // int
	"alarm10", // int
	"alarm11", // int
];


// create `fi_xxx` variables
mixin({
	string res;
	foreach(string name; PredefinedFields) res ~= "private __gshared uint fi_"~name~";\n";
	return res;
}());

// create predefined fields
shared static this(){
	mixin({
		string res;
		foreach(string name; PredefinedFields) res ~= "fi_"~name~" = allocateFieldId(`"~name~"`);\n";
		return res;
	}());
}


package(gaem.runner) uint fieldCount(){ pragma(inline, true); return cast(uint)fields.length; }


// ////////////////////////////////////////////////////////////////////////// //
// game object(instance template)
final class ObjectTpl{
	string name;
	ObjectTpl parent; // 0: no parent -- root object
	uint idx; // globally unique index(should never be zero)
	uint sprite_index;
	uint mask_index;
	bool solid;
	bool visible;
	int depth;
	bool persistent;
	
	uint[][GMEvent.Type.max+1] events; //TODO
	
	InstList ilist; // all instances with this object in parent chain
}

private __gshared ObjectTpl[] objects; // 0 is unused
private __gshared ObjectTpl[string] objByNameMap;
shared static this(){ objects.length = 1; }

enum ObjIdAll = -666;


// ////////////////////////////////////////////////////////////////////////// //
public void createObjects(Gmk gmk){
	gmk.forEachObject((o){
		assert(o !is null);
		if(o.name.length == 0) assert(0, "can't register nameless object");
		if(o.name in objByNameMap) assert(0, "object '"~o.name~"' already registered");
		auto tpl = new ObjectTpl();
		tpl.name = o.name;
		tpl.idx = (o.parentobjidx >= 0 ? o.parentobjidx: uint.max); // temporarily abuse this field
		tpl.sprite_index = o.spridx;
		tpl.mask_index = o.maskspridx;
		tpl.solid = o.solid;
		tpl.visible = o.visible;
		tpl.depth = o.depth;
		tpl.persistent = o.persistent;
		objByNameMap[o.name] = tpl;
		objects ~= tpl;
		return false;
	});
	
	// now fix parents
	foreach(immutable idx, ObjectTpl tpl; objects[1..$]){
		if(tpl.idx != uint.max){
			auto po = gmk.objByNum(tpl.idx);
			if(po is null) assert(0, "invalid parent for object '"~tpl.name~"'");
			if(auto px = po.name in objByNameMap) tpl.parent = *px; else assert(0, "wtf?!");
		}
		tpl.idx = cast(uint)idx; // fix index
	}
}


// 0: no such object
uint objectByName(const(char)[] name){
	if(auto tpp = name in objByNameMap) return(*tpp).idx;
	return 0;
}


bool validObjectId(uint id){ pragma(inline, true); return(id > 0 && id < objects.length); }


// ////////////////////////////////////////////////////////////////////////// //
// circular double-linked list
struct InstList{
	InstProxy head;
	uint count;
	
	void append(InstProxy o){
		if(o is null) return;
		assert(o.prev is null);
		assert(o.next is null);
		// append to circular list
		if(head is null){
			// list has no items
			head = o;
			o.prev = o.next = o;
		}else if(head.next is head){
			// list has only one item
			o.prev = o.next = head;
			head.prev = head.next = o;
		}else{
			// list has more than one item
			auto tail = head.prev;
			o.prev = tail; // previous is list tail
			o.next = head; // next is list head
			tail.next = o;
			head.prev = o;
		}
		++count;
	}
	
	void remove(InstProxy o){
		if(o is null || o.prev is null) return;
		assert(head !is null);
		// remove from circular list
		if(head.prev is head){
			// list has one item
			assert(head is o);
			head = null;
		}else{
			// list has more than one item
			if(head is o) head = head.next; // deleting head item, move head
			o.prev.next = o.next;
			o.next.prev = o.prev;
		}
		o.prev = o.next = null;
		--count;
	}
}


// proxy for instance lists
private final class InstProxy{
	Instance self;
	InstProxy prev, next;
	ObjectTpl parent;
	
	this(Instance aself, ObjectTpl aparent=null){
		self = aself;
		parent = aparent;
		if(aself !is null){
			if(aparent !is null) aparent.ilist.append(this); else iall.append(this);
		}
	}
	
	void removeFromLists(){
		if(next !is null){
			if(parent !is null) parent.ilist.remove(this); else iall.remove(this);
		}
	}
}


// ////////////////////////////////////////////////////////////////////////// //
private __gshared InstList iall; // all created instances
// single-linked list of all dead instances
private __gshared Instance deadList;



// ////////////////////////////////////////////////////////////////////////// //
final class Instance{
private:
	enum IdStart = 100000;
	 __gshared uint nextid = IdStart;
	 __gshared Instance[uint] instById;
	 __gshared ulong curStep = 0; // used for `with` management
	 
	private:
	Instance deadNext; // in `deadList`
	
	private:
	uint mId;
	ObjectTpl mParent;
	bool mDead;
	InstProxy[] proxies;
	Real[] fields;
	Real[][] farrays; // arrays for fields
	ulong stepMark; // used in `with` management
	
	this(ObjectTpl aparent){
		mId = nextid++;
		proxies ~= new InstProxy(this); // add to list of all instances
		mParent = aparent;
		fields.length = fieldCount;
		fields[] = Value();
		// copy initial fields from parent object
		if(aparent !is null){
			fields.ptr[fi_id] = Value(mId);
			fields.ptr[fi_sprite_index] = Value(aparent.sprite_index);
			fields.ptr[fi_mask_index] = Value(aparent.mask_index);
			fields.ptr[fi_solid] = Value(aparent.solid);
			fields.ptr[fi_visible] = Value(aparent.visible);
			fields.ptr[fi_depth] = Value(aparent.depth);
			fields.ptr[fi_persistent] = Value(aparent.persistent);
		}
		// add to parents' lists
		while(aparent !is null){
			proxies ~= new InstProxy(this, aparent);
			aparent = aparent.parent;
		}
		stepMark = curStep;
	}
	
	public:
	@property uint id() const pure nothrow @safe @nogc{ return mId; }
	
	public:
	Real get(uint fieldindex){
		pragma(inline, true);
		return(fieldindex < fields.length ? fields.ptr[fieldindex]: Value());
	}
	
	Real get(uint fieldindex, uint i0, uint i1=0){
		if(i0 >= 32000 || i1 >= 32000) return Value(); // out of range
		if(fieldindex >= farrays.length) return Value();
		i0 |= i1<<16;
		auto v = farrays.ptr[fieldindex];
		return(i0 < v.length ? v.ptr[i0]: Value());
	}
	
	void set(Real val, uint fieldindex){
		if(fieldindex < fields.length) fields.ptr[fieldindex] = val;
	}
	
	void set(Real val, uint fieldindex, uint i0, uint i1=0){
		if(i0 >= 32000 || i1 >= 32000) return; // out of range
		if(fieldindex >= fields.length) return;
		i0 |= i1<<16;
		if(farrays.length < fieldindex+1) farrays.length = fieldindex+1;
		if(farrays.ptr[fieldindex].length < i0+1) farrays.ptr[fieldindex].length = i0+1;
		farrays.ptr[fieldindex].ptr[i0] = val;
	}
	
	bool isInstanceOf(int objid){
		if(objid == ObjIdAll) return true;
		if(objid <= 0 || objid >= objects.length || mParent is null) return false;
		for(ObjectTpl p = objects.ptr[objid]; p !is null; p = p.parent) if(mParent is p) return true;
		return false;
	}
	
	void kill(){
		if(deadNext is null){
			mDead = true;
			deadNext = deadList;
			deadList = this;
			debug(objlist){ import core.stdc.stdio: printf; printf("* instance %u of type '%.*s' marked as dead\n", mId, cast(uint)mParent.name.length, mParent.name.ptr); }
		}
	}
	
	static:
	// should be called
	void advanceFrame(){
		pragma(inline, true);
		++curStep;
	}
	
	// ////////////////////////////////////////////////////////////////////// //
	// iterators API
	static:
	private static struct Iterator{
		InstProxy head; // starting instance
		InstProxy cur; // current instance
		Instance si; // instance for single-instance iterator
		ulong step;
		uint oldSelf;
	}
	private __gshared Iterator[] iters;
	private __gshared uint itersUsed = 1;
	
	shared static this(){
		iters.length = 1024; // arbitrary number
	}
	
	private uint newIId(){
		pragma(inline, true);
		auto iid = itersUsed++;
		if(iid == iters.length) iters.length += 1024;
		return iid;
	}
	
	// create new iterator, return iid or 0
	uint newIterator(int objid, uint aOldSelf){
		if(itersUsed >= short.max) assert(0, "too many iterators");
		// instance id?
		if(objid >= IdStart){
			if(auto i = cast(uint)objid in instById){
				auto iid = newIId;
				iters.ptr[iid].oldSelf = aOldSelf;
				iters.ptr[iid].si = *i;
				return iid;
			}
			return 0;
		}
		// "all" object?
		if(objid == ObjIdAll){
			if(iall.head is null) return 0; // no instances yet
			auto iid = newIId;
			with(iters.ptr[iid]){ step = curStep++; head = cur = iall.head; oldSelf = aOldSelf; }
			return iid;
		}
		// "none" object?
		if(objid <= 0) return 0;
		if(objid < objects.length){
			// object class
			if(auto proxy = objects.ptr[objid].ilist.head){
				auto iid = newIId;
				with(iters.ptr[iid]){ step = curStep++; head = cur = proxy; oldSelf = aOldSelf; }
				return iid;
			}
		}
		// alas
		return 0;
	}
	
	// returns current object or 0 on completion, move iterator to next object
	uint iteratorNext(uint iid){
		if(iid == 0 || iid >= itersUsed) return 0;
		auto it = &iters.ptr[iid];
		if(it.head is null){
			if(it.si is null) return 0; // dead iterator
			auto res = (!it.si.mDead ? it.si.mId: 0);
			it.si = null;
			return res;
		}
		// normal iterator
		do{
			auto ri = it.cur.self;
			if((it.cur = it.cur.next) is it.head) it.head = it.cur = null;
			if(!ri.mDead && ri.stepMark <= it.step) return ri.mId; // good instance
			// bad instance(either dead, or newborn) move on
		}while(it.cur !is null);
		return 0; // no more instances
	}
	
	uint iteratorOldSelf(uint iid){
		pragma(inline, true);
		return(iid == 0 || iid >= itersUsed ? 0: iters.ptr[iid].oldSelf);
	}
	
	void freeAllIterators(){
		if(itersUsed > 1){
			foreach(ref it; iters[1..itersUsed]){ it.head = it.cur = null; it.si = null; }
			itersUsed = 1;
		}
	}
	
	static:
	// return `true` from delegate to stop
	// will skip dead instances
	Instance forEach(int objid, scope bool delegate(Instance inst) dg){
		assert(dg !is null);
		// instance?
		if(objid >= IdStart){
			if(auto i = cast(uint)objid in instById) return(dg(*i) ? *i: null);
			return null;
		}
		// object?
		InstProxy head, cur;
		if(objid == ObjIdAll){
			head = cur = iall.head;
		}else if(objid > 0 && objid < objects.length){
			if((head = cur = objects.ptr[objid].ilist.head) is null) return null;
		}else{
			return null;
		}
		// go on
		for(;;){
			if(!cur.self.mDead && dg(cur.self)) return cur.self;
			if((cur = cur.next) is head) break;
		}
		return null;
	}
	
	Instance firstInstanceOf(int id){
		if(id >= IdStart){
			if(auto pid = cast(uint)id in instById) return *pid;
		}else if(id > 0 && id < objects.length){
			if(auto px = objects.ptr[id].ilist.head) return px.self;
		}
		return null;
	}
	
	static:
	void freeDeadObjects(){
		while(deadList !is null){
			assert(deadList.mDead);
			foreach(InstProxy px; deadList.proxies) px.removeFromLists();
			// remove from alive instances hash
			// do it here instead of `kill()`, so stored objids will still work until script end
			instById.remove(deadList.mId);
			deadList = deadList.deadNext;
		}
	}
	
	static:
	// this should be called when top-level script execution is complete
	void scriptComplete(){
		freeAllIterators();
		freeDeadObjects();
	}
}

public:
