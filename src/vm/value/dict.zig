const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../mod.zig").Heap;
const Ref = @import("../mod.zig").Ref;

const Item = @import("item.zig").Item;

pub const DEFAULT_DICT_SIZE: usize = 64;
pub const DICT_LOAD_CAPACITY: f32 = 0.75;
pub const DICT_GROW_FACTOR: f32 = 2;

pub const DictEntry = struct {
    key: ?Item,
    value: Item,
    tomb: bool,
};

pub const Dict = struct {
    hash_fn: *const fn (Item) u64,
    eq_fn: *const fn (Item, Item) bool,

    items: *Ref,
    capacity: usize,
    count: usize,
    heap: *Heap,

    pub fn create(heap: *Heap, size: usize, hash_fn: *const fn (Item) u64, eq_fn: *const fn (Item, Item) bool) !*Ref {
        var result = try heap.alloc_object(@sizeOf(@This()));
        var items = try heap.alloc(@sizeOf(DictEntry) * size);

        var this = result.item.resolve(*@This());
        this.* = @This(){
            .hash_fn = hash_fn,
            .eq_fn = eq_fn,

            .items = items,
            .capacity = size,
            .count = 0,
            .heap = heap,
        };

        var entry = this.entries();
        var i: usize = 0;
        while (i < size) {
            entry[i].tomb = false;
            entry[i].key = null;
            entry[i].value = Item.default();
            i += 1;
        }

        std.debug.print("items: {}\n", .{result.item.resolve(*@This())});
        std.debug.print("itemsobj: {}\n", .{result.obj.object()});

        return result.obj;
    }

    pub fn default(heap: *Heap, hash_fn: *const fn (Item) u64, eq_fn: *const fn (Item, Item) bool) !*Ref {
        return @This().create(heap, DEFAULT_DICT_SIZE, hash_fn, eq_fn);
    }

    pub fn set(self: *@This(), key: Item, value: Item) !bool {
        const max_load = @intToFloat(f32, self.capacity) * DICT_LOAD_CAPACITY;
        if (self.count + 1 > @floatToInt(usize, max_load)) {
            try self.grow();
        }

        const entry = self.lookup(key);
        const is_new = entry.key == null;
        if (is_new and !entry.tomb) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;
        entry.tomb = false;
        return is_new;
    }

    pub fn grow(self: *@This()) !void {
        const new_size = @floatToInt(usize, @intToFloat(f32, self.capacity) * DICT_GROW_FACTOR);
        var new_dict = try Dict.create(self.heap, new_size, self.hash_fn, self.eq_fn);

        var d = new_dict.object().dict();
        d.hash_fn = self.hash_fn;
        d.eq_fn = self.eq_fn;

        try d.add_all(self);
        self.items = d.items;
        self.count = d.count;
    }

    pub fn add_all(self: *@This(), from: *@This()) anyerror!void {
        var i: usize = 0;
        while (i < from.capacity) {
            const entry = from.items.resolve_array(DictEntry)[i];
            if (entry.key != null and !entry.tomb) {
                _ = try self.set(entry.key.?, entry.value);
            }
            i += 1;
        }
    }

    pub fn lookup(self: *@This(), key: Item) *DictEntry {
        var index = self.hash_fn(key) & (self.capacity - 1);
        var tombstone: ?*DictEntry = null;

        while (true) {
            var entry = &self.items.resolve_array(DictEntry)[index];
            if (entry.key == null) {
                if (entry.tomb) {
                    tombstone = entry;
                } else {
                    if (tombstone != null) {
                        return tombstone.?;
                    } else {
                        return entry;
                    }
                }
            } else if (self.eq_fn(key, entry.key.?)) {
                return entry;
            }

            index = (index + 1) & (self.capacity - 1);
        }
    }

    pub fn delete(self: *@This(), key: Item) bool {
        if (self.count == 0) {
            return false;
        }

        var entry = self.lookup(key);
        if (entry.key == null) {
            return false;
        }

        entry.key = null;
        entry.value = Item.default();
        entry.tomb = true;
        return true;
    }

    pub fn contains(self: *@This(), key: Item) bool {
        return self.lookup(key).key != null;
    }

    pub fn get(self: *@This(), key: Item) ?Item {
        const result = self.lookup(key);
        if (result.key == null) {
            return null;
        }
        return result.value;
    }

    pub fn entries(self: *@This()) [*]DictEntry {
        return self.items.resolve([*]DictEntry);
    }
};
