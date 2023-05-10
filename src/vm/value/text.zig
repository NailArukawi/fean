const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Heap = @import("../mod.zig").Heap;
const Ref = @import("../mod.zig").Ref;
const Object = @import("../mod.zig").Object;

const Item = @import("item.zig").Item;

pub const DEBUG = std.debug.runtime_safety;

pub const DEFAULT_TEXT_SIZE: usize = 32;

pub const Text = struct {
    this: *Ref,
    string: *Ref,
    capacity: usize,
    len: usize,

    pub fn create(heap: *Heap, byte_size: usize) !*Ref {
        var result = try heap.alloc_object(@sizeOf(@This()));

        result.object().* = @This(){
            .feanptr = result,
            .string = try heap.alloc(@sizeOf(u8) * byte_size),
            .capacity = byte_size,
            .len = 0,
        };

        return result;
    }

    pub fn create_const(allocator: Allocator, len: usize, string: *Ref) !*Ref {
        var object = try allocator.create(Object);
        var object_ptr = try Ref.create(allocator, @ptrToInt(object));

        var result = try allocator.create(@This());
        var ptr = try Ref.create(allocator, @ptrToInt(result));

        object.body = ptr;

        result.string = string;
        result.capacity = len;
        result.len = len;

        return object_ptr;
    }

    pub fn default(heap: *Heap) !*@This() {
        return @This().create(heap, DEFAULT_TEXT_SIZE);
    }

    pub fn from_slice(heap: *Heap, slice: []const u8) !*@This() {
        const text = try @This().create(heap, slice.len);
        const string = text.bytes();
        mem.copy(u8, string[0..slice.len], slice);
        text.len = slice.len;
        return text;
    }

    pub fn hash(self: *@This()) u64 {
        const string = self.bytes();
        var step: usize = 1;
        if (self.len >= 16) {
            step = 2;
        }

        var hash_result: u64 = 14695981039346656037;
        var i: usize = 0;

        // TODO idk if this is faster
        while (i < self.len) : (i += step) {
            hash_result ^= string[i];
            if (DEBUG) {
                hash_result = @mulWithOverflow(hash_result, 1099511628211)[0];
            } else {
                hash_result = hash_result * 1099511628211;
            }
        }

        return hash_result;
    }

    pub fn hash_as_item(self: Item) u64 {
        return @This().hash(self.text());
    }

    pub fn eq(self: *@This(), other: *@This()) bool {
        if (self.len != other.len) {
            return false;
        }

        const string_self = self.bytes();
        const string_other = other.bytes();
        return mem.eql(u8, string_self[0..self.len], string_other[0..other.len]);
    }

    pub fn eq_slice(self: *@This(), other: []const u8) bool {
        const string_self = self.bytes();
        return mem.eql(u8, string_self[0..self.len], other[0..other.len]);
    }

    pub fn eq_as_item(self: Item, other: Item) bool {
        return @This().eq(self.text(), other.text());
    }

    pub inline fn bytes(self: *@This()) [*]u8 {
        return self.string.resolve([*]u8);
    }

    pub inline fn as_slice(self: *@This()) []u8 {
        return self.string.resolve([*]u8)[0..self.len];
    }
};
