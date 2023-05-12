const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("../mod.zig").Chunk;

const Item = @import("item.zig").Item;
const Object = @import("object.zig").Object;
const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;

pub const CallFrame = struct {
    function: *Chunk,
    ip: usize,
    depth: usize,
    result: u10,
};

pub const Function = extern union {
    internal: InternalFunction,
    external: ExternFunction,
};

pub const InternalFunction = extern struct { arity: u8, result: bool, body: *Chunk };

pub const ExternFunction = extern struct {
    arity: u8,
    result: bool,
    body: *const fn (args: []Item, result: ?*Item) void,
};

pub const Method = extern union {
    internal: *InternalFunction,
    external: *ExternMethod,
};

pub const ExternMethod = struct {
    arity: u8,
    result: bool,
    body: *const fn (self: *Object, args: []Item, result: ?*Item) void,
};

const METHODS_INITIAL_SIZE: usize = 32;

pub const Methods = struct {
    methods: Dict,

    pub fn create(allocator: Allocator, size: usize) !@This() {
        return @This(){
            .methods = try Dict.create(allocator, size, Text.hash_as_item, Text.eq_as_item),
        };
    }

    pub fn default(allocator: Allocator) !@This() {
        return @This().create(allocator, METHODS_INITIAL_SIZE);
    }

    pub inline fn get(self: *@This(), name: *Text) ?Method {
        const found = self.methods.get(Item.from(Text, name.*));
        if (found == null) {
            return null;
        }
        return found.?.method;
    }

    pub inline fn set(self: *@This(), name: *Text, value: *Method) !bool {
        return self.items.set(Item.from(Text, name), Item.from(Method, value));
    }

    pub inline fn contains(self: *@This(), name: *Text) bool {
        return self.items.contains(Item.from(Text, name));
    }
};
