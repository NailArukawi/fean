const std = @import("std");
const Map = std.StringHashMap;
const Allocator = std.mem.Allocator;

const Chunk = @import("../mod.zig").Chunk;

const Thread = @import("../mod.zig").Thread;
const Item = @import("item.zig").Item;
const Object = @import("object.zig").Object;
const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;

pub const CallFrame = struct {
    function: *Chunk,
    ip: usize,
    depth: usize,
    base: usize,
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
    body: ExternFunctionBody,
};

pub const Method = extern union {
    internal: InternalFunction,
    external: ExternMethod,
};

pub const ExternMethod = extern struct {
    arity: u8,
    result: bool,
    body: ExternFunctionBody,
};

pub const ExternFunctionBody = *const fn (vm: *Thread, arguments: ExternFunctionArguments, result: ?*Item) callconv(.C) void;
pub const ExternFunctionArguments = extern struct { count: u16, arguments: [*]Item };

const METHODS_INITIAL_SIZE: usize = 32;

pub const Methods = struct {
    methods: Map(*Method),

    pub fn create(allocator: Allocator, size: usize) @This() {
        _ = size;

        return @This(){
            .methods = Map(*Method).init(allocator),
        };
    }

    pub fn default(allocator: Allocator) !@This() {
        return @This().create(allocator, METHODS_INITIAL_SIZE);
    }

    pub inline fn getWithText(self: *@This(), name: *Text) ?Method {
        return self.get(name.as_slice());
    }

    pub inline fn get(self: *@This(), name: []const u8) ?Method {
        const found = self.methods.get(name);
        if (found == null)
            return null;

        return found.?.method;
    }

    pub inline fn setWithText(self: *@This(), name: *Text, value: *Method) !void {
        return self.set(name.as_slice(), value);
    }

    pub inline fn set(self: *@This(), name: []const u8, value: *Method) !void {
        return self.methods.put(name, value);
    }

    pub inline fn contains(self: *@This(), name: *Text) bool {
        return self.items.contains(Item.from(Text, name));
    }
};
