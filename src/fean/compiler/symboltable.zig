// a rather direct implimentaition of http://silcnitc.github.io/Data_Structures.html
const std = @import("std");
const Allocator = std.mem.Allocator;

const Kind = @import("kindtable.zig").Kind;

pub const SymbolKind = union(enum) {
    resolved: Kind,
    unresolved: []const u8,
};

pub const Symbol = *SymbolTable;
pub const SymbolTable = struct {
    name: []const u8,
    kind: ?SymbolKind,
    size: usize,
    global: bool = false,
    param: bool = false,
    binding: u10 = 0,
    depth: usize = 0,
    next: ?*@This(),

    fn create(allocator: Allocator) !*@This() {
        var result = try allocator.create(@This());

        result.size = 0;
        result.next = null;
        result.kind = null;

        return result;
    }

    pub fn lookup(self: *@This(), name: []const u8) ?*@This() {
        var cursor: ?Symbol = self;
        while (cursor != null) : (cursor = cursor.?.next) {
            if (std.mem.eql(u8, cursor.?.name, name)) {
                return cursor;
            }
        }
        return null;
    }

    pub fn install(self: *@This(), allocator: Allocator, name: []const u8, kind: ?SymbolKind, size: usize) !*@This() {
        const installee = try allocator.create(@This());
        installee.name = name;
        installee.kind = kind;
        installee.size = size;
        installee.next = self.next;
        installee.global = false;
        installee.param = false;
        installee.binding = 0;
        installee.depth = 0;
        self.next = installee;
        return installee;
    }

    pub fn create_head(allocator: Allocator, name: []const u8, kind: ?SymbolKind, size: usize) !*@This() {
        const installee = try allocator.create(@This());
        installee.name = name;
        installee.kind = kind;
        installee.size = size;
        installee.next = null;
        installee.global = false;
        installee.param = false;
        installee.binding = 0;
        installee.depth = 0;
        return installee;
    }

    pub fn stack_binding(self: *@This()) usize {
        return (self.depth * 1024) + self.binding;
    }
};
