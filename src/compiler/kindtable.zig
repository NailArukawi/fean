// a rather direct implementation of http://silcnitc.github.io/Data_Structures.html
const std = @import("std");

const vm = @import("../vm/mod.zig");
const Allocator = std.mem.Allocator;

pub const Kind = *KindTable;
pub const KindTable = struct {
    name: []const u8,
    // does a type need more than ~4GiB?
    size: usize,
    fields: ?*FieldList,
    next: ?*@This(),

    pub fn create(allocator: Allocator) !*@This() {
        var self = try allocator.create(@This());

        // default data
        self.default();

        return self;
    }

    pub fn create_global(allocator: Allocator) !*@This() {
        var self = try allocator.create(@This());

        // default data
        self.default();

        const Dict = @import("../vm/mod.zig").Dict;
        const Text = @import("../vm/mod.zig").Text;

        // add builtin types
        _ = try self.install("i64", null, 8, allocator);
        _ = try self.install("i32", null, 4, allocator);
        _ = try self.install("i16", null, 2, allocator);
        _ = try self.install("i8", null, 1, allocator);
        _ = try self.install("u64", null, 8, allocator);
        _ = try self.install("u32", null, 4, allocator);
        _ = try self.install("u16", null, 2, allocator);
        _ = try self.install("u8", null, 1, allocator);
        _ = try self.install("f64", null, 8, allocator);
        _ = try self.install("f32", null, 4, allocator);
        _ = try self.install("dict", null, @sizeOf(Dict), allocator);
        _ = try self.install("text", null, @sizeOf(Text), allocator);
        _ = try self.install("bool", null, @sizeOf(bool), allocator);
        _ = try self.install("Fn", null, @sizeOf(vm.Function), allocator);
        _ = try self.install("ExternFn", null, @sizeOf(vm.Function), allocator);
        _ = try self.install("void", null, @sizeOf(void), allocator);

        return self;
    }

    fn default(self: *@This()) void {
        self.size = 0;
        self.fields = null;
        self.next = null;
    }

    pub fn install(self: *@This(), name: []const u8, fields: ?*FieldList, size: usize, allocator: Allocator) !*@This() {
        var cursor = self;
        while (cursor.next != null) {
            cursor = cursor.next.?;
        }

        var installee = try allocator.create(@This());
        installee.default();
        installee.name = name;
        installee.fields = fields;
        installee.size = size;

        cursor.next = installee;
        return installee;
    }

    pub fn lookup(self: *@This(), name: []const u8) ?*@This() {
        var cursor = self;
        while (cursor.next != null and !std.mem.eql(u8, cursor.name, name)) {
            cursor = cursor.next.?;
        }

        if (std.mem.eql(u8, cursor.name, name)) {
            return cursor;
        } else {
            return null;
        }
    }

    pub fn last(self: *@This()) *@This() {
        var cursor = self;
        while (cursor.next != null) {
            cursor = cursor.next.?;
        }
        return cursor;
    }
};

pub const FieldList = struct {
    name: []const u8,
    kind: ?*KindTable,
    next: ?*@This(),

    pub fn install(self: *@This(), name: []const u8, kind: ?*@This(), allocator: Allocator) !void {
        var installee = try allocator.create(@This());
        installee.name = name;
        installee.kind = kind;
        installee.next = null;

        self.next = installee;
    }

    pub fn lookup(self: *@This(), name: []const u8) ?*@This() {
        var cursor = self;
        while (cursor.next != null and !std.mem.eql([]const u8, cursor.name, name)) {
            cursor = cursor.next.?;
        }

        if (std.mem.eql([]const u8, cursor.name, name)) {
            return cursor;
        } else {
            return null;
        }
    }
};
