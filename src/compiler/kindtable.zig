// a rather direct implementation of http://silcnitc.github.io/Data_Structures.html
const std = @import("std");

const vm = @import("../vm/mod.zig");
const Allocator = std.mem.Allocator;
const Methods = @import("../vm/value/function.zig");

pub const KindMeta = struct {
    is_fn: bool,
    is_extern_fn: bool,
};

pub const FN_SIZE: usize = @sizeOf(vm.Function);
pub const EXTERN_FN_SIZE: usize = @sizeOf(vm.Function);

pub const Kind = *KindTable;
pub const Field = *FieldList;
pub const KindTable = struct {
    name: []const u8,
    size: usize = std.math.maxInt(usize),
    fields: ?*FieldList,
    methods: ?Methods,
    meta: KindMeta,
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
        _ = try self.install("i64", null, @sizeOf(i64), allocator);
        _ = try self.install("i32", null, @sizeOf(i32), allocator);
        _ = try self.install("i16", null, @sizeOf(i16), allocator);
        _ = try self.install("i8", null, @sizeOf(i8), allocator);
        _ = try self.install("u64", null, @sizeOf(u64), allocator);
        _ = try self.install("u32", null, @sizeOf(u32), allocator);
        _ = try self.install("u16", null, @sizeOf(u16), allocator);
        _ = try self.install("u8", null, @sizeOf(i64), allocator);
        _ = try self.install("f64", null, @sizeOf(f64), allocator);
        _ = try self.install("f32", null, @sizeOf(f32), allocator);
        _ = try self.install("void", null, @sizeOf(void), allocator);
        _ = try self.install("bool", null, @sizeOf(bool), allocator);
        _ = try self.install("Dict", null, @sizeOf(Dict), allocator);
        _ = try self.install("Text", null, @sizeOf(Text), allocator);
        _ = try self.install("Fn", null, FN_SIZE, allocator);
        _ = try self.install("ExternFn", null, EXTERN_FN_SIZE, allocator);

        return self;
    }

    inline fn default(self: *@This()) void {
        self.size = std.math.maxInt(usize);
        self.fields = null;
        self.methods = null;
        self.meta = KindMeta{
            .is_fn = false,
            .is_extern_fn = false,
        };
        self.next = null;
    }

    pub fn install(self: *@This(), name: []const u8, fields: ?*FieldList, size: usize, allocator: Allocator) !*@This() {
        var installee = try allocator.create(@This());
        installee.default();
        installee.name = name;
        installee.fields = fields;
        installee.size = size;
        installee.next = self.next;

        self.next = installee;
        return installee;
    }

    pub fn install_fn(self: *@This(), name: []const u8, fields: ?*FieldList, allocator: Allocator) !*@This() {
        var installed = try self.install(name, fields, FN_SIZE, allocator);
        installed.meta.is_fn = true;
        return installed;
    }

    pub fn install_extern_fn(self: *@This(), name: []const u8, fields: ?*FieldList, allocator: Allocator) !*@This() {
        var installed = try self.install(name, fields, EXTERN_FN_SIZE, allocator);
        installed.meta.is_extern_fn = true;
        return installed;
    }

    pub fn install_field(self: *@This(), name: []const u8, kind: Kind, allocator: Allocator) !Field {
        if (self.fields == null) {
            self.fields = try FieldList.create(name, kind, allocator);
        } else {
            return self.fields.?.install(name, kind, allocator);
        }
        return self.fields.?;
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
    kind: Kind,
    // (leftshift) or (1 if 0)
    alignment: u2 = 0,
    padding: u2 = 0,
    next: ?*@This(),

    pub fn create(name: []const u8, kind: Kind, allocator: Allocator) !*FieldList {
        var installee = try allocator.create(@This());
        installee.name = name;
        installee.kind = kind;
        installee.alignment = 0;
        installee.padding = 0;
        installee.next = null;

        return installee;
    }

    pub fn install(self: *@This(), name: []const u8, kind: Kind, allocator: Allocator) !*@This() {
        var cursor = self;
        while (cursor.next != null) {
            cursor = cursor.next.?;
        }

        var installee = try allocator.create(@This());
        installee.name = name;
        installee.kind = kind;
        installee.next = null;

        cursor.next = installee;
        return installee;
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
