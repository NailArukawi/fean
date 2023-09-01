const std = @import("std");
const Allocator = std.mem.Allocator;

const runtime = @import("../runtime/mod.zig");
const Methods = @import("../runtime/value/function.zig").Methods;

pub const KindMeta = struct {
    is_fn: bool = false,
    is_extern_fn: bool = false,
};

pub const FN_SIZE: usize = @sizeOf(runtime.Function);
pub const EXTERN_FN_SIZE: usize = @sizeOf(runtime.Function);
// if you see a kind with this size, something has gone wrong lol.
pub const UNSET_SIZE: usize = std.math.maxInt(usize);

pub const KindTable = struct {
    head: ?*Kind = null,
    tail: ?*Kind = null,

    pub fn create(allocator: Allocator) !*@This() {
        var this = try allocator.create(@This());
        this.* = .{};
        return this;
    }

    pub fn destroy(this: *@This(), allocator: Allocator) void {
        this.freeKinds(allocator);
        allocator.free(this);
    }

    fn freeKinds(this: *@This(), allocator: Allocator) void {
        var next = this.head;
        while (next != null) {
            const too_clean = next.?;
            next = too_clean.next;
            allocator.free(too_clean);
        }
    }

    pub fn createGlobal(allocator: Allocator) !*@This() {
        var this = try @This().create(allocator);

        // add builtin types
        _ = try this.install(allocator, "i64", null, @sizeOf(i64));
        _ = try this.install(allocator, "i32", null, @sizeOf(i32));
        _ = try this.install(allocator, "i16", null, @sizeOf(i16));
        _ = try this.install(allocator, "i8", null, @sizeOf(i8));
        _ = try this.install(allocator, "u64", null, @sizeOf(u64));
        _ = try this.install(allocator, "u32", null, @sizeOf(u32));
        _ = try this.install(allocator, "u16", null, @sizeOf(u16));
        _ = try this.install(allocator, "u8", null, @sizeOf(i64));
        _ = try this.install(allocator, "f64", null, @sizeOf(f64));
        _ = try this.install(allocator, "f32", null, @sizeOf(f32));
        _ = try this.install(allocator, "void", null, @sizeOf(void));
        _ = try this.install(allocator, "bool", null, @sizeOf(bool));

        return this;
    }

    pub fn install(this: *@This(), allocator: Allocator, name: []const u8, fields: ?FieldList, size: ?usize) !*Kind {
        var installee = try Kind.create(allocator, name, size orelse UNSET_SIZE);
        installee.fields = fields orelse .{};

        if (this.head == null) {
            this.head = installee;
            this.tail = installee;
        } else {
            this.tail.?.next = installee;
            this.tail = installee;
        }

        return installee;
    }

    pub fn install_fn(self: *@This(), allocator: Allocator, name: []const u8, fields: ?FieldList) !*Kind {
        var installed = try self.install(allocator, name, fields orelse .{}, FN_SIZE);
        installed.meta.is_fn = true;
        return installed;
    }

    pub fn install_extern_fn(self: *@This(), allocator: Allocator, name: []const u8, fields: ?FieldList) !*Kind {
        var installed = try self.install(allocator, name, fields orelse .{}, EXTERN_FN_SIZE);
        installed.meta.is_extern_fn = true;
        return installed;
    }

    pub fn lookup(this: *@This(), name: []const u8) ?*Kind {
        var next = this.head;
        while (next != null) {
            if (std.mem.eql(u8, name, next.?.name))
                return next;

            next = next.?.next;
        }

        return null;
    }
};

pub const Kind = struct {
    name: []const u8,
    size: usize,
    is_struct: bool = false,
    fields: FieldList = .{},
    methods: ?Methods = null,
    meta: KindMeta = .{},
    next: ?*@This() = null,

    pub fn create(allocator: Allocator, name: []const u8, size: usize) !*@This() {
        var this = try allocator.create(@This());
        this.* = .{ .name = name, .size = size };
        return this;
    }

    pub fn install_field(self: *@This(), allocator: Allocator, name: []const u8, kind: *Kind) !*Field {
        return self.fields.install(allocator, name, kind);
    }

    pub fn lookup_field(self: *@This(), name: []const u8) ?*Field {
        return self.fields.lookup(name);
    }

    pub fn destroy(this: *@This(), allocator: Allocator) void {
        if (this.fields != null)
            this.fields.?.freeFields(allocator);

        allocator.free(this);
    }
};

pub const FieldList = struct {
    head: ?*Field = null,
    tail: ?*Field = null,

    pub fn install(this: *@This(), allocator: Allocator, name: []const u8, kind: *Kind) !*Field {
        var index: usize = 0;
        if (this.head != null)
            index = this.head.?.index + 1;

        var installee = try Field.create(
            allocator,
            name,
            kind,
            index,
        );

        if (this.head == null) {
            this.head = installee;
            this.tail = installee;
        } else {
            this.tail.?.next = installee;
            this.tail = installee;
        }

        return installee;
    }

    pub fn lookup(self: *@This(), name: []const u8) ?*Field {
        var next = self.head;
        while (next != null) {
            if (std.mem.eql(u8, name, next.?.name))
                return next;

            next = next.?.next;
        }

        return null;
    }

    fn destroy(this: *@This(), allocator: Allocator) void {
        var next = this.head;
        while (next != null) {
            const too_clean = next.?;
            next = too_clean.next;
            allocator.free(too_clean);
        }
    }
};

pub const Field = struct {
    name: []const u8,
    kind: *Kind,
    index: usize,
    // (leftshift) or (1 if 0)
    alignment: u2 = 0,
    padding: u2 = 0,
    next: ?*@This() = null,

    pub fn create(allocator: Allocator, name: []const u8, kind: *Kind, index: usize) !*@This() {
        var createe = try allocator.create(@This());
        createe.* = .{ .name = name, .kind = kind, .index = index };
        return createe;
    }

    pub fn destroy(this: *@This(), allocator: Allocator) void {
        allocator.free(this);
    }
};
