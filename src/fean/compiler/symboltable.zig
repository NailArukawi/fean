const std = @import("std");
const Allocator = std.mem.Allocator;

const Kind = @import("kindtable.zig").Kind;

pub const SymbolKind = union(enum) {
    none,
    resolved: *Kind,
    unresolved: []const u8,
};

pub const SymbolTable = struct {
    head: ?*Symbol = null,
    tail: ?*Symbol = null,

    pub fn create(allocator: Allocator) !*@This() {
        const this = try allocator.create(@This());
        this.* = .{};
        return this;
    }

    pub fn destroy(this: *@This(), allocator: Allocator) void {
        this.freeKinds(allocator);
        allocator.free(this);
    }

    fn freeSymbols(this: *@This(), allocator: Allocator) void {
        var next = this.head;
        while (next != null) {
            const too_clean = next.?;
            next = too_clean.next;
            allocator.free(too_clean);
        }
    }

    pub fn lookup(this: *@This(), name: []const u8) ?*Symbol {
        var next = this.head;
        while (next != null) {
            if (std.mem.eql(u8, name, next.?.name))
                return next;

            next = next.?.next;
        }

        return null;
    }

    pub fn install(this: *@This(), allocator: Allocator, name: []const u8, kind: SymbolKind, size: usize) !*Symbol {
        const installee = try Symbol.create(allocator, name, kind, size);

        if (this.head == null) {
            this.head = installee;
            this.tail = installee;
        } else {
            this.tail.?.next = installee;
            this.tail = installee;
        }

        return installee;
    }
};

pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind = .none,
    size: usize,
    global: bool = false,
    param: bool = false,
    binding: u10 = 0,
    // none if 0
    literal: usize = 0,
    depth: usize = 0,
    next: ?*@This() = null,

    pub fn create(allocator: Allocator, name: []const u8, kind: SymbolKind, size: usize) !*@This() {
        const this = try allocator.create(@This());
        this.* = .{ .name = name, .kind = kind, .size = size };
        return this;
    }

    pub fn destroy(this: *@This(), allocator: Allocator) void {
        allocator.free(this);
    }

    pub fn stackBinding(this: *@This()) usize {
        return (this.depth * 1024) + this.binding;
    }
};
