const std = @import("std");
const List = std.ArrayList;

const Item = @import("mod.zig").Item;
const Opcode = @import("mod.zig").Opcode;
const Stack = @import("../stack.zig").Stack;

const Allocator = std.mem.Allocator;

pub const Chunk = struct {
    code: List(Opcode),

    pub fn create(allocator: Allocator) !*@This() {
        var this = try allocator.create(@This());

        this.code = List(Opcode).init(allocator);

        return this;
    }

    pub fn destory(self: *@This()) void {
        const allocator = self.code.allocator;
        self.code.deinit();
        self.literals.deinit();
        allocator.free(self);
    }

    pub inline fn next_op(self: *@This(), ip: *usize) Opcode {
        const result = self.code.items[ip.*];
        ip.* += 1;
        return result;
    }

    pub fn append_op(self: *@This(), op: Opcode) !void {
        try self.code.append(op);
    }
};
