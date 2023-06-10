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

    pub fn debug(self: *@This(), fn_print: bool) void {
        for (self.code.items, 0..) |op, i| {
            if (fn_print) {
                std.debug.print("\t", .{});
            }
            switch (op.op) {
                .no_op => std.debug.print("[{}]:\t[{s}]\n", .{ i, @tagName(op.op) }),
                .ret => std.debug.print("[{}]:\t[{s}]\t\t({})\n", .{ i, @tagName(op.op), op.a() > 0 }),
                .call => std.debug.print("[{}]:\t[{s}]\t\t(reg[{}]() -> reg[{}])\n", .{ i, @tagName(op.op), op.c(), op.a() }),
                .call_extern => std.debug.print("[{}]:\t[{s}]\t\t(reg[{}]() -> reg[{}])\n", .{ i, @tagName(op.op), op.c(), op.a() }),
                .invoke => unreachable,
                .invoke_extern => unreachable,
                .dive => std.debug.print("[{}]:\t[{s}]\n", .{ i, @tagName(op.op) }),
                .ascend => std.debug.print("[{}]:\t[{s}]\n", .{ i, @tagName(op.op) }),

                // Memory (10)
                .load_true, .load_false, .load_literal, .load_literal_obj => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = const[{}])\n", .{ i, @tagName(op.op), op.a(), op.y() }),
                .load_global, .load_global_obj => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = global[{}])\n", .{ i, @tagName(op.op), op.b(), op.a() }),
                .store_global => std.debug.print("[{}]:\t[{s}]\t(global[{}] = reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b() }),
                .get_upvalue => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = stack[{}])\n", .{ i, @tagName(op.op), op.a(), op.y() }),
                .set_upvalue => std.debug.print("[{}]:\t[{s}]\t(stack[{}] = reg[{}])\n", .{ i, @tagName(op.op), op.y(), op.a() }),
                .copy => std.debug.print("[{}]:\t[{s}]\t\t(reg[{}] = reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b() }),
                .set_struct_field_i64 => std.debug.print("[{}]:\t[{s}]\t\treg[{}].{} = reg[{}]\n", .{ i, @tagName(op.op), op.b(), op.z(), op.a() }),

                // Arithmetic (40)
                .add_u64, .sub_u64, .mul_u64, .div_u64, .add_u32, .sub_u32, .mul_u32, .div_u32, .add_u16, .sub_u16, .mul_u16, .div_u16, .add_u8, .sub_u8, .mul_u8, .div_u8, .add_i64, .sub_i64, .mul_i64, .div_i64, .add_i32, .sub_i32, .mul_i32, .div_i32, .add_i16, .sub_i16, .mul_i16, .div_i16, .add_i8, .sub_i8, .mul_i8, .div_i8, .add_f64, .sub_f64, .mul_f64, .div_f64, .add_f32, .sub_f32, .mul_f32, .div_f32 => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = reg[{}], reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b(), op.c() }),

                .inc_i64, .inc_u64, .inc_i32, .inc_u32, .inc_i16, .inc_u16, .inc_i8, .inc_u8, .inc_f64, .inc_f32 => std.debug.print("[{}]:\t[{s}]\t(stack[{}]++)\n", .{ i, @tagName(op.op), op.x() }),

                .dec_i64, .dec_u64, .dec_i32, .dec_u32, .dec_i16, .dec_u16, .dec_i8, .dec_u8, .dec_f64, .dec_f32 => std.debug.print("[{}]:\t[{s}]\t(stack[{}]--)\n", .{ i, @tagName(op.op), op.x() }),

                // Control flow (21)
                .less_than_u64, .less_than_u32, .less_than_u16, .less_than_u8, .less_than_i64, .less_than_i32, .less_than_i16, .less_than_i8, .less_than_f64, .less_than_f32 => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = reg[{}] < reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b(), op.c() }),

                .less_equal_u64, .less_equal_u32, .less_equal_u16, .less_equal_u8, .less_equal_i64, .less_equal_i32, .less_equal_i16, .less_equal_i8, .less_equal_f64, .less_equal_f32 => std.debug.print("[{}]:\t[{s}]\t(reg[{}] = reg[{}] =< reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b(), op.c() }),

                .equal_bit => unreachable,
                .equal_f64 => unreachable,
                .equal_f32 => unreachable,

                .not => std.debug.print("[{}]:\t[{s}]\t\t(reg[{}] = !reg[{}])\n", .{ i, @tagName(op.op), op.a(), op.b() }),

                .if_jmp => std.debug.print("[{}]:\t[{s}]\t(if(reg[{}]) ip += {})])\n", .{ i, @tagName(op.op), op.a(), @bitCast(i22, op.y()) }),
                .jmp => std.debug.print("[{}]:\t[{s}]\t\t(ip += {})])\n", .{ i, @tagName(op.op), @bitCast(i32, op.x()) }),

                else => std.debug.print("[{}]:\t[{s}]\tunknown\n", .{ i, @tagName(op.op) }),
            }
        }
    }
};
