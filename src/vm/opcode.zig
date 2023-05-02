const std = @import("std");

// 79|256 used
pub const Op = enum(u8) {
    // Misc (7)
    no_op = 0,
    ret,
    call,
    call_extern,
    invoke,
    invoke_extern,
    make_closure,

    // Memory (10)
    load_literal,
    load_literal_obj,
    load_global,
    load_global_obj,
    store_global,
    get_upvalue,
    get_upvalue_obj,
    set_upvalue,
    open_upvalue,
    close_upvalue,

    // Arithmetic (40)
    add_u64,
    sub_u64,
    mul_u64,
    div_u64,
    add_u32,
    sub_u32,
    mul_u32,
    div_u32,
    add_u16,
    sub_u16,
    mul_u16,
    div_u16,
    add_u8,
    sub_u8,
    mul_u8,
    div_u8,

    add_i64,
    sub_i64,
    mul_i64,
    div_i64,
    add_i32,
    sub_i32,
    mul_i32,
    div_i32,
    add_i16,
    sub_i16,
    mul_i16,
    div_i16,
    add_i8,
    sub_i8,
    mul_i8,
    div_i8,

    add_f64,
    sub_f64,
    mul_f64,
    div_f64,

    add_f32,
    sub_f32,
    mul_f32,
    div_f32,

    // Control flow (21)
    less_than_u64,
    less_than_u32,
    less_than_u16,
    less_than_u8,
    less_than_i64,
    less_than_i32,
    less_than_i16,
    less_than_i8,
    less_than_f64,
    less_than_f32,
    less_equal_u64,
    less_equal_u32,
    less_equal_u16,
    less_equal_u8,
    less_equal_i64,
    less_equal_i32,
    less_equal_i16,
    less_equal_i8,
    less_equal_f64,
    less_equal_f32,
    if_jmp,
    jmp,

    // extended (1)
    extended = 255,
};

pub const Opcode = extern struct {
    op: Op align(1),
    args: u32 align(1),

    pub inline fn a(self: @This()) u10 {
        return @truncate(u10, self.args);
    }

    pub inline fn b(self: @This()) u10 {
        return @truncate(u10, self.args >> 10);
    }

    pub inline fn c(self: @This()) u10 {
        return @truncate(u10, self.args >> 20);
    }

    pub inline fn d(self: @This()) u2 {
        return @truncate(u2, self.args >> 30);
    }

    pub inline fn y(self: @This()) u22 {
        return @truncate(u22, self.args >> 10);
    }

    pub inline fn x(self: @This()) u32 {
        return self.args;
    }

    pub inline fn set_a(self: *@This(), val: u10) void {
        self.args = (self.args & 0xFFFFFC00) | val;
    }

    pub inline fn set_b(self: *@This(), val: u10) void {
        self.args = (self.args & 0xFFC03FFF) | (@intCast(u32, val) << 10);
    }

    pub inline fn set_c(self: *@This(), val: u10) void {
        self.args = (self.args & 0x3FFFFF) | (@intCast(u32, val) << 20);
    }

    pub inline fn set_d(self: *@This(), val: u2) void {
        self.args = (self.args & 0x3FFFFFFF) | (@intCast(u32, val) << 30);
    }

    pub inline fn set_y(self: *@This(), val: u22) void {
        self.args = (self.args & 0x3FF) | (@intCast(u32, val) << 10);
    }

    pub inline fn set_x(self: *@This(), val: u32) void {
        self.args = val;
    }

    pub inline fn new() @This() {
        return @This(){
            .op = .no_op,
            .args = 0,
        };
    }

    pub fn debug(self: @This()) void {
        switch (self.op) {
            // Misc (7)
            .no_op => unreachable,
            .ret => std.debug.print("[return]\n", .{}),
            .call => unreachable,
            .call_extern => unreachable,
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,

            // Memory (10)
            .load_literal, .load_literal_obj => {
                std.debug.print("[{}]:\t{d:4}, {}\n", .{ self.op, self.a(), self.y() });
            },
            .load_global, .load_global_obj, .store_global => {
                std.debug.print("[{}]:\t{d:4}, {}\n", .{ self.op, self.a(), self.y() });
            },
            .get_upvalue => unreachable,
            .get_upvalue_obj => unreachable,
            .set_upvalue => unreachable,
            .open_upvalue => unreachable,
            .close_upvalue => unreachable,

            .add_u64, .add_u32, .add_u16, .add_u8, .add_i64, .add_i32, .add_i16, .add_i8, .add_f64, .add_f32 => {
                std.debug.print("[{}]:\t {d:4} = {d:4} + {d:4}\n", .{ self.op, self.a(), self.b(), self.c() });
            },

            // Control flow (21)
            .less_than_u64 => unreachable,
            .less_than_u32 => unreachable,
            .less_than_u16 => unreachable,
            .less_than_u8 => unreachable,
            .less_than_i64 => unreachable,
            .less_than_i32 => unreachable,
            .less_than_i16 => unreachable,
            .less_than_i8 => unreachable,
            .less_than_f64 => unreachable,
            .less_than_f32 => unreachable,
            .less_equal_u64 => unreachable,
            .less_equal_u32 => unreachable,
            .less_equal_u16 => unreachable,
            .less_equal_u8 => unreachable,
            .less_equal_i64 => unreachable,
            .less_equal_i32 => unreachable,
            .less_equal_i16 => unreachable,
            .less_equal_i8 => unreachable,
            .less_equal_f64 => unreachable,
            .less_equal_f32 => unreachable,
            .if_jmp => unreachable,
            .jmp => unreachable,
            else => unreachable,
        }
    }
};
