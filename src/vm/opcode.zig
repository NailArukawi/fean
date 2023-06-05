const std = @import("std");

// 124|256 used
pub const Op = enum(u8) {
    // Misc (9)
    no_op = 0,
    ret,
    call,
    call_extern,
    invoke,
    invoke_extern,
    dive,
    ascend,

    // Memory (33)
    load_true,
    load_false,
    load_literal,
    load_literal_obj,
    load_global,
    load_global_obj,
    store_global,
    get_upvalue,
    set_upvalue,
    copy,
    create_struct,
    get_struct_field_u64,
    get_struct_field_u32,
    get_struct_field_u16,
    get_struct_field_u8,
    get_struct_field_i64,
    get_struct_field_i32,
    get_struct_field_i16,
    get_struct_field_i8,
    get_struct_field_f64,
    get_struct_field_f32,
    get_struct_field_obj,
    set_struct_field_u64,
    set_struct_field_u32,
    set_struct_field_u16,
    set_struct_field_u8,
    set_struct_field_i64,
    set_struct_field_i32,
    set_struct_field_i16,
    set_struct_field_i8,
    set_struct_field_f64,
    set_struct_field_f32,
    set_struct_field_obj,

    // Arithmetic (60)
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

    inc_i64,
    inc_u64,
    inc_i32,
    inc_u32,
    inc_i16,
    inc_u16,
    inc_i8,
    inc_u8,

    inc_f64,
    inc_f32,

    dec_i64,
    dec_u64,
    dec_i32,
    dec_u32,
    dec_i16,
    dec_u16,
    dec_i8,
    dec_u8,

    dec_f64,
    dec_f32,

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

    equal_bit,
    equal_f64,
    equal_f32,

    not,

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

    pub inline fn x(self: @This()) u32 {
        return self.args;
    }

    pub inline fn y(self: @This()) u22 {
        return @truncate(u22, self.args >> 10);
    }

    pub inline fn z(self: @This()) u12 {
        return @truncate(u12, self.args >> 20);
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

    pub inline fn set_x(self: *@This(), val: u32) void {
        self.args = val;
    }

    pub inline fn set_y(self: *@This(), val: u22) void {
        self.args = (self.args & 0x3FF) | (@intCast(u32, val) << 10);
    }

    pub inline fn set_z(self: *@This(), val: u12) void {
        self.args = (self.args & 0xFFFFF) | (@intCast(u32, val) << 20);
    }

    pub inline fn new() @This() {
        return @This(){
            .op = .no_op,
            .args = 0,
        };
    }
};
