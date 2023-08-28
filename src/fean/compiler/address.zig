const std = @import("std");

const mod = @import("mod.zig");
const IRBlock = mod.IRBlock;
const Symbol = mod.Symbol;
const Kind = mod.Kind;
const Function = @import("../vm/mod.zig").Function;

pub const AddressKind = enum(u8) {
    not_set,
    register,
    literal,
    upvalue,
    global,
    temporary,
    pair,
    field,
    impl_target,

    extra,
};

pub const Extra = enum(u56) {
    detach,
    copy_if_move,
    set_mode,
};

pub const Address = struct {
    inner: usize,

    pub inline fn new_not_set() @This() {
        return @This(){
            .inner = @as(usize, @intCast(0)) | (@as(usize, @intCast(@intFromEnum(AddressKind.not_set))) << 56),
        };
    }

    pub inline fn new_literal(address: u56) @This() {
        return @This(){
            .inner = @as(usize, @intCast(address)) | (@as(usize, @intCast(@intFromEnum(AddressKind.literal))) << 56),
        };
    }

    pub inline fn new_upvalue(address: u56) @This() {
        return @This(){
            .inner = @as(usize, @intCast(address)) | (@as(usize, @intCast(@intFromEnum(AddressKind.upvalue))) << 56),
        };
    }

    pub inline fn new_global(symbol: Symbol) @This() {
        return @This(){
            .inner = @intFromPtr(symbol) | (@as(usize, @intCast(@intFromEnum(AddressKind.upvalue))) << 56),
        };
    }

    pub inline fn new_temporary(address: u10) @This() {
        return @This(){
            .inner = @as(usize, @intCast(address)) | (@as(usize, @intCast(@intFromEnum(AddressKind.temporary))) << 56),
        };
    }

    pub inline fn new_register(address: u10) @This() {
        return @This(){
            .inner = @as(usize, @intCast(address)) | (@as(usize, @intCast(@intFromEnum(AddressKind.register))) << 56),
        };
    }

    pub inline fn new_pair(id: u56) @This() {
        return @This(){
            .inner = @as(usize, @intCast(id)) | (@as(usize, @intCast(@intFromEnum(AddressKind.pair))) << 56),
        };
    }

    pub inline fn new_field(address: u56) @This() {
        return @This(){
            .inner = @as(usize, @intCast(address)) | (@as(usize, @intCast(@intFromEnum(AddressKind.field))) << 56),
        };
    }

    pub inline fn new_impl_target(address: *Kind) @This() {
        return @This(){
            .inner = @intFromPtr(address) | (@as(usize, @intCast(@intFromEnum(AddressKind.impl_target))) << 56),
        };
    }

    pub inline fn new_extra(e: Extra) @This() {
        return @This(){
            .inner = @intFromEnum(e) | (@as(usize, @intCast(@intFromEnum(AddressKind.extra))) << 56),
        };
    }

    // unsafe, use carefully
    pub inline fn new_raw(ptr: usize) @This() {
        return @This(){
            .inner = ptr,
        };
    }

    pub inline fn upvalue(self: @This()) u56 {
        return @as(u56, @truncate(self.inner));
    }

    pub inline fn literal(self: @This()) u56 {
        return @as(u56, @truncate(self.inner));
    }

    pub inline fn global(self: @This()) u56 {
        return @as(u56, @truncate(self.inner));
    }

    pub inline fn temporary(self: @This()) u10 {
        return @as(u10, @truncate(self.inner));
    }

    pub inline fn register(self: @This()) u10 {
        return @as(u10, @truncate(self.inner));
    }

    pub inline fn pair(self: @This()) u56 {
        return @as(u56, @truncate(self.inner));
    }

    pub inline fn kind(self: @This()) AddressKind {
        return @as(AddressKind, @enumFromInt(@as(u8, @truncate(self.inner >> 56))));
    }

    pub inline fn field(self: @This()) u56 {
        return @as(u56, @truncate(self.inner));
    }

    pub inline fn impl_target(self: @This()) *Kind {
        return @as(*Kind, @ptrFromInt(self.inner ^ (0xFFFFFFFFFFFFFF)));
    }

    pub inline fn extra(self: @This()) Extra {
        return @as(Extra, @enumFromInt(@as(u56, @truncate(self.inner))));
    }

    pub inline fn raw(self: @This()) usize {
        return self.inner;
    }

    pub fn debug(self: @This(), buffer: []u8) ![]const u8 {
        const adress_kind = self.kind();

        switch (adress_kind) {
            .not_set => unreachable,
            .register => {
                const reg = self.register();
                return std.fmt.bufPrint(buffer, "reg[{d:4}]", .{reg});
            },
            .literal => {
                const literal_adress = self.literal();
                return std.fmt.bufPrint(buffer, "consts[{}]", .{literal_adress});
            },
            .upvalue => unreachable,
            .global => unreachable,
            .temporary => {
                const reg = self.register();
                return std.fmt.bufPrint(buffer, "tmp[{d:4}]", .{reg});
            },
            .pair => {
                const pair_value = self.literal();
                return std.fmt.bufPrint(buffer, "pair[{}]", .{pair_value});
            },
            else => unreachable,
        }
    }
};

pub const Load = struct {
    result: Address,
    a: Address,
};

pub const Arithmetic = struct {
    result: Address,
    a: Address,
    b: Address,
};

pub const StructAccess = struct {
    reg: Address,
    this: Address,
    index: Address,
};

pub const Instr = union(enum) {
    // Misc
    no_op,
    ret: bool,
    call: struct {
        result: Address,
        arg_start: ?Address,
        callee: Address,
        has_return: bool,
    },
    call_extern: struct {
        result: Address,
        arg_start: ?Address,
        callee: Address,
        has_return: bool,
    },
    invoke,
    invoke_extern,
    make_closure,
    dive,
    ascend,

    // Memory
    load_true: Address,
    load_false: Address,
    load_literal: Load,
    load_literal_obj: Load,
    load_global: Load,
    load_global_obj: Load,
    store_global: Load,
    get_upvalue: Load,
    set_upvalue: Load,
    create_struct: struct {
        result: Address,
        kind: Address,
    },
    copy: Load,
    get_struct_field_u64: StructAccess,
    get_struct_field_u32: StructAccess,
    get_struct_field_u16: StructAccess,
    get_struct_field_u8: StructAccess,
    get_struct_field_i64: StructAccess,
    get_struct_field_i32: StructAccess,
    get_struct_field_i16: StructAccess,
    get_struct_field_i8: StructAccess,
    get_struct_field_f64: StructAccess,
    get_struct_field_f32: StructAccess,
    get_struct_field_obj: StructAccess,
    set_struct_field_u64: StructAccess,
    set_struct_field_u32: StructAccess,
    set_struct_field_u16: StructAccess,
    set_struct_field_u8: StructAccess,
    set_struct_field_i64: StructAccess,
    set_struct_field_i32: StructAccess,
    set_struct_field_i16: StructAccess,
    set_struct_field_i8: StructAccess,
    set_struct_field_f64: StructAccess,
    set_struct_field_f32: StructAccess,
    set_struct_field_obj: StructAccess,

    // Arithmetic
    add_u64: Arithmetic,
    sub_u64: Arithmetic,
    mul_u64: Arithmetic,
    div_u64: Arithmetic,
    add_u32: Arithmetic,
    sub_u32: Arithmetic,
    mul_u32: Arithmetic,
    div_u32: Arithmetic,
    add_u16: Arithmetic,
    sub_u16: Arithmetic,
    mul_u16: Arithmetic,
    div_u16: Arithmetic,
    add_u8: Arithmetic,
    sub_u8: Arithmetic,
    mul_u8: Arithmetic,
    div_u8: Arithmetic,

    add_i64: Arithmetic,
    sub_i64: Arithmetic,
    mul_i64: Arithmetic,
    div_i64: Arithmetic,
    add_i32: Arithmetic,
    sub_i32: Arithmetic,
    mul_i32: Arithmetic,
    div_i32: Arithmetic,
    add_i16: Arithmetic,
    sub_i16: Arithmetic,
    mul_i16: Arithmetic,
    div_i16: Arithmetic,
    add_i8: Arithmetic,
    sub_i8: Arithmetic,
    mul_i8: Arithmetic,
    div_i8: Arithmetic,

    add_f64: Arithmetic,
    sub_f64: Arithmetic,
    mul_f64: Arithmetic,
    div_f64: Arithmetic,

    add_f32: Arithmetic,
    sub_f32: Arithmetic,
    mul_f32: Arithmetic,
    div_f32: Arithmetic,

    inc_i64: Address,
    inc_u64: Address,
    inc_i32: Address,
    inc_u32: Address,
    inc_i16: Address,
    inc_u16: Address,
    inc_i8: Address,
    inc_u8: Address,

    inc_f64: Address,
    inc_f32: Address,

    dec_i64: Address,
    dec_u64: Address,
    dec_i32: Address,
    dec_u32: Address,
    dec_i16: Address,
    dec_u16: Address,
    dec_i8: Address,
    dec_u8: Address,

    dec_f64: Address,
    dec_f32: Address,

    // conditional
    // Control flow (21)
    less_than_u64: Arithmetic,
    less_than_u32: Arithmetic,
    less_than_u16: Arithmetic,
    less_than_u8: Arithmetic,
    less_than_i64: Arithmetic,
    less_than_i32: Arithmetic,
    less_than_i16: Arithmetic,
    less_than_i8: Arithmetic,
    less_than_f64: Arithmetic,
    less_than_f32: Arithmetic,

    less_equal_u64: Arithmetic,
    less_equal_u32: Arithmetic,
    less_equal_u16: Arithmetic,
    less_equal_u8: Arithmetic,
    less_equal_i64: Arithmetic,
    less_equal_i32: Arithmetic,
    less_equal_i16: Arithmetic,
    less_equal_i8: Arithmetic,
    less_equal_f64: Arithmetic,
    less_equal_f32: Arithmetic,

    equal_bit: Arithmetic,
    equal_f64: Arithmetic,
    equal_f32: Arithmetic,

    not: struct {
        source: Address,
        dest: Address,
    },
    if_jmp: struct {
        condition: Address,
        offset: Address,
    },
    jmp: struct {
        offset: Address,
    },

    // meta
    block: *IRBlock,
    destination: Address,
    method_to_assemble: struct {
        // callee object
        result: Address,

        // function objects
        memory: *Function,
        lit: Address,
        // block is an raw adress
        block: ?Address,
        self: Address,
    },
    fn_to_assemble: struct {
        // callee object
        result: Address,

        // function objects
        memory: *Function,
        lit: Address,
        // block is an raw adress
        block: ?Address,
    },
    reserve: struct { nr: u10, reg: bool },
    drop: struct { nr: u10, reg: bool },

    // extended
    extended,

    pub fn debug(self: @This(), buffer: *[512]u8) !?[]const u8 {
        switch (self) {
            .no_op => return "",
            .ret => return try std.fmt.bufPrint(buffer, "return", .{}),
            .call => |c| {
                const fn_pos = c.callee.register();
                const result = c.result.register();
                if (c.arg_start == null) {
                    return try std.fmt.bufPrint(buffer, "call\treg[{}]() -> reg[{}]", .{ fn_pos, result });
                } else {
                    return try std.fmt.bufPrint(buffer, "call\treg[{}](start reg:{}) -> reg[{}]", .{ fn_pos, c.arg_start.?.register(), result });
                }
            },
            .call_extern => |ce| {
                const fn_pos = ce.callee.register();
                const result = ce.result.register();

                if (ce.arg_start == null) {
                    return try std.fmt.bufPrint(buffer, "call\textern reg[{}]() -> reg[{}]", .{ fn_pos, result });
                } else {
                    return try std.fmt.bufPrint(buffer, "call\textern reg[{}](start reg:{}) -> reg[{}]", .{ fn_pos, ce.arg_start.?.register(), result });
                }
            },
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,
            .dive => return try std.fmt.bufPrint(buffer, "dive", .{}),
            .ascend => return try std.fmt.bufPrint(buffer, "ascend", .{}),

            // Memory
            .load_true => |lt| {
                const result = lt.register();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = true", .{ @tagName(self), result });
            },
            .load_false => |lf| {
                const result = lf.register();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = true", .{ @tagName(self), result });
            },
            .load_literal_obj, .load_literal => |l| {
                const result = l.result.register();
                const lit_adress = l.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = consts[{}]", .{ @tagName(self), result, lit_adress });
            },
            .load_global_obj, .load_global => |g| {
                const global_name = g.result.register();
                const result = g.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = Global(\"reg[{}]\")", .{ @tagName(self), result, global_name });
            },
            .store_global => |g| {
                const global_name = g.result.register();
                const value = g.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\tGlobal(\"reg[{}]\") = reg[{}]", .{ @tagName(self), global_name, value });
            },
            .get_upvalue => |gu| {
                const result = gu.result.register();
                const real = gu.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = stack[{}]", .{ @tagName(self), result, real });
            },
            .set_upvalue => |gu| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const real = gu.result.upvalue();
                const source = try gu.a.debug(&nested_buffer);
                return try std.fmt.bufPrint(buffer, "{s}:\tstack[{}] = {s}", .{ @tagName(self), real, source });
            },
            .copy => |cp| {
                const real = cp.result.register();
                const source = cp.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = reg[{}]", .{ @tagName(self), real, source });
            },
            .create_struct => |ct| {
                const result = ct.result.register();
                const kind = ct.kind.register();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = consts[{}]", .{ @tagName(self), result, kind });
            },
            .get_struct_field_u64, .get_struct_field_u32, .get_struct_field_u16, .get_struct_field_u8, .get_struct_field_i64, .get_struct_field_i32, .get_struct_field_i16, .get_struct_field_i8, .get_struct_field_f64, .get_struct_field_f32, .get_struct_field_obj => |get_struct_field| {
                const reg = get_struct_field.reg.register();
                const this = get_struct_field.this.register();
                const index = get_struct_field.index.field();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = reg[{}].{}", .{ @tagName(self), reg, this, index });
            },
            .set_struct_field_u64, .set_struct_field_u32, .set_struct_field_u16, .set_struct_field_u8, .set_struct_field_i64, .set_struct_field_i32, .set_struct_field_i16, .set_struct_field_i8, .set_struct_field_f64, .set_struct_field_f32, .set_struct_field_obj => |set_struct_field| {
                const reg = set_struct_field.reg.register();
                const this = set_struct_field.this.register();
                const index = set_struct_field.index.field();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}].{} = reg[{}]", .{ @tagName(self), this, index, reg });
            },

            // meta
            .block => |b| {
                var i: usize = 0;
                for (b.body.as_slice()) |ir| {
                    const ir_text = try ir.debug(buffer);
                    const is_drop_or_reserve: bool = (ir_text.?.len > 4 and std.mem.eql(u8, "drop:", ir_text.?[0..5])) or (ir_text.?.len > 7 and std.mem.eql(u8, "reserve:", ir_text.?[0..8]));
                    if (is_drop_or_reserve) {
                        std.debug.print("-----:\t<{s}>\n", .{ir_text.?});
                    } else {
                        std.debug.print("   [{}]:\t({s})\n", .{ i, ir_text.? });
                        i += 1;
                    }
                }
                return "Block end";
            },

            // extended
            .extended => unreachable,

            .add_u64, .sub_u64, .mul_u64, .div_u64, .add_u32, .sub_u32, .mul_u32, .div_u32, .add_u16, .sub_u16, .mul_u16, .div_u16, .add_u8, .sub_u8, .mul_u8, .div_u8, .add_i64, .sub_i64, .mul_i64, .div_i64, .add_i32, .sub_i32, .mul_i32, .div_i32, .add_i16, .sub_i16, .mul_i16, .div_i16, .add_i8, .sub_i8, .mul_i8, .div_i8, .add_f64, .sub_f64, .mul_f64, .div_f64, .add_f32, .sub_f32, .mul_f32, .div_f32 => |a| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const result = try a.result.debug(&nested_buffer);
                const adress_a_offset = result.len;
                const adress_a = try a.a.debug(nested_buffer[adress_a_offset..nested_size]);
                const adress_b_offset = result.len + adress_a.len;
                const adress_b = try a.b.debug(nested_buffer[adress_b_offset..nested_size]);

                return try std.fmt.bufPrint(buffer, "{s}:\t{s} = {s}, {s}", .{ @tagName(self), result, adress_a, adress_b });
            },

            .inc_i64, .inc_u64, .inc_i32, .inc_u32, .inc_i16, .inc_u16, .inc_i8, .inc_u8, .inc_f64, .inc_f32 => |to_inc| {
                return try std.fmt.bufPrint(buffer, "{s}:\tstack[{}]++", .{ @tagName(self), to_inc.upvalue() });
            },

            .dec_i64, .dec_u64, .dec_i32, .dec_u32, .dec_i16, .dec_u16, .dec_i8, .dec_u8, .dec_f64, .dec_f32 => |to_dec| {
                return try std.fmt.bufPrint(buffer, "{s}:\tstack[{}]--", .{ @tagName(self), to_dec.upvalue() });
            },

            // Control flow (21)
            .less_than_u64, .less_than_u32, .less_than_u16, .less_than_u8, .less_than_i64, .less_than_i32, .less_than_i16, .less_than_i8, .less_than_f64, .less_than_f32 => |lt| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const result = try lt.result.debug(&nested_buffer);
                const adress_a_offset = result.len;
                const adress_a = try lt.a.debug(nested_buffer[adress_a_offset..nested_size]);
                const adress_b_offset = result.len + adress_a.len;
                const adress_b = try lt.b.debug(nested_buffer[adress_b_offset..nested_size]);

                return try std.fmt.bufPrint(buffer, "{s}:\t{s} = {s} < {s}", .{ @tagName(self), result, adress_a, adress_b });
            },

            .less_equal_u64, .less_equal_u32, .less_equal_u16, .less_equal_u8, .less_equal_i64, .less_equal_i32, .less_equal_i16, .less_equal_i8, .less_equal_f64, .less_equal_f32 => |le| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const result = try le.result.debug(&nested_buffer);
                const adress_a_offset = result.len;
                const adress_a = try le.a.debug(nested_buffer[adress_a_offset..nested_size]);
                const adress_b_offset = result.len + adress_a.len;
                const adress_b = try le.b.debug(nested_buffer[adress_b_offset..nested_size]);

                return try std.fmt.bufPrint(buffer, "{s}:\t{s} = {s} < {s}", .{ @tagName(self), result, adress_a, adress_b });
            },

            .not => |n| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const dest = try n.dest.debug(&nested_buffer);
                const source_offset = dest.len;
                const source = try n.source.debug(nested_buffer[source_offset..nested_size]);
                return try std.fmt.bufPrint(buffer, "{s}:\t{s} = !{s}", .{ @tagName(self), dest, source });
            },

            .if_jmp => |ij| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const condition = try ij.condition.debug(&nested_buffer);
                const offset_offset = condition.len;
                const offset = try ij.offset.debug(nested_buffer[offset_offset..nested_size]);
                return try std.fmt.bufPrint(buffer, "{s}:\tif({s}) jump to {s}", .{ @tagName(self), condition, offset });
            },

            .jmp => |j| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const offset = try j.offset.debug(&nested_buffer);
                return try std.fmt.bufPrint(buffer, "{s}:\tjump to {s}", .{ @tagName(self), offset });
            },

            .destination => |a| {
                const nested_size: usize = 32;
                var nested_buffer: [nested_size]u8 = [_]u8{0} ** nested_size;

                const dest = try a.debug(&nested_buffer);

                return try std.fmt.bufPrint(buffer, "{s}:\t{s}", .{ @tagName(self), dest });
            },
            .fn_to_assemble => return null,
            .reserve => |r| {
                if (r.reg)
                    return try std.fmt.bufPrint(buffer, "{s}:\t reg[{}]", .{ @tagName(self), r.nr });
                return try std.fmt.bufPrint(buffer, "{s}:\t temp[{}]", .{ @tagName(self), r.nr });
            },
            .drop => |d| {
                if (d.reg)
                    return try std.fmt.bufPrint(buffer, "{s}:\t reg[{}]", .{ @tagName(self), d.nr });
                return try std.fmt.bufPrint(buffer, "{s}:\t temp[{}]", .{ @tagName(self), d.nr });
            },
            else => return try std.fmt.bufPrint(buffer, "{s}:\tUNKNOWN", .{@tagName(self)}),
        }
    }
};
