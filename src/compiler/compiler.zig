const std = @import("std");

const Symbol = @import("symboltable.zig").Symbol;
const SymbolTable = @import("symboltable.zig").SymbolTable;
const Kind = @import("kindtable.zig").Kind;
const KindTable = @import("kindtable.zig").KindTable;
const Parser = @import("parser.zig").Parser;
const Resolver = @import("resolver.zig").Resolver;
const AST = @import("parser.zig").AST;
const Node = @import("parser.zig").Node;
const Item = @import("../vm/mod.zig").Item;
const Function = @import("../vm/mod.zig").Function;
const Text = @import("../vm/mod.zig").Text;
const Object = @import("../vm/mod.zig").Object;
const Stack = @import("../stack.zig").Stack;
const heap = @import("../vm/heap.zig");

const FeanConfig = @import("../mod.zig").FeanConfig;

const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const mem = std.mem;

pub const AddressKind = enum(u8) {
    not_set,
    register,
    literal,
    upvalue,
    global,
    temporary,
    pair,

    // extra enums
    detach,
};

pub const Address = struct {
    inner: usize,

    pub inline fn new_not_set() @This() {
        return @This(){
            .inner = @intCast(usize, 0) | (@intCast(usize, @enumToInt(AddressKind.not_set)) << 56),
        };
    }

    pub inline fn new_literal(address: u56) @This() {
        return @This(){
            .inner = @intCast(usize, address) | (@intCast(usize, @enumToInt(AddressKind.literal)) << 56),
        };
    }

    pub inline fn new_upvalue(address: u56) @This() {
        return @This(){
            .inner = @intCast(usize, address) | (@intCast(usize, @enumToInt(AddressKind.upvalue)) << 56),
        };
    }

    pub inline fn new_global(symbol: Symbol) @This() {
        return @This(){
            .inner = @ptrToInt(symbol) | (@intCast(usize, @enumToInt(AddressKind.upvalue)) << 56),
        };
    }

    pub inline fn new_temporary(address: u10) @This() {
        return @This(){
            .inner = @intCast(usize, address) | (@intCast(usize, @enumToInt(AddressKind.temporary)) << 56),
        };
    }

    pub inline fn new_register(address: u10) @This() {
        return @This(){
            .inner = @intCast(usize, address) | (@intCast(usize, @enumToInt(AddressKind.register)) << 56),
        };
    }

    pub inline fn new_pair(id: u56) @This() {
        return @This(){
            .inner = @intCast(usize, id) | (@intCast(usize, @enumToInt(AddressKind.pair)) << 56),
        };
    }

    pub inline fn new_detach() @This() {
        return @This(){
            .inner = @intCast(usize, 0) | (@intCast(usize, @enumToInt(AddressKind.detach)) << 56),
        };
    }

    // unsafe, use carefully
    pub inline fn new_raw(ptr: usize) @This() {
        return @This(){
            .inner = ptr,
        };
    }

    pub inline fn upvalue(self: @This()) u56 {
        return @truncate(u56, self.inner);
    }

    pub inline fn literal(self: @This()) u56 {
        return @truncate(u56, self.inner);
    }

    pub inline fn global(self: @This()) u56 {
        return @truncate(u56, self.inner);
    }

    pub inline fn temporary(self: @This()) u10 {
        return @truncate(u10, self.inner);
    }

    pub inline fn register(self: @This()) u10 {
        return @truncate(u10, self.inner);
    }

    pub inline fn pair(self: @This()) u56 {
        return @truncate(u56, self.inner);
    }

    pub inline fn kind(self: @This()) AddressKind {
        return @intToEnum(AddressKind, @truncate(u8, self.inner >> 56));
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
    copy: Load,

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
    fn_to_assemble: struct {
        // callee object
        result: Address,

        // function objects
        memory: *Function,
        lit: Address,
        // block is an raw adress
        block: ?Address,
    },

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
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = Global(\"reg[{}]\")", .{ @tagName(self), global_name, result });
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

                const real = gu.result.register();
                const source = try gu.a.debug(&nested_buffer);
                return try std.fmt.bufPrint(buffer, "{s}:\tstack[{}] = {s}", .{ @tagName(self), real, source });
            },
            .copy => |cp| {
                const real = cp.result.register();
                const source = cp.a.literal();
                return try std.fmt.bufPrint(buffer, "{s}:\treg[{}] = reg[{}]", .{ @tagName(self), real, source });
            },

            // meta
            .block => |b| {
                for (b.body.as_slice(), 0..) |ir, i| {
                    std.debug.print("   [{}]:\t({s})\n", .{ i, (try ir.debug(buffer)).? });
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

            else => {
                return try std.fmt.bufPrint(buffer, "{s}:\tUNKNOWN", .{@tagName(self)});
            },
        }
    }
};

pub const IRBlock = struct {
    parent: Scope,
    body: Stack(Instr),

    symbols: ?*SymbolTable,

    // meta
    registers: u10 = 0,
    temporaries: u10 = 0,

    inlining: bool = false,

    pub fn create(allocator: Allocator, parent: Scope, symbols: ?*SymbolTable) !*@This() {
        var result = try allocator.create(@This());

        result.* = @This(){
            .parent = parent,
            .body = try Stack(Instr).create(allocator, 32),
            .symbols = symbols,
        };

        return result;
    }

    pub fn destroy(self: *@This()) void {
        self.body.destroy();
    }

    pub fn lookup_symbol(self: *@This(), identifier: []const u8) ?Symbol {
        if (self.symbols == null) {
            return self.parent.lookup_symbol(identifier);
        }
        return self.symbols.?.lookup(identifier) orelse self.parent.lookup_symbol(identifier);
    }

    pub fn get_temp(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023) {
            // we out of registers
            return null;
        } else {
            const temp = 1023 - self.temporaries;
            self.temporaries += 1;

            // return can't be a temp register
            std.debug.assert(temp != 0);
            return Address.new_temporary(temp);
        }
    }

    pub fn get_reg(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023) {
            // we out of registers
            return null;
        } else {
            self.registers += 1;
            return Address.new_register(self.registers);
        }
    }

    pub fn drop_temp(self: *@This()) void {
        std.debug.assert(-1 < (self.temporaries - 1));
        self.temporaries -= 1;
    }

    pub fn drop_temps(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.temporaries - count));
        self.temporaries -= count;
    }

    pub fn drop_reg(self: *@This()) void {
        std.debug.assert(-1 < (self.registers - 1));
        self.registers -= 1;
    }

    pub fn drop_regs(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.registers - count));
        self.registers -= count;
    }
};

pub const Global = struct {
    value: Item,
    object: bool = false,
    symbol: Symbol,
};

pub const IR = struct {
    globals: *Stack(Global),
    globals_offset: *usize,
    body: Stack(Instr),

    symbols: ?*SymbolTable,
    kinds: *KindTable,

    // meta
    registers: u10 = 0,
    temporaries: u10 = 0,

    pub fn create(allocator: Allocator, meta: *CompilerMeta) !*@This() {
        var result = try allocator.create(@This());

        result.globals = &meta.globals;
        result.globals_offset = &meta.globals_offset;
        result.body = try Stack(Instr).create(allocator, 64);
        result.registers = 0;
        result.temporaries = 0;

        return result;
    }

    pub fn lookup_global(self: *@This(), identifier: []const u8) ?Global {
        for (self.globals.as_slice()) |g| {
            const found = mem.eql(u8, g.symbol.name, identifier);
            if (found) {
                return g;
            }
        }

        return null;
    }

    pub fn lookup_symbol(self: *@This(), identifier: []const u8) ?Symbol {
        if (self.symbols == null) {
            return null;
        }
        return self.symbols.?.lookup(identifier);
    }

    pub fn get_temp(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023) {
            // we out of registers
            return null;
        } else {
            const temp = 1023 - self.temporaries;
            self.temporaries += 1;

            // return can't be a temp register
            std.debug.assert(temp != 0);
            return Address.new_temporary(temp);
        }
    }

    pub fn get_reg(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023) {
            // we out of registers
            return null;
        } else {
            self.registers += 1;
            return Address.new_register(self.registers);
        }
    }

    pub fn drop_temp(self: *@This()) void {
        std.debug.assert(-1 < (self.temporaries - 1));
        self.temporaries -= 1;
    }

    pub fn drop_temps(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.temporaries - count));
        self.temporaries -= count;
    }

    pub fn drop_reg(self: *@This()) void {
        std.debug.assert(-1 < (self.registers - 1));
        self.registers -= 1;
    }

    pub fn drop_regs(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.registers - count));
        self.registers -= count;
    }

    pub fn debug(self: *@This(), buffer: *[512]u8, meta: *CompilerMeta) !void {
        std.debug.print("[Const Literals]:\n", .{});
        for (meta.literals_typing.as_slice(), 0..) |lk, i| {
            if (i == 0) {
                continue;
            }
            const item: Item = meta.literals.get(i);

            switch (lk) {
                .u64 => unreachable,
                .u32 => unreachable,
                .u16 => unreachable,
                .u8 => unreachable,
                .i64 => {
                    const too_print = item.i64;
                    std.debug.print("[{} : {s}]: {}\n", .{
                        i,
                        @tagName(lk),
                        too_print,
                    });
                },
                .i32 => unreachable,
                .i16 => unreachable,
                .i8 => unreachable,
                .f64 => {
                    const too_print = item.f64;
                    std.debug.print("[{} : {s}]: {}\n", .{
                        i,
                        @tagName(lk),
                        too_print,
                    });
                },
                .f32 => unreachable,
                .bool => unreachable,
                .Text => {
                    const too_print = item.resolve_object().text();
                    std.debug.print("[{} : {s}]: \"{s}\"\n", .{
                        i,
                        @tagName(lk),
                        too_print.as_slice(),
                    });
                },
                .ExternFn => {
                    const too_print = "a fn"; //item.resolve_object().Fn();
                    std.debug.print("[{} : {s}]: \"{s}\"\n", .{
                        i,
                        @tagName(lk),
                        too_print, //.as_slice(),
                    });
                },
                .InternalFn => {
                    const too_print = "a fn";
                    const function = item.resolve_object().function();
                    std.debug.print("[{} : {s}]: \"{s}\"\n", .{
                        i,
                        @tagName(lk),
                        too_print, //.as_slice(),
                    });
                    std.debug.print("      ↳ fn_body:\n", .{});
                    function.internal.body.debug(true);
                },

                // obhects that are not builtin
                .Object => unreachable,
            }
        }

        std.debug.print("\n[Program]:\n", .{});
        var i: usize = 0;
        for (self.body.as_slice()) |ir| {
            const ir_text = try ir.debug(buffer);
            if (ir_text != null) {
                std.debug.print("[{}]:\t({s})\n", .{ i, ir_text.? });
                i += 1;
            }
        }
    }
};

pub const CompilerMeta = struct {
    globals: Stack(Global),
    globals_offset: usize = 0,
    symbols: ?*SymbolTable,
    kinds: ?*KindTable,
    literals: Stack(Item),
    literal_count: u40 = 0,
    literals_typing: Stack(LitKind),
    objects: Stack(*Object),

    pub fn default(allocator: Allocator) !*@This() {
        var result = try allocator.create(@This());

        result.* = @This(){
            .globals = try Stack(Global).create(allocator, 32),
            .globals_offset = 0,
            .symbols = null,
            .kinds = null,
            .literals = try Stack(Item).create(allocator, 32),
            .literal_count = 0,
            .literals_typing = try Stack(LitKind).create(allocator, 32),
            .objects = try Stack(*Object).create(allocator, 32),
        };

        result.literals.used = 1;
        result.literals_typing.used = 1;

        return result;
    }
};

pub const Scope = union(enum) {
    head: *IR,
    block: *IRBlock,

    pub fn create_head(allocator: Allocator, meta: *CompilerMeta) !@This() {
        var head = try IR.create(allocator, meta);
        return @This(){
            .head = head,
        };
    }

    pub fn create_block(allocator: Allocator, parent: Scope, symbols: ?*SymbolTable) !@This() {
        var block = try IRBlock.create(allocator, parent, symbols);
        return @This(){
            .block = block,
        };
    }

    pub inline fn is_head(self: @This()) bool {
        switch (self) {
            .head => return true,
            .block => return false,
        }
    }

    pub fn param_count(self: @This()) u8 {
        switch (self) {
            .head => return 0,
            .block => |b| {
                var count: u8 = 0;
                var cursor = b.symbols;
                while (cursor != null) {
                    if (cursor.?.param) {
                        count += 1;
                    }

                    cursor = cursor.?.next;
                }

                return count;
            },
        }
    }

    pub fn lookup_symbol(self: @This(), identifier: []const u8) ?Symbol {
        switch (self) {
            .head => |head| return head.lookup_symbol(identifier),
            .block => |block| return block.lookup_symbol(identifier),
        }
    }

    pub fn lookup_global(self: @This(), identifier: []const u8) ?Global {
        switch (self) {
            .head => |head| return head.lookup_global(identifier),
            .block => |block| {
                return block.parent.lookup_global(identifier);
            },
        }
    }

    pub fn push_global(self: @This(), global: Global) !void {
        switch (self) {
            .head => |head| try head.globals.push(global),
            // todo implement
            .block => unreachable,
        }
    }

    pub fn push_instr(self: @This(), instr: Instr) !void {
        switch (self) {
            .head => |head| try head.body.push(instr),
            .block => |block| try block.body.push(instr),
        }
    }

    pub fn get_temp(self: @This()) ?Address {
        switch (self) {
            .head => |head| return head.get_temp(),
            .block => |block| return block.get_temp(),
        }
    }

    pub fn drop_temp(self: @This()) void {
        switch (self) {
            .head => |head| head.drop_temp(),
            .block => |block| block.drop_temp(),
        }
    }

    pub fn drop_temps(self: @This(), count: u10) void {
        switch (self) {
            .head => |head| head.drop_temps(count),
            .block => |block| block.drop_temps(count),
        }
    }

    pub fn get_reg(self: @This()) ?Address {
        switch (self) {
            .head => |head| return head.get_reg(),
            .block => |block| return block.get_reg(),
        }
    }

    pub fn drop_reg(self: @This()) void {
        switch (self) {
            .head => |head| head.drop_reg(),
            .block => |block| block.drop_reg(),
        }
    }

    pub fn drop_regs(self: @This(), count: u10) void {
        switch (self) {
            .head => |head| head.drop_regs(count),
            .block => |block| block.drop_regs(count),
        }
    }

    pub fn temp_count(self: @This()) u10 {
        switch (self) {
            .head => |head| return head.temporaries,
            .block => |block| return block.temporaries,
        }
    }

    pub fn reg_count(self: @This()) u10 {
        switch (self) {
            .head => |head| return head.registers,
            .block => |block| return block.registers,
        }
    }

    pub fn set_reg_count(self: @This(), count: u10) void {
        switch (self) {
            .head => |head| head.registers = count,
            .block => |block| block.registers = count,
        }
    }
};

pub const Compiler = struct {
    allocator: Allocator,
    config: *FeanConfig,
    heap: *heap.Heap,
    literals: *Stack(Item),
    literal_count: *u40,
    literals_typing: *Stack(LitKind),
    objects: *Stack(*Object),
    depth: usize = 0,
    dest: u56 = 0,
    in_function: bool = false,
    function_head_depth: u56 = 0,

    pub fn compile(src: []const u8, config: *FeanConfig, meta: *CompilerMeta) !*IR {
        var ast = try Parser.parse(src, config);

        try Resolver.resolve(&ast, config.allocator);

        var self = @This(){
            .allocator = config.allocator,
            .config = config,
            // todo share it with vm
            .heap = try heap.Heap.create(config.allocator),
            .literals = &meta.literals,
            .literal_count = &meta.literal_count,
            .literals_typing = &meta.literals_typing,
            .objects = &meta.objects,
        };

        return self.compile_to_ir(ast, meta);
    }

    inline fn dive(self: *@This(), scope: Scope) !void {
        try scope.push_instr(.dive);
        self.depth += 1;
    }

    inline fn ascend(self: *@This(), scope: Scope) !void {
        try scope.push_instr(.ascend);
        self.depth -= 1;
    }

    inline fn new_dest(self: *@This()) Address {
        self.dest += 1;
        return Address.new_pair(self.dest);
    }

    fn compile_to_ir(self: *@This(), ast: AST, meta: *CompilerMeta) !*IR {
        if (meta.kinds == null and ast.kinds != null) {
            meta.kinds = ast.kinds;
        } else {
            ast.kinds.?.last().next = meta.kinds.?;
            meta.kinds.? = ast.kinds.?;
        }
        var head = try Scope.create_head(self.allocator, meta);
        head.head.symbols = ast.symbols;
        head.head.kinds = ast.kinds.?;

        var head_body = ast.head;

        for (head_body) |n| {
            _ = try self.generate(n, head, null, null);
        }

        return head.head;
    }

    fn generate(self: *@This(), node: *Node, scope: Scope, result: ?Address, extra: ?Address) anyerror!?Address {
        switch (node.*) {
            .scope => |s| {
                // don't emit the irblock into parent ir.
                var detatch = false;
                var old_depth: usize = self.depth;
                if (extra != null and extra.?.kind() == .detach) detatch = true;

                // todo make sure this is ok
                var to_inline = false; // = scope != .head;

                // todo maybe allow scoped typing
                var block = try Scope.create_block(self.allocator, scope, s.symbols);

                if (self.in_function) {
                    block.set_reg_count(block.param_count());
                }

                if (to_inline) {
                    block.block.inlining = true;
                    block.block.registers = scope.reg_count();
                    block.block.temporaries = scope.temp_count();
                } else if (!detatch) {
                    try self.dive(scope);
                } else {
                    self.depth = 0;
                }

                defer {
                    // try unallowed in defer
                    if (!to_inline and !detatch) {
                        self.ascend(scope) catch unreachable;
                    } else if (detatch) {
                        self.depth = old_depth;
                    }
                }

                for (s.statments.?) |stmnt| {
                    _ = try self.generate(stmnt, block, null, null);
                }

                if (detatch) {
                    return Address.new_raw(@ptrToInt(block.block));
                } else {
                    try scope.push_instr(Instr{ .block = block.block });
                }
            },
            .structure => |s| {
                _ = s;
                unreachable;
            },
            .variable => {
                if (scope.is_head()) {
                    _ = try self.generate_global_variable(node, scope);
                } else {
                    _ = try self.generate_local_variable(node, scope);
                }
            },
            .constant => {
                if (scope.is_head()) {
                    _ = try self.generate_global_constant(node, scope);
                } else {
                    _ = try self.generate_local_constant(node, scope);
                }
            },
            .assignment => try self.generate_assignment(node, scope),
            .statment => |s| {
                switch (s.kind) {
                    .Expression => {
                        // todo handle no registers!
                        const address = result orelse scope.get_temp().?;
                        defer if (result == null) scope.drop_temp();

                        _ = try self.generate(s.value.?, scope, address, null);
                    },
                    .Return => {
                        if (s.value == null) {
                            try scope.push_instr(Instr{ .ret = false });
                        } else {
                            // we are not nested
                            var address = scope.get_temp().?;
                            var address_moved = false;

                            var gen_result = try self.generate(s.value.?, scope, address, null);

                            defer {
                                if (!address_moved) {
                                    scope.drop_temp();
                                }
                            }

                            if (gen_result != null) {
                                address_moved = true;
                                scope.drop_temp();
                                address = gen_result.?;
                            }

                            _ = try self.generate(s.value.?, scope, address, null);

                            try scope.push_instr(Instr{ .copy = .{ .result = Address.new_register(0), .a = address } });
                            try scope.push_instr(Instr{ .ret = false });
                        }
                    },
                    .ReturnRoot => {
                        if (s.value == null) {
                            try scope.push_instr(Instr{ .ret = true });
                        } else {
                            if (self.depth == 0) {
                                // we are not nested
                                const address = scope.get_temp().?;
                                defer scope.drop_temp();

                                _ = try self.generate(s.value.?, scope, address, null);

                                try scope.push_instr(Instr{ .copy = .{ .result = Address.new_register(0), .a = address } });
                                try scope.push_instr(Instr{ .ret = true });
                            } else {
                                // we are nested
                                var address = scope.get_temp().?;
                                var address_moved = false;

                                var gen_result = try self.generate(s.value.?, scope, address, null);

                                defer {
                                    if (!address_moved) {
                                        scope.drop_temp();
                                    }
                                }

                                if (gen_result != null) {
                                    address_moved = true;
                                    scope.drop_temp();
                                    address = gen_result.?;
                                }

                                // todo make smarter?
                                const lower_o: u56 = 0;

                                try scope.push_instr(Instr{ .set_upvalue = .{ .result = Address.new_upvalue(lower_o), .a = address } });
                                try scope.push_instr(Instr{ .ret = true });
                            }
                        }
                    },
                }
            },
            .function => |func| {
                if (func.is_extern) {
                    try self.generate_extern_function(node, scope, result.?);
                } else {
                    try self.generate_internal_function(node, scope, result.?);
                }
            },
            .conditional_if => try self.generate_conditional_if(node, scope),
            .conditional_while => try self.generate_conditional_while(node, scope),
            .binary_expression => {
                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                try self.generate_binary_expression(node, scope, address);
            },
            .unary_expression => |ue| {
                const op = ue.op.data.symbol;
                if (op == .bang) {
                    // todo make it look for kind
                    const address = if (result != null) result.? else scope.get_temp().?;
                    defer if (result == null) scope.drop_temp();

                    var tmp = scope.get_temp().?;
                    var tmp_moved = false;

                    var gen_result = try self.generate(ue.value, scope, tmp, null);

                    defer {
                        if (!tmp_moved) {
                            scope.drop_temp();
                        }
                    }

                    if (gen_result != null) {
                        tmp_moved = true;
                        scope.drop_temp();
                        tmp = gen_result.?;
                    }

                    try scope.push_instr(Instr{ .not = .{ .source = tmp, .dest = address } });
                    return null;
                } else if (op == .plus_plus) {
                    const symbol = scope.lookup_symbol(ue.value.literal.data.identifier).?;
                    var binding = Address.new_upvalue(@intCast(u56, symbol.stack_binding()));
                    // todo we should use kind everywhere but no methods so no clue.
                    var kind_name: []const u8 = "";
                    switch (symbol.kind.?) {
                        .resolved => |k| kind_name = k.name,
                        .unresolved => |n| kind_name = n,
                    }

                    var global_tmp: ?Address = null;
                    var global_name: ?Address = null;
                    if (symbol.global) {
                        var obj = try self.copy_text(symbol.name);
                        global_tmp = scope.get_temp().?;
                        defer scope.drop_temp();

                        // the stack adress for the lookup name of the global variable
                        global_name = scope.get_temp().?;
                        defer scope.drop_temp();

                        const lit = try self.push_literal(.{
                            .object = obj,
                        }, .Text);

                        try scope.push_instr(.{ .load_literal = .{
                            .result = global_name.?,
                            .a = lit,
                        } });

                        try scope.push_instr(.{ .load_global = .{
                            .a = global_name.?,
                            .result = global_tmp.?,
                        } });

                        const up_adress: u56 = @intCast(u56, global_tmp.?.temporary() + (self.depth * 1024));
                        binding = Address.new_upvalue(up_adress);
                    }

                    if (mem.eql(u8, kind_name, "u64")) {
                        try scope.push_instr(Instr{ .inc_u64 = binding });
                    } else if (mem.eql(u8, kind_name, "i64")) {
                        try scope.push_instr(Instr{ .inc_i64 = binding });
                    } else if (mem.eql(u8, kind_name, "u32")) {
                        try scope.push_instr(Instr{ .inc_u32 = binding });
                    } else if (mem.eql(u8, kind_name, "i32")) {
                        try scope.push_instr(Instr{ .inc_i32 = binding });
                    } else if (mem.eql(u8, kind_name, "u16")) {
                        try scope.push_instr(Instr{ .inc_u16 = binding });
                    } else if (mem.eql(u8, kind_name, "i16")) {
                        try scope.push_instr(Instr{ .inc_i16 = binding });
                    } else if (mem.eql(u8, kind_name, "u8")) {
                        try scope.push_instr(Instr{ .inc_u8 = binding });
                    } else if (mem.eql(u8, kind_name, "f64")) {
                        try scope.push_instr(Instr{ .inc_f64 = binding });
                    } else if (mem.eql(u8, kind_name, "f32")) {
                        try scope.push_instr(Instr{ .inc_f32 = binding });
                    } else unreachable; // todo add custom inc methods

                    if (symbol.global) {
                        try scope.push_instr(.{ .store_global = .{
                            .a = global_tmp.?,
                            .result = global_name.?,
                        } });
                    }

                    return null;
                } else if (op == .minus_minus) {
                    const symbol = scope.lookup_symbol(ue.value.literal.data.identifier).?;
                    var binding = Address.new_upvalue(@intCast(u56, symbol.stack_binding()));
                    // todo we should use kind everywhere but no methods so no clue.
                    var kind_name: []const u8 = "";
                    switch (symbol.kind.?) {
                        .resolved => |k| kind_name = k.name,
                        .unresolved => |n| kind_name = n,
                    }

                    var global_tmp: ?Address = null;
                    var global_name: ?Address = null;
                    if (symbol.global) {
                        var obj = try self.copy_text(symbol.name);
                        global_tmp = scope.get_temp().?;
                        defer scope.drop_temp();

                        // the stack adress for the lookup name of the global variable
                        global_name = scope.get_temp().?;
                        defer scope.drop_temp();

                        const lit = try self.push_literal(.{
                            .object = obj,
                        }, .Text);

                        try scope.push_instr(.{ .load_literal = .{
                            .result = global_name.?,
                            .a = lit,
                        } });

                        try scope.push_instr(.{ .load_global = .{
                            .a = global_name.?,
                            .result = global_tmp.?,
                        } });

                        const up_adress: u56 = @intCast(u56, global_tmp.?.temporary() + (self.depth * 1024));
                        binding = Address.new_upvalue(up_adress);
                    }

                    if (mem.eql(u8, kind_name, "u64")) {
                        try scope.push_instr(Instr{ .dec_u64 = binding });
                    } else if (mem.eql(u8, kind_name, "i64")) {
                        try scope.push_instr(Instr{ .dec_i64 = binding });
                    } else if (mem.eql(u8, kind_name, "u32")) {
                        try scope.push_instr(Instr{ .dec_u32 = binding });
                    } else if (mem.eql(u8, kind_name, "i32")) {
                        try scope.push_instr(Instr{ .dec_i32 = binding });
                    } else if (mem.eql(u8, kind_name, "u16")) {
                        try scope.push_instr(Instr{ .dec_u16 = binding });
                    } else if (mem.eql(u8, kind_name, "i16")) {
                        try scope.push_instr(Instr{ .dec_i16 = binding });
                    } else if (mem.eql(u8, kind_name, "u8")) {
                        try scope.push_instr(Instr{ .dec_u8 = binding });
                    } else if (mem.eql(u8, kind_name, "f64")) {
                        try scope.push_instr(Instr{ .dec_f64 = binding });
                    } else if (mem.eql(u8, kind_name, "f32")) {
                        try scope.push_instr(Instr{ .dec_f32 = binding });
                    } else unreachable; // todo add custom dec methods

                    if (symbol.global) {
                        try scope.push_instr(.{ .store_global = .{
                            .a = global_tmp.?,
                            .result = global_name.?,
                        } });
                    }

                    return null;
                }
                unreachable;
            },
            .call => {
                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                try self.generate_call(node, scope, address, extra);
            },
            .literal => {
                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                var moved = try self.generate_literal(node, scope, address);
                if (moved != null) {
                    return moved;
                }
            },
        }

        return null;
    }

    fn generate_global_variable(self: *@This(), node: *Node, scope: Scope) !void {
        const variable = node.variable;

        if (variable.value != null) {
            var obj = try self.copy_text(variable.name);

            // the stack adress for the lookup name of the global variable
            const global_name = scope.get_temp().?;
            defer scope.drop_temp();

            const lit = try self.push_literal(.{
                .object = obj,
            }, .Text);

            try scope.push_instr(.{ .load_literal = .{
                .result = global_name,
                .a = lit,
            } });

            // the value we want to put in the global variable
            var tmp = scope.get_temp().?;
            var tmp_moved = false;
            var gen_result = try self.generate(variable.value.?, scope, tmp, null);
            defer {
                if (!tmp_moved) {
                    scope.drop_temp();
                }
            }

            if (gen_result != null) {
                tmp_moved = true;
                scope.drop_temp();
                tmp = gen_result.?;
            }

            try scope.push_instr(.{ .store_global = .{
                .a = tmp,
                .result = global_name,
            } });
        }

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(variable.name).?;

        try scope.push_global(Global{
            .value = Item.default(),
            .symbol = symbol,
        });
    }

    fn generate_global_constant(self: *@This(), node: *Node, scope: Scope) !void {
        const constant = node.constant;

        var obj = try self.copy_text(constant.name);

        // the stack adress for the lookup name of the global variable
        const global_name = scope.get_temp().?;
        defer scope.drop_temp();

        const lit = try self.push_literal(.{
            .object = obj,
        }, .Text);

        try scope.push_instr(.{ .load_literal = .{
            .result = global_name,
            .a = lit,
        } });

        // the value we want to put in the global variable
        var tmp = scope.get_temp().?;
        var tmp_moved = false;
        var gen_result = try self.generate(constant.value, scope, tmp, null);
        defer {
            if (!tmp_moved) {
                scope.drop_temp();
            }
        }

        if (gen_result != null) {
            tmp_moved = true;
            scope.drop_temp();
            tmp = gen_result.?;
        }

        try scope.push_instr(.{ .store_global = .{
            .a = tmp,
            .result = global_name,
        } });

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(constant.name).?;

        try scope.push_global(Global{
            .value = Item.default(),
            .symbol = symbol,
        });
    }

    fn generate_local_variable(self: *@This(), node: *Node, scope: Scope) !void {
        const variable = node.variable;
        const register = scope.get_reg().?;

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(variable.name).?;

        symbol.binding = register.register();
        symbol.depth = self.depth;

        if (variable.value != null) {
            _ = try self.generate(variable.value.?, scope, register, null);
        }
    }

    fn generate_head_constant(self: *@This(), node: *Node, head: *IR) !void {
        const variable = node.constant;

        if (variable.value != null) {
            var obj = try self.copy_text(variable.name);

            // the stack adress for the lookup name of the global variable
            const global_name = head.get_temp().?;
            defer head.drop_temp();

            const lit = try self.push_literal(.{
                .object = obj,
            });

            try head.body.push(.{ .load_literal = .{
                .result = global_name,
                .a = lit,
            } });

            // the value we want to put in the global variable
            const tmp = head.get_temp().?;
            defer head.drop_temp();
            self.generate_head(variable.value.?, head, tmp);

            head.body.push(.{ .store_global = .{
                .a = tmp,
                .result = global_name,
            } });
        }

        // todo maybe try to resolve the kind one last time?
        var symbol = head.lookup_symbol(variable.name).?;

        try head.globals.push(Global{
            .value = Item.default(),
            .symbol = symbol,
        });
    }

    fn generate_local_constant(self: *@This(), node: *Node, scope: Scope) !void {
        const constant = node.constant;
        const register = scope.get_reg().?;

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(constant.name).?;

        symbol.binding = register.register();
        symbol.depth = self.depth;

        _ = try self.generate(constant.value, scope, register, null);
    }
    fn generate_assignment(self: *@This(), node: *Node, scope: Scope) !void {
        const assignment = node.assignment;
        const symbol = scope.lookup_symbol(assignment.name).?;

        if (symbol.global) {
            var obj = try self.copy_text(assignment.name);

            // the stack adress for the lookup name of the global variable
            const global_name = scope.get_temp().?;
            defer scope.drop_temp();

            const lit = try self.push_literal(.{
                .object = obj,
            }, .Text);

            try scope.push_instr(.{ .load_literal = .{
                .result = global_name,
                .a = lit,
            } });

            // the value we want to put in the global variable
            var tmp = scope.get_temp().?;
            var tmp_moved = false;
            var gen_result = try self.generate(assignment.value, scope, tmp, null);
            defer {
                if (!tmp_moved) {
                    scope.drop_temp();
                }
            }

            if (gen_result != null) {
                tmp_moved = true;
                scope.drop_temp();
                tmp = gen_result.?;
            }

            try scope.push_instr(.{ .store_global = .{
                .a = tmp,
                .result = global_name,
            } });
        } else {
            // todo upvalues
            if (self.depth != symbol.depth) {
                // we have an upvalue
                var local = scope.get_temp().?;
                var local_moved = false;
                var gen_result = try self.generate(assignment.value, scope, local, null);
                defer {
                    if (!local_moved) {
                        scope.drop_temp();
                    }
                }

                if (gen_result != null) {
                    local_moved = true;
                    scope.drop_temp();
                    local = gen_result.?;
                }

                const result = Address.new_upvalue(@intCast(u56, symbol.stack_binding()));

                try scope.push_instr(.{ .set_upvalue = .{
                    .a = local,
                    .result = result,
                } });
            } else {
                const result = Address.new_register(symbol.binding);
                _ = try self.generate(assignment.value, scope, result, null);
            }
        }
    }

    fn generate_conditional_if(self: *@This(), node: *Node, scope: Scope) !void {
        var conditional = node.conditional_if;

        var condition = scope.get_temp().?;
        var condition_moved = false;
        var gen_result = try self.generate(conditional.condition, scope, condition, null);
        defer {
            if (!condition_moved) {
                scope.drop_temp();
            }
        }

        if (gen_result != null) {
            condition_moved = true;
            scope.drop_temp();
            condition = gen_result.?;
        }

        if (conditional.if_else == null) {
            // single prong if
            const end_dest = self.new_dest();
            try scope.push_instr(Instr{ .if_jmp = .{
                .condition = condition,
                .offset = end_dest,
            } });

            _ = try self.generate(conditional.if_then, scope, null, null);

            try scope.push_instr(Instr{ .destination = end_dest });
        } else {
            // multi prong if
            const end_dest = self.new_dest();
            const else_dest = self.new_dest();
            try scope.push_instr(Instr{ .if_jmp = .{
                .condition = condition,
                .offset = else_dest,
            } });

            _ = try self.generate(conditional.if_then, scope, null, null);

            try scope.push_instr(Instr{ .jmp = .{
                .offset = end_dest,
            } });

            try scope.push_instr(Instr{ .destination = else_dest });

            _ = try self.generate(conditional.if_else.?, scope, null, null);

            try scope.push_instr(Instr{ .destination = end_dest });
        }
    }

    fn generate_conditional_while(self: *@This(), node: *Node, scope: Scope) !void {
        var conditional = node.conditional_while;
        const loop_dest = self.new_dest();

        try scope.push_instr(Instr{ .destination = loop_dest });

        var condition = scope.get_temp().?;
        var condition_moved = false;
        var gen_result = try self.generate(conditional.condition, scope, condition, null);
        defer {
            if (!condition_moved) {
                scope.drop_temp();
            }
        }

        if (gen_result != null) {
            condition_moved = true;
            scope.drop_temp();
            condition = gen_result.?;
        }

        const end_dest = self.new_dest();

        try scope.push_instr(Instr{ .if_jmp = .{
            .condition = condition,
            .offset = end_dest,
        } });

        _ = try self.generate(conditional.body, scope, null, null);

        try scope.push_instr(Instr{ .jmp = .{
            .offset = loop_dest,
        } });

        try scope.push_instr(Instr{ .destination = end_dest });
    }

    fn generate_binary_expression(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const exp = node.binary_expression;

        // todo handle no registers!
        var lhs = scope.get_temp().?;
        var lhs_moved = false;
        var gen_result = try self.generate(exp.lhs, scope, lhs, null);
        defer {
            if (!lhs_moved) {
                scope.drop_temp();
            }
        }

        if (gen_result != null) {
            lhs_moved = true;
            scope.drop_temp();
            lhs = gen_result.?;
        }

        var rhs = scope.get_temp().?;
        var rhs_moved = false;
        gen_result = try self.generate(exp.rhs, scope, rhs, null);
        defer {
            if (!rhs_moved) {
                scope.drop_temp();
            }
        }

        if (gen_result != null) {
            rhs_moved = true;
            scope.drop_temp();
            rhs = gen_result.?;
        }

        var kind = switch (exp.kind.?) {
            .unresolved => |k| k,
            .resolved => |s| s.name,
        };

        switch (exp.op.data.symbol) {
            .plus => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .add_u64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .add_i64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .add_f64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else {
                    unreachable;
                }
            },
            .minus => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .sub_u64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .sub_i64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .sub_f64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else {
                    unreachable;
                }
            },
            .slash => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .div_u64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .div_i64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .div_f64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else {
                    unreachable;
                }
            },
            .star => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .mul_u64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .mul_i64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .mul_f64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else {
                    unreachable;
                }
            },
            .less, .greater => {
                if (exp.op.data.symbol == .greater) {
                    const tmp = lhs;
                    lhs = rhs;
                    rhs = tmp;
                }

                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .less_than_u64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .less_than_i64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .less_than_f64 = .{
                        .result = result,
                        .a = lhs,
                        .b = rhs,
                    } });
                }
            },
            else => unreachable,
        }
    }

    fn generate_extern_function(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const function = node.function;

        const ptr = self.config.fn_lookup.?(function.body.name);
        const object = try self.heap.alloc_object(@sizeOf(Function));
        const body = object.item.resolve(*Function);
        body.* = Function{ .external = .{
            .arity = @intCast(u8, function.params.len),
            .result = (function.result != null),
            .body = ptr.?,
        } };

        const lit = try self.push_literal(Item{ .object = object.obj }, .ExternFn);
        try scope.push_instr(.{ .load_literal = .{
            .result = result,
            .a = lit,
        } });
    }

    fn generate_internal_function(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const function = node.function;

        const object = try self.heap.alloc_object(@sizeOf(Function));
        const body = object.item.resolve(*Function);

        var fn_head = false;
        if (!self.in_function) {
            fn_head = true;
            self.in_function = true;
        }
        defer {
            if (fn_head) self.in_function = false;
        }

        body.*.internal.arity = @intCast(u8, function.params.len);
        body.*.internal.result = (function.result != null);

        const lit = try self.push_literal(Item{ .object = object.obj }, .InternalFn);

        // todo params

        try scope.push_instr(.{ .fn_to_assemble = .{
            .result = result,
            .memory = body,
            .lit = lit,
            .block = try self.generate(function.body.body, scope, null, Address.new_detach()),
        } });

        try scope.push_instr(.{ .load_literal = .{
            .result = result,
            .a = lit,
        } });
    }

    fn generate_call(self: *@This(), node: *Node, scope: Scope, result: Address, extra: ?Address) !void {
        _ = extra;
        const call = node.call;
        const fn_identity = scope.lookup_symbol(call.name).?;

        const name = try self.copy_text(call.name);
        const name_lit = try self.push_literal(.{ .object = name }, .Text);

        // the stack adress for the callable function
        const function_register = scope.get_temp().?;
        defer scope.drop_temp();

        if (fn_identity.global) {
            // the stack adress for the lookup name of the global variable
            const global_name = scope.get_temp().?;
            defer scope.drop_temp();

            try scope.push_instr(.{ .load_literal = .{
                .result = global_name,
                .a = name_lit,
            } });

            // load the global into a register
            try scope.push_instr(.{ .load_global = .{
                .result = function_register,
                .a = global_name,
            } });
        } else {
            unreachable;
        }

        var arg_start: ?Address = null;

        if (call.arguments != null) {
            defer scope.drop_regs(@intCast(u10, call.arguments.?.len));
            arg_start = scope.get_reg().?;
            const args = call.arguments.?;

            var ree = try self.generate(args[0], scope, arg_start, null);
            std.debug.assert(ree == null); // can't move, if you do we corrupt

            if (args.len > 1) {
                for (1..args.len) |reg_i| {
                    const reg = scope.get_reg().?;
                    ree = try self.generate(args[reg_i], scope, reg, null);
                    std.debug.assert(ree == null); // can't move, if you do we corrupt

                }
            }
        }

        var kind_name: []const u8 = "";
        switch (fn_identity.kind.?) {
            .resolved => |k| kind_name = k.name,
            .unresolved => |n| kind_name = n,
        }

        if (std.mem.eql(u8, kind_name, "ExternFn")) {
            try scope.push_instr(.{ .call_extern = .{
                .result = result,
                .arg_start = arg_start,
                .callee = function_register,
                .has_return = true,
            } });
        } else {
            try scope.push_instr(.{ .call = .{
                .result = result,
                .arg_start = arg_start,
                .callee = function_register,
                .has_return = true,
            } });
        }
    }

    fn generate_literal(self: *@This(), node: *Node, scope: Scope, result: Address) !?Address {
        // todo use heap, and jut mark the as non collectable
        switch (node.literal.data) {
            .integer => |val| {
                var item: Item = Item{ .i64 = val };

                const lit = try self.push_literal(item, .i64);

                try scope.push_instr(.{ .load_literal = .{
                    .result = result,
                    .a = lit,
                } });

                return null;
            },
            .decimal => |val| {
                var item: Item = Item{ .f64 = val };

                const lit = try self.push_literal(item, .f64);

                try scope.push_instr(.{ .load_literal = .{
                    .result = result,
                    .a = lit,
                } });

                return null;
            },
            .text => |text_raw| {
                const text = try self.copy_text(text_raw);
                const text_lit = try self.push_literal(.{ .object = text }, .Text);

                try scope.push_instr(.{ .load_literal_obj = .{
                    .result = result,
                    .a = text_lit,
                } });
            },
            .identifier => |name_raw| {
                const symbol = scope.lookup_symbol(name_raw);

                if (symbol.?.global) {
                    const name = try self.copy_text(name_raw);
                    const name_lit = try self.push_literal(.{ .object = name }, .Text);

                    // the stack adress for the lookup name of the global variable
                    const global_name = scope.get_temp().?;
                    defer scope.drop_temp();

                    try scope.push_instr(.{ .load_literal = .{
                        .result = global_name,
                        .a = name_lit,
                    } });

                    // load the global into a register
                    if (scope.lookup_global(name_raw).?.object) {
                        try scope.push_instr(.{ .load_global_obj = .{
                            .result = result,
                            .a = global_name,
                        } });
                    } else {
                        try scope.push_instr(.{ .load_global = .{
                            .result = result,
                            .a = global_name,
                        } });
                    }

                    return null;
                } else {
                    // todo upvalues
                    if (self.depth != symbol.?.depth) {
                        // we have an upvalue
                        const zamn = symbol.?;

                        const real = @intCast(u56, zamn.stack_binding());
                        const real_address = Address.new_upvalue(real);

                        try scope.push_instr(.{ .get_upvalue = .{
                            .result = result,
                            .a = real_address,
                        } });
                    } else {
                        return Address.new_register(symbol.?.binding);
                    }
                }
            },
            .keyword => |kw| {
                switch (kw) {
                    .True => {
                        try scope.push_instr(.{ .load_true = result });
                    },
                    .False => {
                        try scope.push_instr(.{ .load_false = result });
                    },
                    else => unreachable,
                }
            },
            else => std.debug.panic("generate literal does not implement: {s}\n", .{@tagName(node.literal.data)}),
        }

        return null;
    }

    //fn generate_binary_expression(self: *@This(), node: *Node, scope: *IRBlock) !void {
    //}

    fn push_literal(self: *@This(), literal: Item, kind: LitKind) !Address {
        // folding consts to remove dupes
        if (self.config.compile_flags.literal_const_folding) {
            for (self.literals_typing.as_slice(), 0..) |lk, i| {
                if (lk != kind) {
                    continue;
                }

                var same: bool = false;
                const found = self.literals.get(i);
                switch (lk) {
                    .u64 => same = found.u64 == literal.u64,
                    .u32 => same = found.u32 == literal.u32,
                    .u16 => same = found.u16 == literal.u16,
                    .u8 => same = found.u8 == literal.u8,
                    .i64 => same = found.i64 == literal.i64,
                    .i32 => same = found.i32 == literal.i32,
                    .i16 => same = found.i16 == literal.i16,
                    .i8 => same = found.i8 == literal.i8,
                    .f64 => same = found.f64 == literal.f64,
                    .f32 => same = found.f32 == literal.f32,
                    .bool => unreachable,
                    .Text => {
                        const found_text = found.resolve_object().text();
                        same = literal.resolve_object().text().eq(found_text);
                    },
                    .ExternFn => {
                        same = false;
                    },
                    .InternalFn => {
                        same = false;
                    },

                    // objects that are not builtin
                    .Object => unreachable,
                }

                // we have a dupe!
                if (same) {
                    return Address.new_literal(@intCast(u56, i));
                }
            }
        }
        // no dupe found!
        try self.literals.push(literal);
        try self.literals_typing.push(kind);

        self.literal_count.* += 1;
        return Address.new_literal(@intCast(u56, self.literal_count.*));
    }

    fn copy_text(self: *@This(), data: []const u8) !*heap.Ref {
        var string = try self.allocator.alloc(u8, data.len);
        mem.copy(u8, string, data);
        var body = try heap.Ref.create(self.allocator, @ptrToInt(string.ptr));

        var text_copy = try Text.create_const(self.allocator, data.len, body);
        return text_copy;
    }

    // todo const pool should have methods lol
    fn alloc_object(self: *@This(), body: *heap.Ref) !*heap.Ref {
        var obj = try self.allocator.create(Object);

        obj.body = body;
        obj.methods = null;

        try self.objects.push(obj);
        return heap.Ref.create(self.allocator, @ptrToInt(obj));
    }
};

const LitKind = enum {
    u64,
    u32,
    u16,
    u8,
    i64,
    i32,
    i16,
    i8,
    f64,
    f32,
    bool,
    Text,
    ExternFn,
    InternalFn,

    // objects that are not builtin
    Object,
};
