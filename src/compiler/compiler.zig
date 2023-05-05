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

    pub inline fn new_upvalue(symbol: Symbol) @This() {
        return @This(){
            .inner = @ptrToInt(symbol) | (@intCast(usize, @enumToInt(AddressKind.upvalue)) << 56),
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

    pub inline fn kind(self: @This()) AddressKind {
        return @intToEnum(AddressKind, @truncate(u8, self.inner >> 56));
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
    ret,
    call,
    call_extern,
    invoke,
    invoke_extern,
    make_closure,

    // Memory
    load_literal: Load,
    load_literal_obj: Load,
    load_global: Load,
    load_global_obj: Load,
    store_global: Load,
    get_upvalue: Load,
    get_upvalue_obj: Load,
    set_upvalue: Load,
    open_upvalue: struct {
        a: Address,
    },
    close_upvalue,

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

    // meta
    block: *IRBlock,

    // extended
    extended,

    pub fn debug(self: @This(), buffer: *[512]u8) ![]const u8 {
        switch (self) {
            .no_op => return "",
            .ret => return std.fmt.bufPrint(buffer, "return", .{}),
            .call => unreachable,
            .call_extern => unreachable,
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,

            // Memory
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
            .get_upvalue_obj => unreachable,
            .set_upvalue => unreachable,
            .open_upvalue => unreachable,
            .close_upvalue => unreachable,

            // meta
            .block => unreachable,

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

                return std.fmt.bufPrint(buffer, "{s}:\t{s} = {s}, {s}", .{ @tagName(self), result, adress_a, adress_b });
            },

            else => {
                return std.fmt.bufPrint(buffer, "{s}:\tUNKNOWN", .{@tagName(self)});
            },
        }
    }
};

pub const IRBlock = struct {
    uptable: List(Symbol),
    body: Stack(Instr),

    symbols: ?*SymbolTable,

    // meta
    registers: u10 = 0,
    temporaries: u10 = 0,

    pub fn create(allocator: Allocator, symbols: ?*SymbolTable) !*@This() {
        var result = try allocator.create(@This());

        result.* = @This(){
            .uptable = List(Symbol).init(allocator),
            .body = try Stack(Instr).create(allocator, 32),
            .symbols = symbols,
        };

        return result;
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
                    std.debug.print("[{} : {}]: {}\n", .{
                        i,
                        lk,
                        too_print,
                    });
                },
                .i32 => unreachable,
                .i16 => unreachable,
                .i8 => unreachable,
                .f64 => unreachable,
                .f32 => unreachable,
                .bool => unreachable,
                .Text => {
                    const too_print = item.resolve_object().text();
                    std.debug.print("[{} : {}]: \"{s}\"\n", .{
                        i,
                        lk,
                        too_print.as_slice(),
                    });
                },

                // obhects that are not builtin
                .Object => unreachable,
            }
        }

        std.debug.print("\n[Program]:\n", .{});
        for (self.body.as_slice(), 0..) |ir, i| {
            std.debug.print("[{}]:\t({s})\n", .{ i, try ir.debug(buffer) });
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

    pub fn create_block(allocator: Allocator, symbols: ?*SymbolTable) !@This() {
        var block = try IRBlock.create(allocator, symbols);
        return @This(){
            .block = block,
        };
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
            // todo implement
            .block => unreachable,
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

    pub fn get_temp(self: @This()) ?Address {
        switch (self) {
            .head => |head| return head.get_temp(),
            .block => |block| return block.get_temp(),
        }
    }

    pub fn get_reg(self: @This()) ?Address {
        switch (self) {
            .head => |head| return head.get_reg(),
            .block => |block| return block.get_reg(),
        }
    }
};

pub const Compiler = struct {
    allocator: Allocator,
    config: *FeanConfig,
    literals: *Stack(Item),
    literal_count: *u40,
    literals_typing: *Stack(LitKind),
    objects: *Stack(*Object),

    pub fn compile(src: []const u8, config: *FeanConfig, meta: *CompilerMeta) !*IR {
        var ast = try Parser.parse(src, config);

        try Resolver.resolve(&ast, config.allocator);

        var self = @This(){
            .allocator = config.allocator,
            .config = config,
            .literals = &meta.literals,
            .literal_count = &meta.literal_count,
            .literals_typing = &meta.literals_typing,
            .objects = &meta.objects,
        };

        return self.compile_to_ir(ast, meta);
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
            try self.generate(n, head, null);
        }

        return head.head;
    }

    fn generate(self: *@This(), node: *Node, scope: Scope, result: ?Address) anyerror!void {
        switch (node.*) {
            .scope => |s| {
                // todo symbol table
                var block = try Scope.create_block(self.allocator, null);

                for (s.statments.?) |stmnt| {
                    try self.generate(stmnt, block, null);
                }

                try scope.push_instr(Instr{ .block = block.block });
            },
            .variable => try self.generate_variable(node, scope),
            .constant => try self.generate_variable(node, scope),
            .assignment => unreachable,
            .statment => |s| {
                switch (s.kind) {
                    .Expression => {
                        // todo handle no registers!
                        const address = result orelse scope.get_temp().?;
                        defer if (result == null) scope.drop_temp();

                        try self.generate(s.value, scope, address);
                    },
                    .Return => {
                        const address = Address.new_register(0);
                        try self.generate(s.value, scope, address);
                        try scope.push_instr(.ret);
                    },
                }
            },
            .binary_expression => {
                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                try self.generate_binary_expression(node, scope, address);
            },
            .unary_expression => unreachable,
            .literal => {

                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                try self.generate_literal(node, scope, address);
            },
        }
    }

    fn generate_variable(self: *@This(), node: *Node, scope: Scope) !void {
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
            const tmp = scope.get_temp().?;
            defer scope.drop_temp();
            try self.generate(variable.value.?, scope, tmp);

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

    fn generate_binary_expression(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const exp = node.binary_expression;

        // todo handle no registers!
        const lhs = scope.get_temp().?;
        try self.generate(exp.lhs, scope, lhs);
        const rhs = scope.get_temp().?;
        try self.generate(exp.rhs, scope, rhs);
        defer scope.drop_temps(2);

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
                }
            },
            .minus => {
                unreachable;
            },
            .slash => {
                unreachable;
            },
            .star => {
                unreachable;
            },
            else => unreachable,
        }
    }

    fn generate_literal(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        switch (node.literal.data) {
            .integer => |val| {
                var item: Item = Item{ .i64 = val };

                const lit = try self.push_literal(item, .i64);

                try scope.push_instr(.{ .load_literal = .{
                    .result = result,
                    .a = lit,
                } });
            },
            .decimal => |val| {
                var item: Item = Item{ .f64 = val };

                const lit = try self.push_literal(item, .f64);

                try scope.push_instr(.{ .load_literal = .{
                    .result = result,
                    .a = lit,
                } });
            },
            .identifier => |name_raw| {
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

                return;
            },
            else => unreachable,
        }
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

    // objects that are not builtin
    Object,
};
