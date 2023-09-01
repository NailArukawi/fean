const std = @import("std");

const mod = @import("mod.zig");
const Parser = mod.parser.Parser;
const AST = mod.parser.AST;
const Node = mod.parser.Node;

const Resolver = mod.Resolver;
const Symbol = mod.Symbol;
const SymbolKind = mod.SymbolKind;
const SymbolTable = mod.SymbolTable;
const Kind = mod.Kind;
const KindTable = mod.KindTable;

pub const AddressKind = mod.AddressKind;
pub const Extra = mod.AddressExtra;
pub const Address = mod.Address;
pub const Load = mod.Load;
pub const Arithmetic = mod.Arithmetic;
pub const StructAccess = mod.StructAccess;
pub const Instr = mod.Instr;

const Item = @import("../runtime/mod.zig").Item;
const Methods = @import("../runtime/mod.zig").Methods;
const Function = @import("../runtime/mod.zig").Function;
const Text = @import("../runtime/mod.zig").Text;
const Object = @import("../runtime/mod.zig").Object;
const Stack = @import("../common/stack.zig").Stack;
const heap = @import("../runtime/heap.zig");

const FeanConfig = @import("../fean.zig").FeanConfig;

const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const mem = std.mem;

pub const IRBlock = struct {
    parent: Scope,
    body: Stack(Instr),

    symbols: *SymbolTable,
    kinds: *KindTable,

    // meta
    registers: u10 = 0,
    temporaries: u10 = 0,

    inlining: bool = false,
    optimize: bool = true,

    pub fn create(allocator: Allocator, parent: Scope, symbols: *SymbolTable, kinds: *KindTable) !*@This() {
        var result = try allocator.create(@This());

        result.* = @This(){
            .parent = parent,
            .body = try Stack(Instr).create(allocator, 32),
            .symbols = symbols,
            .kinds = kinds,
        };

        return result;
    }

    pub fn destroy(self: *@This()) void {
        self.body.destroy();
    }

    pub fn lookup_symbol(self: *@This(), identifier: []const u8) ?*Symbol {
        return self.symbols.lookup(identifier) orelse self.parent.lookup_symbol(identifier);
    }

    pub fn install_symbol(self: *@This(), allocator: Allocator, name: []const u8, kind: SymbolKind, size: usize) !*Symbol {
        return self.symbols.install(allocator, name, kind, size);
    }

    pub fn lookup_kind(self: *@This(), identifier: []const u8) ?*Kind {
        return self.kinds.lookup(identifier);
    }

    pub fn get_temp(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023)
            return null; // we out of registers

        const temp = 1023 - self.temporaries;
        self.temporaries += 1;

        if (self.optimize)
            self.body.push(.{ .reserve = .{ .nr = temp, .reg = false } }) catch unreachable;

        // return can't be a temp register
        std.debug.assert(temp != 0);
        return Address.new_temporary(temp);
    }

    pub fn get_reg(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023)
            return null; // we out of registers

        self.registers += 1;

        if (self.optimize)
            self.body.push(.{ .reserve = .{ .nr = self.registers, .reg = true } }) catch unreachable;

        return Address.new_register(self.registers);
    }

    pub fn drop_temp(self: *@This()) void {
        std.debug.assert(-1 < (self.temporaries - 1));
        if (self.optimize)
            self.body.push(.{ .drop = .{ .nr = 1023 - self.temporaries + 1, .reg = false } }) catch unreachable;
        self.temporaries -= 1;
    }

    pub fn drop_temps(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.temporaries - count));
        if (self.optimize)
            for (self.temporaries..(self.temporaries + count)) |tnr|
                self.body.push(.{ .drop = .{ .nr = @as(u10, @intCast(1023 - tnr)), .reg = false } }) catch unreachable;

        self.temporaries -= count;
    }

    pub fn drop_reg(self: *@This()) void {
        std.debug.assert(-1 < (self.registers - 1));
        if (self.optimize)
            self.body.push(.{ .drop = .{ .nr = self.registers, .reg = true } }) catch unreachable;
        self.registers -= 1;
    }

    pub fn drop_regs(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.registers - count));
        if (self.optimize)
            for (self.registers..(self.registers + count)) |rnr|
                self.body.push(.{ .drop = .{ .nr = @as(u10, @intCast(rnr)), .reg = true } }) catch unreachable;
        self.registers -= count;
    }
};

pub const Global = struct {
    value: Item,
    object: bool = false,
    symbol: *Symbol,
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

    optimize: bool = true,

    pub fn create(allocator: Allocator, meta: *CompilerMeta) !*@This() {
        var result = try allocator.create(@This());

        result.globals = &meta.globals;
        result.globals_offset = &meta.globals_offset;
        result.body = try Stack(Instr).create(allocator, 64);
        result.registers = 0;
        result.temporaries = 0;

        result.optimize = true;

        return result;
    }

    pub fn lookup_global(self: *@This(), identifier: []const u8) ?Global {
        for (self.globals.as_slice()) |g| {
            const found = mem.eql(u8, g.symbol.name, identifier);
            if (found)
                return g;
        }

        return null;
    }

    pub fn lookup_symbol(self: *@This(), identifier: []const u8) ?*Symbol {
        if (self.symbols == null)
            return null;

        return self.symbols.?.lookup(identifier);
    }

    pub fn install_symbol(self: *@This(), allocator: Allocator, name: []const u8, kind: SymbolKind, size: usize) !*Symbol {
        return self.symbols.?.install(allocator, name, kind, size);
    }

    pub fn lookup_kind(self: *@This(), identifier: []const u8) ?*Kind {
        return self.kinds.lookup(identifier);
    }

    pub fn get_temp(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023)
            return null; // we out of registers

        const temp = 1023 - self.temporaries;
        self.temporaries += 1;

        if (self.optimize)
            self.body.push(.{ .reserve = .{ .nr = temp, .reg = false } }) catch unreachable;

        // return can't be a temp register
        std.debug.assert(temp != 0);
        return Address.new_temporary(temp);
    }

    pub fn get_reg(self: *@This()) ?Address {
        const used_registers = self.registers + self.temporaries;
        if ((used_registers + 1) > 1023)
            return null; // we out of registers

        self.registers += 1;

        if (self.optimize)
            self.body.push(.{ .reserve = .{ .nr = self.registers, .reg = true } }) catch unreachable;

        return Address.new_register(self.registers);
    }

    pub fn drop_temp(self: *@This()) void {
        std.debug.assert(-1 < (self.temporaries - 1));
        if (self.optimize)
            self.body.push(.{ .drop = .{ .nr = 1023 - self.temporaries + 1, .reg = false } }) catch unreachable;
        self.temporaries -= 1;
    }

    pub fn drop_temps(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.temporaries - count));
        if (self.optimize)
            for (self.temporaries..(self.temporaries + count)) |tnr|
                self.body.push(.{ .drop = .{ .nr = @as(u10, @intCast(1023 - tnr)), .reg = false } }) catch unreachable;

        self.temporaries -= count;
    }

    pub fn drop_reg(self: *@This()) void {
        std.debug.assert(-1 < (self.registers - 1));
        if (self.optimize)
            self.body.push(.{ .drop = .{ .nr = self.registers, .reg = true } }) catch unreachable;
        self.registers -= 1;
    }

    pub fn drop_regs(self: *@This(), count: u10) void {
        std.debug.assert(-1 < (self.registers - count));
        if (self.optimize)
            for (self.registers..(self.registers + count)) |rnr|
                self.body.push(.{ .drop = .{ .nr = @as(u10, @intCast(rnr)), .reg = true } }) catch unreachable;
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
                .Kind => {
                    const too_print = item.kind.name;
                    std.debug.print("[{} : {s}]: {s}\n", .{
                        i,
                        @tagName(lk),
                        too_print,
                    });
                },

                // obhects that are not builtin
                .Object => unreachable,
                else => unreachable,
            }
        }

        std.debug.print("\n[Program]:\n", .{});
        var i: usize = 0;
        for (self.body.as_slice()) |ir| {
            const ir_text = try ir.debug(buffer) orelse continue;
            const is_drop_or_reserve: bool = slice_start_eql(u8, 5, ir_text, "drop:") or slice_start_eql(u8, 7, ir_text, "reserve:");

            switch (is_drop_or_reserve) {
                true => std.debug.print("--:\t<{s}>\n", .{ir_text}),
                false => {
                    std.debug.print("[{}]:\t({s})\n", .{ i, ir_text });
                    i += 1;
                },
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

    pub fn create_block(allocator: Allocator, parent: Scope, symbols: *SymbolTable, kinds: *KindTable) !@This() {
        var block = try IRBlock.create(allocator, parent, symbols, kinds);
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
                var cursor = b.symbols.head;
                while (cursor != null) {
                    if (cursor.?.param)
                        count += 1;

                    cursor = cursor.?.next;
                }

                return count;
            },
        }
    }

    pub fn lookup_symbol(self: @This(), identifier: []const u8) ?*Symbol {
        switch (self) {
            .head => |head| return head.lookup_symbol(identifier),
            .block => |block| return block.lookup_symbol(identifier),
        }
    }

    pub fn install_symbol(self: @This(), allocator: Allocator, name: []const u8, kind: SymbolKind, size: usize) !*Symbol {
        switch (self) {
            .head => |head| return head.install_symbol(allocator, name, kind, size),
            .block => |block| return block.install_symbol(allocator, name, kind, size),
        }
    }

    pub fn lookup_global(self: @This(), identifier: []const u8) ?Global {
        switch (self) {
            .head => |head| return head.lookup_global(identifier),
            .block => |block| return block.parent.lookup_global(identifier),
        }
    }

    pub fn lookup_kind(self: @This(), identifier: []const u8) ?*Kind {
        switch (self) {
            .head => |head| return head.lookup_kind(identifier),
            .block => |block| return block.lookup_kind(identifier),
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
    optimize: bool = false,
    in_function: bool = false,
    function_head_depth: u56 = 0,

    pub fn compile(reader: std.fs.File.Reader, config: *FeanConfig, meta: *CompilerMeta) !*IR {
        var ast = try Parser.parse(reader, config);

        try Resolver.resolve(&ast, config.allocator);

        var self = @This(){
            .allocator = config.allocator,
            .config = config,
            // todo share it with vm
            // maybe make static or const blocks
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
        if (meta.kinds == null) {
            meta.kinds = ast.kinds;
        } else {
            ast.kinds.tail.?.next = meta.kinds.?.head;
            meta.kinds.? = ast.kinds;
        }
        var head = try Scope.create_head(self.allocator, meta);
        head.head.symbols = ast.symbols;
        head.head.kinds = ast.kinds;
        head.head.optimize = self.optimize;

        var head_body = ast.head;

        for (head_body) |n|
            _ = try self.generate(n, head, null, null);

        return head.head;
    }

    fn generate(self: *@This(), node: *Node, scope: Scope, result: ?Address, extra: ?Address) anyerror!?Address {
        switch (node.*) {
            .scope => |s| {
                // don't emit the irblock into parent ir.
                var detatch = false;
                var old_depth: usize = self.depth;
                if (extra != null and extra.?.kind() == .extra and extra.?.extra() == .detach) detatch = true;

                // todo make sure this is ok
                var to_inline = false; //= scope != .head;

                // todo maybe allow scoped typing
                var block = try Scope.create_block(self.allocator, scope, s.symbols, s.kinds);
                block.block.optimize = self.optimize;

                if (self.in_function)
                    block.set_reg_count(block.param_count());

                if (to_inline) {
                    block.block.inlining = true;
                    block.block.registers = scope.reg_count();
                    block.block.temporaries = scope.temp_count();
                } else switch (!detatch) {
                    true => try self.dive(scope),
                    false => self.depth = 0,
                }

                defer {
                    // try unallowed in defer
                    if (!to_inline and !detatch) {
                        self.ascend(scope) catch unreachable;
                    } else if (detatch) {
                        self.depth = old_depth;
                    }
                }

                var passalong_extra: ?Address = null;
                if (extra) |e| {
                    switch (e.kind()) {
                        .impl_target => passalong_extra = e,
                        else => {},
                    }
                }

                for (s.statments.?) |stmnt|
                    _ = try self.generate(stmnt, block, null, passalong_extra);

                switch (detatch) {
                    true => return Address.new_raw(@intFromPtr(block.block)),
                    false => try scope.push_instr(Instr{ .block = block.block }),
                }
            },
            .impl => |*i| {
                const impl_extra = Address.new_impl_target(i.this.resolved);

                _ = try self.generate(i.body, scope, null, impl_extra);
                unreachable;
            },
            .structure => |*s| {
                if (scope.lookup_symbol(s.name) == null) {
                    const kind = SymbolKind{ .resolved = s.this.? };
                    _ = try scope.install_symbol(self.allocator, s.name, kind, s.this.?.size);
                }

                _ = try self.push_literal(.{ .kind = s.this.? }, .Kind);
                return null;
            },
            .construct => |*c| {
                if (c.kind == .unresolved) {
                    const kind_lookup = scope.lookup_kind(c.kind.unresolved);
                    // error no kind found for struct instansiation.
                    c.kind = .{ .resolved = kind_lookup.? };
                }

                const kind = self.lookup_literal(.{ .kind = c.kind.resolved }, .Kind).?;
                try scope.push_instr(Instr{ .create_struct = .{ .result = result.?, .kind = kind } });

                try self.generate_construct_fields(node, scope, result.?);

                return null;
            },
            .variable => {
                switch (scope.is_head()) {
                    true => try self.generate_global_variable(node, scope, extra),
                    false => try self.generate_local_variable(node, scope, extra),
                }
            },
            .constant => {
                switch (scope.is_head()) {
                    true => try self.generate_global_constant(node, scope, extra),
                    false => try self.generate_local_constant(node, scope, extra),
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

                            defer if (!address_moved) scope.drop_temp();

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

                                defer if (!address_moved) scope.drop_temp();

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
                switch (func.is_extern) {
                    true => {
                        if (extra != null and extra.?.kind() == .impl_target) {
                            @panic("No external methods yet.");
                        } else {
                            try self.generate_extern_function(node, scope, result.?);
                        }
                    },
                    false => {
                        if (extra != null and extra.?.kind() == .impl_target) {
                            @panic("No internal methods yet.");
                        } else {
                            try self.generate_internal_function(node, scope, result.?);
                        }
                    },
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

                    defer if (!tmp_moved) scope.drop_temp();

                    if (gen_result != null) {
                        tmp_moved = true;
                        scope.drop_temp();
                        tmp = gen_result.?;
                    }

                    try scope.push_instr(Instr{ .not = .{ .source = tmp, .dest = address } });
                    return null;
                } else if (op == .plus_plus) {
                    const symbol = scope.lookup_symbol(ue.value.literal.data.identifier).?;
                    var binding = Address.new_upvalue(@as(u56, @intCast(symbol.stackBinding())));
                    // todo we should use kind everywhere but no methods so no clue.
                    var kind_name: []const u8 = "";
                    switch (symbol.kind) {
                        .none => @panic("Missing kind"),
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

                        const up_adress: u56 = @as(u56, @intCast(global_tmp.?.temporary() + (self.depth * 1024)));
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
                    var binding = Address.new_upvalue(@as(u56, @intCast(symbol.stackBinding())));
                    // todo we should use kind everywhere but no methods so no clue.
                    var kind_name: []const u8 = "";
                    switch (symbol.kind) {
                        .none => @panic("Missing kind"),
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

                        const up_adress: u56 = @as(u56, @intCast(global_tmp.?.temporary() + (self.depth * 1024)));
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
            .get => |g| {
                const child = (try self.generate(g.object, scope, null, extra)).?;

                // todo handle no registers!
                const address = if (result != null) result.? else scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                const kind_name = g.field.resolved.kind.name;
                const field = g.field.resolved;
                const access = StructAccess{
                    .reg = address,
                    .this = child,
                    .index = Address.new_field(@as(u56, @intCast(field.index))),
                };

                if (extra != null and extra.?.extra() == .set_mode) {
                    if (mem.eql(u8, kind_name, "u64")) {
                        try scope.push_instr(Instr{ .set_struct_field_u64 = access });
                    } else if (mem.eql(u8, kind_name, "i64")) {
                        try scope.push_instr(Instr{ .set_struct_field_i64 = access });
                    }
                } else {
                    if (mem.eql(u8, kind_name, "u64")) {
                        try scope.push_instr(Instr{ .get_struct_field_u64 = access });
                    } else if (mem.eql(u8, kind_name, "i64")) {
                        try scope.push_instr(Instr{ .get_struct_field_i64 = access });
                    }
                }
            },
            .set => |s| {
                // todo handle no registers!
                const child = scope.get_temp().?;
                defer scope.drop_temp();
                var moved = try self.generate(s.object, scope, child, null);
                if (moved != null)
                    return moved;

                // todo handle no registers!
                const address = scope.get_temp().?;
                defer scope.drop_temp();
                moved = try self.generate(s.value, scope, address, null);
                if (moved != null)
                    return moved;

                const kind_name = s.field.resolved.kind.name;
                const field = s.field.resolved;
                const access = StructAccess{
                    .reg = address,
                    .this = child,
                    .index = Address.new_field(@as(u56, @intCast(field.index))),
                };

                if (mem.eql(u8, kind_name, "u64")) {
                    try scope.push_instr(Instr{ .set_struct_field_u64 = access });
                } else if (mem.eql(u8, kind_name, "i64")) {
                    try scope.push_instr(Instr{ .set_struct_field_i64 = access });
                }

                const set_extra = Address.new_extra(.set_mode);
                if (s.object.* == .get) {
                    _ = try self.generate(s.object, scope, address, set_extra);
                } else {
                    _ = try self.generate(s.object, scope, child, set_extra);
                }
            },
            .object => {
                unreachable;
            },
            .literal => {
                // todo handle no registers!
                const address = result orelse scope.get_temp().?;
                defer if (result == null) scope.drop_temp();

                var moved = try self.generate_literal(node, scope, address, extra);
                if (moved != null)
                    return moved;

                if (result == null)
                    return address;
            },
        }

        return null;
    }

    fn generate_global_variable(self: *@This(), node: *Node, scope: Scope, extra: ?Address) !void {
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
            var gen_result = try self.generate(variable.value.?, scope, tmp, extra);
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

    fn generate_global_constant(self: *@This(), node: *Node, scope: Scope, extra: ?Address) !void {
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
        var gen_result = try self.generate(constant.value, scope, tmp, extra);
        defer if (!tmp_moved) scope.drop_temp();

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

    fn generate_local_variable(self: *@This(), node: *Node, scope: Scope, extra: ?Address) !void {
        const variable = node.variable;
        const register = scope.get_reg().?;

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(variable.name).?;

        symbol.binding = register.register();
        symbol.depth = self.depth;

        if (variable.value != null)
            _ = try self.generate(variable.value.?, scope, register, extra);
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

    fn generate_local_constant(self: *@This(), node: *Node, scope: Scope, extra: ?Address) !void {
        const constant = node.constant;
        const register = scope.get_reg().?;

        // todo maybe try to resolve the kind one last time?
        var symbol = scope.lookup_symbol(constant.name).?;

        symbol.binding = register.register();
        symbol.depth = self.depth;

        _ = try self.generate(constant.value, scope, register, extra);
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
                defer if (!local_moved) scope.drop_temp();

                if (gen_result != null) {
                    local_moved = true;
                    scope.drop_temp();
                    local = gen_result.?;
                }

                const result = Address.new_upvalue(@as(u56, @intCast(symbol.stackBinding())));

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
        defer if (!condition_moved) scope.drop_temp();

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
        defer if (!lhs_moved) scope.drop_temp();

        if (gen_result != null) {
            lhs_moved = true;
            scope.drop_temp();
            lhs = gen_result.?;
        }

        var rhs = scope.get_temp().?;
        var rhs_moved = false;
        gen_result = try self.generate(exp.rhs, scope, rhs, null);
        defer if (!rhs_moved) scope.drop_temp();

        if (gen_result != null) {
            rhs_moved = true;
            scope.drop_temp();
            rhs = gen_result.?;
        }

        var kind = switch (exp.kind) {
            .none => @panic("Missing kind"),
            .unresolved => |k| k,
            .resolved => |s| s.name,
        };

        var arithmatic: Arithmetic = .{ .result = result, .a = lhs, .b = rhs };

        switch (exp.op.data.symbol) {
            .plus => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .add_u64 = arithmatic });
                } else if (mem.eql(u8, kind, "u32")) {
                    try scope.push_instr(Instr{ .add_u32 = arithmatic });
                } else if (mem.eql(u8, kind, "u16")) {
                    try scope.push_instr(Instr{ .add_u16 = arithmatic });
                } else if (mem.eql(u8, kind, "u8")) {
                    try scope.push_instr(Instr{ .add_u8 = arithmatic });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .add_i64 = arithmatic });
                } else if (mem.eql(u8, kind, "i32")) {
                    try scope.push_instr(Instr{ .add_i32 = arithmatic });
                } else if (mem.eql(u8, kind, "i16")) {
                    try scope.push_instr(Instr{ .add_i16 = arithmatic });
                } else if (mem.eql(u8, kind, "i8")) {
                    try scope.push_instr(Instr{ .add_i8 = arithmatic });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .add_f64 = arithmatic });
                } else if (mem.eql(u8, kind, "f32")) {
                    try scope.push_instr(Instr{ .add_f32 = arithmatic });
                } else {
                    unreachable;
                }
            },
            .minus => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .sub_u64 = arithmatic });
                } else if (mem.eql(u8, kind, "u32")) {
                    try scope.push_instr(Instr{ .sub_u32 = arithmatic });
                } else if (mem.eql(u8, kind, "u16")) {
                    try scope.push_instr(Instr{ .sub_u16 = arithmatic });
                } else if (mem.eql(u8, kind, "u8")) {
                    try scope.push_instr(Instr{ .sub_u8 = arithmatic });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .sub_i64 = arithmatic });
                } else if (mem.eql(u8, kind, "i32")) {
                    try scope.push_instr(Instr{ .sub_i32 = arithmatic });
                } else if (mem.eql(u8, kind, "i16")) {
                    try scope.push_instr(Instr{ .sub_i16 = arithmatic });
                } else if (mem.eql(u8, kind, "i8")) {
                    try scope.push_instr(Instr{ .sub_i8 = arithmatic });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .sub_f64 = arithmatic });
                } else if (mem.eql(u8, kind, "f32")) {
                    try scope.push_instr(Instr{ .sub_f32 = arithmatic });
                } else {
                    unreachable;
                }
            },
            .slash => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .div_u64 = arithmatic });
                } else if (mem.eql(u8, kind, "u32")) {
                    try scope.push_instr(Instr{ .div_u32 = arithmatic });
                } else if (mem.eql(u8, kind, "u16")) {
                    try scope.push_instr(Instr{ .div_u16 = arithmatic });
                } else if (mem.eql(u8, kind, "u8")) {
                    try scope.push_instr(Instr{ .div_u8 = arithmatic });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .div_i64 = arithmatic });
                } else if (mem.eql(u8, kind, "i32")) {
                    try scope.push_instr(Instr{ .div_i32 = arithmatic });
                } else if (mem.eql(u8, kind, "i16")) {
                    try scope.push_instr(Instr{ .div_i16 = arithmatic });
                } else if (mem.eql(u8, kind, "i8")) {
                    try scope.push_instr(Instr{ .div_i8 = arithmatic });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .div_f64 = arithmatic });
                } else if (mem.eql(u8, kind, "f32")) {
                    try scope.push_instr(Instr{ .div_f32 = arithmatic });
                } else {
                    unreachable;
                }
            },
            .star => {
                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .mul_u64 = arithmatic });
                } else if (mem.eql(u8, kind, "u32")) {
                    try scope.push_instr(Instr{ .mul_u32 = arithmatic });
                } else if (mem.eql(u8, kind, "u16")) {
                    try scope.push_instr(Instr{ .mul_u16 = arithmatic });
                } else if (mem.eql(u8, kind, "u8")) {
                    try scope.push_instr(Instr{ .mul_u8 = arithmatic });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .mul_i64 = arithmatic });
                } else if (mem.eql(u8, kind, "i32")) {
                    try scope.push_instr(Instr{ .mul_i32 = arithmatic });
                } else if (mem.eql(u8, kind, "i16")) {
                    try scope.push_instr(Instr{ .mul_i16 = arithmatic });
                } else if (mem.eql(u8, kind, "i8")) {
                    try scope.push_instr(Instr{ .mul_i8 = arithmatic });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .mul_f64 = arithmatic });
                } else if (mem.eql(u8, kind, "f32")) {
                    try scope.push_instr(Instr{ .mul_f32 = arithmatic });
                } else {
                    unreachable;
                }
            },
            .less, .greater => {
                if (exp.op.data.symbol == .greater) {
                    arithmatic.b = rhs;
                    arithmatic.a = lhs;
                }

                if (mem.eql(u8, kind, "u64")) {
                    try scope.push_instr(Instr{ .less_than_u64 = arithmatic });
                } else if (mem.eql(u8, kind, "u32")) {
                    try scope.push_instr(Instr{ .less_than_u32 = arithmatic });
                } else if (mem.eql(u8, kind, "u16")) {
                    try scope.push_instr(Instr{ .less_than_u16 = arithmatic });
                } else if (mem.eql(u8, kind, "u8")) {
                    try scope.push_instr(Instr{ .less_than_u8 = arithmatic });
                } else if (mem.eql(u8, kind, "i64")) {
                    try scope.push_instr(Instr{ .less_than_i64 = arithmatic });
                } else if (mem.eql(u8, kind, "i32")) {
                    try scope.push_instr(Instr{ .less_than_i32 = arithmatic });
                } else if (mem.eql(u8, kind, "i16")) {
                    try scope.push_instr(Instr{ .less_than_i16 = arithmatic });
                } else if (mem.eql(u8, kind, "i8")) {
                    try scope.push_instr(Instr{ .less_than_i8 = arithmatic });
                } else if (mem.eql(u8, kind, "f64")) {
                    try scope.push_instr(Instr{ .less_than_f64 = arithmatic });
                } else if (mem.eql(u8, kind, "f32")) {
                    try scope.push_instr(Instr{ .less_than_f32 = arithmatic });
                } else {
                    unreachable;
                }
            },
            else => unreachable,
        }
    }

    fn generate_extern_function(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const function = node.function;

        const ptr = self.config.fn_lookup.?(function.body.name);
        const object = try self.heap.alloc(@sizeOf(Function) + @sizeOf(?*Methods));
        const body = object.object().function();
        body.* = Function{ .external = .{
            .arity = @as(u8, @intCast(function.params.len)),
            .result = (function.result != .none),
            .body = ptr.?,
        } };

        const lit = try self.push_literal(Item{ .object = object }, .ExternFn);
        try scope.push_instr(.{ .load_literal = .{
            .result = result,
            .a = lit,
        } });
    }

    fn generate_internal_function(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const function = node.function;

        const object = try self.heap.alloc(@sizeOf(Function) + @sizeOf(?*Methods));
        const body = object.object().function();

        var fn_head = false;
        if (!self.in_function) {
            fn_head = true;
            self.in_function = true;
        }
        defer if (fn_head) {
            self.in_function = false;
        };

        body.*.internal.arity = @as(u8, @intCast(function.params.len));
        body.*.internal.result = (function.result != .none);

        const lit = try self.push_literal(Item{ .object = object }, .InternalFn);

        // todo params
        try scope.push_instr(.{ .fn_to_assemble = .{
            .result = result,
            .memory = body,
            .lit = lit,
            .block = try self.generate(function.body.body, scope, null, Address.new_extra(.detach)),
        } });

        try scope.push_instr(.{ .load_literal = .{
            .result = result,
            .a = lit,
        } });
    }

    fn generate_call(self: *@This(), node: *Node, scope: Scope, result: Address, _: ?Address) !void {
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
        } else unreachable;

        var arg_start: ?Address = null;
        const copy_if_move = Address.new_extra(.copy_if_move);

        if (call.arguments != null) {
            defer scope.drop_regs(@as(u10, @intCast(call.arguments.?.len)));
            arg_start = scope.get_reg().?;
            const args = call.arguments.?;

            var ree = try self.generate(args[0], scope, arg_start, copy_if_move);
            std.debug.assert(ree == null); // can't move, if you do we corrupt

            if (args.len > 1) {
                for (1..args.len) |reg_i| {
                    const reg = scope.get_reg().?;
                    ree = try self.generate(args[reg_i], scope, reg, copy_if_move);
                    std.debug.assert(ree == null); // can't move, if you do we corrupt

                }
            }
        }

        var kind_name: []const u8 = "";
        switch (fn_identity.kind) {
            .none => @panic("Missing kind"),
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

    fn generate_literal(self: *@This(), node: *Node, scope: Scope, result: Address, extra: ?Address) !?Address {
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

                    if (extra != null and extra.?.extra() == .set_mode) {
                        try scope.push_instr(.{ .store_global = .{
                            .result = global_name,
                            .a = result,
                        } });

                        return null;
                    }

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

                        const real = @as(u56, @intCast(zamn.stackBinding()));
                        const real_address = Address.new_upvalue(real);

                        try scope.push_instr(.{ .get_upvalue = .{
                            .result = result,
                            .a = real_address,
                        } });
                    } else {
                        if (extra != null and extra.?.extra() == .copy_if_move) {
                            try scope.push_instr(.{ .copy = .{
                                .result = result,
                                .a = Address.new_register(symbol.?.binding),
                            } });
                            return null;
                        } else return Address.new_register(symbol.?.binding);
                    }
                }
            },
            .keyword => |kw| {
                switch (kw) {
                    .True => try scope.push_instr(.{ .load_true = result }),
                    .False => try scope.push_instr(.{ .load_false = result }),
                    else => unreachable,
                }
            },
            else => std.debug.panic("generate literal does not implement: {s}\n", .{@tagName(node.literal.data)}),
        }

        return null;
    }

    fn generate_construct_fields(self: *@This(), node: *Node, scope: Scope, result: Address) !void {
        const construct = node.construct;
        const tmp = scope.get_temp().?;
        defer scope.drop_temp();
        for (construct.fields.?, 0..) |field, i| {
            var moved = try self.generate(field.value.?, scope, tmp, null);
            if (moved != null)
                @panic("Moving here is not handled");

            const payload: StructAccess = .{
                .reg = tmp,
                .this = result,
                .index = Address.new_field(@as(u56, @intCast(i))),
            };

            const lk: LitKind = LitKind.from_kind(field.kind.resolved);
            var field_copy: Instr = undefined;
            switch (lk) {
                LitKind.u64 => field_copy = .{ .set_struct_field_u64 = payload },
                LitKind.u32 => field_copy = .{ .set_struct_field_u32 = payload },
                LitKind.u16 => field_copy = .{ .set_struct_field_u16 = payload },
                LitKind.u8 => field_copy = .{ .set_struct_field_u8 = payload },
                LitKind.i64 => field_copy = .{ .set_struct_field_i64 = payload },
                LitKind.i32 => field_copy = .{ .set_struct_field_i32 = payload },
                LitKind.i16 => field_copy = .{ .set_struct_field_i16 = payload },
                LitKind.i8 => field_copy = .{ .set_struct_field_i8 = payload },
                LitKind.f64 => field_copy = .{ .set_struct_field_f64 = payload },
                LitKind.f32 => field_copy = .{ .set_struct_field_f32 = payload },
                LitKind.bool => @panic("Not implimented"),
                LitKind.Text => @panic("Not implimented"),
                LitKind.ExternFn => @panic("Not implimented"),
                LitKind.InternalFn => @panic("Not implimented"),
                LitKind.MethodSet => @panic("Not implimented"),
                LitKind.Kind => @panic("Not implimented"),
                LitKind.Object => field_copy = .{ .set_struct_field_obj = payload },
                LitKind.Custom => {
                    moved = try self.generate(field.value.?, scope, tmp, null);
                    if (moved != null)
                        @panic("Moving here is not handled");

                    field_copy = .{ .set_struct_field_obj = payload };
                },
            }
            try scope.push_instr(field_copy);
        }
    }

    fn lookup_literal(self: *@This(), literal: Item, kind: LitKind) ?Address {
        for (self.literals_typing.as_slice(), 0..) |lk, i| {
            if (lk != kind)
                continue;

            const found = self.literals.get(i);
            switch (lk) {
                .Kind => if (found.kind == literal.kind) return Address.new_literal(@as(u56, @intCast(i))),

                // objects that are not builtin
                .Object => unreachable,
                else => unreachable,
            }
        }

        return null;
    }

    fn push_literal(self: *@This(), literal: Item, kind: LitKind) !Address {
        // folding consts to remove dupes
        if (self.config.compile_flags.literal_const_folding) {
            for (self.literals_typing.as_slice(), 0..) |lk, i| {
                if (lk != kind)
                    continue;

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
                        // Todo ?
                        same = false;
                    },
                    .Kind => {
                        // Todo ?
                        same = false;
                    },
                    .InternalFn => {
                        // Todo ?
                        same = false;
                    },

                    // objects that are not builtin
                    .Object => unreachable,
                    else => unreachable,
                }

                // we have a dupe!
                if (same) {
                    return Address.new_literal(@as(u56, @intCast(i)));
                }
            }
        }
        // no dupe found!
        try self.literals.push(literal);
        try self.literals_typing.push(kind);

        self.literal_count.* += 1;
        return Address.new_literal(@as(u56, @intCast(self.literal_count.*)));
    }

    fn copy_text(self: *@This(), data: []const u8) !*heap.Ref {
        var string = try self.allocator.alloc(u8, data.len);
        mem.copy(u8, string, data);
        var body = try heap.Ref.create(self.allocator, @intFromPtr(string.ptr));

        var text_copy = try Text.create_const(self.allocator, data.len, body);
        return text_copy;
    }

    // todo const pool should have methods lol
    fn alloc_object(self: *@This(), body: *heap.Ref) !*heap.Ref {
        var obj = try self.allocator.create(Object);

        obj.body = body;
        obj.methods = null;

        try self.objects.push(obj);
        return heap.Ref.create(self.allocator, @intFromPtr(obj));
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
    MethodSet,
    Kind,

    // objects that are not builtin
    Object,
    Custom,
    pub fn from_kind(kind: *Kind) @This() {
        if (std.mem.eql(u8, kind.name, "u64")) return LitKind.u64;
        if (std.mem.eql(u8, kind.name, "u32")) return LitKind.u32;
        if (std.mem.eql(u8, kind.name, "u16")) return LitKind.u16;
        if (std.mem.eql(u8, kind.name, "u8")) return LitKind.u8;
        if (std.mem.eql(u8, kind.name, "i64")) return LitKind.i64;
        if (std.mem.eql(u8, kind.name, "i32")) return LitKind.i32;
        if (std.mem.eql(u8, kind.name, "i16")) return LitKind.i16;
        if (std.mem.eql(u8, kind.name, "i8")) return LitKind.i8;
        if (std.mem.eql(u8, kind.name, "f64")) return LitKind.f64;
        if (std.mem.eql(u8, kind.name, "f32")) return LitKind.f32;
        if (std.mem.eql(u8, kind.name, "bool")) return LitKind.bool;
        if (std.mem.eql(u8, kind.name, "Text")) return LitKind.Text;
        return LitKind.Custom;
    }
};

inline fn slice_start_eql(comptime T: type, denominating_len: usize, a: []const u8, b: []const u8) bool {
    if (a.len < denominating_len or b.len < denominating_len)
        return false;

    return std.mem.eql(T, a[0..denominating_len], b[0..denominating_len]);
}
