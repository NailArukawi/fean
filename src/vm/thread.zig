const std = @import("std");
const Linked = std.SinglyLinkedList;
const Allocator = std.mem.Allocator;

const Op = @import("mod.zig").Op;
const Opcode = @import("mod.zig").Opcode;
const Item = @import("mod.zig").Item;
const Chunk = @import("chunk.zig").Chunk;
const Global = @import("mod.zig").Global;
const Heap = @import("mod.zig").Heap;
const Ref = @import("mod.zig").Ref;
const CallFrame = @import("mod.zig").CallFrame;
const Function = @import("mod.zig").Function;
const Methods = @import("mod.zig").Methods;
const Object = @import("mod.zig").Object;
const Text = @import("mod.zig").Text;
const Dict = @import("mod.zig").Dict;

const Stack = @import("../stack.zig").Stack;
const DynamicBitSet = @import("../dynamicbitset.zig").DynamicBitSet;
const AssembledResult = @import("../compiler/mod.zig").AssembledResult;
const CompilerMeta = @import("../compiler/mod.zig").CompilerMeta;

const REGISTER_COUNT: usize = 1024;
const STACK_DEPTH: usize = 64;
const STACK_SIZE: usize = REGISTER_COUNT * STACK_DEPTH;
const CALL_STACK_SIZE: usize = 32;

pub const CallStack = struct {
    inner: Stack(CallFrame),

    // todo cringe
    pub fn create(allocator: Allocator, size: usize) !@This() {
        return @This(){
            .inner = try Stack(CallFrame).create(allocator, size),
        };
    }

    pub inline fn push(self: *@This(), frame: CallFrame) !void {
        try self.inner.push(frame);
    }

    pub inline fn pop(self: *@This()) CallFrame {
        return self.inner.pop();
    }

    pub inline fn is_bottom(self: *@This()) bool {
        return self.inner.used == 0;
    }
};

pub const Thread = struct {
    stack: [STACK_SIZE]Item = [_]Item{Item.default()} ** STACK_SIZE,
    stack_base: usize,
    obj_map: DynamicBitSet,
    heap: *Heap,

    literals: *Stack(Item),
    globals: Global,

    methods: Linked(Methods),

    stack_view: [*]Item,
    register: [*]Item,
    depth: usize,

    call_stack: CallStack,

    chunk: *Chunk,
    ip: usize,

    allocator: Allocator,

    pub fn create(meta: *CompilerMeta, main: AssembledResult, allocator: Allocator) !*@This() {
        const result = try allocator.create(@This());

        result.stack = [_]Item{Item.default()} ** STACK_SIZE;
        result.stack_base = 0;
        result.heap = try Heap.create(allocator);

        result.literals = &meta.literals;
        result.globals = try Global.default(result.heap);

        result.stack_view = result.stack[0..STACK_SIZE];
        result.register = result.stack[0..REGISTER_COUNT];
        result.depth = 0;
        result.obj_map = try DynamicBitSet.create(allocator, STACK_SIZE);

        result.call_stack = try CallStack.create(allocator, CALL_STACK_SIZE);
        result.ip = 0;

        result.allocator = allocator;

        result.heap.debug();

        result.load_code(main);

        return result;
    }

    pub fn load_code(self: *@This(), code: AssembledResult) void {
        self.chunk = code.chunk;
        self.ip = 0;
    }

    pub fn destroy(self: *@This()) void {
        self.stack.destroy();
        self.chunk.destroy();
    }

    pub inline fn load_literal(self: *@This(), address: u22) Item {
        return self.literals.get(@intCast(usize, address));
    }

    pub inline fn append_literals(self: *@This(), items: []const Item) !usize {
        const offset = self.literals.count();
        try self.literals.ensure_capacity(offset + items.len);
        for (items) |item| {
            try self.literals.push(item);
        }

        return offset;
    }

    pub inline fn get_global(self: *@This(), name_text: Item) Item {
        return self.globals.get(name_text).?;
    }

    pub inline fn set_global(self: *@This(), name_text: Item, value: Item) !void {
        _ = try self.globals.set(name_text, value);
    }

    pub inline fn recalc_reg(self: *@This()) !void {
        // out of stack
        std.debug.assert(STACK_SIZE > (self.depth + self.stack_base));
        const depth_offset = self.depth * 1024;
        self.register = self.stack_view[depth_offset..(depth_offset + 1024)].ptr;
    }

    pub inline fn recalc_stack(self: *@This()) !void {
        // out of stack
        std.debug.assert(STACK_SIZE > (self.depth + self.stack_base));
        const depth_offset = self.depth * 1024;
        const stack_base_offset = self.stack_base * 1024;
        self.stack_view = self.stack[stack_base_offset..(STACK_SIZE)].ptr;
        self.register = self.stack_view[depth_offset..(depth_offset + 1024)].ptr;
    }

    pub fn call_fn(self: *@This(), body: *Chunk, result: u10) !void {
        const frame = CallFrame{
            .function = self.chunk,
            .ip = self.ip,
            .depth = self.depth,
            .base = self.stack_base,
            .result = result,
        };

        self.chunk = body;
        self.ip = 0;
        self.stack_base += (self.depth + 1);
        self.depth = 0;

        try self.recalc_stack();

        try self.call_stack.push(frame);
        return;
    }

    pub fn ret_fn(self: *@This()) !void {
        var frame = self.call_stack.pop();

        self.chunk = frame.function;
        self.ip = frame.ip;
        self.stack_base = frame.base;
        self.depth = frame.depth;

        const result = self.register[0];

        try self.recalc_stack();

        self.register[frame.result] = result;
        return;
    }

    pub fn call(self: *@This(), function: Item, arguments: ?[]Item) void {
        const to_call = function.function();
        self.call_fn(to_call.body, 0) catch unreachable;
        if (arguments != null) {
            @memcpy(self.register[1 .. to_call.arity + 1], arguments.?);
        }
    }

    pub fn execute(self: *@This()) void {
        //self.chunk.debug(false);
        while (true) {
            const opcode: Opcode = self.chunk.next_op(&self.ip);
            switch (opcode.op) {
                .no_op => continue,
                .ret => {
                    const root = opcode.a() > 0;
                    const bottom = self.call_stack.is_bottom();
                    if (root or bottom) {
                        //we are returning from a script not function
                        self.depth = 0;
                        self.recalc_stack() catch unreachable;
                        return;
                    } else {
                        self.ret_fn() catch unreachable;
                    }
                },
                // todo
                .dive => {
                    self.depth += 1;
                    self.recalc_reg() catch unreachable;
                },
                // todo
                .ascend => {
                    self.depth -= 1;
                    self.recalc_reg() catch unreachable;
                },
                .call => {
                    const result = opcode.a();
                    const arg_start = opcode.b();
                    const callee = opcode.c();
                    const has_return = opcode.d();

                    const function = self.register[callee].function().internal;
                    const args = self.register[arg_start .. arg_start + function.arity];

                    //function.body.debug(true);

                    if (has_return == 0) {
                        // call extern method with no return value
                        self.call_fn(function.body, 0) catch unreachable;
                        @memcpy(self.register[1 .. function.arity + 1], args);
                    } else {
                        // call extern method with return value
                        self.call_fn(function.body, result) catch unreachable;
                        @memcpy(self.register[1 .. function.arity + 1], args);
                    }
                },
                .call_extern => {
                    const result = opcode.a();
                    const arg_start = opcode.b();
                    const callee = opcode.c();
                    const has_return = opcode.d();
                    const function = self.register[callee].function().external;

                    if (has_return == 0) {
                        // call extern method with no return value
                        const arity = function.arity;
                        function.body(self.register[arg_start..(arg_start + arity)], null);
                    } else {
                        // call extern method with return value
                        const arity = function.arity;
                        function.body(self.register[arg_start..(arg_start + arity)], &self.register[result]);
                    }
                },
                //.invoke_extern => {
                //    const a = opcode.a();
                //    const b = opcode.b();
                //    const c = opcode.c();
                //    const d = opcode.d();
                //    const method_name = self.register[c].text();
                //    const object: *Object = self.register[b].object.object();
                //    const method = object.methods.?.get(method_name).?.external;
                //
                //    if (d == 0) {
                //        // call extern method with no return value
                //        const arity = method.arity;
                //        method.body(&object, self.register[b..(b + arity)], null);
                //    } else {
                //        // call extern method with return value
                //        const arity = method.arity;
                //        method.body(&object, self.register[(b + 1)..(b + arity)], &self.register[a]);
                //    }
                //},

                // Memory
                .load_true => {
                    const dest = opcode.a();
                    self.register[dest] = Item.from(bool, true);
                },
                .load_false => {
                    const dest = opcode.a();
                    self.register[dest] = Item.from(bool, false);
                },
                .load_literal => {
                    const a = opcode.a();
                    const y = opcode.y();
                    self.register[a] = self.load_literal(y);
                },
                .load_global => {
                    const name_register = opcode.a();
                    const name: Item = self.register[name_register];
                    const load_register = opcode.b();
                    const value = self.get_global(name);
                    self.register[load_register] = value;
                },
                .store_global => {
                    const name_register = opcode.b();
                    const value_register = opcode.a();
                    const value = self.register[value_register];
                    const name = self.register[name_register];
                    self.set_global(name, value) catch unreachable;
                },
                // todo copy obj bit
                .get_upvalue => {
                    const a = opcode.a();
                    const y = opcode.y();
                    //std.debug.print("get up: {}\n", .{self.stack_view[y].i64});
                    self.register[a] = self.stack_view[y];
                },
                // todo copy obj bit
                .set_upvalue => {
                    const a = opcode.a();
                    const y = opcode.y();
                    //std.debug.print("set up: {}\n", .{self.register[a].i64});
                    self.stack_view[y] = self.register[a];
                },
                // todo copy obj bit
                .copy => {
                    const result = opcode.a();
                    const value = opcode.b();
                    self.register[result] = self.register[value];
                },

                // Arithmetic
                .add_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(i64, b + c);
                },
                .sub_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(i64, b - c);
                },
                .mul_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(i64, b * c);
                },
                .div_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(i64, @divExact(b, c));
                },
                .add_i32 => {
                    const b = self.register[opcode.b()].i32;
                    const c = self.register[opcode.c()].i32;
                    self.register[opcode.a()] = Item.from(i32, b + c);
                },
                .sub_i32 => {
                    const b = self.register[opcode.b()].i32;
                    const c = self.register[opcode.c()].i32;
                    self.register[opcode.a()] = Item.from(i32, b - c);
                },
                .mul_i32 => {
                    const b = self.register[opcode.b()].i32;
                    const c = self.register[opcode.c()].i32;
                    self.register[opcode.a()] = Item.from(i32, b * c);
                },
                .div_i32 => {
                    const b = self.register[opcode.b()].i32;
                    const c = self.register[opcode.c()].i32;
                    self.register[opcode.a()] = Item.from(i32, @divExact(b, c));
                },
                .add_i16 => {
                    const b = self.register[opcode.b()].i16;
                    const c = self.register[opcode.c()].i16;
                    self.register[opcode.a()] = Item.from(i16, b + c);
                },
                .sub_i16 => {
                    const b = self.register[opcode.b()].i16;
                    const c = self.register[opcode.c()].i16;
                    self.register[opcode.a()] = Item.from(i16, b - c);
                },
                .mul_i16 => {
                    const b = self.register[opcode.b()].i16;
                    const c = self.register[opcode.c()].i16;
                    self.register[opcode.a()] = Item.from(i16, b * c);
                },
                .div_i16 => {
                    const b = self.register[opcode.b()].i16;
                    const c = self.register[opcode.c()].i16;
                    self.register[opcode.a()] = Item.from(i16, @divExact(b, c));
                },
                .add_i8 => {
                    const b = self.register[opcode.b()].i8;
                    const c = self.register[opcode.c()].i8;
                    self.register[opcode.a()] = Item.from(i8, b + c);
                },
                .sub_i8 => {
                    const b = self.register[opcode.b()].i8;
                    const c = self.register[opcode.c()].i8;
                    self.register[opcode.a()] = Item.from(i8, b - c);
                },
                .mul_i8 => {
                    const b = self.register[opcode.b()].i8;
                    const c = self.register[opcode.c()].i8;
                    self.register[opcode.a()] = Item.from(i8, b * c);
                },
                .div_i8 => {
                    const b = self.register[opcode.b()].i8;
                    const c = self.register[opcode.c()].i8;
                    self.register[opcode.a()] = Item.from(i8, @divExact(b, c));
                },

                .add_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(u64, b + c);
                },
                .sub_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(u64, b - c);
                },
                .mul_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(u64, b * c);
                },
                .div_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(u64, b / c);
                },
                .add_u32 => {
                    const b = self.register[opcode.b()].u32;
                    const c = self.register[opcode.c()].u32;
                    self.register[opcode.a()] = Item.from(u32, b + c);
                },
                .sub_u32 => {
                    const b = self.register[opcode.b()].u32;
                    const c = self.register[opcode.c()].u32;
                    self.register[opcode.a()] = Item.from(u32, b - c);
                },
                .mul_u32 => {
                    const b = self.register[opcode.b()].u32;
                    const c = self.register[opcode.c()].u32;
                    self.register[opcode.a()] = Item.from(u32, b * c);
                },
                .div_u32 => {
                    const b = self.register[opcode.b()].u32;
                    const c = self.register[opcode.c()].u32;
                    self.register[opcode.a()] = Item.from(u32, b / c);
                },
                .add_u16 => {
                    const b = self.register[opcode.b()].u16;
                    const c = self.register[opcode.c()].u16;
                    self.register[opcode.a()] = Item.from(u16, b + c);
                },
                .sub_u16 => {
                    const b = self.register[opcode.b()].u16;
                    const c = self.register[opcode.c()].u16;
                    self.register[opcode.a()] = Item.from(u16, b - c);
                },
                .mul_u16 => {
                    const b = self.register[opcode.b()].u16;
                    const c = self.register[opcode.c()].u16;
                    self.register[opcode.a()] = Item.from(u16, b * c);
                },
                .div_u16 => {
                    const b = self.register[opcode.b()].u16;
                    const c = self.register[opcode.c()].u16;
                    self.register[opcode.a()] = Item.from(u16, b / c);
                },
                .add_u8 => {
                    const b = self.register[opcode.b()].u8;
                    const c = self.register[opcode.c()].u8;
                    self.register[opcode.a()] = Item.from(u8, b + c);
                },
                .sub_u8 => {
                    const b = self.register[opcode.b()].u8;
                    const c = self.register[opcode.c()].u8;
                    self.register[opcode.a()] = Item.from(u8, b - c);
                },
                .mul_u8 => {
                    const b = self.register[opcode.b()].u8;
                    const c = self.register[opcode.c()].u8;
                    self.register[opcode.a()] = Item.from(u8, b * c);
                },
                .div_u8 => {
                    const b = self.register[opcode.b()].u8;
                    const c = self.register[opcode.c()].u8;
                    self.register[opcode.a()] = Item.from(u8, b / c);
                },

                .add_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(f64, b + c);
                },
                .sub_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(f64, b - c);
                },
                .mul_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(f64, b * c);
                },
                .div_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(f64, b / c);
                },

                .add_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(f32, b + c);
                },
                .sub_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(f32, b - c);
                },
                .mul_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(f32, b * c);
                },
                .div_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(f32, b / c);
                },

                .inc_i64 => {
                    const x = opcode.x();
                    self.stack_view[x].i64 += 1;
                },
                .inc_u64 => unreachable,
                .inc_i32 => unreachable,
                .inc_u32 => unreachable,
                .inc_i16 => unreachable,
                .inc_u16 => unreachable,
                .inc_i8 => unreachable,
                .inc_u8 => unreachable,

                .inc_f64 => unreachable,
                .inc_f32 => unreachable,

                .dec_i64 => unreachable,
                .dec_u64 => unreachable,
                .dec_i32 => unreachable,
                .dec_u32 => unreachable,
                .dec_i16 => unreachable,
                .dec_u16 => unreachable,
                .dec_i8 => unreachable,
                .dec_u8 => unreachable,

                .dec_f64 => unreachable,
                .dec_f32 => unreachable,

                // Control flow
                .less_than_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_than_u32 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_than_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_than_i32 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_than_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_than_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(bool, b < c);
                },
                .less_equal_u64 => {
                    const b = self.register[opcode.b()].u64;
                    const c = self.register[opcode.c()].u64;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .less_equal_u32 => {
                    const b = self.register[opcode.b()].u32;
                    const c = self.register[opcode.c()].u32;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .less_equal_i64 => {
                    const b = self.register[opcode.b()].i64;
                    const c = self.register[opcode.c()].i64;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .less_equal_i32 => {
                    const b = self.register[opcode.b()].i32;
                    const c = self.register[opcode.c()].i32;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .less_equal_f64 => {
                    const b = self.register[opcode.b()].f64;
                    const c = self.register[opcode.c()].f64;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .less_equal_f32 => {
                    const b = self.register[opcode.b()].f32;
                    const c = self.register[opcode.c()].f32;
                    self.register[opcode.a()] = Item.from(bool, b < c or b == c);
                },
                .not => {
                    const dest = opcode.a();
                    const source = opcode.b();

                    const inverse = !self.register[source].bool;

                    self.register[dest] = Item.from(bool, inverse);
                },
                .if_jmp => {
                    const a = self.register[opcode.a()].bool;
                    const offset: i22 = @bitCast(i22, opcode.y());

                    if (a) {
                        const change: i64 = @intCast(i64, self.ip) + offset - 1;
                        self.ip = @intCast(usize, change);
                    }
                },
                .jmp => {
                    // zig is 1 filling alot of the bits on the git version.
                    // todo fix
                    const offset: i32 = @bitCast(i32, opcode.x());
                    const change: i64 = @intCast(i64, self.ip) + offset - 1;
                    self.ip = @intCast(usize, change);
                },

                .extended => return,
                inline else => {
                    std.debug.print("[THREAD]: ({s}) not implimented.\n", .{@tagName(opcode.op)});
                    unreachable;
                },
            }
        }
    }
};
