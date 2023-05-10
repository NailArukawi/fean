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
const REGISTER_STACK_DEPTH: usize = 4;
const LITERALS_INITIAL_SIZE: usize = 256;
const CALL_STACK_SIZE: usize = 64;

pub const Thread = struct {
    heap: *Heap,

    literals: *Stack(Item),
    globals: Global,

    methods: Linked(Methods),

    register: [*]Item,
    stack: Stack(Item),
    depth: usize,
    obj_map: DynamicBitSet,
    stack_depth: usize,

    call_stack: Stack(CallFrame),
    chunk: *Chunk,
    ip: usize,

    allocator: Allocator,

    pub fn create(meta: *CompilerMeta, main: AssembledResult, allocator: Allocator) !*@This() {
        const result = try allocator.create(@This());

        const stack = try Stack(Item).create(allocator, REGISTER_COUNT * REGISTER_STACK_DEPTH);
        result.heap = try Heap.create(allocator);

        result.literals = &meta.literals;
        result.globals = try Global.default(result.heap);

        result.register = stack.inner[0..REGISTER_COUNT];
        result.stack = stack;
        result.depth = 0;
        result.obj_map = try DynamicBitSet.create(allocator, result.stack.capacity);
        result.stack_depth = 0;

        result.call_stack = try Stack(CallFrame).create(allocator, CALL_STACK_SIZE);
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

    pub fn clear_register(self: *@This()) void {
        var i: usize = 0;
        while (i < REGISTER_COUNT) : (i += 1) {
            self.register[i] = Item.default();
            self.obj_map.set(i + self.stack_depth);
        }
    }

    pub inline fn load_literal(self: *@This(), address: u22) Item {
        const to_get = @intCast(usize, address);
        return self.literals.get(to_get);
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

    pub inline fn reg_to_top(self: *@This()) !void {
        const depth_offset = self.depth * 1024;
        if (self.stack.capacity < self.depth * 1024) {
            const new_size = self.depth * 1024;
            try self.stack.ensure_capacity(new_size);
        }

        self.register = self.stack.inner[depth_offset..(depth_offset + 1024)].ptr;
    }

    pub inline fn execute(self: *@This()) void {
        while (true) {
            const opcode: Opcode = self.chunk.next_op(&self.ip);
            switch (opcode.op) {
                .no_op => continue,
                .ret => return,
                // todo
                .dive => {
                    self.depth += 1;
                    self.reg_to_top() catch unreachable;
                },
                // todo
                .ascend => {
                    self.depth -= 1;
                    self.reg_to_top() catch unreachable;
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
                    self.register[a] = self.stack.inner[y];
                },
                // todo copy obj bit
                .set_upvalue => {
                    const a = opcode.a();
                    const y = opcode.y();
                    self.stack.inner[y] = self.register[a];
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
                else => {
                    std.debug.print("[THREAD]: ({s}) not implimented.\n", .{@tagName(opcode.op)});
                    unreachable;
                },
            }
        }
    }
};
