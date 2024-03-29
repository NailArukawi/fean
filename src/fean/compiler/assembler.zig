const std = @import("std");

const SymbolTable = @import("symboltable.zig").SymbolTable;
const KindTable = @import("kindtable.zig").KindTable;
const IR = @import("mod.zig").IR;
const IRBlock = @import("mod.zig").IRBlock;
const Instr = @import("mod.zig").Instr;
const Address = @import("mod.zig").Address;
const CompilerMeta = @import("mod.zig").CompilerMeta;
const Chunk = @import("../runtime/mod.zig").Chunk;
const Op = @import("../runtime/mod.zig").Op;
const Opcode = @import("../runtime/mod.zig").Opcode;
const Stack = @import("../common/stack.zig").Stack;

const Allocator = std.mem.Allocator;

pub const AssembledResult = struct {
    chunk: *Chunk,

    pub fn default(allocator: Allocator) !@This() {
        return @This(){
            .chunk = try Chunk.create(allocator),
        };
    }
};

pub const JumpRecord = struct {
    id: Address,
    pos: usize,
};

pub const JumpRecordStack = struct {
    inner: Stack(JumpRecord),

    pub fn create(allocator: Allocator) !@This() {
        return @This(){
            .inner = try Stack(JumpRecord).create(allocator, 32),
        };
    }

    pub fn destroy(self: *@This()) void {
        self.inner.destroy();
    }

    pub fn push(self: *@This(), id: Address, pos: usize) !void {
        const asertee = JumpRecord{ .id = id, .pos = pos };

        try self.inner.push(asertee);
    }

    pub fn get(self: *@This(), id: Address) ?JumpRecord {
        const mem: []JumpRecord = self.inner.as_slice();
        var result: ?JumpRecord = null;
        var found: ?usize = null;

        for (mem, 0..) |jr, i| {
            if (jr.id.inner == id.inner) {
                result = jr;
                found = i;
                break;
            }
        }

        if (found == null) {
            return null;
        }
        if (self.inner.used == 1) {
            self.inner.used = 0;
        } else if (found.? == (self.inner.used - 1)) {
            self.inner.used -= 1;
        } else {
            for (found.?..(mem.len - 1)) |i| {
                mem[i] = mem[i + 1];
            }

            self.inner.used -= 1;
        }

        return result;
    }
};

pub const Assembler = struct {
    allocator: Allocator,
    result: AssembledResult,
    meta: *CompilerMeta,
    body: *IR,

    in_fn: bool = false,
    detatched_body: ?*IRBlock = null,
    op_count: usize = 0,

    jumps: JumpRecordStack,

    pub fn assemble(ir: *IR, allocator: Allocator, meta: *CompilerMeta) !AssembledResult {
        var assembler = @This(){
            .allocator = allocator,
            .result = try AssembledResult.default(allocator),
            .meta = meta,
            .body = ir,
            .jumps = try JumpRecordStack.create(allocator),
        };

        var i: usize = 0;
        for (ir.body.as_slice()) |instr| {
            i += try assembler.assemble_instr(instr, i);
        }

        const end = assembler.op_count - 1;
        const last: Opcode = assembler.result.chunk.code.items[end];
        if (last.z.op != .ret) {
            var opcode = Opcode.default();
            opcode.z.op = Op.ret;

            opcode.z.a = 1;

            try assembler.push_op(opcode);
        }

        return assembler.result;
    }

    fn assemble_fn(self: *@This(), fn_body: *IRBlock) !AssembledResult {
        // store old
        const result = self.result;
        defer self.result = result;

        const op_count = self.op_count;
        defer self.op_count = op_count;

        const jumps = self.jumps;
        defer self.jumps = jumps;

        // store new
        const new_result = AssembledResult.default(self.allocator) catch unreachable;
        self.op_count = 0;
        self.result = new_result;

        var new_jumps: JumpRecordStack = try JumpRecordStack.create(self.allocator);
        self.jumps = new_jumps;
        defer new_jumps.destroy();

        self.detatched_body = fn_body;
        defer self.detatched_body = null;

        self.in_fn = true;
        defer self.in_fn = false;

        var i: usize = 0;
        for (fn_body.body.as_slice()) |instr| {
            i += (try self.assemble_instr(instr, i));
        }

        const end = self.op_count - 1;
        const last: Opcode = self.result.chunk.code.items[end];
        if (last.z.op != .ret) {
            var opcode = Opcode.default();
            opcode.z.op = Op.ret;

            opcode.z.a = 0;

            try self.push_op(opcode);
        }

        return new_result;
    }

    fn assemble_meth(self: *@This(), meth_body: *IRBlock) !AssembledResult {
        // store old
        const result = self.result;
        defer self.result = result;

        const op_count = self.op_count;
        defer self.op_count = op_count;

        const jumps = self.jumps;
        defer self.jumps = jumps;

        // store new
        const new_result = AssembledResult.default(self.allocator) catch unreachable;
        self.op_count = 0;
        self.result = new_result;

        var new_jumps: JumpRecordStack = try JumpRecordStack.create(self.allocator);
        self.jumps = new_jumps;
        defer new_jumps.destroy();

        self.detatched_body = meth_body;
        defer self.detatched_body = null;

        self.in_fn = true;
        defer self.in_fn = false;

        var i: usize = 0;
        for (meth_body.body.as_slice()) |instr| {
            i += (try self.assemble_instr(instr, i));
        }

        const end = self.op_count - 1;
        const last: Opcode = self.result.chunk.code.items[end];
        if (last.z.op != .ret) {
            var opcode = Opcode.default();
            opcode.z.op = Op.ret;

            opcode.z.a = 0;

            try self.push_op(opcode);
        }

        return new_result;
    }

    fn assemble_instr(self: *@This(), instr: Instr, i: usize) anyerror!usize {
        var lines_written: usize = 1;

        switch (instr) {
            // Misc
            .no_op => unreachable,
            .ret => |r| {
                var opcode = Opcode.default();
                opcode.z.op = Op.ret;

                if (r)
                    opcode.z.a = 1;

                try self.push_op(opcode);
            },
            // todo mode
            // todo no result
            .call => |c| {
                var opcode = Opcode.default();
                opcode.z.op = Op.call;

                const has_args = (c.arg_start != null);

                opcode.z.a = c.result.register();
                if (has_args)
                    opcode.z.b = c.arg_start.?.register();

                opcode.z.c = c.callee.register();

                try self.push_op(opcode);
            },
            // todo mode
            // todo no result
            .call_extern => |ce| {
                var opcode = Opcode.default();
                opcode.z.op = Op.call_extern;

                const has_args = (ce.arg_start != null);

                opcode.z.a = ce.result.register();
                if (has_args)
                    opcode.z.b = ce.arg_start.?.register();

                opcode.z.c = ce.callee.register();

                try self.push_op(opcode);
            },
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,
            .dive => {
                var opcode = Opcode.default();
                opcode.z.op = Op.dive;
                try self.push_op(opcode);
            },
            .ascend => {
                var opcode = Opcode.default();
                opcode.z.op = Op.ascend;
                try self.push_op(opcode);
            },

            // Memory
            .load_true => |load| {
                var opcode = Opcode.default();
                opcode.z.op = Op.load_true;

                const result = load.register();
                opcode.z.a = result;

                try self.push_op(opcode);
            },
            .load_false => |load| {
                var opcode = Opcode.default();
                opcode.z.op = Op.load_false;

                const result = load.register();
                opcode.z.a = result;

                try self.push_op(opcode);
            },
            .load_literal => |load| {
                var opcode = Opcode.default();
                opcode.y.op = Op.load_literal;

                const result = load.result.register();
                opcode.y.a = result;

                const lit = load.a.literal();
                opcode.y.y = @intCast(lit);

                try self.push_op(opcode);
            },
            .load_literal_obj => unreachable,
            .load_global => |load| {
                var opcode = Opcode.default();
                opcode.z.op = Op.load_global;

                const result = load.a.register();
                opcode.z.a = result;

                const global_name = load.result.register();
                opcode.z.b = global_name;

                try self.push_op(opcode);
            },
            .load_global_obj => unreachable,
            .store_global => |load| {
                var opcode = Opcode.default();
                opcode.z.op = Op.store_global;

                const storee = load.a.register();
                opcode.z.a = storee;

                const global_name = load.result.register();
                opcode.z.b = global_name;

                try self.push_op(opcode);
            },
            .get_upvalue => |load| {
                var opcode = Opcode.default();
                opcode.y.op = Op.get_upvalue;

                const result = load.result.register();
                opcode.y.a = result;

                const real = load.a.upvalue();
                opcode.y.y = @intCast(real);

                try self.push_op(opcode);
            },
            .set_upvalue => |load| {
                var opcode = Opcode.default();
                opcode.y.op = Op.set_upvalue;

                const storee = load.a.register();
                opcode.y.a = storee;

                const real = load.result.upvalue();
                opcode.y.y = @intCast(real);

                try self.push_op(opcode);
            },
            .create_struct => |create| {
                var opcode = Opcode.default();
                opcode.y.op = Op.create_struct;

                const storee = create.result.register();
                opcode.y.a = storee;

                const real = create.kind.upvalue();
                opcode.y.y = @intCast(real);

                try self.push_op(opcode);
            },
            .copy => |load| {
                var opcode = Opcode.default();
                opcode.z.op = Op.copy;

                const storee = load.result.register();
                opcode.z.a = storee;

                const value = load.a.register();
                opcode.z.b = value;

                try self.push_op(opcode);
            },
            .get_struct_field_u64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_u64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_u32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_u32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_u16 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_u16;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_u8 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_u8;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_i64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_i64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_i32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_i32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_i16 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_i16;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_i8 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_i8;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_f64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_f64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_f32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_f32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .get_struct_field_obj => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.get_struct_field_obj;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_u64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_u64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_u32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_u32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_u16 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_u16;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_u8 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_u8;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_i64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_i64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_i32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_i32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_i16 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_i16;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_i8 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_i8;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_f64 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_f64;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_f32 => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_f32;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },
            .set_struct_field_obj => |struct_access| {
                var opcode = Opcode.default();
                opcode.z.op = Op.set_struct_field_obj;

                const reg = struct_access.reg.register();
                opcode.z.a = reg;

                const this = struct_access.this.register();
                opcode.z.b = this;

                const index = struct_access.index.field();
                opcode.z.c = @intCast(index);

                try self.push_op(opcode);
            },

            // Arithmetic
            .add_u64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.add_u64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .sub_u64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.sub_u64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .mul_u64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.mul_u64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .div_u64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.div_u64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .add_u32 => unreachable,
            .sub_u32 => unreachable,
            .mul_u32 => unreachable,
            .div_u32 => unreachable,
            .add_u16 => unreachable,
            .sub_u16 => unreachable,
            .mul_u16 => unreachable,
            .div_u16 => unreachable,
            .add_u8 => unreachable,
            .sub_u8 => unreachable,
            .mul_u8 => unreachable,
            .div_u8 => unreachable,

            .add_i64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.add_i64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .sub_i64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.sub_i64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .mul_i64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.mul_i64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .div_i64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.div_i64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .add_i32 => unreachable,
            .sub_i32 => unreachable,
            .mul_i32 => unreachable,
            .div_i32 => unreachable,
            .add_i16 => unreachable,
            .sub_i16 => unreachable,
            .mul_i16 => unreachable,
            .div_i16 => unreachable,
            .add_i8 => unreachable,
            .sub_i8 => unreachable,
            .mul_i8 => unreachable,
            .div_i8 => unreachable,

            .add_f64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.add_f64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .sub_f64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.sub_f64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .mul_f64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.mul_f64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .div_f64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.div_f64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },

            .add_f32 => unreachable,
            .sub_f32 => unreachable,
            .mul_f32 => unreachable,
            .div_f32 => unreachable,

            .inc_i64 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_i64;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_u64 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_u64;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_i32 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_i32;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_u32 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_u32;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_i16 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_i16;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_u16 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_u16;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_i8 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_i8;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_u8 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_u8;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },

            .inc_f64 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_f64;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .inc_f32 => |inc| {
                var opcode = Opcode.default();
                opcode.x.op = Op.inc_f32;

                const result = inc.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },

            .dec_i64 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_i64;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_u64 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_u64;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_i32 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_i32;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_u32 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_u32;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_i16 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_i16;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_u16 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_u16;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_i8 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_i8;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_u8 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_u8;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },

            .dec_f64 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_f64;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },
            .dec_f32 => |dec| {
                var opcode = Opcode.default();
                opcode.x.op = Op.dec_f32;

                const result = dec.upvalue();
                opcode.x.x = @intCast(result);

                try self.push_op(opcode);
            },

            // conditional
            .less_than_i64 => |arithmetic| {
                var opcode = Opcode.default();
                opcode.z.op = Op.less_than_i64;

                const result = arithmetic.result.register();
                opcode.z.a = result;

                const a = arithmetic.a.register();
                opcode.z.b = a;

                const b = arithmetic.b.register();
                opcode.z.c = b;

                try self.push_op(opcode);
            },
            .not => |n| {
                var opcode = Opcode.default();
                opcode.z.op = Op.not;

                const dest = n.dest.register();
                opcode.z.a = dest;

                const source = n.source.register();
                opcode.z.b = source;

                try self.push_op(opcode);
            },
            .if_jmp => |ij| {
                var opcode = Opcode.default();
                opcode.y.op = Op.if_jmp;

                const result = ij.condition.register();
                opcode.y.a = result;

                const jump = self.jumps.get(ij.offset);
                if (jump != null) {
                    const pos: isize = @as(isize, @intCast(jump.?.pos));
                    const offset = @as(i20, @intCast(pos - @as(isize, @intCast(i))));
                    opcode.y.y = @bitCast(offset);

                    try self.push_op(opcode);
                } else {
                    try self.push_op(opcode);

                    try self.jumps.push(ij.offset, self.op_count - 1);
                }
            },
            .jmp => |j| {
                var opcode = Opcode.default();
                opcode.z.op = Op.jmp;

                const jump = self.jumps.get(j.offset);
                if (jump != null) {
                    const pos: isize = @as(isize, @intCast(jump.?.pos));
                    const offset = @as(i30, @intCast(pos - @as(isize, @intCast(i))));
                    opcode.x.x = @bitCast(offset);

                    try self.push_op(opcode);
                } else {
                    try self.push_op(opcode);

                    try self.jumps.push(j.offset, self.op_count - 1);
                }
            },

            // meta
            .block => |blck| {
                var j: usize = 0;

                for (blck.body.as_slice()) |inner_instr|
                    j += try self.assemble_instr(inner_instr, i + j);

                lines_written = j;
            },
            .destination => |dest| {
                lines_written = 0;

                const jump = self.jumps.get(dest);
                if (jump == null) {
                    try self.jumps.push(dest, self.op_count);
                } else {
                    var opcode = &self.result.chunk.code.items[jump.?.pos];

                    if (opcode.z.op == .if_jmp) {
                        const offset = @as(i20, @intCast(i - jump.?.pos));
                        opcode.y.y = @bitCast(offset);
                    } else if (opcode.z.op == .jmp) {
                        const offset = @as(i30, @intCast(i - jump.?.pos));
                        opcode.x.x = @bitCast(offset);
                    }
                }
            },
            .fn_to_assemble => |fta| {
                lines_written = 0;

                const block_addr = fta.block.?.raw();
                const fn_body = @as(*IRBlock, @ptrFromInt(block_addr));
                const assembled_fn_body = try self.assemble_fn(fn_body);

                fn_body.destroy();

                fta.memory.internal.body = assembled_fn_body.chunk;
            },
            .method_to_assemble => |mta| {
                lines_written = 0;

                const block_addr = mta.block.?.raw();
                const meth_body = @as(*IRBlock, @ptrFromInt(block_addr));
                const assembled_method_body = try self.assemble_meth(meth_body);

                meth_body.destroy();

                mta.memory.internal.body = assembled_method_body.chunk;
                try mta.Self.methods.?.set(mta.name, mta.memory);
            },
            .reserve => |r| {
                _ = r;
                lines_written = 0;
            },
            .drop => |d| {
                _ = d;
                lines_written = 0;
            },

            // extended
            .extended => unreachable,
            else => error_(@tagName(instr)),
        }

        return lines_written;
    }

    fn push_op(self: *@This(), opcode: Opcode) !void {
        try self.result.chunk.append_op(opcode);
        self.op_count += 1;
    }
};

pub fn error_(missing: []const u8) void {
    std.debug.panic("[Assembler]: ({s}) not implimented.\n", .{missing});
}
