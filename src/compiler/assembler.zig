const std = @import("std");

const SymbolTable = @import("symboltable.zig").SymbolTable;
const KindTable = @import("kindtable.zig").KindTable;
const IR = @import("mod.zig").IR;
const Instr = @import("mod.zig").Instr;
const Address = @import("mod.zig").Address;
const CompilerMeta = @import("mod.zig").CompilerMeta;
const Chunk = @import("../vm/mod.zig").Chunk;
const Op = @import("../vm/mod.zig").Op;
const Opcode = @import("../vm/mod.zig").Opcode;
const Stack = @import("../stack.zig").Stack;

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

        return assembler.result;
    }

    fn assemble_instr(self: *@This(), instr: Instr, i: usize) !usize {
        var lines_written: usize = 1;

        switch (instr) {
            // Misc
            .no_op => unreachable,
            .ret => {
                var opcode = Opcode.new();
                opcode.op = Op.ret;
                try self.push_op(opcode);
            },
            .call => unreachable,
            .call_extern => |ce| {
                var opcode = Opcode.new();
                opcode.op = Op.call_extern;

                opcode.set_a(ce.result.register());
                opcode.set_b(0); // todo
                opcode.set_c(ce.callee.register());
                opcode.set_d(1); //todo

                try self.push_op(opcode);
            },
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,
            .dive => {
                var opcode = Opcode.new();
                opcode.op = Op.dive;
                try self.push_op(opcode);
            },
            .ascend => {
                var opcode = Opcode.new();
                opcode.op = Op.ascend;
                try self.push_op(opcode);
            },

            // Memory
            .load_true => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_true;

                const result = load.register();
                opcode.set_a(result);

                try self.push_op(opcode);
            },
            .load_false => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_false;

                const result = load.register();
                opcode.set_a(result);

                try self.push_op(opcode);
            },
            .load_literal => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_literal;

                const result = load.result.register();
                opcode.set_a(result);

                const lit = @intCast(u22, load.a.literal());
                opcode.set_y(lit);

                try self.push_op(opcode);
            },
            .load_literal_obj => unreachable,
            .load_global => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_global;

                const result = load.a.register();
                opcode.set_a(result);

                const global_name = load.result.register();
                opcode.set_b(global_name);

                try self.push_op(opcode);
            },
            .load_global_obj => unreachable,
            .store_global => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.store_global;

                const storee = load.a.register();
                opcode.set_a(storee);

                const global_name = load.result.register();
                opcode.set_b(global_name);

                try self.push_op(opcode);
            },
            .get_upvalue => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.get_upvalue;

                const result = load.result.register();
                opcode.set_a(result);

                const real = load.a.upvalue();
                opcode.set_y(@intCast(u22, real));

                try self.push_op(opcode);
            },
            .set_upvalue => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.set_upvalue;

                const storee = load.a.register();
                opcode.set_a(storee);

                const real = load.result.upvalue();
                opcode.set_y(@intCast(u22, real));

                try self.push_op(opcode);
            },
            .copy => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.copy;

                const storee = load.result.register();
                opcode.set_a(storee);

                const value = load.a.register();
                opcode.set_b(value);

                try self.push_op(opcode);
            },

            // Arithmetic
            .add_u64 => |arithmetic| {
                var opcode = Opcode.new();
                opcode.op = Op.add_u64;

                const result = arithmetic.result.register();
                opcode.set_a(result);

                const a = arithmetic.a.register();
                opcode.set_b(a);

                const b = arithmetic.b.register();
                opcode.set_c(b);

                try self.push_op(opcode);
            },
            .sub_u64 => unreachable,
            .mul_u64 => unreachable,
            .div_u64 => unreachable,
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
                var opcode = Opcode.new();
                opcode.op = Op.add_i64;

                const result = arithmetic.result.register();
                opcode.set_a(result);

                const a = arithmetic.a.register();
                opcode.set_b(a);

                const b = arithmetic.b.register();
                opcode.set_c(b);

                try self.push_op(opcode);
            },
            .sub_i64 => unreachable,
            .mul_i64 => unreachable,
            .div_i64 => unreachable,
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

            .add_f64 => unreachable,
            .sub_f64 => unreachable,
            .mul_f64 => unreachable,
            .div_f64 => unreachable,

            .add_f32 => unreachable,
            .sub_f32 => unreachable,
            .mul_f32 => unreachable,
            .div_f32 => unreachable,

            // conditional
            .less_than_i64 => |arithmetic| {
                var opcode = Opcode.new();
                opcode.op = Op.less_than_i64;

                const result = arithmetic.result.register();
                opcode.set_a(result);

                const a = arithmetic.a.register();
                opcode.set_b(a);

                const b = arithmetic.b.register();
                opcode.set_c(b);

                try self.push_op(opcode);
            },
            .not => |n| {
                var opcode = Opcode.new();
                opcode.op = Op.not;

                const dest = n.dest.register();
                opcode.set_a(dest);

                const source = n.source.register();
                opcode.set_b(source);

                try self.push_op(opcode);
            },
            .if_jmp => |ij| {
                var opcode = Opcode.new();
                opcode.op = Op.if_jmp;

                const result = ij.condition.register();
                opcode.set_a(result);

                var jump = self.jumps.get(ij.offset);
                if (jump != null) {
                    var pos: isize = @intCast(isize, jump.?.pos);
                    var offset = @intCast(i22, pos - @intCast(isize, i));
                    opcode.set_y(@bitCast(u22, offset));

                    try self.push_op(opcode);
                } else {
                    try self.push_op(opcode);

                    try self.jumps.push(ij.offset, self.op_count - 1);
                }
            },
            .jmp => |j| {
                var opcode = Opcode.new();
                opcode.op = Op.jmp;

                var jump = self.jumps.get(j.offset);
                if (jump != null) {
                    var pos: isize = @intCast(isize, jump.?.pos);
                    var offset = @intCast(i32, pos - @intCast(isize, i));
                    opcode.set_x(@bitCast(u32, offset));

                    try self.push_op(opcode);
                } else {
                    try self.push_op(opcode);

                    try self.jumps.push(j.offset, self.op_count - 1);
                }
            },

            // meta
            .block => |blck| {
                var j: usize = 0;

                for (blck.body.as_slice()) |inner_instr| {
                    j += try self.assemble_instr(inner_instr, i + j);
                }

                lines_written = j;
            },
            .destination => |dest| {
                lines_written = 0;

                var jump = self.jumps.get(dest);
                if (jump == null) {
                    try self.jumps.push(dest, self.op_count);
                } else {
                    var opcode = &self.result.chunk.code.items[jump.?.pos];

                    if (opcode.op == .if_jmp) {
                        var offset = @intCast(i22, i - jump.?.pos);
                        opcode.set_y(@bitCast(u22, offset));
                    } else if (opcode.op == .jmp) {
                        var offset = @intCast(i32, i - jump.?.pos);
                        opcode.set_x(@bitCast(u32, offset));
                    }
                }
            },

            // extended
            .extended => unreachable,
            else => unreachable,
        }

        return lines_written;
    }

    fn push_op(self: *@This(), opcode: Opcode) !void {
        try self.result.chunk.append_op(opcode);
        self.op_count += 1;
    }
};
