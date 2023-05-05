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

const Allocator = std.mem.Allocator;

pub const AssembledResult = struct {
    chunk: *Chunk,

    pub fn default(allocator: Allocator) !@This() {
        return @This(){
            .chunk = try Chunk.create(allocator),
        };
    }
};

pub const Assembler = struct {
    allocator: Allocator,
    result: AssembledResult,
    meta: *CompilerMeta,
    body: *IR,

    pub fn assemble(ir: *IR, allocator: Allocator, meta: *CompilerMeta) !AssembledResult {
        var assembler = @This(){
            .allocator = allocator,
            .result = try AssembledResult.default(allocator),
            .meta = meta,
            .body = ir,
        };

        for (ir.body.as_slice()) |instr| {
            try assembler.assemble_instr(instr);
        }

        return assembler.result;
    }

    fn assemble_instr(self: *@This(), instr: Instr) !void {
        switch (instr) {
            // Misc
            .no_op => unreachable,
            .ret => {
                var opcode = Opcode.new();
                opcode.op = Op.ret;
                try self.result.chunk.append_op(opcode);
            },
            .call => unreachable,
            .call_extern => unreachable,
            .invoke => unreachable,
            .invoke_extern => unreachable,
            .make_closure => unreachable,

            // Memory
            .load_literal => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_literal;

                const result = load.result.register();
                opcode.set_a(result);

                const lit = @intCast(u22, load.a.literal());
                opcode.set_y(lit);

                try self.result.chunk.append_op(opcode);
            },
            .load_literal_obj => unreachable,
            .load_global => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.load_global;

                const result = load.a.register();
                opcode.set_a(result);

                const global_name = load.result.register();
                opcode.set_b(global_name);

                try self.result.chunk.append_op(opcode);
            },
            .load_global_obj => unreachable,
            .store_global => |load| {
                var opcode = Opcode.new();
                opcode.op = Op.store_global;

                const storee = load.a.register();
                opcode.set_a(storee);

                const global_name = load.result.register();
                opcode.set_b(global_name);

                try self.result.chunk.append_op(opcode);
            },
            .get_upvalue => unreachable,
            .get_upvalue_obj => unreachable,
            .set_upvalue => unreachable,
            .open_upvalue => unreachable,
            .close_upvalue => unreachable,

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

                try self.result.chunk.append_op(opcode);
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

                try self.result.chunk.append_op(opcode);
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

            // meta
            .block => |blck| {
                for (blck.body.as_slice()) |block_instr| {
                    try self.assemble_instr(block_instr);
                }
            },

            // extended
            .extended => unreachable,
        }
    }
};
