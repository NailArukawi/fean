const std = @import("std");
pub const vm = @import("vm/mod.zig");
pub const compiler = @import("compiler/mod.zig");

const Allocator = std.mem.Allocator;

pub const Fean = struct {
    config: *FeanConfig,
    thread: *vm.Thread,
    compiler_meta: *compiler.CompilerMeta,

    pub fn create(src: []const u8, config: *FeanConfig) !@This() {
        var compiler_meta = try compiler.CompilerMeta.default(config.allocator);

        // compiles a text file into a semi-linear instruction stack
        var ir = try compiler.Compiler.compile(src, config, compiler_meta);
        //var chunk = try compiler.Compiler.compile(ir_chunck, config.allocator);
        //var buffer: [512]u8 = [_]u8{0} ** 512;
        //try ir.debug(&buffer, compiler_meta);

        // transform the semi-linear instructions into somthing the thread can execute
        var main = try compiler.Assembler.assemble(ir, config.allocator, compiler_meta);
        //main.chunk.debug();

        var fean = @This(){
            .config = config,
            .thread = try vm.Thread.create(compiler_meta, main, config.allocator),
            .compiler_meta = compiler_meta,
        };

        fean.thread.execute();

        return fean;
    }
};

pub const FeanConfig = struct {
    allocator: Allocator,
    silent: bool = false,

    file_debug: bool = false,
    file_lookup: ?*const fn (compiler.token.Span) FileLookup = null,

    fn_lookup: ?*const fn (name: []const u8) ?*const fn (args: []vm.Item, result: ?*vm.Item) void,

    file_id_count: u32 = 1,
    compile_flags: FeanCompileFlags,

    pub fn default(allocator: Allocator) @This() {
        return @This(){
            .allocator = allocator,
            .silent = false,

            .file_debug = false,
            .file_lookup = null,

            .fn_lookup = null,

            .file_id_count = 1,
            .compile_flags = .{},
        };
    }
};

// how fean asks for files in case of error
// it asks for the file by name.
pub const FileLookup = struct {
    filename: []const u8,
    content: []const u8,
};

// how you set if you want the code to be optimized
pub const FeanCompileFlags = struct {
    // folds consts so that the pool will not have dupes.
    literal_const_folding: bool = true,

    // will compute literals at compile time
    literal_folding: bool = true,
};
