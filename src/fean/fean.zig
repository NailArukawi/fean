const std = @import("std");
pub const runtime = @import("runtime/mod.zig");
pub const parser = @import("parser/mod.zig");
pub const compiler = @import("compiler/mod.zig");
pub const Ref = runtime.Ref;

const Allocator = std.mem.Allocator;

pub const Fean = struct {
    config: *FeanConfig,
    thread: *runtime.Thread,
    compiler_meta: *compiler.CompilerMeta,

    pub fn create(reader: std.fs.File.Reader, config: *FeanConfig) !@This() {
        const compiler_meta = try compiler.CompilerMeta.default(config.allocator);

        // compiles a text file into a semi-linear instruction stack
        var ir = try compiler.Compiler.compile(reader, config, compiler_meta);
        //var chunk = try compiler.Compiler.compile(ir_chunck, config.allocator)

        // transform the semi-linear instructions into somthing the thread can execute
        const main = try compiler.Assembler.assemble(ir, config.allocator, compiler_meta);

        //debugging
        var buffer: [512]u8 = [_]u8{0} ** 512;
        try ir.debug(&buffer, compiler_meta);
        //std.debug.print("\n[main.chunk.debug]\n", .{});
        //main.chunk.debug(false);

        var fean = @This(){
            .config = config,
            .thread = try runtime.Thread.create(compiler_meta, main, config.allocator),
            .compiler_meta = compiler_meta,
        };

        fean.thread.execute();

        return fean;
    }

    pub fn call(self: *@This(), name_text: runtime.Item, arguments: ?[]runtime.Item) void {
        self.thread.call(name_text, arguments);
    }

    pub inline fn result(self: *@This()) runtime.Item {
        return self.thread.register[0];
    }
};

pub const FeanConfig = struct {
    allocator: Allocator,
    silent: bool = false,

    file_debug: bool = false,
    file_lookup: ?*const fn (compiler.parser.Span) FileLookup = null,

    fn_lookup: ?*const fn (name: []const u8) ?*const fn (vm: *runtime.Thread, args: []runtime.Item, result: ?*runtime.Item) void,

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
