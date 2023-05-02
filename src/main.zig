const std = @import("std");
const fean = @import("mod.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var config = try allocator.create(fean.FeanConfig);
    config.* = fean.FeanConfig.default(allocator);

    var file = try std.fs.cwd().openFile("scratch.fe", std.fs.File.OpenFlags{});
    defer file.close();

    const buffer_size = 1024;
    const file_buffer = try file.readToEndAlloc(allocator, buffer_size);
    defer allocator.free(file_buffer);

    var vm = try fean.Fean.create(file_buffer, config);

    std.debug.print("We got: {}\n", .{vm.thread.stack.inner[0].i64});
}
