const std = @import("std");

const fean = @import("mod.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var config = try allocator.create(fean.FeanConfig);
    config.* = fean.FeanConfig.default(allocator);
    var vm = try fean.Fean.create("a : i64 = 2;b : i64 = 6;c : i64 = 9;return a + b + c;", config);

    std.debug.print("We got: {}\n", .{vm.thread.stack.inner[0].i64});
}
