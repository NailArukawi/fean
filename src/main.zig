const std = @import("std");
const fean = @import("mod.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var config = try allocator.create(fean.FeanConfig);
    config.* = fean.FeanConfig.default(allocator);

    config.fn_lookup = lookup_fn;

    var file = try std.fs.cwd().openFile("scratch.fe", std.fs.File.OpenFlags{});
    defer file.close();

    const buffer_size = 1024;
    const file_buffer = try file.readToEndAlloc(allocator, buffer_size);
    defer allocator.free(file_buffer);

    var vm = try fean.Fean.create(file_buffer, config);

    std.debug.print("We got: {}\n", .{vm.thread.stack_view[0].i64});
}

const fean_fn = *const fn (args: []fean.vm.Item, result: ?*fean.vm.Item) void;

fn lookup_fn(name: []const u8) ?fean_fn {
    if (std.mem.eql(u8, "zamn", name)) {
        return &zamn;
    } else if (std.mem.eql(u8, "time", name)) {
        return &time;
    }

    unreachable;
}

fn zamn(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = args;
    const z: i64 = 42069;
    result.?.* = fean.vm.Item.from(i64, z);
}

var time_stamp: ?i128 = null;

fn time(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = result;
    _ = args;
    if (time_stamp == null) {
        time_stamp = std.time.nanoTimestamp();
    } else {
        const delta = std.time.nanoTimestamp() - time_stamp.?;
        std.debug.print("Time elapsed: {} ns\n", .{delta});
        time_stamp = null;
    }
}
