const std = @import("std");
const fean = @import("mod.zig");

const Point = struct { x: i64, y: i64, z: i64 };

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
    const g = vm.result().resolve_object().body.resolve(*Point);
    std.debug.print("x: {}\n", .{g.x});
    std.debug.print("y: {}\n", .{g.y});
    std.debug.print("z: {}\n", .{g.z});
    vm.thread.heap.debug();
}

const fean_fn = *const fn (args: []fean.vm.Item, result: ?*fean.vm.Item) void;

fn lookup_fn(name: []const u8) ?fean_fn {
    if (std.mem.eql(u8, "zamn", name)) {
        return &zamn;
    } else if (std.mem.eql(u8, "time", name)) {
        return &time;
    } else if (std.mem.eql(u8, "print_i64", name)) {
        return &print_i64;
    } else if (std.mem.eql(u8, "print_f64", name)) {
        return &print_f64;
    }

    std.debug.print("tried to find fn: {s}\n", .{name});
    unreachable;
}

fn zamn(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = args;
    const z: i64 = 420691337;
    result.?.* = fean.vm.Item.from(i64, z);
}

fn print_i64(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = result;
    std.debug.print("[Fean]: {}\n", .{args[0].i64});
}

fn print_f64(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = result;
    std.debug.print("[Fean]: {d:.3}\n", .{args[0].f64});
}

var time_stamp: ?i128 = null;

fn time(args: []fean.vm.Item, result: ?*fean.vm.Item) void {
    _ = args;
    if (time_stamp == null) {
        time_stamp = std.time.nanoTimestamp();
    } else {
        const delta = std.time.nanoTimestamp() - time_stamp.?;
        result.?.* = fean.vm.Item.from(i64, @intCast(i64, delta));
        time_stamp = null;
    }
}
