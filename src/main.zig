const std = @import("std");
const fean = @import("fean/fean.zig");
const Arguments = fean.runtime.ExternFunctionArguments;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var config = try allocator.create(fean.FeanConfig);
    config.* = fean.FeanConfig.default(allocator);

    config.fn_lookup = lookup_fn;

    var file = try std.fs.cwd().openFile("fib.fe", std.fs.File.OpenFlags{});
    defer file.close();

    const vm = try fean.Fean.create(file.reader(), config);
    _ = vm;
}

const fean_fn = fean.runtime.ExternFunctionBody;

fn lookup_fn(name: []const u8) ?fean_fn {
    if (std.mem.eql(u8, "zamn", name)) {
        return &zamn;
    } else if (std.mem.eql(u8, "heap_debug", name)) {
        return &heap_debug;
    } else if (std.mem.eql(u8, "heap_collect", name)) {
        return &heap_recycle;
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

fn heap_debug(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = result;
    _ = args;
    vm.heap.debug();
}

fn heap_recycle(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = result;
    _ = args;
    vm.gc() catch unreachable;
}

fn zamn(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = vm;
    _ = args;
    const z: i64 = 420691337;
    result.?.* = fean.runtime.Item.from(i64, z);
}

fn print_i64(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = vm;
    _ = result;
    std.debug.print("[Fean]: {}\n", .{args[0].i64});
}

fn print_f64(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = vm;
    _ = result;
    std.debug.print("[Fean]: {d:.3}\n", .{args[0].f64});
}

var time_stamp: ?i128 = null;

fn time(vm: *fean.runtime.Thread, args: Arguments, result: ?*fean.runtime.Item) callconv(.C) void {
    _ = vm;
    _ = args;
    if (time_stamp == null) {
        time_stamp = std.time.nanoTimestamp();
    } else {
        const delta = std.time.nanoTimestamp() - time_stamp.?;
        result.?.* = fean.runtime.Item.from(i64, @as(i64, @intCast(delta)));
        time_stamp = null;
    }
}
