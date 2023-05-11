const std = @import("std");
const Allocator = std.mem.Allocator;

pub const DEFAULT_STACK_SIZE: usize = 256;

pub fn Stack(comptime T: type) type {
    return struct {
        inner: [*]T,
        used: usize,
        capacity: usize,

        allocator: Allocator,

        pub fn create(allocator: Allocator, size: usize) !@This() {
            const memory = try allocator.alloc(T, size);
            return @This(){
                .allocator = allocator,
                .inner = memory.ptr,
                .used = 0,
                .capacity = size,
            };
        }

        pub fn default(allocator: Allocator) !@This() {
            return @This().create(allocator, DEFAULT_STACK_SIZE);
        }

        pub fn destroy(self: *@This()) void {
            self.allocator.free(self.as_slice());
        }

        pub fn from_slice(allocator: Allocator, items: []T) !@This() {
            const stack = try @This().create(allocator, items.len);
            for (items) |item| {
                stack.push(item);
            }
            return stack;
        }

        pub fn shrink_to_fit(self: *@This()) !void {
            if (self.used == self.capacity) {
                return;
            }
            self.inner = (try self.allocator.realloc(self.as_slice(), self.used)).ptr;
            self.capacity = self.used;
        }

        pub fn as_slice(self: *@This()) []T {
            return self.inner[0..self.used];
        }

        pub inline fn get(self: *const @This(), pos: usize) T {
            return self.inner[pos];
        }

        pub inline fn top(self: *const @This()) T {
            return self.inner[self.used - 1];
        }

        pub fn pop(self: *@This()) T {
            self.used -= 1;
            const result = self.inner[self.used];
            return result;
        }

        pub inline fn push(self: *@This(), item: T) !void {
            const pos = self.used;
            self.used += 1;
            try self.ensure_capacity(self.used);
            self.inner[pos] = item;
        }

        pub inline fn set(self: *@This(), pos: usize, item: T) void {
            self.inner[pos] = item;
        }

        pub inline fn hold(self: *@This(), size: usize) !void {
            const new_used: usize = self.used + size;
            try self.ensure_capacity(new_used);
            self.used = new_used;
        }

        pub inline fn release(self: *@This(), size: usize) void {
            self.used -= size;
        }

        pub inline fn ensure_capacity(self: *@This(), size: usize) !void {
            if (size > self.capacity) {
                const new_capacity = self.capacity * 2;
                self.inner = (try self.allocator.realloc(self.inner[0..self.capacity], new_capacity)).ptr;
                self.capacity = new_capacity;
            }
        }

        pub inline fn is_empty(self: *const @This()) bool {
            return self.used == 0;
        }

        pub inline fn is_full(self: *const @This()) bool {
            return self.used == self.capacity;
        }

        pub inline fn count(self: *const @This()) usize {
            return self.used;
        }
    };
}
