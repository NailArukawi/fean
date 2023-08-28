const std = @import("std");

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Linked = std.SinglyLinkedList;

const Object = @import("./value/object.zig").Object;

pub const BLOCK_DEFAULT_SIZE: usize = std.math.powi(usize, 2, 14) catch unreachable;

pub const LINE_SIZE: usize = std.math.powi(usize, 2, 6) catch unreachable;
pub const LINE_COUNT: usize = BLOCK_DEFAULT_SIZE / LINE_SIZE;

// Size ranges for different allocation logic.
pub const SizeRange = struct {
    start: usize,
    end: usize,

    pub inline fn fits(self: *const @This(), size: usize) bool {
        return (self.start <= size) and (size <= self.end);
    }
};
pub const SMALL = SizeRange{ .start = 0, .end = LINE_SIZE };
pub const MEDIUM = SizeRange{ .start = LINE_SIZE + 1, .end = BLOCK_DEFAULT_SIZE / 4 };
pub const LARGE = SizeRange{ .start = (BLOCK_DEFAULT_SIZE / 4) + 1, .end = std.math.maxInt(usize) };

pub const Ref = struct {
    // ptr adress value
    ptr: usize,

    pub inline fn create(allocator: Allocator, ptr: usize) !*@This() {
        var result = try allocator.create(@This());

        result.ptr = ptr;
        result.set_pinned(false);
        result.set_large(false);

        return result;
    }

    pub inline fn destroy(allocator: Allocator, self: @This()) void {
        allocator.free(self);
    }

    pub inline fn object(self: @This()) *Object {
        return self.resolve(*Object);
    }

    pub inline fn resolve_array(self: @This(), comptime T: type) [*]T {
        return @as([*]T, @ptrFromInt(self.ptr & 0xFFFFFFFFFFFFFFF8));
    }

    pub inline fn resolve(self: @This(), comptime T: type) T {
        const result = @as(T, @ptrFromInt(self.ptr & 0xFFFFFFFFFFFFFFF8));
        return result;
    }

    // Mark and sweep
    pub inline fn get_mark(self: *@This()) bool {
        return (@as([*]u8, @ptrCast(&self.ptr))[0] & 0b0000_0001) > 0;
    }

    pub inline fn set_mark(self: *@This(), val: bool) void {
        if (val) {
            @as([*]u8, @ptrCast(&self.ptr))[0] |= 0b0000_0001;
        } else {
            @as([*]u8, @ptrCast(&self.ptr))[0] &= 0b1111_1110;
        }
    }

    // Pinned in memory
    pub inline fn get_pinned(self: *@This()) bool {
        return (@as(*u8, @ptrCast(&self.ptr))[0] & 0b0000_0010) > 0;
    }

    pub inline fn set_pinned(self: *@This(), val: bool) void {
        if (val) {
            @as([*]u8, @ptrCast(&self.ptr))[0] |= 0b0000_0010;
        } else {
            @as([*]u8, @ptrCast(&self.ptr))[0] &= 0b1111_1101;
        }
    }

    // in the large section
    pub inline fn get_large(self: *@This()) bool {
        return (@as(*u8, @ptrCast(&self.ptr))[0] & 0b0000_0100) > 0;
    }

    pub inline fn set_large(self: *@This(), val: bool) void {
        if (val) {
            @as([*]u8, @ptrCast(&self.ptr))[0] |= 0b0000_0100;
        } else {
            @as([*]u8, @ptrCast(&self.ptr))[0] &= 0b1111_1011;
        }
    }
};

pub const Block = struct {
    ptr: [*]u8,
    size: usize,

    pub fn create(allocator: Allocator, size: usize) !@This() {
        // if its not a power of 2, we find a power of 2 that fits size.
        var actual_size = size;
        if (!std.math.isPowerOfTwo(actual_size))
            actual_size = closest_pow_2_greater(actual_size);

        return @This(){
            .ptr = (try allocator.alloc(u8, actual_size)).ptr,
            .size = size,
        };
    }

    pub fn destroy(self: @This(), allocator: Allocator) void {
        allocator.free(self.ptr);
        self.size = 0;
    }
};

const Hole = struct {
    start: usize,
    end: usize,
};

// todo use bits instead of bytes
pub const BlockMeta = struct {
    used: [LINE_COUNT]bool,

    pub fn default() @This() {
        return @This(){
            .used = [_]bool{false} ** LINE_COUNT,
        };
    }

    pub fn find_next_available_hole_byte(self: *@This(), starting_byte: usize) ?Hole {
        return self.find_next_available_hole(starting_byte / LINE_SIZE);
    }

    pub fn find_next_available_hole(self: *@This(), starting_line: usize) ?Hole {
        var start: ?usize = null;
        var end: usize = 0;

        var i: usize = starting_line;
        while (i < LINE_COUNT) : (i += 1) {
            if (!self.used[i]) {
                // found start of a hole
                if (start == null) {
                    start = i;
                    end = i + 1;
                } else {
                    end += 1;
                }
            } else i += 1;
        }

        if (start != null)
            return Hole{ .start = start.?, .end = end }; // we found a hole!

        //no hole found
        return null;
    }
};

pub const BumpBlock = struct {
    block: Block,
    meta: BlockMeta,
    limit: usize,
    cursor: usize,

    pub fn create(allocator: Allocator) !*@This() {
        var result = try allocator.create(@This());

        result.* = @This(){
            .meta = BlockMeta.default(),
            .block = try Block.create(allocator, BLOCK_DEFAULT_SIZE),
            .limit = BLOCK_DEFAULT_SIZE,
            .cursor = 0,
        };

        return result;
    }

    pub fn alloc(self: *@This(), context: *Heap, comptime T: type) !?*Ref {
        const ptr = self.alloc_size(context, @sizeOf(T));
        if (ptr == null)
            return null;

        return @as(*T, @ptrCast(ptr.?));
    }

    pub fn dealloc(self: *@This(), context: *Heap, ref: *Ref) void {
        context.free(self.block.ptr);

        const start = @intFromPtr(ref.ptr) - @intFromPtr(self.block);
        var block_index = start / LINE_SIZE;
        const block_end = block_index + ref.heap_size;

        // todo check that loop is correct
        while (block_index < block_end) {
            self.meta.used[block_index] = false;
            block_index += 1;
        }
    }

    // todo fix everything!
    pub fn alloc_size(self: *@This(), context: *Heap, size: usize) !?*Ref {
        const header = @sizeOf(usize) + @sizeOf(*Ref);
        const actual_size = size + header;
        const lines_spanned: usize = @as(u8, @intCast(try std.math.divCeil(usize, actual_size, LINE_SIZE)));
        const next_bump = self.cursor + (lines_spanned * LINE_SIZE);
        const holes = self.limit - self.cursor;

        if (next_bump > (holes * LINE_SIZE)) {
            const hole = self.meta.find_next_available_hole(0) orelse return null; // we out of holes to find_next_available_hole
            self.cursor = hole.start;
            self.limit = hole.end;
            const dif = self.limit - self.cursor;
            if (dif * LINE_SIZE < actual_size)
                return null;

            return self.alloc_size(context, actual_size);
        } else {
            var i: usize = 0;
            while (i < (lines_spanned)) {
                const ree = i + self.cursor / LINE_SIZE;
                self.meta.used[ree] = true;
                i += 1;
            }

            // todo do meta stuf in meta struct
            const offset = self.cursor;
            self.cursor = next_bump;
            const start = @intFromPtr(self.block.ptr);
            const result = try context.create_fean_ptr(start + offset + header);

            @as(*usize, @ptrFromInt(start + offset)).* = lines_spanned;
            @as(**Ref, @ptrFromInt(start + offset + @sizeOf(usize))).* = result;

            return result;
        }
    }
};

// TODO Mark-Sweep
// TODO Defragmentation
pub const Heap = struct {
    pointers: Linked(*Ref),
    underlying_allocator: Allocator,
    mark: bool,
    collecting: std.atomic.Atomic(bool),

    head: *BumpBlock,
    overflow: *BumpBlock,
    rest: List(*BumpBlock),

    large: Linked(*Ref),

    pub fn create(allocator: Allocator) !*@This() {
        var result = try allocator.create(@This());

        result.mark = false;
        result.collecting = std.atomic.Atomic(bool).init(false);

        result.head = try BumpBlock.create(allocator);
        result.overflow = try BumpBlock.create(allocator);
        result.rest = List(*BumpBlock).init(allocator);
        result.underlying_allocator = allocator;

        return result;
    }

    pub fn create_fean_ptr(self: *@This(), ptr: usize) !*Ref {
        var ref = try Ref.create(self.underlying_allocator, ptr);
        ref.set_mark(!self.mark);
        return ref;
    }

    pub fn alloc(self: *@This(), size: usize) !*Ref {
        if (LARGE.fits(size)) {
            return self.large_allocate(size);
        } else if (SMALL.fits(size)) {
            const head_space = try self.head.alloc_size(self, size);

            if (head_space == null) {
                // we can't even fit one line in head, It has to go
                try self.rest.append(self.head);
                self.head = try BumpBlock.create(self.underlying_allocator);
                return (try self.head.alloc_size(self, size)).?;
            }

            return head_space.?;
        } else {
            // we have a medium object.
            const head_space = try self.head.alloc_size(self, size);

            if (head_space == null) {
                return self.overflow_allocate(size);
            }

            return head_space.?;
        }
    }

    fn overflow_allocate(self: *@This(), size: usize) !*Ref {
        std.debug.assert(size <= BLOCK_DEFAULT_SIZE);

        const space = try self.overflow.alloc_size(self, size);

        if (space == null) {
            // our overflow is not fit to be an overflow.
            // lets make a new one
            try self.rest.append(self.overflow);
            self.overflow = try BumpBlock.create(self.underlying_allocator);
            const result = try self.overflow.alloc_size(self, size);
            return result.?;
        }

        return space.?;
    }

    fn large_allocate(self: *@This(), size: usize) !*Ref {
        const ptr = (try self.underlying_allocator.alloc(u8, size)).ptr;
        const result = try self.create_fean_ptr(@intFromPtr(ptr));

        var node = try self.underlying_allocator.create(Linked(*Ref).Node);
        node.data = result;
        self.large.prepend(node);

        return result;
    }

    pub fn gc(self: *@This()) !void {
        std.debug.assert(!self.collecting.load(.Acquire)); // can only collect once at a time
        self.collecting.store(true, .Release);
        var thread = try std.Thread.spawn(.{}, @This().__gc, .{self});
        _ = thread;
    }

    fn __gc(self: *@This()) void {
        for (self.rest.items) |bb| {
            var bump: *BumpBlock = bb;
            var i: usize = 0;
            const start = @intFromPtr(bb.block.ptr);
            while (i < LINE_COUNT) {
                if (bump.meta.used[i]) {
                    const offset = i * LINE_SIZE;
                    const size = @as(*usize, @ptrFromInt(start + offset)).*;
                    var ref: *Ref = @as(**Ref, @ptrFromInt(start + offset + @sizeOf(usize))).*;

                    const mark = ref.get_mark();
                    if (mark != self.mark) {
                        for (i..(i + size)) |to_free| {
                            bump.meta.used[to_free] = false;
                        }

                        self.underlying_allocator.destroy(ref);
                    }

                    i += size;
                } else {
                    while (i < LINE_COUNT and !bump.meta.used[i]) i += 1;
                }
            }
        }
        self.mark = !self.mark;
        self.collecting.store(false, .Release);
    }

    pub fn debug(self: *@This()) void {
        while (self.collecting.load(.Acquire)) continue;
        std.debug.print("[HEAP]\n", .{});

        var head_bytes: usize = 0;
        for (self.head.meta.used) |u| {
            if (u) {
                head_bytes += LINE_SIZE;
            }
        }

        var overflow_bytes: usize = 0;
        for (self.overflow.meta.used) |u| {
            if (u) {
                overflow_bytes += LINE_SIZE;
            }
        }

        var rest_bytes: usize = 0;
        for (self.rest.items) |block| {
            for (block.meta.used) |u| {
                if (u) {
                    rest_bytes += LINE_SIZE;
                }
            }
        }

        var large_count: usize = 0;
        var large_bytes: usize = 0;

        std.debug.print("head_bytes: {} B\n", .{head_bytes});
        std.debug.print("overflow_bytes: {} B\n", .{overflow_bytes});

        std.debug.print("rest: {} blocks\n", .{self.rest.items.len});
        std.debug.print("      ↪ {} B\n", .{rest_bytes});

        std.debug.print("large: {} Blocks\n", .{large_count});
        std.debug.print("       ↪ {} B\n", .{large_bytes});
    }
};

inline fn closest_pow_2_greater(n: usize) usize {
    var i: usize = 2;
    while (i < n) {
        i *= 2;
    }
    return i;
}
