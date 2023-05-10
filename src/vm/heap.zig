const std = @import("std");

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Linked = std.SinglyLinkedList;

const Object = @import("./value/object.zig").Object;

pub const BLOCK_DEFAULT_SIZE: usize = std.math.powi(usize, 2, 16) catch unreachable;

pub const LINE_SIZE: usize = std.math.powi(usize, 2, 7) catch unreachable;
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
pub const LARGE = SizeRange{ .start = (BLOCK_DEFAULT_SIZE / 4) + 1, .end = ((std.math.powi(usize, 2, 63) catch unreachable) - 1) * 2 };

pub const Ref = struct {
    // ptr adress value
    ptr: usize,

    // How many heap lines the object spans
    heap_size: u16,

    // flags,
    // [0,      1,      2,     3,               4, 5, 6, 7]
    // [Marked, Pinned, Large, Destruct_Method, 0, 0, 0, 0]
    flag: u8,

    pub fn create(allocator: Allocator, ptr: usize) !*@This() {
        var result = try allocator.create(@This());

        result.ptr = ptr;
        result.flag = 0;
        result.heap_size = 0;

        return result;
    }

    pub fn destroy(allocator: Allocator, self: *@This()) void {
        allocator.free(self);
    }

    pub inline fn object(self: *@This()) *Object {
        return self.resolve(*Object);
    }

    pub inline fn resolve_array(self: *@This(), comptime T: type) [*]T {
        return @intToPtr([*]T, self.ptr);
    }

    pub inline fn resolve(self: *@This(), comptime T: type) T {
        const result = @intToPtr(T, self.ptr);
        return result;
    }

    // Mark and sweep
    pub inline fn get_mark(self: @This()) bool {
        return (self.bits & 0b0000_0001) > 0;
    }

    pub inline fn set_mark(self: *@This(), val: bool) void {
        if (val) {
            self.flag |= 0b0000_0001;
        } else {
            self.flag &= 0b1111_1110;
        }
    }

    // Pinned in memory
    pub inline fn get_pin(self: @This()) bool {
        return (self.flag & 0b0000_0010) > 0;
    }

    pub inline fn set_pin(self: *@This(), val: bool) void {
        if (val) {
            self.flag |= 0b0000_0010;
        } else {
            self.flag &= 0b1111_1101;
        }
    }

    // in the large section
    pub inline fn get_large(self: @This()) bool {
        return (self.flag & 0b0000_0100) > 0;
    }

    pub inline fn set_large(self: *@This(), val: bool) void {
        if (val) {
            self.flag |= 0b0000_0100;
        } else {
            self.flag &= 0b1111_1011;
        }
    }

    // Destruct method
    pub inline fn get_method_destruct(self: @This()) bool {
        return (self.flag & 0b0000_1000) > 0;
    }

    pub inline fn set_method_destruct(self: *@This(), val: bool) void {
        if (val) {
            self.flag |= 0b0000_1000;
        } else {
            self.flag &= 0b1111_0111;
        }
    }
};

pub const Block = struct {
    ptr: [*]u8,
    size: usize,

    pub fn create(allocator: Allocator, size: usize) !@This() {
        // if its not a power of 2, we find a power of 2 that fits size.
        var actual_size = size;
        if (!std.math.isPowerOfTwo(actual_size)) {
            actual_size = closest_pow_2_greater(actual_size);
        }

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
    allocations: Linked(*Ref),

    pub fn default() @This() {
        return @This(){
            .used = [_]bool{false} ** LINE_COUNT,
            .allocations = Linked(*Ref){
                .first = null,
            },
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
            } else if (start != null) {
                // we found a hole!
                return Hole{ .start = start.?, .end = end };
            } else if (self.used[i]) {
                i += 1;
            }
        }

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
        if (ptr == null) {
            return null;
        }
        return @ptrCast(*T, ptr.?);
    }

    pub fn dealloc(self: *@This(), context: *Heap, ref: *Ref) void {
        context.free(self.block.ptr);

        const start = @ptrToInt(ref.ptr) - @ptrToInt(self.block);
        var block_index = start / LINE_SIZE;
        const block_end = block_index + ref.heap_size;

        // todo check that loop is correct
        while (block_index < block_end) {
            self.meta.used[block_index] = false;
            block_index += 1;
        }
    }

    pub fn alloc_size(self: *@This(), context: *Heap, size: usize) !?*Ref {
        const lines_spanned = @intCast(u8, try std.math.divCeil(usize, size, LINE_COUNT));
        const next_bump = self.cursor + (lines_spanned * LINE_SIZE);
        const holes = try std.math.divCeil(usize, self.limit - self.cursor, LINE_COUNT);

        if (next_bump > (holes * LINE_SIZE)) {
            const hole = self.meta.find_next_available_hole(0);
            if (hole == null) {
                // we out of holes to find_next_available_hole
                return null;
            } else {
                self.cursor = hole.?.start;
                self.limit = hole.?.end;
                const dif = self.limit - self.cursor;
                if (dif * LINE_SIZE < size) {
                    return null;
                } else {
                    return self.alloc_size(context, size);
                }
            }
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
            const start = @ptrToInt(self.block.ptr);
            const result = try context.create_fean_ptr(start + offset);
            result.heap_size = lines_spanned;

            var node = try context.underlying_allocator.create(Linked(*Ref).Node);
            node.data = result;

            self.meta.allocations.prepend(node);

            return result;
        }
    }
};

pub const ObjectAllocation = struct {
    obj: *Ref,
    item: *Ref,
};

// TODO Mark-Sweep
// TODO Defragmentation
pub const Heap = struct {
    pointers: Linked(*Ref),
    underlying_allocator: Allocator,
    mark: bool,

    head: *BumpBlock,
    overflow: *BumpBlock,
    rest: List(*BumpBlock),

    large: Linked(*Ref),

    pub fn create(allocator: Allocator) !*@This() {
        var result = try allocator.create(@This());

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

    pub fn alloc_object(self: *@This(), size: usize) !ObjectAllocation {
        const item = try self.alloc(size);
        var obj = try self.create_object(item);

        return ObjectAllocation{
            .obj = obj,
            .item = item,
        };
    }

    fn create_object(self: *@This(), inner: *Ref) !*Ref {
        var obj = try self.alloc(@sizeOf(Object));
        var inner_obj = obj.resolve(*Object);
        inner_obj.methods = null;
        inner_obj.body = inner;
        return obj;
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
        const result = try self.create_fean_ptr(@ptrToInt(ptr));

        var node = try self.underlying_allocator.create(Linked(*Ref).Node);
        node.data = result;
        self.large.prepend(node);

        return result;
    }

    pub fn debug(self: *@This()) void {
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
