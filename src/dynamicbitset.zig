const std = @import("std");
const Allocator = std.mem.Allocator;

pub const DynamicBitSet = struct {
    chunks: [*]u8,
    bit_len: usize,
    allocator: Allocator,

    pub fn create(allocator: Allocator, size: usize) !@This() {
        var chunk_count: usize = 0;
        while (size > (chunk_count * 8)) {
            chunk_count += 1;
        }

        return @This(){
            .chunks = (try allocator.alloc(u8, chunk_count)).ptr,
            .bit_len = size,
            .allocator = allocator,
        };
    }

    pub fn destory(self: *@This()) void {
        self.allocator.free(self.chunks);
        self.bit_len = 0;
    }

    pub fn get(self: *@This(), index: usize) bool {
        std.debug.assert(index < self.bit_len);

        const chunk_index = index / 8;
        const chunk_offset = index % 8;
        const chunk = self.chunks[chunk_index];

        return (chunk & (1 << chunk_offset)) != 0;
    }

    pub fn set(self: *@This(), index: usize, value: bool) void {
        std.debug.assert(index < self.bit_len);

        const chunk_index = index / 8;
        const chunk_offset = index % 8;

        if (value) {
            self.chunks[chunk_index] |= (1 << chunk_offset);
        } else {
            self.chunks[chunk_index] &= ~(1 << chunk_offset);
        }
    }

    pub fn grow(self: *@This(), new_bit_len: usize) !void {
        std.debug.assert(new_bit_len > self.bit_len);

        var new_chunk_count: usize = 0;
        while (new_bit_len > (new_chunk_count * 8)) {
            new_chunk_count += 1;
        }

        const new_chunks = try self.allocator.alloc(u8, new_chunk_count);
        std.mem.zero(new_chunks, new_chunk_count);

        // Copy the old chunks to the new buffer
        const old_chunk_count = self.bit_len / 8;
        std.mem.copy(
            u8,
            new_chunks,
            self.chunks[0..old_chunk_count],
        );

        self.allocator.free(self.chunks);
        self.chunks = new_chunks.ptr;
        self.bit_len = new_bit_len;
    }
};
