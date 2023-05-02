const std = @import("std");
const Allocator = std.mem.Allocator;

const Item = @import("mod.zig").Item;
const Dict = @import("mod.zig").Dict;
const Text = @import("mod.zig").Text;
const Heap = @import("mod.zig").Heap;
const Ref = @import("mod.zig").Ref;

const GLOBALS_INITAL_SIZE: usize = 32;

pub const Global = struct {
    items: *Ref,

    pub fn create(heap: *Heap, size: usize) !@This() {
        return @This(){
            .items = try Dict.create(heap, size, Text.hash_as_item, Text.eq_as_item),
        };
    }

    pub fn default(heap: *Heap) !@This() {
        const result = try @This().create(heap, GLOBALS_INITAL_SIZE);
        std.debug.print("items: {}\n", .{result.items});
        std.debug.print("itemsobj: {}\n", .{result.items.object()});
        return result;
    }

    pub inline fn get(self: *@This(), name_text: Item) ?Item {
        std.debug.print("ree:{}\n", .{self.items.object()});
        var dict = self.items.object().dict();
        return dict.get(name_text);
    }

    pub inline fn set(self: *@This(), name_text: Item, value: Item) !bool {
        var dict = self.items.object().dict();
        const t = name_text.text();
        std.debug.print("set: {s}, as: {} \n", .{ t.bytes()[0..1], value });
        return dict.set(name_text, value);
    }

    pub inline fn contains(self: *@This(), name_text: Item) bool {
        var dict = self.items.object().dict();
        return dict.contains(name_text);
    }
};
