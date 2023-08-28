const std = @import("std");

const Item = @import("mod.zig").Item;
const Dict = @import("mod.zig").Dict;
const DictEntry = @import("mod.zig").DictEntry;
const Text = @import("mod.zig").Text;
const Heap = @import("mod.zig").Heap;
const Ref = @import("mod.zig").Ref;

const GLOBALS_INITAL_SIZE: usize = 32;

const GlobalEntry = struct {
    item: Item,
    object: bool,
};

pub const Global = struct {
    items: *Ref,

    pub fn create(heap: *Heap, size: usize) !@This() {
        return @This(){
            .items = try Dict.create(heap, size, Text.hash_as_item, Text.eq_as_item),
        };
    }

    pub fn default(heap: *Heap) !@This() {
        const result = try @This().create(heap, GLOBALS_INITAL_SIZE);
        return result;
    }

    pub inline fn get(self: *@This(), name_text: Item) ?Item {
        //std.debug.print("Global.get(\"{s}\")\n", .{name_text.text().as_slice()});
        var dict = self.items.object().dict();
        const result = dict.get(name_text) orelse return null;
        return @as(*GlobalEntry, @ptrCast(@alignCast(result.any))).item;
    }

    pub inline fn set(self: *@This(), name_text: Item, value: Item) !bool {
        var dict = self.items.object().dict();
        var entry = try dict.heap.underlying_allocator.create(GlobalEntry);
        entry.item = value;
        entry.object = false;
        const item_entry = Item{ .any = @as(*anyopaque, @ptrCast(@alignCast(entry))) };
        return dict.set(name_text, item_entry);
    }

    pub inline fn set_obj(self: *@This(), name_text: Item, value: Item) !bool {
        var dict = self.items.object().dict();
        var entry = try dict.heap.underlying_allocator.create(GlobalEntry);
        entry.item = value;
        entry.object = true;
        const item_entry = Item{ .any = @as(*anyopaque, @ptrCast(@alignCast(entry))) };
        return dict.set(name_text, item_entry);
    }

    pub inline fn contains(self: *@This(), name_text: Item) bool {
        var dict = self.items.object().dict();
        return dict.contains(name_text);
    }

    pub fn mark(self: *@This()) void {
        const dict = self.items.object().dict();
        var entries: []DictEntry = dict.entries()[0..dict.capacity];
        for (entries) |e| {
            if (e.key != null and !e.tomb) {
                var item = @as(*GlobalEntry, @ptrCast(@alignCast(e.value.any)));
                if (item.object) item.item.object.set_mark(dict.heap.mark);
            }
        }
    }
};
