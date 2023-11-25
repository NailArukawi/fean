const Ref = @import("../mod.zig").Ref;

const Methods = @import("function.zig").Methods;
const Method = @import("function.zig").Method;
const Function = @import("function.zig").Function;
const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;

pub const Object = struct {
    methods: ?*Methods,
    // body

    pub inline fn body(self: *@This(), comptime T: type) *T {
        const body_int = @intFromPtr(self) + @sizeOf(?*Methods);
        return @as(*T, @ptrFromInt(body_int));
    }

    pub inline fn body_array(self: *@This(), comptime T: type) [*]T {
        const body_int = @intFromPtr(self) + @sizeOf(?*Methods);
        return @as([*]T, @ptrFromInt(body_int));
    }

    pub inline fn text(self: *@This()) *Text {
        return self.body(Text);
    }

    pub inline fn dict(self: *@This()) *Dict {
        return self.body(Dict);
    }

    pub inline fn function(self: *@This()) *Function {
        return self.body(Function);
    }
};
