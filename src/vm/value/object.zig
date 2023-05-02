const Ref = @import("../mod.zig").Ref;

const Methods = @import("function.zig").Methods;
const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;

pub const Object = struct {
    methods: ?*Methods,
    body: *Ref,

    pub inline fn dict(self: *@This()) *Dict {
        return self.body.resolve(*Dict);
    }

    pub inline fn text(self: *@This()) *Text {
        return self.body.resolve(*Text);
    }
};
