const Ref = @import("../mod.zig").Ref;

const Methods = @import("function.zig").Methods;
const Function = @import("function.zig").Function;
const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;

pub const Object = struct {
    methods: ?*Methods,
    body: *Ref,

    pub inline fn text(self: *@This()) *Text {
        return self.body.resolve(*Text);
    }

    pub inline fn dict(self: *@This()) *Dict {
        return self.body.resolve(*Dict);
    }

    pub inline fn function(self: *@This()) *Function {
        return self.body.resolve(*Function);
    }
};
