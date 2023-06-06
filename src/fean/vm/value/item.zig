const Dict = @import("dict.zig").Dict;
const Text = @import("text.zig").Text;
const Function = @import("function.zig").Function;
const Object = @import("object.zig").Object;
const Kind = @import("../../compiler/mod.zig").Kind;
const Ref = @import("../mod.zig").Ref;

pub const Item = extern union {
    u64: u64,
    u32: u32,
    u16: u16,
    u8: u8,
    i64: i64,
    i32: i32,
    i16: i16,
    i8: i8,
    f64: f64,
    f32: f32,
    bool: bool,
    // todo make nil a null type
    nil: u8,
    object: *Ref,
    kind: Kind,

    // raw data,
    any: ?*anyopaque,

    pub inline fn resolve_object(self: @This()) *Object {
        return self.object.resolve(*Object);
    }

    pub inline fn text(self: @This()) *Text {
        return self.resolve_object().text();
    }

    pub inline fn dict(self: @This()) *Dict {
        return self.resolve_object().dict();
    }

    pub inline fn function(self: @This()) *Function {
        return self.resolve_object().function();
    }

    pub inline fn from(comptime T: type, V: anytype) @This() {
        if (T == u64) {
            return @This(){ .u64 = V };
        }
        if (T == u32) {
            return @This(){ .u32 = V };
        }
        if (T == u16) {
            return @This(){ .u16 = V };
        }
        if (T == u8) {
            return @This(){ .u8 = V };
        }
        if (T == i64) {
            return @This(){ .i64 = V };
        }
        if (T == i32) {
            return @This(){ .i32 = V };
        }
        if (T == i16) {
            return @This(){ .i16 = V };
        }
        if (T == i8) {
            return @This(){ .i8 = V };
        } else if (T == f64) {
            return @This(){ .f64 = V };
        } else if (T == f32) {
            return @This(){ .f32 = V };
        } else if (T == bool) {
            return @This(){ .bool = V };
        }
    }

    pub inline fn default() @This() {
        return Item{
            .nil = 0,
        };
    }
};
