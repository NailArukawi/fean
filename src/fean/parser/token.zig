const std = @import("std");
const Span = @import("mod.zig").Span;
const List = std.ArrayList;

pub const Token = struct {
    span: Span,
    data: TokenData,

    pub inline fn new_integer(val: i64, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .integer = val,
            },
        };
    }

    pub inline fn new_decimal(val: f64, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .decimal = val,
            },
        };
    }

    pub inline fn new_text(val: []const u8, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .text = val,
            },
        };
    }

    pub inline fn new_identifier(val: []const u8, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .identifier = val,
            },
        };
    }

    pub inline fn new_symbol(val: Symbol, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .symbol = val,
            },
        };
    }

    pub inline fn new_keyword(val: Keyword, span: Span) @This() {
        return @This(){
            .span = span,
            .data = TokenData{
                .keyword = val,
            },
        };
    }
};

pub const TokenData = union(enum) {
    integer: i64,
    decimal: f64,
    text: []const u8,
    identifier: []const u8,
    symbol: Symbol,
    keyword: Keyword,
};

pub const Symbol = enum {
    no_symbol,
    EOS,

    right_arrow,

    paren_left,
    paren_right,
    brace_left,
    brace_right,
    curley_left,
    curley_right,
    comma,
    dot,
    semi_colon,
    colon,

    plus,
    plus_equal,
    minus,
    minus_equal,
    slash,
    slash_equal,
    star,
    star_equal,

    minus_minus,
    plus_plus,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    variable_asignment,
    constant_asignment,
};

pub const Keyword = enum {
    no_keyword,
    And,
    Or,
    Struct,
    Self,
    self,
    Trait,
    Impl,
    If,
    Else,
    For,
    Pub,
    Return,
    True,
    False,
    While,
    Fn,
    Extern,
};

pub const TokenKind = enum {
    integer,
    decimal,
    text,
    identifier,
    symbol,
    keyword,

    // symbols
    no_symbol,
    EOS,

    right_arrow,

    paren_left,
    paren_right,
    brace_left,
    brace_right,
    curley_left,
    curley_right,
    comma,
    dot,
    semi_colon,
    colon,

    plus,
    plus_equal,
    minus,
    minus_equal,
    slash,
    slash_equal,
    star,
    star_equal,

    minus_minus,
    plus_plus,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    variable_asignment,
    constant_asignment,

    // keywords
    no_keyword,
    And,
    Or,
    Struct,
    Self,
    self,
    Trait,
    Impl,
    If,
    Else,
    For,
    Pub,
    Return,
    True,
    False,
    While,
    Fn,
    Extern,

    pub fn translate_from(other: TokenData) @This() {
        switch (other) {
            .integer => return @This().integer,
            .decimal => return @This().decimal,
            .text => return @This().text,
            .identifier => return @This().identifier,
            .symbol => |s| {
                switch (s) {
                    .no_symbol => return @This().no_symbol,
                    .EOS => return @This().EOS,
                    .right_arrow => return @This().right_arrow,
                    .paren_left => return @This().paren_left,
                    .paren_right => return @This().paren_right,
                    .brace_left => return @This().brace_left,
                    .brace_right => return @This().brace_right,
                    .curley_left => return @This().curley_left,
                    .curley_right => return @This().curley_right,
                    .comma => return @This().comma,
                    .dot => return @This().dot,
                    .semi_colon => return @This().semi_colon,
                    .colon => return @This().colon,
                    .plus => return @This().plus,
                    .plus_equal => return @This().plus_equal,
                    .minus => return @This().minus,
                    .minus_equal => return @This().minus_equal,
                    .slash => return @This().slash,
                    .slash_equal => return @This().slash_equal,
                    .star => return @This().star,
                    .star_equal => return @This().star_equal,
                    .minus_minus => return @This().minus_minus,
                    .plus_plus => return @This().plus_plus,
                    .bang => return @This().bang,
                    .bang_equal => return @This().bang_equal,
                    .equal => return @This().equal,
                    .equal_equal => return @This().equal_equal,
                    .greater => return @This().greater,
                    .greater_equal => return @This().greater_equal,
                    .less => return @This().less,
                    .less_equal => return @This().less_equal,
                    .variable_asignment => return @This().variable_asignment,
                    .constant_asignment => return @This().constant_asignment,
                }
            },
            .keyword => |k| {
                switch (k) {
                    .no_keyword => return @This().no_keyword,
                    .And => return @This().And,
                    .Or => return @This().Or,
                    .Struct => return @This().Struct,
                    .Self => return @This().Self,
                    .self => return @This().self,
                    .Trait => return @This().Trait,
                    .Impl => return @This().Impl,
                    .If => return @This().If,
                    .Else => return @This().Else,
                    .For => return @This().For,
                    .Pub => return @This().Pub,
                    .Return => return @This().Return,
                    .True => return @This().True,
                    .False => return @This().False,
                    .While => return @This().While,
                    .Fn => return @This().Fn,
                    .Extern => return @This().Extern,
                }
            },
        }
    }

    pub fn translate_to(self: @This()) TokenData {
        switch (self) {
            .integer => return TokenData.new_integer(0, Span.default()),
            .decimal => return TokenData.new_decimal(0.0, Span.default()),
            .text => return TokenData.new_text("", Span.default()),
            .identifier => return TokenData.new_identifier("", Span.default()),
            .symbol => return TokenData.new_symbol(.no_symbol, Span.default()),
            .keyword => return TokenData.new_keyword(.no_keyword, Span.default()),

            // symbols
            .no_symbol => return TokenData.new_symbol(.no_symbol, Span.default()),
            .EOS => return TokenData.new_symbol(.EOS, Span.default()),
            .right_arrow => return TokenData.new_symbol(.right_arrow, Span.default()),
            .paren_left => return TokenData.new_symbol(.paren_left, Span.default()),
            .paren_right => return TokenData.new_symbol(.paren_right, Span.default()),
            .brace_left => return TokenData.new_symbol(.brace_left, Span.default()),
            .brace_right => return TokenData.new_symbol(.brace_right, Span.default()),
            .curley_left => return TokenData.new_symbol(.curley_left, Span.default()),
            .curley_right => return TokenData.new_symbol(.curley_right, Span.default()),
            .comma => return TokenData.new_symbol(.comma, Span.default()),
            .dot => return TokenData.new_symbol(.dot, Span.default()),
            .semi_colon => return TokenData.new_symbol(.semi_colon, Span.default()),
            .colon => return TokenData.new_symbol(.colon, Span.default()),
            .plus => return TokenData.new_symbol(.plus, Span.default()),
            .plus_equal => return TokenData.new_symbol(.plus_equal, Span.default()),
            .minus => return TokenData.new_symbol(.minus, Span.default()),
            .minus_equal => return TokenData.new_symbol(.minus_equal, Span.default()),
            .slash => return TokenData.new_symbol(.slash, Span.default()),
            .slash_equal => return TokenData.new_symbol(.slash_equal, Span.default()),
            .star => return TokenData.new_symbol(.star, Span.default()),
            .star_equal => return TokenData.new_symbol(.star_equal, Span.default()),
            .minus_minus => return TokenData.new_symbol(.minus_minus, Span.default()),
            .plus_plus => return TokenData.new_symbol(.plus_plus, Span.default()),
            .bang => return TokenData.new_symbol(.bang, Span.default()),
            .bang_equal => return TokenData.new_symbol(.bang_equal, Span.default()),
            .equal => return TokenData.new_symbol(.equal, Span.default()),
            .equal_equal => return TokenData.new_symbol(.equal_equal, Span.default()),
            .greater => return TokenData.new_symbol(.greater, Span.default()),
            .greater_equal => return TokenData.new_symbol(.greater_equal, Span.default()),
            .less => return TokenData.new_symbol(.less, Span.default()),
            .less_equal => return TokenData.new_symbol(.less_equal, Span.default()),
            .variable_asignment => return TokenData.new_symbol(.variable_asignment, Span.default()),
            .constant_asignment => return TokenData.new_symbol(.constant_asignment, Span.default()),

            // keywords
            .And => return TokenData.new_keyword(.And, Span.default()),
            .Or => return TokenData.new_keyword(.Or, Span.default()),
            .Struct => return TokenData.new_keyword(.Struct, Span.default()),
            .Self => return TokenData.new_keyword(.Self, Span.default()),
            .self => return TokenData.new_keyword(.self, Span.default()),
            .Trait => return TokenData.new_keyword(.Trait, Span.default()),
            .Impl => return TokenData.new_keyword(.Impl, Span.default()),
            .If => return TokenData.new_keyword(.If, Span.default()),
            .Else => return TokenData.new_keyword(.Else, Span.default()),
            .For => return TokenData.new_keyword(.For, Span.default()),
            .Pub => return TokenData.new_keyword(.Pub, Span.default()),
            .Return => return TokenData.new_keyword(.Return, Span.default()),
            .True => return TokenData.new_keyword(.True, Span.default()),
            .False => return TokenData.new_keyword(.False, Span.default()),
            .While => return TokenData.new_keyword(.While, Span.default()),
            .Fn => return TokenData.new_keyword(.Fn, Span.default()),
            .Extern => return TokenData.new_keyword(.Extern, Span.default()),
        }
    }
};
