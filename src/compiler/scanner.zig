const std = @import("std");
const Allocator = std.mem.Allocator;
const uni = std.unicode;

const Span = @import("token.zig").Span;
const Token = @import("token.zig").Token;
const TokenData = @import("token.zig").TokenData;
const Keyword = @import("token.zig").Keyword;

const FileLookup = @import("../mod.zig").FileLookup;
const FeanConfig = @import("../mod.zig").FeanConfig;

const Rune = u21;
const List = std.ArrayList;

fn kw_hash(string: []const u8) u64 {
    var step: usize = 1;
    if (string.len >= 16) {
        step = 2;
    }

    var hash_result: u64 = 14695981039346656037;
    var i: usize = 0;
    while (i < string.len) : (i += step) {
        hash_result ^= string[i];
        hash_result = @mulWithOverflow(hash_result, 1099511628211)[0];
    }

    return hash_result;
}

pub const KWPair = struct {
    hash: u64,
    keyword: Keyword,
};
const KW_MAP = [_]KWPair{
    KWPair{ .hash = kw_hash("and"), .keyword = Keyword.And },
    KWPair{ .hash = kw_hash("or"), .keyword = Keyword.Or },
    KWPair{ .hash = kw_hash("trait"), .keyword = Keyword.Trait },
    KWPair{ .hash = kw_hash("impl"), .keyword = Keyword.Impl },
    KWPair{ .hash = kw_hash("if"), .keyword = Keyword.If },
    KWPair{ .hash = kw_hash("else"), .keyword = Keyword.Else },
    KWPair{ .hash = kw_hash("for"), .keyword = Keyword.For },
    KWPair{ .hash = kw_hash("pub"), .keyword = Keyword.Pub },
    KWPair{ .hash = kw_hash("return"), .keyword = Keyword.Return },
    KWPair{ .hash = kw_hash("true"), .keyword = Keyword.True },
    KWPair{ .hash = kw_hash("false"), .keyword = Keyword.False },
    KWPair{ .hash = kw_hash("while"), .keyword = Keyword.While },
    KWPair{ .hash = kw_hash("fn"), .keyword = Keyword.Fn },
    KWPair{ .hash = kw_hash("extern"), .keyword = Keyword.Extern },
};

fn is_keyword(token: *Token) void {
    const token_hash = kw_hash(token.data.identifier);

    for (KW_MAP) |kwp| {
        if (kwp.hash == token_hash) {
            token.data = TokenData{ .keyword = kwp.keyword };
        }
    }
}

const ScannerError = error{
    could_not_decode,
};

pub const Scanner = struct {
    src: []const u8,
    runes: List(Rune),
    rune_index: usize,
    cursor: Span,
    config: *FeanConfig,

    pub fn new(src: []const u8, config: *FeanConfig) anyerror!@This() {
        var runes = try List(Rune).initCapacity(config.allocator, src.len);
        var rune_view = try uni.Utf8View.init(src);
        var rune_iter = rune_view.iterator();

        while (rune_iter.nextCodepoint()) |rune| {
            try runes.append(rune);
        }

        return @This(){
            .src = src,
            .runes = runes,
            .rune_index = 0,
            .cursor = Span.default(),
            .config = config,
        };
    }

    pub fn next_token(self: *@This()) !Token {
        const result = try self.decode_next_token();
        self.cursor.reset();
        return result;
    }

    fn decode_next_token(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.EOS, self.cursor);
        }

        while (!self.is_eos() and self.is_whitespace()) {
            const w = self.pop();
            self.cursor.reset();
            if (w == '\n') {
                self.cursor.newline();
            }
        }

        if (self.is_eos()) {
            return Token.new_symbol(.EOS, self.cursor);
        }

        if (self.is_comment()) {
            while (self.peek() != '\n') {
                _ = self.pop();
            }

            _ = self.pop();
            return self.decode_next_token();
        }

        const first = self.pop();
        switch (first) {
            '(' => return Token.new_symbol(.paren_left, self.cursor),
            ')' => return Token.new_symbol(.paren_right, self.cursor),
            '[' => return Token.new_symbol(.brace_left, self.cursor),
            ']' => return Token.new_symbol(.brace_right, self.cursor),
            '{' => return Token.new_symbol(.curley_left, self.cursor),
            '}' => return Token.new_symbol(.curley_right, self.cursor),
            ',' => return Token.new_symbol(.comma, self.cursor),
            '.' => return Token.new_symbol(.dot, self.cursor),
            ':' => return self.handle_colon(),
            ';' => return Token.new_symbol(.semi_colon, self.cursor),
            '+' => return Token.new_symbol(.plus, self.cursor),
            '-' => return self.handle_minus(),
            '/' => return Token.new_symbol(.slash, self.cursor),
            '!' => return self.handle_bang(),
            '=' => return self.handle_equal(),
            '>' => return self.handle_greater(),
            '<' => return Token.new_symbol(.less, self.cursor),
            else => {},
        }

        // text!
        if (first == '"') {
            return try self.next_text();
        }

        // Decimal | integer
        if (is_numeric(first)) {
            return try self.next_numeric(first);
        }

        // identifier | keyword | builtin!
        if (is_alpha(first)) {
            var result = try self.next_identifier(first);

            is_keyword(&result);

            return result;
        }

        print_error(self.config, self.cursor, "Could not decode token");
        return error.could_not_decode;
    }

    inline fn handle_minus(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.minus, self.cursor);
        }

        const next = self.peek();
        switch (next) {
            '>' => {
                _ = self.pop();
                return Token.new_symbol(.right_arrow, self.cursor);
            },
            else => return Token.new_symbol(.minus, self.cursor),
        }
    }

    inline fn handle_colon(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.colon, self.cursor);
        }

        const next = self.peek();
        switch (next) {
            '=' => {
                _ = self.pop();
                return Token.new_symbol(.variable_asignment, self.cursor);
            },
            ':' => {
                _ = self.pop();
                return Token.new_symbol(.constant_asignment, self.cursor);
            },
            else => return Token.new_symbol(.colon, self.cursor),
        }
    }

    inline fn handle_bang(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.bang, self.cursor);
        }

        const next = self.peek();
        switch (next) {
            '=' => {
                _ = self.pop();
                return Token.new_symbol(.bang_equal, self.cursor);
            },
            else => return Token.new_symbol(.bang, self.cursor),
        }
    }

    inline fn handle_equal(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.equal, self.cursor);
        }

        const next = self.peek();
        switch (next) {
            '=' => {
                _ = self.pop();
                return Token.new_symbol(.equal_equal, self.cursor);
            },
            '<' => {
                _ = self.pop();
                return Token.new_symbol(.less_equal, self.cursor);
            },
            else => return Token.new_symbol(.equal, self.cursor),
        }
    }

    inline fn handle_greater(self: *@This()) !Token {
        if (self.is_eos()) {
            return Token.new_symbol(.greater, self.cursor);
        }

        const next = self.peek();
        switch (next) {
            '=' => {
                _ = self.pop();
                return Token.new_symbol(.greater_equal, self.cursor);
            },
            else => return Token.new_symbol(.greater, self.cursor),
        }
    }

    // todo hande stuff like \n \t as parts of the text,
    fn next_text(self: *@This()) !Token {
        var result = List(u8).init(self.config.allocator);
        var total_len: usize = 0;

        while (!self.is_eos() and self.peek() != '"') {
            const symbol = self.pop();
            const len = try uni.utf8CodepointSequenceLength(symbol);
            total_len += len;

            if (len == 1) {
                try result.append(@intCast(u8, symbol));
            } else {
                var bytes = [4]u8{ 0, 0, 0, 0 };
                _ = try uni.utf8Encode(symbol, &bytes);

                if (len == 2) {
                    try result.append(bytes[0]);
                    try result.append(bytes[1]);
                } else if (len == 3) {
                    try result.append(bytes[0]);
                    try result.append(bytes[1]);
                    try result.append(bytes[3]);
                } else {
                    // todo return error
                    unreachable;
                }
            }
        }

        const text = try self.config.allocator.alloc(u8, total_len);
        std.mem.copy(u8, text, result.items);
        result.deinit();

        _ = self.pop();

        return Token.new_text(text, self.cursor);
    }

    fn next_identifier(self: *@This(), start: u21) !Token {
        var result = List(u8).init(self.config.allocator);
        var total_len: usize = 1;

        try result.append(@intCast(u8, start));

        while (!self.is_eos() and !self.is_whitespace()) {
            if (!is_alphanumeric(self.peek())) {
                break;
            }
            total_len += 1;
            try result.append(@intCast(u8, self.pop()));
        }

        const identifier = try self.config.allocator.alloc(u8, total_len);
        std.mem.copy(u8, identifier, result.items);
        result.deinit();

        return Token.new_identifier(identifier, self.cursor);
    }

    fn next_numeric(self: *@This(), start: u21) !Token {
        const is_pure_decimal = start == '.';

        if (!is_pure_decimal) {
            var whole: i64 = 0;
            whole += ascii_to_int(start);

            while (!self.is_eos()) {
                const n = self.peek();

                if (is_numeric(n) or n == '_') {
                    if (n == '_') {
                        _ = self.pop();
                        continue;
                    }
                    whole *= 10;
                    whole += ascii_to_int(self.pop());
                } else if (n == '.') {
                    _ = self.pop();
                    var part: f64 = 0;
                    var depth: f64 = 1;

                    while (!self.is_eos()) {
                        const d = self.peek();

                        if (is_numeric(d) or n == '_') {
                            if (n == '_') {
                                _ = self.pop();
                                continue;
                            }
                            const div = std.math.pow(f64, 10, depth);
                            part += @intToFloat(f64, ascii_to_int(self.pop())) / div;
                            depth += 1;
                        }
                    }
                    return Token.new_decimal(@intToFloat(f64, whole) + part, self.cursor);
                } else {
                    break;
                }
            }

            return Token.new_integer(whole, self.cursor);
        } else {
            var part: f64 = 0;
            var depth: f64 = 1;
            while (!self.is_eos()) {
                const d = self.peek();

                if (is_numeric(d)) {
                    const div = std.math.pow(f64, 10, depth);
                    part += @intToFloat(f64, ascii_to_int(self.pop())) / div;
                    depth += 1;
                }
            }

            return Token.new_decimal(part, self.cursor);
        }
    }

    inline fn ahead(self: *@This()) Rune {
        return self.runes.items[self.rune_index + 1];
    }

    inline fn peek(self: *@This()) Rune {
        return self.runes.items[self.rune_index];
    }

    inline fn pop(self: *@This()) Rune {
        const result = self.peek();
        self.rune_index += 1;
        self.cursor.len += 1;
        return result;
    }

    inline fn is_whitespace(self: *@This()) bool {
        if (self.peek() == ' ') {
            return true;
        }
        if (self.peek() == '\t') {
            return true;
        }
        if (self.peek() == '\n') {
            return true;
        }
        return false;
    }

    inline fn is_eos(self: *@This()) bool {
        return self.rune_index > (self.runes.items.len - 1);
    }

    inline fn is_comment(self: *@This()) bool {
        if ((self.peek() == '/') and (self.ahead() == '/')) {
            return true;
        }

        return false;
    }
};

inline fn is_alphanumeric(r: Rune) bool {
    return is_alpha(r) or is_numeric(r);
}

inline fn is_numeric(r: Rune) bool {
    if (r > 47 and r < 58) {
        return true;
    }

    return false;
}

inline fn is_alpha(r: Rune) bool {
    if (r > 64 and r < 91) {
        return true;
    }

    if (r > 96 and r < 123) {
        return true;
    }

    if (r == '_') {
        return true;
    }

    return false;
}

fn ascii_to_int(a: u21) u8 {
    return @intCast(u8, a - 48);
}

pub fn print_error(config: *FeanConfig, span: Span, msg: []const u8) void {
    if (config.silent) {
        return;
    }

    if (config.file_debug) {
        const file = config.file_lookup.?(span);
        std.debug.print("[{},{}]{s}: {s}\n\t-> {s}\n", .{ span.line, span.pos, file.filename, file.content, msg });
    } else {
        std.debug.print("[{},{}] -> {s}\n", .{ span.line, span.pos, msg });
    }
}
