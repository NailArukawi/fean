const std = @import("std");
const Allocator = std.mem.Allocator;
const uni = std.unicode;

const mod = @import("mod.zig");
const Span = mod.Span;
const Token = mod.Token;
const TokenData = mod.TokenData;
const Keyword = mod.Keyword;

const FileLookup = @import("../fean.zig").FileLookup;
const FeanConfig = @import("../fean.zig").FeanConfig;

const Rune = u21;
const Stack = @import("../common/stack.zig").Stack;

pub const KWPair = struct {
    hash: u64,
    keyword: Keyword,

    pub fn kw_hash(string: []const u8) u64 {
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
};

const KW_MAP = [_]KWPair{
    KWPair{ .hash = KWPair.kw_hash("and"), .keyword = Keyword.And },
    KWPair{ .hash = KWPair.kw_hash("or"), .keyword = Keyword.Or },
    KWPair{ .hash = KWPair.kw_hash("trait"), .keyword = Keyword.Trait },
    KWPair{ .hash = KWPair.kw_hash("struct"), .keyword = Keyword.Struct },
    KWPair{ .hash = KWPair.kw_hash("Self"), .keyword = Keyword.Self },
    KWPair{ .hash = KWPair.kw_hash("self"), .keyword = Keyword.self },
    KWPair{ .hash = KWPair.kw_hash("impl"), .keyword = Keyword.Impl },
    KWPair{ .hash = KWPair.kw_hash("if"), .keyword = Keyword.If },
    KWPair{ .hash = KWPair.kw_hash("else"), .keyword = Keyword.Else },
    KWPair{ .hash = KWPair.kw_hash("for"), .keyword = Keyword.For },
    KWPair{ .hash = KWPair.kw_hash("pub"), .keyword = Keyword.Pub },
    KWPair{ .hash = KWPair.kw_hash("return"), .keyword = Keyword.Return },
    KWPair{ .hash = KWPair.kw_hash("true"), .keyword = Keyword.True },
    KWPair{ .hash = KWPair.kw_hash("false"), .keyword = Keyword.False },
    KWPair{ .hash = KWPair.kw_hash("while"), .keyword = Keyword.While },
    KWPair{ .hash = KWPair.kw_hash("fn"), .keyword = Keyword.Fn },
    KWPair{ .hash = KWPair.kw_hash("extern"), .keyword = Keyword.Extern },
};

const ScannerError = error{
    could_not_decode,
    unicode_read_malformed,
    identifier_contains_non_alphanumeric,
};

pub fn Scanner(comptime Reader: type) type {
    const Stream = struct {
        reader: Reader,
        buffer: []u8,
        start: u32,
        end: u32,

        pub fn new(buffer: []u8, reader: Reader) @This() {
            return @This(){
                .reader = reader,
                .buffer = buffer,
                .start = 0,
                .end = 0,
            };
        }

        fn read(self: *@This()) !void {
            var rest_count: u8 = @as(u8, @intCast(self.end - self.start));
            std.debug.assert(self.buffer.len > rest_count);

            if (rest_count > 0)
                @memcpy(self.buffer[0..rest_count], self.buffer[(self.start)..(self.end)]);

            self.start = 0;
            self.end = rest_count;

            const read_count = @as(u32, @intCast(try self.reader.read(self.buffer[rest_count..self.buffer.len])));

            if (read_count == 0) {
                self.buffer[self.end] = 0;
                self.end += 1;
            } else self.end += read_count;
        }

        pub fn peek(self: *@This()) !Rune {
            var count = self.end - self.start;
            if (count == 0)
                try self.read();

            count = self.end - self.start;
            const rune_len: u3 = try uni.utf8ByteSequenceLength(self.buffer[self.start]);
            if (rune_len > count)
                try self.read();

            count = self.end - self.start;
            const rune = try uni.utf8Decode(self.buffer[self.start..(self.start + rune_len)]);

            return rune;
        }

        pub fn pop(self: *@This()) !Rune {
            var count = self.end - self.start;
            if (count == 0)
                try self.read();

            count = self.end - self.start;
            const rune_len: u3 = try uni.utf8ByteSequenceLength(self.buffer[self.start]);
            if (rune_len > count)
                try self.read();

            count = self.end - self.start;
            const rune = try uni.utf8Decode(self.buffer[self.start..(self.start + rune_len)]);

            self.start += rune_len;
            return rune;
        }
    };

    return struct {
        stream: Stream,
        buffer: Stack(u8),
        cursor: Span,
        config: *FeanConfig,

        pub fn create(buffer: []u8, reader: Reader, config: *FeanConfig) anyerror!@This() {
            std.debug.assert(buffer.len > 3);
            return @This(){
                .stream = Stream.new(buffer, reader),
                .buffer = try Stack(u8).create(config.allocator, 8),
                .cursor = Span.default(),
                .config = config,
            };
        }

        pub fn destroy(self: *@This()) void {
            self.buffer.destroy();
        }

        pub fn next_token(self: *@This()) !Token {
            const result = try self.decode_next_token();
            self.cursor.reset();
            return result;
        }

        fn decode_next_token(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.EOS, self.cursor);

            while (!self.is_eos() and self.is_whitespace()) {
                const w = try self.pop();
                self.cursor.reset();
                if (w == '\n') {
                    self.cursor.newline();
                }
            }

            if (self.is_eos())
                return Token.new_symbol(.EOS, self.cursor);

            const first = try self.pop();
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
                '+' => return self.handle_plus(),
                '-' => return self.handle_minus(),
                '/' => return self.handle_slash(),
                '*' => return self.handle_star(),
                '!' => return self.handle_bang(),
                '=' => return self.handle_equal(),
                '>' => return self.handle_greater(),
                '<' => return Token.new_symbol(.less, self.cursor),
                else => {},
            }

            // text!
            if (first == '"')
                return try self.next_text();

            // Decimal | integer
            if (is_numeric(first))
                return try self.next_numeric(first);

            // identifier | keyword | builtin!
            if (is_alpha(first)) {
                var result = try self.next_identifier(first);

                is_keyword(&result);

                return result;
            }

            print_error(self.config, self.cursor, "Could not decode token");
            return error.could_not_decode;
        }

        inline fn handle_plus(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.plus, self.cursor);

            const next = try self.peek();
            switch (next) {
                '+' => {
                    _ = try self.pop();
                    return Token.new_symbol(.plus_plus, self.cursor);
                },
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.plus_equal, self.cursor);
                },
                else => return Token.new_symbol(.plus, self.cursor),
            }
        }

        inline fn handle_minus(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.minus, self.cursor);

            const next = try self.peek();
            switch (next) {
                '-' => {
                    _ = try self.pop();
                    return Token.new_symbol(.minus_minus, self.cursor);
                },
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.minus_equal, self.cursor);
                },
                '>' => {
                    _ = try self.pop();
                    return Token.new_symbol(.right_arrow, self.cursor);
                },
                else => return Token.new_symbol(.minus, self.cursor),
            }
        }

        inline fn handle_slash(self: *@This()) anyerror!Token {
            if (self.is_eos())
                return Token.new_symbol(.slash, self.cursor);

            const next = try self.peek();
            switch (next) {
                '/' => { // comment
                    while ((!self.is_eos()) and (try self.peek()) != '\n') _ = try self.pop();
                    if (try self.peek() == 0) {
                        _ = try self.pop();
                        return Token.new_symbol(.EOS, self.cursor);
                    }
                    return self.decode_next_token();
                },
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.slash_equal, self.cursor);
                },
                else => return Token.new_symbol(.slash, self.cursor),
            }
        }

        inline fn handle_star(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.star, self.cursor);

            const next = try self.peek();
            switch (next) {
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.star_equal, self.cursor);
                },
                else => return Token.new_symbol(.star, self.cursor),
            }
        }

        inline fn handle_colon(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.colon, self.cursor);

            const next = try self.peek();
            switch (next) {
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.variable_asignment, self.cursor);
                },
                else => return Token.new_symbol(.colon, self.cursor),
            }
        }

        inline fn handle_bang(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.bang, self.cursor);

            const next = try self.peek();
            switch (next) {
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.bang_equal, self.cursor);
                },
                else => return Token.new_symbol(.bang, self.cursor),
            }
        }

        inline fn handle_equal(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.equal, self.cursor);

            const next = try self.peek();
            switch (next) {
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.equal_equal, self.cursor);
                },
                '<' => {
                    _ = try self.pop();
                    return Token.new_symbol(.less_equal, self.cursor);
                },
                else => return Token.new_symbol(.equal, self.cursor),
            }
        }

        inline fn handle_greater(self: *@This()) !Token {
            if (self.is_eos())
                return Token.new_symbol(.greater, self.cursor);

            const next = try self.peek();
            switch (next) {
                '=' => {
                    _ = try self.pop();
                    return Token.new_symbol(.greater_equal, self.cursor);
                },
                else => return Token.new_symbol(.greater, self.cursor),
            }
        }

        // todo hande stuff like \n \t as parts of the text,
        fn next_text(self: *@This()) !Token {
            var total_len: usize = 0;
            while (!self.is_eos() and (try self.peek()) != '"') {
                const symbol = try self.pop();
                const len = try uni.utf8CodepointSequenceLength(symbol);
                total_len += len;

                if (len == 1) {
                    try self.buffer.push(@as(u8, @intCast(symbol)));
                } else {
                    var bytes = [3]u8{ 0, 0, 0 };
                    _ = try uni.utf8Encode(symbol, &bytes);

                    if (len == 2) {
                        try self.buffer.push(bytes[0]);
                        try self.buffer.push(bytes[1]);
                    } else if (len == 3) {
                        try self.buffer.push(bytes[0]);
                        try self.buffer.push(bytes[1]);
                        try self.buffer.push(bytes[2]);
                    } else return ScannerError.unicode_read_malformed;
                }
            }

            const text = try self.config.allocator.alloc(u8, total_len);
            std.mem.copy(u8, text, self.buffer.as_slice());

            _ = try self.pop();
            self.buffer.clear();

            return Token.new_text(text, self.cursor);
        }

        fn next_identifier(self: *@This(), start: Rune) !Token {
            if (!is_alphanumeric(start)) return ScannerError.identifier_contains_non_alphanumeric;

            var total_len: usize = 1;
            try self.buffer.push(@as(u8, @intCast(start)));

            while (!self.is_eos() and !self.is_whitespace()) {
                if (!is_alphanumeric(try self.peek()))
                    break;

                total_len += 1;
                try self.buffer.push(@as(u8, @intCast(try self.pop())));
            }

            const identifier = try self.config.allocator.alloc(u8, total_len);
            std.mem.copy(u8, identifier, self.buffer.as_slice());
            self.buffer.clear();

            return Token.new_identifier(identifier, self.cursor);
        }

        fn next_numeric(self: *@This(), start: Rune) !Token {
            const is_pure_decimal = start == '.';

            if (!is_pure_decimal) {
                var whole: i64 = 0;
                whole += ascii_to_int(start);

                while (!self.is_eos()) {
                    const n = try self.peek();

                    if (is_numeric(n) or n == '_') {
                        if (n == '_') {
                            _ = try self.pop();
                            continue;
                        }
                        whole *= 10;
                        whole += ascii_to_int(try self.pop());
                    } else if (n == '.') {
                        _ = try self.pop();
                        var part: f64 = 0;
                        var depth: f64 = 1;

                        while (!self.is_eos()) {
                            const d = try self.peek();

                            if (is_numeric(d) or n == '_') {
                                if (n == '_') {
                                    _ = try self.pop();
                                    continue;
                                }
                                const div = std.math.pow(f64, 10, depth);
                                part += @as(f64, @floatFromInt(ascii_to_int(try self.pop()))) / div;
                                depth += 1;
                            } else {
                                break;
                            }
                        }
                        return Token.new_decimal(@as(f64, @floatFromInt(whole)) + part, self.cursor);
                    } else {
                        break;
                    }
                }

                return Token.new_integer(whole, self.cursor);
            } else {
                var part: f64 = 0;
                var depth: f64 = 1;
                while (!self.is_eos()) {
                    const d = try self.peek();

                    if (is_numeric(d)) {
                        const div = std.math.pow(f64, 10, depth);
                        part += @as(f64, @floatFromInt(ascii_to_int(try self.pop()))) / div;
                        depth += 1;
                    }
                }

                return Token.new_decimal(part, self.cursor);
            }
        }

        inline fn peek(self: *@This()) !Rune {
            return self.stream.peek();
        }

        inline fn pop(self: *@This()) !Rune {
            const result = try self.stream.pop();
            self.cursor.len += 1;
            return result;
        }

        inline fn is_whitespace(self: *@This()) bool {
            const next = self.peek() catch return false;
            if (next == ' ')
                return true;

            if (next == '\t')
                return true;

            if (next == '\r')
                return true;

            if (next == '\n')
                return true;

            return false;
        }

        inline fn is_eos(self: *@This()) bool {
            const next = self.peek() catch return true;
            return next == 0;
        }
    };
}

fn is_keyword(token: *Token) void {
    const token_hash = KWPair.kw_hash(token.data.identifier);

    for (KW_MAP) |kwp| {
        if (kwp.hash == token_hash)
            token.data = TokenData{ .keyword = kwp.keyword };
    }
}

inline fn is_alphanumeric(r: Rune) bool {
    return is_alpha(r) or is_numeric(r);
}

inline fn is_numeric(r: Rune) bool {
    if (r > 47 and r < 58)
        return true;

    return false;
}

inline fn is_alpha(r: Rune) bool {
    if (r > 64 and r < 91)
        return true;

    if (r > 96 and r < 123)
        return true;

    if (r == '_')
        return true;

    return false;
}

fn ascii_to_int(a: u21) u8 {
    return @as(u8, @intCast(a - 48));
}

pub fn print_error(config: *FeanConfig, span: Span, msg: []const u8) void {
    if (config.silent)
        return;

    if (config.file_debug) {
        const file = config.file_lookup.?(span);
        std.debug.print("[{},{}]{s}: {s}\n\t-> {s}\n", .{ span.line, span.pos, file.filename, file.content, msg });
    } else {
        std.debug.print("[{},{}] -> {s}\n", .{ span.line, span.pos, msg });
    }
}
