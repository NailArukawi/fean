const std = @import("std");

const Token = @import("token.zig").Token;
const Span = @import("token.zig").Span;
const TokenData = @import("token.zig").TokenData;
const TokenKind = @import("token.zig").TokenKind;

const Scanner = @import("scanner.zig").Scanner;

pub const AST = @import("ast.zig").AST;
pub const Node = @import("ast.zig").Node;
pub const Parameter = @import("ast.zig").Parameter;
pub const FunctionBody = @import("ast.zig").FunctionBody;

pub const KindTable = @import("kindtable.zig").KindTable;
pub const SymbolTable = @import("symboltable.zig").SymbolTable;
pub const SymbolKind = @import("symboltable.zig").SymbolKind;
pub const Symbol = @import("symboltable.zig").Symbol;

const FileLookup = @import("../mod.zig").FileLookup;
const FeanConfig = @import("../mod.zig").FeanConfig;

const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;
const Stack = @import("../stack.zig").Stack;

pub const Parser = struct {
    allocator: Allocator,
    tokens: List(Token),
    cursor: usize,
    rooted: bool = true,

    pub fn parse(src: []const u8, config: *FeanConfig) !AST {
        var parser = @This(){
            .allocator = config.allocator,
            .tokens = List(Token).init(config.allocator),
            .cursor = 0,
            .rooted = true,
        };

        var scanner = try Scanner.new(src, config);

        while (true) {
            const next = try scanner.next_token();
            if (next.data == .symbol and next.data.symbol == .EOS) {
                break;
            }

            const span = next.span;
            _ = span;
            //std.debug.print("[{}:{}] scanned:\t{}\n", .{ span.line, span.pos, next.data });

            try parser.tokens.append(next);
        }

        var global_scope = try config.allocator.create(Node);

        global_scope.* = Node{
            .scope = .{
                .parent = null,
                .statments = null,
                .symbols = null,
                .kinds = try KindTable.create_global(config.allocator),
            },
        };

        try parser.start_parse(global_scope);

        //return parser.resolve();
        const ast = AST{
            .head = global_scope.scope.statments.?,
            .symbols = global_scope.scope.symbols,
            .kinds = global_scope.scope.kinds,
        };

        var sym: ?Symbol = ast.symbols;
        while (sym != null) {
            sym.?.global = true;

            sym = sym.?.next;
        }

        return ast;
    }

    fn start_parse(self: *@This(), scope: *Node) !void {
        var lines = List(*Node).init(self.allocator);
        var j: usize = 0;
        while (!self.is_eos()) {
            try lines.append(self.declaration(scope));
            j += 1;
        }

        var mem = try self.allocator.alloc(*Node, j);

        var i: usize = 0;
        while (i < j) {
            mem[i] = lines.items[i];
            i += 1;
        }

        scope.scope.statments = mem;
    }

    fn declaration(self: *@This(), scope: *Node) *Node {
        // variable (X: num)
        if (self.check(.identifier) // X
        and self.check_next(.colon) // :
        and self.check_ahead(.identifier, 2) // num
        ) {
            return self.declaration_variable(scope);
        }
        return self.statment(scope);
    }

    fn declaration_variable(self: *@This(), scope: *Node) *Node {
        const name = self.pop().?;
        self.consume_kind(.colon, "variable/constant missing trailing :");
        const kind_name = self.pop().?;

        var result = self.allocator.create(Node) catch unreachable;

        // zig makes the debug mode transfrom result allocation into constant even it the colon case fail
        // weird and maybe a future FIXME
        if (self.of_kind(.colon)) {
            result.* = Node{
                .constant = .{
                    .name = name.data.identifier,
                    .kind = SymbolKind{ .unresolved = kind_name.data.identifier },
                    .value = self.statment(scope),
                    .symbol = null,
                },
            };

            if (std.mem.eql(u8, result.constant.kind.unresolved, "ExternFn")) {
                result.constant.value.function.body.name = result.constant.name;
            }
        } else if (self.of_kind(.equal)) {
            result.* = Node{
                .variable = .{
                    .name = name.data.identifier,
                    .kind = SymbolKind{ .unresolved = kind_name.data.identifier },
                    .value = self.statment(scope),
                    .symbol = null,
                },
            };

            if (std.mem.eql(u8, result.variable.kind.unresolved, "ExternFn")) {
                result.variable.value.?.function.body.name = result.variable.name;
            }
        } else if (self.of_kind(.semi_colon)) {
            result.* = Node{
                .variable = .{
                    .name = name.data.identifier,
                    .kind = SymbolKind{ .unresolved = kind_name.data.identifier },
                    .value = null,
                    .symbol = null,
                },
            };

            if (std.mem.eql(u8, result.variable.kind.unresolved, "ExternFn")) {
                result.variable.value.?.function.body.name = result.variable.name;
            }
        } else {
            // todo do better
            if (self.is_eos()) {
                @panic("Hit found the end of the file before the end of variable decleration!\n");
            } else {
                // todo fix
                const ree = self.peek().?;
                std.debug.print("[{}:{}] Expected semicolon to end variable decleration!", .{ ree.span.line, ree.span.pos });
            }
        }
        // todo deal with shadowing
        _ = scope.scope.install_symbol(result, self.allocator) catch unreachable;

        return result;
    }

    fn statment(self: *@This(), scope: *Node) *Node {
        // return expr;
        if (self.of_kind(.Return)) {
            return self.statment_return(scope);
        }
        // we have a extern fn
        else if (self.check(.Extern) and self.check_next(.Fn)) {
            _ = self.pop().?;
            _ = self.pop().?;
            return self.statment_function(true, scope);
        }
        // we have a native fn
        else if (self.of_kind(.Fn)) {
            return self.statment_function(false, scope);
        }
        // we have an if
        else if (self.of_kind(.If)) {
            return self.statment_if(scope);
        }
        // we have an while
        else if (self.of_kind(.While)) {
            return self.statment_while(scope);
        }
        // a block of code
        else if (self.of_kind(.curley_left)) {
            return self.statment_block(scope);
        }
        return self.statment_expression(scope);
    }

    fn statment_function(self: *@This(), is_extern: bool, scope: *Node) *Node {
        var fn_exited_root = false;
        if (self.rooted) {
            fn_exited_root = true;
            self.rooted = false;
        }
        defer if (fn_exited_root) {
            self.rooted = true;
        };

        // parameter body
        self.consume_kind(.paren_left, "expected fn parameters to start with a (");
        const param_count = self.parameter_count();
        var parameters = self.allocator.alloc(Parameter, param_count) catch unreachable;
        self.parameter_parse(&parameters, scope);
        self.consume_kind(.paren_right, "expected fn parameters to end with a )");

        // return
        self.consume_kind(.right_arrow, "fn to have a -> type after body");
        self.check_err(.identifier, "expected an identifier after ->");
        const kind_name = self.pop().?.data.identifier;
        var result: ?SymbolKind = null;
        if (!std.mem.eql(u8, kind_name, "void")) {
            result = SymbolKind{ .unresolved = kind_name };
        }

        var function = self.allocator.create(Node) catch unreachable;

        // body
        if (!is_extern) {
            self.consume_kind(.curley_left, "expected a body after function\n fn() -> type {\n...\n}");
            var body = self.statment_block(scope);

            function.* = Node{
                .function = .{
                    .params = parameters,
                    .result = result,
                    .body = FunctionBody{ .body = body },
                    .is_extern = is_extern,
                },
            };
        } else {
            function.* = Node{
                .function = .{
                    .params = parameters,
                    .result = result,
                    .body = FunctionBody{ .name = "" },
                    .is_extern = is_extern,
                },
            };
            self.consume_kind(.semi_colon, "expected extern fn to have a semicolon at the end");
        }

        return function;
    }

    inline fn parameter_count(self: *@This()) usize {
        var count: usize = 0;
        var cursor: usize = 0;

        if (!self.check(.paren_right)) {
            count = 1;
        }

        // todo make better
        while (!self.check_ahead(.paren_right, cursor)) {
            if (self.check_ahead(.comma, cursor)) {
                count += 1;
            }
            cursor += 1;
        }

        return count;
    }

    inline fn parameter_parse(self: *@This(), write_to: *[]Parameter, scope: *Node) void {
        _ = scope;
        var i: usize = 0;
        while (!self.check(.paren_right)) {
            // parameter
            self.check_err(.identifier, "expected an identifier for parameter");
            const name = self.pop().?;
            self.consume_kind(.colon, "expected parameter identifier to have a delimiting :");
            self.check_err(.identifier, "expected an identifier for parameter");
            const kind = self.pop().?;

            // commas
            if ((i + 1) < write_to.len) {
                self.consume_kind(.comma, "expected parameter to have a delimiting ,");
            }

            const parameter = Parameter{ .name = name.data.identifier, .symbol = null, .kind = .{ .unresolved = kind.data.identifier } };
            write_to.*[i] = parameter;
            i += 1;
        }
    }

    fn statment_if(self: *@This(), scope: *Node) *Node {
        var condition = self.expression(scope);
        var if_then = self.statment(scope);
        var if_else: ?*Node = null;

        if (self.of_kind(.Else)) {
            if_else = self.statment(scope);
        }

        var conditional = self.allocator.create(Node) catch unreachable;

        conditional.* = Node{ .conditional_if = .{
            .condition = condition,
            .if_then = if_then,
            .if_else = if_else,
        } };

        return conditional;
    }

    fn statment_while(self: *@This(), scope: *Node) *Node {
        var condition = self.expression(scope);
        var body = self.statment(scope);

        var conditional = self.allocator.create(Node) catch unreachable;

        conditional.* = Node{ .conditional_while = .{
            .condition = condition,
            .body = body,
        } };

        return conditional;
    }

    fn statment_print(self: *@This(), scope: *Node) *Node {
        const op = self.prev();
        const value = self.expression(scope);
        self.consume_kind(.semi_colon, "Expected print statment to end with a ;");

        var result = self.allocator.create(Node) catch unreachable;
        var params = self.allocator.alloc(*Node, 1) catch unreachable;
        params[0] = value;
        result.* = Node{ .builtin = .{ .op = op, .parameters = params } };
        return result;
    }

    fn statment_return(self: *@This(), scope: *Node) *Node {
        if (!self.check(.semi_colon)) {
            const expr = self.expression(scope);
            self.consume_kind(.semi_colon, "Expected return statment to end with a ;");
            var result = self.allocator.create(Node) catch unreachable;
            if (self.rooted) {
                result.* = Node{ .statment = .{ .kind = .ReturnRoot, .value = expr } };
            } else {
                result.* = Node{ .statment = .{ .kind = .Return, .value = expr } };
            }

            return result;
        } else {
            self.consume_kind(.semi_colon, "Expected return statment to end with a ;");
            var result = self.allocator.create(Node) catch unreachable;

            if (self.rooted) {
                result.* = Node{ .statment = .{ .kind = .ReturnRoot, .value = null } };
            } else {
                result.* = Node{ .statment = .{ .kind = .Return, .value = null } };
            }
            return result;
        }
    }

    fn statment_block(self: *@This(), scope: *Node) *Node {
        var block = self.allocator.create(Node) catch unreachable;
        block.* = Node{ .scope = .{
            .parent = scope,
            .statments = null,
            .symbols = null,
            .kinds = null,
        } };

        var stmnts = Stack(*Node).create(self.allocator, 16) catch unreachable;

        while (!self.of_kind(.curley_right)) {
            stmnts.push(self.declaration(block)) catch unreachable;
        }

        stmnts.shrink_to_fit() catch unreachable;
        block.scope.statments = stmnts.as_slice();
        return block;
    }

    fn statment_expression(self: *@This(), scope: *Node) *Node {
        const value = self.expression(scope);
        self.consume_kind(.semi_colon, "Expected statment to end");

        var result = self.allocator.create(Node) catch unreachable;
        result.* = Node{ .statment = .{ .kind = .Expression, .value = value } };
        return result;
    }

    fn expression(self: *@This(), scope: *Node) *Node {
        // variable (X: num)
        if (self.check(.identifier) // X
        and self.check_next(.equal) // =
        ) {
            return self.expression_assignment(scope);
        }
        if (self.check(.identifier) // X
        and (self.check_next(.plus_equal) or self.check_next(.minus_equal) or self.check_next(.slash_equal) or self.check_next(.star_equal)) // x=
        ) {
            return self.expression_assignment(scope);
        }
        // identifier++ or identifier--
        if (self.check(.identifier) // X
        and (self.check_next(.plus_plus) or self.check_next(.minus_minus)) // ++ or --
        ) {
            return self.expression_inc_dec(scope);
        }

        return self.equality(scope);
    }

    fn expression_inc_dec(self: *@This(), scope: *Node) *Node {
        var value = self.primary(scope);
        const operator = self.pop().?; // ++ or --

        var result = self.allocator.create(Node) catch unreachable;
        result.* = Node{ .unary_expression = .{ .op = operator, .value = value } };
        return result;
    }

    // todo lookup symbol
    fn expression_assignment(self: *@This(), scope: *Node) *Node {
        var identity = self.pop().?;
        const is_short = (self.check(.plus_equal) or self.check(.minus_equal) or self.check(.slash_equal) or self.check(.star_equal));
        var short: ?Token = null;
        if (is_short) {
            if (self.check(.plus_equal)) {
                const old = self.pop().?;
                short = Token.new_symbol(.plus, old.span);
            } else if (self.check(.minus_equal)) {
                const old = self.pop().?;
                short = Token.new_symbol(.minus, old.span);
            } else if (self.check(.slash_equal)) {
                const old = self.pop().?;
                short = Token.new_symbol(.slash, old.span);
            } else if (self.check(.star_equal)) {
                const old = self.pop().?;
                short = Token.new_symbol(.star, old.span);
            } else {
                unreachable;
            }
        } else {
            _ = self.pop().?; // =
        }

        var value = self.equality(scope);

        var result = self.allocator.create(Node) catch unreachable;
        if (is_short) {
            var this = self.allocator.create(Node) catch unreachable;
            this.* = Node{ .literal = identity };

            var expanded = self.allocator.create(Node) catch unreachable;
            expanded.* = Node{ .binary_expression = .{ .lhs = this, .op = short.?, .rhs = value, .kind = null } };

            result.* = Node{ .assignment = .{ .name = identity.data.identifier, .symbol = null, .value = expanded } };
        } else {
            result.* = Node{ .assignment = .{ .name = identity.data.identifier, .symbol = null, .value = value } };
        }

        return result;
    }

    fn equality(self: *@This(), scope: *Node) *Node {
        var result = self.comparison(scope);

        const match = [_]TokenKind{ .bang_equal, .equal_equal };
        while (self.of_kinds(&match)) {
            const lhs = result;
            var operator = self.prev();
            var rhs = self.comparison(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = null } };
        }

        return result;
    }

    fn comparison(self: *@This(), scope: *Node) *Node {
        var result = self.term(scope);

        const match = [_]TokenKind{ .greater, .greater_equal, .less, .less_equal };
        while (self.of_kinds(&match)) {
            const lhs = result;
            var operator = self.prev();
            var rhs = self.term(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = null } };
        }

        return result;
    }

    fn term(self: *@This(), scope: *Node) *Node {
        var result = self.factor(scope);

        const match = [_]TokenKind{ .minus, .plus };
        while (self.of_kinds(&match)) {
            const lhs = result;
            var operator = self.prev();
            var rhs = self.factor(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = null } };
        }

        return result;
    }

    fn factor(self: *@This(), scope: *Node) *Node {
        var result = self.unary(scope);

        const match = [_]TokenKind{ .slash, .star };
        while (self.of_kinds(&match)) {
            const lhs = result;
            var operator = self.prev();
            var rhs = self.unary(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = null } };
        }

        return result;
    }

    fn unary(self: *@This(), scope: *Node) *Node {
        const match = [_]TokenKind{ .bang, .minus };
        if (self.of_kinds(&match)) {
            var operator = self.prev();
            var value = self.call(scope);

            var result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .unary_expression = .{ .op = operator, .value = value } };
            return result;
        }

        return self.call(scope);
    }

    fn call(self: *@This(), scope: *Node) *Node {
        var result = self.primary(scope);

        if (self.of_kind(.paren_left)) {
            const args = self.arguments(scope);
            self.consume_kind(.paren_right, "Expected call arguments to end with a )");

            const name = result;

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{
                .call = .{
                    // todo mem leak
                    .name = name.literal.data.identifier,
                    .symbol = null,
                    .arguments = args,
                },
            };
        }

        return result;
    }

    inline fn arguments(self: *@This(), scope: *Node) ?[]*Node {
        var count: usize = 0;
        var cursor: usize = 0;

        while (!self.check_ahead(.paren_right, cursor)) {
            if (self.check_ahead(.comma, cursor)) {
                count += 1;
            }
            cursor += 1;
        }

        var i: usize = 0;
        var write_to = self.allocator.alloc(*Node, count) catch unreachable;
        while (!self.check(.paren_right)) {
            var argument = self.expression(scope);
            write_to[i] = argument;
            i += 1;
        }

        return write_to;
    }

    fn primary(self: *@This(), scope: *Node) *Node {
        if (self.check(.True) or self.check(.False)) {
            var result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }
        //if(self.check(.nil)) {
        //    var result = self.allocator.create(Node) catch unreachable;
        //    result.* =  Node { .literal = self.pop().?};
        //    return result;
        //}
        if (self.check(.decimal) or self.check(.integer)) {
            var result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.check(.text)) {
            var result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.check(.identifier)) {
            var result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.of_kind(.paren_left)) {
            var result = self.expression(scope);
            self.consume_kind(.paren_right, "Expect ')' after grouping.");
            return result;
        }

        std.debug.print("[{}:{}]Unexpected token: {}\n", .{ self.peek().?.span.line, self.peek().?.span.pos, self.peek().? });
        unreachable;
    }

    fn consume_kinds(self: *@This(), kinds: []const TokenKind, comptime err: []const u8) void {
        if (self.of_kinds(kinds)) {
            return;
        } else {
            std.debug.panic("{s}\n", .{err});
        }
    }

    fn consume_kind(self: *@This(), kind: TokenKind, comptime err: []const u8) void {
        if (self.of_kind(kind)) {
            return;
        } else {
            std.debug.panic("{s}\n", .{err});
        }
    }

    fn of_kinds(self: *@This(), kinds: []const TokenKind) bool {
        for (kinds) |kind| {
            if (self.check(kind)) {
                _ = self.pop();
                return true;
            }
        }
        return false;
    }

    fn of_kind(self: *@This(), kind: TokenKind) bool {
        if (self.check(kind)) {
            _ = self.pop();
            return true;
        }

        return false;
    }

    fn ahead_of_kind(self: *@This(), kind: TokenKind, offset: usize) bool {
        if (self.check_ahead(kind, offset)) {
            _ = self.pop();
            return true;
        }

        return false;
    }

    fn next_of_kind(self: *@This(), kind: TokenKind) bool {
        if (self.check_next(kind)) {
            _ = self.pop();
            return true;
        }

        return false;
    }

    fn check_ahead(self: *@This(), kind: TokenKind, offset: usize) bool {
        if (self.is_eos()) {
            return false;
        }
        const next = self.tokens.items[self.cursor + offset];
        return TokenKind.translate_from(next.data) == kind;
    }

    fn check_next(self: *@This(), kind: TokenKind) bool {
        if (self.is_eos()) {
            return false;
        }
        const next = self.tokens.items[self.cursor + 1];
        return TokenKind.translate_from(next.data) == kind;
    }

    fn check(self: *@This(), kind: TokenKind) bool {
        if (self.is_eos()) {
            return false;
        }

        return TokenKind.translate_from(self.peek().?.data) == kind;
    }

    fn check_err(self: *@This(), kind: TokenKind, comptime err: []const u8) void {
        if (self.check(kind)) {
            return;
        } else {
            std.debug.panic("{s}\n", .{err});
        }
    }

    fn is_eos(self: *@This()) bool {
        return self.cursor >= self.tokens.items.len;
    }

    fn peek(self: *@This()) ?Token {
        if (self.is_eos()) {
            return null;
        }

        return self.tokens.items[self.cursor];
    }

    fn pop(self: *@This()) ?Token {
        var result = self.peek();
        //std.debug.print("[{}:{}] popped:\t{}\n", .{ result.?.span.line, result.?.span.pos, result.?.data });

        self.cursor += 1;

        return result;
    }

    fn prev(self: *@This()) Token {
        return self.tokens.items[self.cursor - 1];
    }

    pub fn debug(self: *@This()) void {

        // print our token list
        std.debug.print("Tokens: {{", .{});

        const unused_tokens = self.tokens.items[self.cursor..self.tokens.items.len];
        for (unused_tokens, 0..) |t, i| {
            if (i != (unused_tokens.len - 1)) {
                std.debug.print(" {},", .{t.data});
            } else {
                std.debug.print(" {} }}\n", .{t.data});
            }
        }
    }
};
