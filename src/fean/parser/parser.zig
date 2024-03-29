const std = @import("std");

const Token = @import("token.zig").Token;
const Span = @import("token.zig").Span;
const TokenData = @import("token.zig").TokenData;
const TokenKind = @import("token.zig").TokenKind;

const Scanner = @import("scanner.zig").Scanner;

pub const AST = @import("ast.zig").AST;
pub const Node = @import("ast.zig").Node;
pub const Parameter = @import("ast.zig").Parameter;
pub const Field = @import("ast.zig").Field;
pub const FieldOrName = @import("ast.zig").FieldOrName;
pub const FunctionBody = @import("ast.zig").FunctionBody;

pub const KindTable = @import("../compiler/kindtable.zig").KindTable;
pub const SymbolTable = @import("../compiler/symboltable.zig").SymbolTable;
pub const SymbolKind = @import("../compiler/symboltable.zig").SymbolKind;
pub const Symbol = @import("../compiler/symboltable.zig").Symbol;

const FileLookup = @import("../fean.zig").FileLookup;
const FeanConfig = @import("../fean.zig").FeanConfig;

const Allocator = std.mem.Allocator;
const Map = std.AutoHashMap;
const List = std.ArrayList;
const Stack = @import("../common/stack.zig").Stack;

pub const Parser = struct {
    allocator: Allocator,
    tokens: List(Token),
    cursor: usize,
    rooted: bool = true,

    pub fn parse(reader: std.fs.File.Reader, config: *FeanConfig) !AST {
        var parser = @This(){
            .allocator = config.allocator,
            .tokens = List(Token).init(config.allocator),
            .cursor = 0,
            .rooted = true,
        };

        var scanner_buffer: [16]u8 = undefined;
        var scanner = try Scanner(std.fs.File.Reader).create(&scanner_buffer, reader, config);
        defer scanner.destroy();

        while (true) {
            const next = try scanner.next_token();
            if (next.data == .symbol and next.data.symbol == .EOS) {
                break;
            }

            //const span = next.span;
            //if (next.data == .identifier) {
            //    std.debug.print("[{}:{}] scanned(Identifier):\t{s}\n", .{ span.line, span.pos, next.data.identifier });
            //} else {
            //    std.debug.print("[{}:{}] scanned( ? ):\t{}\n", .{ span.line, span.pos, next.data });
            //}

            try parser.tokens.append(next);
        }

        const global_scope = try config.allocator.create(Node);

        global_scope.* = Node{
            .scope = .{
                .parent = null,
                .statments = null,
                .symbols = try SymbolTable.create(config.allocator),
                .kinds = try KindTable.createGlobal(config.allocator),
            },
        };

        try parser.start_parse(global_scope);

        //return parser.resolve();
        const ast = AST{
            .head = global_scope.scope.statments.?,
            .symbols = global_scope.scope.symbols,
            .kinds = global_scope.scope.kinds,
        };

        var sym: ?*Symbol = ast.symbols.head;
        while (sym != null) {
            sym.?.global = true;

            sym = sym.?.next;
        }

        parser.tokens.deinit();
        return ast;
    }

    fn start_parse(self: *@This(), scope: *Node) !void {
        var lines = List(*Node).init(self.allocator);
        var j: usize = 0;
        while (!self.is_eos()) {
            try lines.append(self.declaration(scope, false));
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

    fn declaration(self: *@This(), scope: *Node, impl: bool) *Node {
        if (self.check(.Struct) and self.check_next(.identifier)) {
            _ = self.pop().?;
            return self.declaration_struct(scope);
        } else if (self.check(.Impl) and self.check_next(.identifier) and !self.check_next(.For)) {
            _ = self.pop().?;
            if (impl)
                @panic("Nested impl not allowed!");
            return self.declaration_impl(scope);
        } else if (self.check(.identifier) // X
        and self.check_next(.colon) // :
        and self.check_ahead(.identifier, 2) // num
        ) {
            const variable = self.declaration_variable(scope, false);

            if (impl)
                self.declaration_try_method(scope, variable);

            return variable;
        } else if (self.check(.identifier) // X
        and self.check_next(.colon) // :
        and (self.check_ahead(.colon, 2) or self.check_ahead(.equal, 2)) // : or =
        ) {
            const variable = self.declaration_variable(scope, true);

            if (impl)
                self.declaration_try_method(scope, variable);

            return variable;
        }
        return self.statment(scope);
    }

    fn declaration_try_method(self: *@This(), scope: *Node, parent_variable: *Node) void {
        _ = self;
        _ = scope;
        const variable_value: ?*Node = switch (parent_variable.*) {
            .variable => |v| v.value,
            .constant => |c| c.value,
            else => @panic("Variable is not variable"),
        };

        const variable_name = switch (parent_variable.*) {
            .variable => |v| v.name,
            .constant => |c| c.name,
            else => @panic("Variable is not variable"),
        };

        if (variable_value != null and variable_value.?.* == .function) {
            const function = variable_value.?.function; // todo look for mem leak

            const result: Node = .{
                .method = .{
                    .name = variable_name,
                    .params = function.params,
                    .result = function.result,
                    .body = function.body,
                    .is_extern = function.is_extern,
                },
            };

            //self.allocator.free(function);
            parent_variable.* = result;
        }
    }

    fn declaration_impl(self: *@This(), scope: *Node) *Node {
        const struct_name = self.pop().?;
        self.consume_kind(.curley_left, "struct is missing { to start struct body");
        const body = self.statment_block(scope, true);

        const result = self.allocator.create(Node) catch unreachable;

        result.* = Node{ .impl = .{
            .this = SymbolKind{ .unresolved = struct_name.data.identifier },
            .body = body,
        } };

        return result;
    }

    fn declaration_struct(self: *@This(), scope: *Node) *Node {
        _ = scope;
        const struct_name = self.pop().?;
        self.consume_kind(.curley_left, "struct is missing { to start struct body");
        const fields = self.struct_fields();

        const result = self.allocator.create(Node) catch unreachable;

        result.* = Node{ .structure = .{
            .name = struct_name.data.identifier,
            .symbol = null,
            .this = null,
            .fields = fields,
        } };

        return result;
    }

    fn struct_fields(self: *@This()) ?[]Field {
        var fields = Stack(Field).create(self.allocator, 2) catch unreachable;

        while (!self.check(.curley_right)) {
            if (self.check(.comma)) {
                _ = self.pop().?;
            }

            self.check_err(.identifier, "expected a name for the struct field");
            const field_name = self.pop().?;
            self.consume_kind(.colon, "field name is missing trailing :");
            self.check_err(.identifier, "expected a type for the struct field");
            const field_kind_name = self.pop().?;
            fields.push(Field{ .name = field_name.data.identifier, .symbol = null, .kind = SymbolKind{ .unresolved = field_kind_name.data.identifier }, .value = null }) catch unreachable;
        }
        _ = self.pop().?;

        if (fields.count() == 0) {
            fields.destroy();
            return null;
        }

        fields.shrink_to_fit() catch unreachable;

        return fields.as_slice();
    }

    fn declaration_variable(self: *@This(), scope: *Node, infered: bool) *Node {
        const name = self.pop().?;
        self.consume_kind(.colon, "variable/constant missing trailing :");
        var kind: SymbolKind = .none;
        if (!infered) {
            const kind_name = self.pop().?;
            kind = SymbolKind{ .unresolved = kind_name.data.identifier };
        }

        var result = self.allocator.create(Node) catch unreachable;

        // zig makes the debug mode transfrom result allocation into constant even it the colon case fail
        // weird and maybe a future FIXME
        if (self.of_kind(.colon)) {
            result.* = Node{
                .constant = .{
                    .name = name.data.identifier,
                    .kind = kind,
                    .value = self.statment(scope),
                    .symbol = null,
                },
            };

            const constant = result.constant;
            if (constant.value.* == .function and constant.value.function.is_extern) {
                result.constant.value.function.body.name = result.constant.name;
            }
        } else if (self.of_kind(.equal)) {
            result.* = Node{
                .variable = .{
                    .name = name.data.identifier,
                    .kind = kind,
                    .value = self.statment(scope),
                    .symbol = null,
                },
            };

            const variable = result.variable;
            if (variable.value != null and variable.value.?.* == .function and variable.value.?.function.is_extern) {
                result.variable.value.?.function.body.name = result.variable.name;
            }
        } else if (self.of_kind(.semi_colon)) {
            result.* = Node{
                .variable = .{
                    .name = name.data.identifier,
                    .kind = kind,
                    .value = null,
                    .symbol = null,
                },
            };

            const variable = result.variable;
            if (variable.value != null and variable.value.?.* == .function and variable.value.?.function.is_extern) {
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
        _ = scope.scope.installSymbol(result, self.allocator) catch unreachable;

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
            return self.statment_block(scope, false);
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
        self.parameter_parse(&parameters);
        self.consume_kind(.paren_right, "expected fn parameters to end with a )");

        // return
        self.consume_kind(.right_arrow, "fn to have a -> type after body");
        self.check_err(.identifier, "expected an identifier after ->");
        const kind_name = self.pop().?.data.identifier;
        var result: SymbolKind = .none;
        if (!std.mem.eql(u8, kind_name, "void")) {
            result = SymbolKind{ .unresolved = kind_name };
        }

        const function = self.allocator.create(Node) catch unreachable;

        // body
        if (!is_extern) {
            self.consume_kind(.curley_left, "expected a body after function\n fn() -> type {\n...\n}");
            const body = self.statment_block(scope, false);

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

    inline fn parameter_parse(self: *@This(), write_to: *[]Parameter) void {
        var i: usize = 0;
        while (!self.check(.paren_right)) {
            if (self.of_kind(.Self)) {
                if (i != 0)
                    @panic("Self has to be first parameter");
                const parameter = Parameter{ .name = "self", .symbol = null, .kind = .{ .unresolved = "Self" } };
                write_to.*[i] = parameter;
                i += 1;
                continue;
            }

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
        const condition = self.expression(scope);
        const if_then = self.statment(scope);
        var if_else: ?*Node = null;

        if (self.of_kind(.Else)) {
            if_else = self.statment(scope);
        }

        const conditional = self.allocator.create(Node) catch unreachable;

        conditional.* = Node{ .conditional_if = .{
            .condition = condition,
            .if_then = if_then,
            .if_else = if_else,
        } };

        return conditional;
    }

    fn statment_while(self: *@This(), scope: *Node) *Node {
        const condition = self.expression(scope);
        const body = self.statment(scope);

        const conditional = self.allocator.create(Node) catch unreachable;

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

        const result = self.allocator.create(Node) catch unreachable;
        var params = self.allocator.alloc(*Node, 1) catch unreachable;
        params[0] = value;
        result.* = Node{ .builtin = .{ .op = op, .parameters = params } };
        return result;
    }

    fn statment_return(self: *@This(), scope: *Node) *Node {
        if (!self.check(.semi_colon)) {
            const expr = self.expression(scope);
            self.consume_kind(.semi_colon, "Expected return statment to end with a ;");
            const result = self.allocator.create(Node) catch unreachable;
            if (self.rooted) {
                result.* = Node{ .statment = .{ .kind = .ReturnRoot, .value = expr } };
            } else {
                result.* = Node{ .statment = .{ .kind = .Return, .value = expr } };
            }

            return result;
        } else {
            self.consume_kind(.semi_colon, "Expected return statment to end with a ;");
            const result = self.allocator.create(Node) catch unreachable;

            if (self.rooted) {
                result.* = Node{ .statment = .{ .kind = .ReturnRoot, .value = null } };
            } else {
                result.* = Node{ .statment = .{ .kind = .Return, .value = null } };
            }
            return result;
        }
    }

    fn statment_block(self: *@This(), scope: *Node, impl: bool) *Node {
        var block = self.allocator.create(Node) catch unreachable;
        block.* = Node{ .scope = .{
            .parent = scope,
            .statments = null,
            .symbols = SymbolTable.create(self.allocator) catch unreachable,
            .kinds = KindTable.create(self.allocator) catch unreachable,
        } };

        var stmnts = Stack(*Node).create(self.allocator, 16) catch unreachable;

        while (!self.of_kind(.curley_right)) {
            stmnts.push(self.declaration(block, impl)) catch unreachable;
        }

        stmnts.shrink_to_fit() catch unreachable;
        block.scope.statments = stmnts.as_slice();
        return block;
    }

    fn statment_expression(self: *@This(), scope: *Node) *Node {
        const value = self.expression(scope);
        self.consume_kind(.semi_colon, "Expected statment to end");

        const result = self.allocator.create(Node) catch unreachable;
        result.* = Node{ .statment = .{ .kind = .Expression, .value = value } };
        return result;
    }

    fn expression(self: *@This(), scope: *Node) *Node {
        // variable (identifier: num)
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

        const result = self.equality(scope);

        if (result.* == .get and self.check(.equal)) { // *.X =
            return self.expression_set(scope, result);
        }

        if (result.* == .get and (self.check_next(.plus_equal) or self.check_next(.minus_equal) or self.check_next(.slash_equal) or self.check_next(.star_equal)) // *.x (+|-|/|*)=
        ) {
            return self.expression_set(scope, result);
        }

        return result;
    }

    fn expression_inc_dec(self: *@This(), scope: *Node) *Node {
        const value = self.primary(scope);
        const operator = self.pop().?; // ++ or --

        const result = self.allocator.create(Node) catch unreachable;
        result.* = Node{ .unary_expression = .{ .op = operator, .value = value } };
        return result;
    }

    fn expression_set(self: *@This(), scope: *Node, get: *Node) *Node {
        const field = get.get.field;
        const object = get.get.object;

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

        const value = self.equality(scope);
        if (is_short) {
            const result = self.allocator.create(Node) catch unreachable;
            const expanded = self.allocator.create(Node) catch unreachable;
            expanded.* = Node{ .binary_expression = .{ .lhs = get, .op = short.?, .rhs = value, .kind = .none } };
            result.* = Node{ .set = .{ .field = field, .object = object, .kind = .none, .value = expanded } };
            return result;
        } else {
            get.* = Node{ .set = .{ .field = field, .object = object, .kind = .none, .value = value } };
            return get;
        }
    }

    fn expression_assignment(self: *@This(), scope: *Node) *Node {
        const identity = self.pop().?;
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

        const value = self.equality(scope);

        const result = self.allocator.create(Node) catch unreachable;
        if (is_short) {
            const this = self.allocator.create(Node) catch unreachable;
            this.* = Node{ .literal = identity };

            const expanded = self.allocator.create(Node) catch unreachable;
            expanded.* = Node{ .binary_expression = .{ .lhs = this, .op = short.?, .rhs = value, .kind = .none } };
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
            const operator = self.prev();
            const rhs = self.comparison(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = .none } };
        }

        return result;
    }

    fn comparison(self: *@This(), scope: *Node) *Node {
        var result = self.term(scope);

        const match = [_]TokenKind{ .greater, .greater_equal, .less, .less_equal };
        while (self.of_kinds(&match)) {
            const lhs = result;
            const operator = self.prev();
            const rhs = self.term(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = .none } };
        }

        return result;
    }

    fn term(self: *@This(), scope: *Node) *Node {
        var result = self.factor(scope);

        const match = [_]TokenKind{ .minus, .plus };
        while (self.of_kinds(&match)) {
            const lhs = result;
            const operator = self.prev();
            const rhs = self.factor(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = .none } };
        }

        return result;
    }

    fn factor(self: *@This(), scope: *Node) *Node {
        var result = self.unary(scope);

        const match = [_]TokenKind{ .slash, .star };
        while (self.of_kinds(&match)) {
            const lhs = result;
            const operator = self.prev();
            const rhs = self.unary(scope);

            result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .binary_expression = .{ .lhs = lhs, .op = operator, .rhs = rhs, .kind = .none } };
        }

        return result;
    }

    fn unary(self: *@This(), scope: *Node) *Node {
        const match = [_]TokenKind{ .bang, .minus };
        if (self.of_kinds(&match)) {
            const operator = self.prev();
            const value = self.call(scope);

            const result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .unary_expression = .{ .op = operator, .value = value } };
            return result;
        }

        return self.call(scope);
    }

    fn call(self: *@This(), scope: *Node) *Node {
        var result = self.primary(scope);

        while (true) {
            if (self.of_kind(.paren_left)) {
                const args = self.arguments(scope);
                self.consume_kind(.paren_right, "Expected call arguments to end with a )");

                if (result.* == Node.get) {
                    const callee = result.get;

                    result.* = Node{
                        .call = .{
                            // todo mem leak
                            .callee = .{ .method = callee },
                            .symbol = null,
                            .arguments = args,
                        },
                    };
                } else {
                    const callee = result;

                    result = self.allocator.create(Node) catch unreachable;
                    result.* = Node{
                        .call = .{
                            // todo mem leak
                            .callee = .{ .function = callee.literal.data.identifier },
                            .symbol = null,
                            .arguments = args,
                        },
                    };
                }
            } else if (self.of_kind(.dot)) {
                self.check_err(.identifier, "missing identifier at X identifier.x");
                const name = self.pop().?;
                const expr = result;

                result = self.allocator.create(Node) catch unreachable;
                result.* = Node{
                    .get = .{
                        // todo mem leak
                        .field = FieldOrName{ .unresolved = name.data.identifier },
                        .kind = .none,
                        .object = expr,
                    },
                };
            } else break;
        }

        return result;
    }

    inline fn arguments(self: *@This(), scope: *Node) ?[]*Node {
        var params = Stack(*Node).create(self.allocator, 255) catch unreachable;

        while (!self.check(.paren_right)) {
            if (self.check(.comma)) {
                _ = self.pop().?;
            }

            params.push(self.expression(scope)) catch unreachable;
        }

        if (params.count() == 0) {
            params.destroy();
            return null;
        }

        params.shrink_to_fit() catch unreachable;

        return params.as_slice();
    }

    fn primary(self: *@This(), scope: *Node) *Node {
        if (self.check(.identifier) and self.check_next(.curley_left)) {
            return self.primary_construct(scope);
        }
        if (self.check(.True) or self.check(.False)) {
            const result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }
        //if(self.check(.nil)) {
        //    var result = self.allocator.create(Node) catch unreachable;
        //    result.* =  Node { .literal = self.pop().?};
        //    return result;
        //}
        if (self.check(.decimal) or self.check(.integer)) {
            const result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.check(.text)) {
            const result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.check(.identifier)) {
            const result = self.allocator.create(Node) catch unreachable;
            result.* = Node{ .literal = self.pop().? };
            return result;
        }

        if (self.check(.self)) {
            const result = self.allocator.create(Node) catch unreachable;
            const popped = self.pop().?;
            result.* = Node{ .literal = Token.new_identifier("self", popped.span) };
            return result;
        }

        if (self.of_kind(.paren_left)) {
            const result = self.expression(scope);
            self.consume_kind(.paren_right, "Expect ')' after grouping.");
            return result;
        }

        std.debug.print("[{}:{}]Unexpected token: {}\n", .{ self.peek().?.span.line, self.peek().?.span.pos, self.peek().? });
        unreachable;
    }

    fn primary_construct(self: *@This(), scope: *Node) *Node {
        const kind_name = self.pop().?.data.identifier;
        _ = self.pop(); // pop the {
        const fields = self.primary_construct_fields(scope);

        const result = self.allocator.create(Node) catch unreachable;
        result.* = Node{ .construct = .{
            .kind = SymbolKind{ .unresolved = kind_name },
            .fields = fields,
        } };
        return result;
    }

    fn primary_construct_fields(self: *@This(), scope: *Node) ?[]Field {
        var fields = Stack(Field).create(self.allocator, 2) catch unreachable;

        while (!self.check(.curley_right)) {
            if (self.check(.dot)) {
                _ = self.pop().?;
            }

            if (self.check(.curley_right)) break;

            self.check_err(.identifier, "expected a name for the struct field");
            const field_name = self.pop().?;
            self.consume_kind(.equal, "field name is missing trailing =");
            const field_value = self.expression(scope);
            fields.push(Field{
                .name = field_name.data.identifier,
                .symbol = null,
                .kind = .none,
                .value = field_value,
            }) catch unreachable;

            if (self.check(.comma)) {
                _ = self.pop().?;
            }
        }
        _ = self.pop().?;

        if (fields.count() == 0) {
            fields.destroy();
            return null;
        }

        fields.shrink_to_fit() catch unreachable;

        return fields.as_slice();
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
            const p = self.peek().?;
            std.debug.panic("[{}:{}]: {s} ({?})\n", .{ p.span.line, p.span.pos, err, p.data });
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
        const result = self.peek();
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
