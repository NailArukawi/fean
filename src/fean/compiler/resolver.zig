const std = @import("std");

const mod = @import("mod.zig");
const Token = mod.parser.Token;
const Span = mod.parser.Span;
const TokenData = mod.parser.TokenData;
const TokenKind = mod.parser.TokenKind;

const Parser = mod.parser.Parser;
const AST = mod.parser.AST;
const Parameter = mod.parser.Parameter;
const Node = mod.parser.Node;

const Symbol = mod.Symbol;
const SymbolTable = mod.SymbolTable;
const SymbolKind = mod.SymbolKind;
const Kind = mod.Kind;
const KindTable = mod.KindTable;
const FieldList = mod.FieldList;
const Field = mod.Field;
const FieldOrName = mod.parser.FieldOrName;

const Stack = @import("../stack.zig").Stack;
const Allocator = std.mem.Allocator;

const KIND_UNSET_SIZE = mod.KIND_UNSET_SIZE;

const Scope = struct {
    parent: ?*@This(),
    symbols: ?*SymbolTable,
    kinds: ?*KindTable,

    pub inline fn into(self: *const @This(), scope: *Node) void {
        scope.scope.kinds = self.kinds;
        scope.scope.symbols = self.symbols;
    }

    pub fn lookup_symbol(self: *@This(), name: []const u8) ?Symbol {
        if (self.symbols == null and self.parent == null) {
            return null;
        } else if (self.symbols == null) {
            return self.parent.?.lookup_symbol(name);
        }

        const result = self.symbols.?.lookup(name);
        if (result == null and self.parent == null) {
            return null;
        } else if (result == null) {
            return self.parent.?.lookup_symbol(name);
        }

        return result;
    }

    pub fn lookup_kind(self: *@This(), name: []const u8) ?Kind {
        if ((self.kinds == null) and (self.parent == null)) {
            return null;
        }
        if (self.kinds == null) {
            return self.parent.?.lookup_kind(name);
        }

        const result = self.kinds.?.lookup(name);
        if (result == null and self.parent == null) {
            return null;
        }
        if (result == null) {
            return self.parent.?.lookup_kind(name);
        }

        return result;
    }

    pub fn install_symbolkind(self: *@This(), name: []const u8, kind: SymbolKind, allocator: Allocator) !Symbol {
        const size = switch (kind) {
            .resolved => |r| r.size,
            .unresolved => KIND_UNSET_SIZE,
        };

        if (self.symbols == null) {
            self.symbols = try SymbolTable.create_head(allocator, name, kind, size);
            return self.symbols.?;
        }
        return self.symbols.?.install(allocator, name, kind, size);
    }

    pub fn install_symbol(self: *@This(), name: []const u8, kind: ?SymbolKind, size: usize, allocator: Allocator) !Symbol {
        if (self.symbols == null) {
            self.symbols = try SymbolTable.create_head(allocator, name, kind, size);
            return self.symbols.?;
        }
        return self.symbols.?.install(allocator, name, kind, size);
    }

    pub fn install_kind(self: *@This(), name: []const u8, fields: ?*FieldList, size: ?usize, allocator: Allocator) !Kind {
        return self.kinds.?.install(name, fields, size, allocator);
    }

    pub fn install_kind_fn(self: *@This(), name: []const u8, fields: ?*FieldList, allocator: Allocator) !Kind {
        if (self.kinds == null) {
            self.kinds = try KindTable.create(allocator);
            var kind: Kind = self.kinds.?;
            kind.fields = fields;
            kind.name = name;
            kind.size = mod.KIND_FN_SIZE;
            kind.meta.is_fn = true;
        }
        return self.kinds.?.install_fn(name, fields, allocator);
    }

    pub fn install_kind_extern_fn(self: *@This(), name: []const u8, fields: ?*FieldList, allocator: Allocator) !Kind {
        return self.kinds.?.install_extern_fn(name, fields, allocator);
    }
};

const TodoStruct = struct {
    scope: *Scope,
    todo: *Node,
    is_complete: bool = false,
};

pub const TYPE_GEN_BUFFER = 256;

pub const Resolver = struct {
    ast: *AST,
    allocator: Allocator,
    buffer: [TYPE_GEN_BUFFER]u8 = [_]u8{0} ** TYPE_GEN_BUFFER,
    structs_todo: Stack(TodoStruct),

    pub fn resolve(ast: *AST, allocator: Allocator) !void {
        var resolver = @This(){
            .ast = ast,
            .allocator = allocator,
            .structs_todo = try Stack(TodoStruct).create(allocator, 16),
        };

        var head = Scope{
            .parent = null,
            .kinds = ast.kinds,
            .symbols = ast.symbols,
        };

        // todo make smarter.. maybe make each node remember itself in a list,
        // so that we can process said list later when we run out of work that is this run solvable
        for (ast.head) |node| try resolver.expand_visit(node, &head);
        try resolver.struct_resolve();
        for (ast.head) |node| _ = try resolver.symbol_visit(node, &head);
        for (ast.head) |node| _ = try resolver.kind_visit(node, &head);
        for (resolver.structs_todo.as_slice()) |*todo|
            todo.is_complete = false;
        try resolver.struct_resolve();
        for (ast.head) |node| _ = try resolver.kind_visit(node, &head);
        for (ast.head) |node| _ = try resolver.symbol_visit(node, &head);

        ast.kinds = head.kinds;
        ast.symbols = head.symbols;
    }

    fn expand_visit(self: *@This(), node: *Node, scope: *Scope) !void {
        switch (node.*) {
            .scope => |*s| {
                if (s.statments == null)
                    return;

                var head = Scope{
                    .parent = scope,
                    .kinds = s.kinds,
                    .symbols = s.symbols,
                };

                for (s.statments.?) |scope_node| {
                    try self.expand_visit(scope_node, &head);
                }

                head.into(node);
            },
            .impl => |*i| {
                var head = Scope{
                    .parent = scope,
                    .kinds = i.body.scope.kinds,
                    .symbols = i.body.scope.symbols,
                };

                for (i.body.scope.statments.?) |n| {
                    try impl_dive(node, n, &head, self.allocator);
                    _ = try self.kind_visit(n, &head);
                }

                head.into(i.body);
            },
            .structure => |*s| {
                if (scope.lookup_kind(s.name) != null)
                    return;

                for (s.fields.?, 0..) |field, i| {
                    if (field.kind.? == .unresolved) {
                        const kind_result = scope.lookup_kind(field.kind.?.unresolved);
                        if (kind_result != null) {
                            s.fields.?[i].kind = SymbolKind{ .resolved = kind_result.? };
                        }
                    }
                }

                const this = try scope.install_kind(s.name, null, null, self.allocator);
                s.this = this;

                try self.structs_todo.push(TodoStruct{ .todo = node, .scope = scope });
            },
            .construct => |c| {
                _ = c;
            },
            .variable => |v| {
                if (v.value != null) try self.expand_visit(v.value.?, scope);
            },
            .constant => |c| {
                try self.expand_visit(c.value, scope);
            },
            .assignment => |a| {
                try self.expand_visit(a.value, scope);
            },
            .statment => |v| {
                if (v.value != null) {
                    try self.expand_visit(v.value.?, scope);
                }
            },
            .function => |func| {
                // generate params in function
                if (!func.is_extern and func.params.len > 0) {
                    // tmp scope
                    var fn_symbol = Scope{
                        .parent = scope,
                        .kinds = func.body.body.scope.kinds,
                        .symbols = func.body.body.scope.symbols,
                    };

                    //expand function params
                    for (func.params, 1..) |param, i| {
                        // todo maybe dupe before cleanup
                        // todo size
                        var kind: SymbolKind = undefined;
                        if (param.kind == .unresolved) {
                            const found = scope.lookup_kind(param.kind.unresolved) orelse {
                                kind = param.kind;
                                break;
                            };
                            kind = SymbolKind{ .resolved = found };
                        } else {
                            kind = param.kind;
                        }
                        var param_symbol = try fn_symbol.install_symbol(param.name, kind, 42069, self.allocator);
                        param_symbol.*.param = true;
                        param_symbol.*.binding = @intCast(u10, i);
                    }

                    fn_symbol.into(func.body.body);
                }

                if (func.is_extern) {
                    //
                } else {
                    try self.expand_visit(func.body.body, scope);
                }
            },
            .conditional_if => |cif| {
                try self.expand_visit(cif.condition, scope);

                // todo span maybe?
                var inverse_condition = try self.allocator.create(Node);
                inverse_condition.* = Node{ .unary_expression = .{
                    .op = Token.new_symbol(.bang, Span.default()),
                    .value = cif.condition,
                } };

                node.*.conditional_if.condition = inverse_condition;
                try self.expand_visit(cif.if_then, scope);
                if (cif.if_else != null) {
                    try self.expand_visit(cif.if_else.?, scope);
                }
            },
            .conditional_while => |cw| {
                try self.expand_visit(cw.condition, scope);

                // todo span maybe?
                var inverse_condition = try self.allocator.create(Node);
                inverse_condition.* = Node{ .unary_expression = .{
                    .op = Token.new_symbol(.bang, Span.default()),
                    .value = cw.condition,
                } };

                node.*.conditional_while.condition = inverse_condition;
                try self.expand_visit(cw.body, scope);
            },
            .binary_expression => |b| {
                try self.expand_visit(b.lhs, scope);
                try self.expand_visit(b.rhs, scope);
            },
            .unary_expression => |u| {
                try self.expand_visit(u.value, scope);
            },
            .call => {
                // todo
                return;
            },
            .get => {
                // todo
                return;
            },
            .set => {
                // todo
                return;
            },
            .object => {
                return;
            },
            .literal => {
                return;
            },
        }
    }

    fn kind_visit(self: *@This(), node: *Node, scope: *Scope) !?SymbolKind {
        switch (node.*) {
            .scope => |s| {
                if (s.statments == null)
                    return null;

                var head = Scope{
                    .parent = scope,
                    .kinds = s.kinds,
                    .symbols = s.symbols,
                };

                for (s.statments.?) |scope_node|
                    _ = try self.kind_visit(scope_node, &head);

                head.into(node);

                // todo maybe allow a scope to be a kind
                return null;
            },
            .impl => |*i| {
                switch (i.this.?) {
                    .unresolved => |n| i.this = SymbolKind{ .resolved = scope.lookup_kind(n).? },
                    .resolved => {},
                }

                _ = try self.kind_visit(i.body, scope);

                return null;
            },
            .structure => {
                return null;
            },
            .construct => |*c| {
                if (c.kind.? == .unresolved) {
                    const kind_lookup = scope.lookup_kind(c.kind.?.unresolved);
                    if (kind_lookup != null) {
                        c.kind = .{ .resolved = kind_lookup.? };
                    }
                }

                if (c.fields != null) {
                    for (c.fields.?) |*field| {
                        const field_kind = try self.kind_visit(field.value.?, scope);
                        field.kind = field_kind;
                    }
                }

                return c.kind.?;
            },
            .variable => |v| {
                var symbol = (scope.lookup_symbol(v.name) orelse return null);

                if (v.kind == null) {
                    const infered_kind = (try self.kind_visit(v.value.?, scope)).?;
                    node.variable.kind = infered_kind;
                }

                var kind_name: []const u8 = "";
                switch (node.variable.kind.?) {
                    .resolved => |k| kind_name = k.name,
                    .unresolved => |n| kind_name = n,
                }

                if (std.mem.eql(u8, "Fn", kind_name) or std.mem.eql(u8, "Fn", kind_name)) {
                    symbol.kind = (try self.kind_visit(v.value.?, scope)).?;
                }

                if (v.value != null) {
                    _ = try self.kind_visit(v.value.?, scope);
                }

                // todo inferense
                return null;
            },
            .constant => |c| {
                var symbol = (scope.lookup_symbol(c.name) orelse return null);

                if (c.kind == null) {
                    const infered_kind = (try self.kind_visit(c.value, scope)).?;
                    node.constant.kind = infered_kind;
                }

                var kind_name: []const u8 = "";
                switch (node.constant.kind.?) {
                    .resolved => |k| kind_name = k.name,
                    .unresolved => |n| kind_name = n,
                }

                if (std.mem.eql(u8, "Fn", kind_name) or std.mem.eql(u8, "Fn", kind_name)) {
                    symbol.kind = (try self.kind_visit(c.value, scope)).?;
                }

                // todo inferense
                return null;
            },
            .assignment => |a| {
                return self.kind_visit(a.value, scope);
            },
            .statment => |v| {
                if (v.value != null) {
                    return self.kind_visit(v.value.?, scope);
                }

                return null;
            },
            .function => {
                var function = &node.function;
                const is_extern = function.is_extern;

                // resolve kinds in body
                if (!is_extern)
                    _ = try self.kind_visit(function.body.body, scope);

                // resolve fn kind name
                var kind_name: []u8 = &self.buffer;
                var cursor: usize = 0;

                if (is_extern) {
                    cursor = (try std.fmt.bufPrint(&self.buffer, "ExternFn(", .{})).len;
                } else {
                    cursor = (try std.fmt.bufPrint(&self.buffer, "Fn(", .{})).len;
                }

                if (function.params.len > 0) {
                    for (function.params, 1..) |param, i| {
                        var buffer_cursor = self.buffer[cursor..];
                        const is_end = function.params.len == i;

                        var param_kind_name: []const u8 = "";
                        switch (param.kind) {
                            .resolved => |k| param_kind_name = k.name,
                            .unresolved => |n| param_kind_name = n,
                        }

                        if (is_end) {
                            const written = try std.fmt.bufPrint(buffer_cursor, "{s}) -> ", .{param_kind_name});
                            cursor += written.len;
                        } else {
                            const written = try std.fmt.bufPrint(buffer_cursor, "{s}, ", .{param_kind_name});
                            cursor += written.len;
                        }
                    }
                } else {
                    var buffer_cursor = self.buffer[cursor..];
                    const written = try std.fmt.bufPrint(buffer_cursor, ") -> ", .{});
                    cursor += written.len;
                }

                var is_void = false;
                if (function.result == null) {
                    var buffer_cursor = self.buffer[cursor..];
                    const written = try std.fmt.bufPrint(buffer_cursor, "void", .{});
                    cursor += written.len;

                    is_void = true;
                } else {
                    var result_kind_name: []const u8 = "";
                    switch (function.result.?) {
                        .resolved => |k| result_kind_name = k.name,
                        .unresolved => |n| result_kind_name = n,
                    }

                    var buffer_cursor = self.buffer[cursor..];
                    const written = try std.fmt.bufPrint(buffer_cursor, "{s}", .{result_kind_name});
                    cursor += written.len;

                    if (std.mem.eql(u8, "void", result_kind_name)) {
                        is_void = true;
                    }
                }

                // result of the above formatting
                const result = kind_name[0..cursor];

                // check for dupe
                var result_kind = scope.lookup_kind(result);
                if (result_kind == null) {
                    var return_kind: Kind = undefined;
                    if (is_void) {
                        return_kind = scope.lookup_kind("void").?;
                    } else {
                        var return_kind_name: []const u8 = "";
                        switch (function.result.?) {
                            .resolved => |k| return_kind_name = k.name,
                            .unresolved => |n| return_kind_name = n,
                        }

                        return_kind = scope.lookup_kind(return_kind_name).?;
                    }

                    const new_kind = try install_fn_kind(self, scope, result, function.params, return_kind, is_extern);
                    return SymbolKind{ .resolved = new_kind };
                } else return SymbolKind{ .resolved = result_kind.? };
            },
            .conditional_if => |cif| {
                _ = try self.kind_visit(cif.condition, scope);
                _ = try self.kind_visit(cif.if_then, scope);
                if (cif.if_else != null)
                    _ = try self.kind_visit(cif.if_else.?, scope);

                return null;
            },
            .conditional_while => |cw| {
                _ = try self.kind_visit(cw.condition, scope);
                _ = try self.kind_visit(cw.body, scope);
                return null;
            },
            .binary_expression => |b| {
                const l_kind = unpack(b.lhs, try self.kind_visit(b.lhs, scope));
                const r_kind = unpack(b.lhs, try self.kind_visit(b.rhs, scope));

                // todo actually make work lol
                if (l_kind != null) {
                    node.binary_expression.kind = l_kind;
                } else {
                    node.binary_expression.kind = r_kind;
                }

                return l_kind;
            },
            .unary_expression => |u| {
                const kind = try self.kind_visit(u.value, scope);
                return kind;
            },
            .call => |c| {
                if (c.arguments != null) {
                    for (c.arguments.?) |arg| {
                        _ = try self.kind_visit(arg, scope);
                    }
                }

                if (c.symbol != null) {
                    // TODO make sure this is correct
                    return SymbolKind{ .resolved = c.symbol.?.kind.?.resolved.fields.?.kind };
                }

                return null;
            },
            .get => |*g| {
                const accessing = (try self.kind_visit(g.object, scope)) orelse return null;
                if (g.kind == null)
                    g.kind = accessing;
                if (g.field == .unresolved and accessing == .resolved) {
                    g.field = FieldOrName{ .resolved = accessing.resolved.lookup_field(g.field.unresolved) orelse return null };
                }

                return g.kind;
            },
            .set => |*s| {
                const accessing = (try self.kind_visit(s.object, scope)).?;
                if (s.kind == null)
                    s.kind = accessing;
                if (s.field == .unresolved and accessing == .resolved)
                    s.field = FieldOrName{ .resolved = accessing.resolved.lookup_field(s.field.unresolved) orelse return null };

                _ = try self.kind_visit(s.value, scope);

                return s.kind;
            },
            .object => |o| {
                _ = o;
                unreachable;
            },
            .literal => |l| {
                switch (l.data) {
                    .integer => {
                        // todo make it smarter about typing, cache
                        return SymbolKind{ .resolved = scope.lookup_kind("i64").? };
                    },
                    .decimal => {
                        // todo make it smarter about typing, cache
                        return SymbolKind{ .resolved = scope.lookup_kind("f64").? };
                    },
                    .text => unreachable,
                    .identifier => |name| {
                        // todo make it smarter about typing
                        var symbol = scope.lookup_symbol(name) orelse return null;

                        if (symbol.kind != null and symbol.kind.? == .unresolved) {
                            const symbol_resolved_kind = scope.lookup_kind(symbol.kind.?.unresolved) orelse return symbol.kind;
                            symbol.kind = SymbolKind{ .resolved = symbol_resolved_kind };
                        }

                        return symbol.kind;
                    },
                    .keyword => |kw| {
                        switch (kw) {
                            .True => return SymbolKind{ .resolved = scope.lookup_kind("bool").? },
                            .False => return SymbolKind{ .resolved = scope.lookup_kind("bool").? },
                            else => {
                                std.debug.print("[resolver.kind.literal.keyword]: ({s}) not implimented.\n", .{@tagName(l.data.keyword)});
                                unreachable;
                            },
                        }
                    },
                    else => {
                        std.debug.print("[resolver.kind.literal]: ({s}) not implimented.\n", .{@tagName(l.data)});
                        unreachable;
                    },
                }
            },
        }
    }

    fn symbol_visit(self: *@This(), node: *Node, scope: *Scope) !void {
        switch (node.*) {
            .scope => |s| {
                var head = Scope{
                    .parent = scope,
                    .kinds = s.kinds,
                    .symbols = s.symbols,
                };

                for (s.statments.?) |scope_node|
                    try self.symbol_visit(scope_node, &head);

                head.into(node);
            },
            .impl => |*i| {
                try self.symbol_visit(i.body, scope);
            },
            .structure => |s| {
                _ = s;
            },
            .construct => |c| {
                _ = c;
            },
            .variable => |*v| {
                if (v.symbol == null)
                    v.symbol = scope.lookup_symbol(v.name) orelse try scope.install_symbol(v.name, v.kind, 42096, self.allocator);

                if (v.value) |value|
                    try self.symbol_visit(value, scope);
            },
            .constant => |*c| {
                if (c.symbol == null)
                    c.symbol = scope.lookup_symbol(c.name) orelse try scope.install_symbol(c.name, c.kind, 42096, self.allocator);

                try self.symbol_visit(c.value, scope);
            },
            .assignment => |a| {
                try self.symbol_visit(a.value, scope);
            },
            .statment => |v| {
                if (v.value != null) {
                    try self.symbol_visit(v.value.?, scope);
                }
            },
            .function => |func| {
                if (!func.is_extern) {
                    var func_scope = Scope{
                        .parent = scope,
                        .kinds = func.body.body.scope.kinds,
                        .symbols = func.body.body.scope.symbols,
                    };

                    for (func.params) |*param|
                        param.*.symbol = try func_scope.install_symbol(param.name, param.kind, 42069, self.allocator);

                    try self.symbol_visit(func.body.body, scope);

                    func_scope.into(func.body.body);
                }
            },
            .conditional_if => |cif| {
                try self.symbol_visit(cif.condition, scope);
                try self.symbol_visit(cif.if_then, scope);
                if (cif.if_else != null) {
                    try self.symbol_visit(cif.if_else.?, scope);
                }
            },
            .conditional_while => |cw| {
                try self.symbol_visit(cw.condition, scope);
                try self.symbol_visit(cw.body, scope);
            },
            .binary_expression => |b| {
                try self.symbol_visit(b.lhs, scope);
                try self.symbol_visit(b.rhs, scope);
            },
            .unary_expression => |u| {
                try self.symbol_visit(u.value, scope);
            },
            .call => {
                // todo
                return;
            },
            .get => |*g| {
                try self.symbol_visit(g.object, scope);
            },
            .set => {
                // todo
                return;
            },
            .object => {
                return;
            },
            .literal => {
                return;
            },
        }
    }

    // a workaround for possible mixed struct dependencies.
    // todo could maybe be solved better
    fn struct_resolve(self: *@This()) !void {
        if (self.structs_todo.count() == 0) return;
        var all_done: bool = false;
        var work_done: bool = true;
        while (!all_done and work_done) {
            all_done = true;
            work_done = false;
            for (self.structs_todo.as_slice()) |*todo| {
                if (todo.is_complete) continue;
                all_done = false;

                if (process_struct(todo)) work_done = true;
            }
        }

        for (self.structs_todo.as_slice()) |*todo|
            todo.is_complete = false;

        all_done = false;
        work_done = true;
        while (!all_done and work_done) {
            all_done = true;
            work_done = false;
            for (self.structs_todo.as_slice()) |*todo| {
                if (todo.is_complete) continue;
                all_done = false;

                if (try process_struct_fields(self.allocator, todo)) work_done = true;
            }
        }

        //for (self.structs_todo.as_slice()) |*todo|
        //    todo.is_complete = false;
        //
        //all_done = false;
        //work_done = true;
        //while (!all_done and work_done) {
        //    all_done = true;
        //    work_done = false;
        //    for (self.structs_todo.as_slice()) |*todo| {
        //        if (todo.is_complete) continue;
        //        all_done = false;
        //
        //        if (process_struct_size(todo)) work_done = true;
        //    }
        //}
    }
};

fn process_struct(todo: *TodoStruct) bool {
    var work_done: bool = false;
    var structure = &todo.todo.structure;
    for (structure.fields.?, 0..) |field, i| {
        if (field.kind.? == .unresolved) //continue here was an error idk why.
        {
            const unresolved = field.kind.?.unresolved;
            const kind = todo.scope.lookup_kind(unresolved);
            if (kind == null) continue;
            work_done = true;
            structure.fields.?[i].kind = SymbolKind{ .resolved = kind.? };
        }
    }

    return work_done;
}

fn process_struct_fields(allocator: Allocator, todo: *TodoStruct) !bool {
    var structure = &todo.todo.structure;
    if (structure.this.?.fields != null)
        return false;

    var largest: usize = 0;
    for (structure.fields.?) |field| {
        if (field.kind.?.resolved.size == std.math.maxInt(usize)) return false;
        if (field.kind.?.resolved.size > largest) largest = field.kind.?.resolved.size;
    }

    var alignment: u2 = 0;
    if (largest == 2) alignment = 1;
    if (largest == 3 or largest == 4) alignment = 2;
    if (largest > 4) alignment = 3;

    var actual_alignment: u8 = 1;
    if (largest == 2) actual_alignment = 2;
    if (largest == 3 or largest == 4) actual_alignment = 4;
    if (largest > 4) actual_alignment = 8;

    var size: usize = 0;
    var fields: ?*FieldList = null;
    for (structure.fields.?) |field| {
        if (field.kind.? == .resolved and field.kind.?.resolved.is_struct) {
            const closest = closest_aligned(size, actual_alignment);
            const padding = closest - size;
            const shifted_padding = @shlWithOverflow(padding, 1)[0];

            var field_kind = field.kind.?.resolved;
            var result: Field = undefined;
            if (fields == null) {
                fields = try FieldList.create(field.name, field_kind, allocator);
                fields.?.*.padding = @truncate(u2, shifted_padding);
                fields.?.*.alignment = alignment;
            } else {
                result = try fields.?.install(field.name, field_kind, allocator);
                result.*.padding = @truncate(u2, shifted_padding);
                result.*.alignment = alignment;
            }

            size += (8 + padding);
        } else if (actual_alignment == 1) {
            const field_kind = switch (field.kind orelse return false) {
                .resolved => |r| r,
                .unresolved => return false,
            };

            size += field_kind.size;

            var result: Field = undefined;
            if (fields == null) {
                fields = try FieldList.create(field.name, field_kind, allocator);
                fields.?.*.padding = 0;
                fields.?.*.alignment = 1;
            } else {
                result = try fields.?.install(field.name, field_kind, allocator);
                result.*.padding = 0;
                result.*.alignment = 1;
            }
        } else {
            const closest = closest_aligned(size, actual_alignment);
            const padding = closest - size;
            const shifted_padding = @shlWithOverflow(padding, 1)[0];
            const field_kind = switch (field.kind orelse return false) {
                .resolved => |r| r,
                .unresolved => return false,
            };

            var result: Field = undefined;
            if (fields == null) {
                fields = try FieldList.create(field.name, field_kind, allocator);
                fields.?.*.padding = @truncate(u2, shifted_padding);
                fields.?.*.alignment = alignment;
            } else {
                result = try fields.?.install(field.name, field_kind, allocator);
                result.*.padding = @truncate(u2, shifted_padding);
                result.*.alignment = alignment;
            }

            size += (field_kind.size + padding);
        }
    }

    todo.is_complete = true;
    structure.this.?.size = size + 8;
    structure.this.?.fields = fields;
    allocator.free(structure.fields.?);

    return true;
}

//fn process_struct_size(todo: *TodoStruct) bool {
//    var fields_sized: bool = true;
//    const fields = todo.todo.structure.fields.?;
//    for (fields) |field| {
//        if (!field.kind.?.resolved.is_size_set()) fields_sized = false;
//    }
//
//    if (!fields_sized) return false;
//
//    var size: usize = 0;
//
//    for (fields) |field| {
//        const kind = field.kind.?.resolved;
//        size += kind.size;
//    }
//
//    todo.todo.structure.this.?.size = size;
//    todo.is_complete = true;
//
//    // TODO IMPORTANT alignment!
//
//    return true;
//}

fn closest_aligned(starting: usize, alignment: u8) usize {
    var closest: usize = 0;
    while (closest < starting) {
        closest += alignment;
    }
    return closest;
}

fn install_fn_kind(self: *Resolver, scope: *Scope, name: []const u8, params: []Parameter, result: Kind, is_extern: bool) !Kind {
    const allocator = self.allocator;
    var cloned_name = try allocator.alloc(u8, name.len);
    @memcpy(cloned_name, name);

    var kind: Kind = switch (is_extern) {
        true => try scope.install_kind_extern_fn(cloned_name, null, allocator),
        false => try scope.install_kind_fn(cloned_name, null, allocator),
    };

    _ = try kind.install_field("return", result, allocator);
    for (params) |param| {
        var param_kind: Kind = undefined;
        switch (param.kind) {
            .resolved => |k| param_kind = k,
            .unresolved => |n| {
                param_kind = scope.lookup_kind(n) orelse std.debug.panic("ree: {s}", .{n});
            },
        }

        _ = try kind.install_field("", param_kind, allocator);
    }

    return kind;
}

inline fn unpack(node: *Node, kind: ?SymbolKind) ?SymbolKind {
    if (kind == null)
        return null;

    if (node.* == .get) {
        var cursor = node;
        while (cursor.get.object.* == .get) {
            cursor = cursor.get.object;
        }

        const field = cursor.get.field;
        switch (field) {
            .resolved => |r| {
                return SymbolKind{ .resolved = r.kind };
            },
            .unresolved => {
                return kind;
            },
        }
    }

    return kind;
}

fn impl_dive(impl_node: *Node, node: *Node, scope: *Scope, allocator: Allocator) !void {
    var impl = impl_node.impl;
    switch (node.*) {
        .function => |*f| {
            var is_self = switch (f.params[0].kind) {
                .unresolved => |name| std.mem.eql(u8, name, "Self"),
                .resolved => false,
            };

            if (is_self) {
                var head = Scope{
                    .parent = scope,
                    .kinds = f.body.body.scope.kinds,
                    .symbols = f.body.body.scope.symbols,
                };

                var self_symbol = head.lookup_symbol("self");
                if (self_symbol == null) {
                    self_symbol = try head.install_symbolkind("self", impl_node.impl.this.?, allocator);
                }

                f.params[0].symbol = self_symbol;

                if (f.params[0].kind == .unresolved) {
                    switch (impl.this.?) {
                        .unresolved => |u| {
                            const self = head.lookup_kind(u);
                            if (self) |self_sym| {
                                f.params[0].kind = SymbolKind{ .resolved = self_sym };
                            } else {
                                f.params[0].kind = impl.this.?;
                            }
                        },
                        .resolved => |r| f.params[0].kind = SymbolKind{ .resolved = r },
                    }
                }

                head.into(f.body.body);
            }
        },
        .constant => |c| {
            try impl_dive(impl_node, c.value, scope, allocator);
        },
        .variable => |v| {
            var value = v.value orelse return;
            try impl_dive(impl_node, value, scope, allocator);
        },
        else => |n| std.debug.print("impl_dive: {s}\n", .{@tagName(n)}),
    }
}
