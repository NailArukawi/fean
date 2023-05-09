const std = @import("std");

const Symbol = @import("symboltable.zig").Symbol;
const SymbolTable = @import("symboltable.zig").SymbolTable;
const SymbolKind = @import("symboltable.zig").SymbolKind;
const Kind = @import("kindtable.zig").Kind;
const KindTable = @import("kindtable.zig").KindTable;
const Parser = @import("parser.zig").Parser;
const AST = @import("parser.zig").AST;
const Node = @import("parser.zig").Node;

const Allocator = std.mem.Allocator;

const Scope = struct {
    parent: ?*@This(),
    symbols: ?*SymbolTable,
    kinds: ?*KindTable,

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
};

pub const Resolver = struct {
    ast: *AST,
    allocator: Allocator,

    pub fn resolve(ast: *AST, allocator: Allocator) !void {
        var resolver = @This(){
            .ast = ast,
            .allocator = allocator,
        };

        var head = Scope{
            .parent = null,
            .kinds = ast.kinds,
            .symbols = ast.symbols,
        };

        for (ast.head) |node| {
            try resolver.visit(node, &head);
        }
    }

    fn visit(self: *@This(), node: *Node, scope: *Scope) !void {
        _ = try self.kind_visit(node, scope);
        try self.symbol_visit(node, scope);
    }

    fn kind_visit(self: *@This(), node: *Node, scope: *Scope) !?SymbolKind {
        switch (node.*) {
            .scope => |s| {
                if (s.statments == null) {
                    return null;
                }

                var head = Scope{
                    .parent = scope,
                    .kinds = s.kinds,
                    .symbols = s.symbols,
                };

                for (s.statments.?) |scope_node| {
                    _ = try self.kind_visit(scope_node, &head);
                }

                // todo maybe allow a scope to be a kind
                return null;
            },
            .variable => |v| {
                return (scope.lookup_symbol(v.name) orelse return null).kind;
            },
            .constant => |c| {
                _ = c;
                return null;
            },
            .assignment => |a| {
                return self.kind_visit(a.value, scope);
            },
            .statment => |v| {
                return self.kind_visit(v.value, scope);
            },
            .conditional_if => |cif| {
                _ = try self.kind_visit(cif.condition, scope);
                _ = try self.kind_visit(cif.if_then, scope);
                if (cif.if_else != null) {
                    _ = try self.kind_visit(cif.if_else.?, scope);
                }
                return null;
            },
            .conditional_while => |cw| {
                _ = try self.kind_visit(cw.condition, scope);
                _ = try self.kind_visit(cw.body, scope);
                return null;
            },
            .binary_expression => |b| {
                const l_kind = try self.kind_visit(b.lhs, scope);
                const r_kind = try self.kind_visit(b.rhs, scope);

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
                        const symbol = scope.lookup_symbol(name) orelse return null;

                        return symbol.kind;
                    },
                    .keyword => |kw| {
                        switch (kw) {
                            .True => return SymbolKind{ .resolved = scope.lookup_kind("bool").? },
                            else => {
                                std.debug.print("[resolver.kind.literal.keyword]: ({s}) not implimented.\n", .{@tagName(l.data)});
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
                if (s.statments == null) {
                    return;
                }

                var head = Scope{
                    .parent = scope,
                    .kinds = s.kinds,
                    .symbols = s.symbols,
                };

                for (s.statments.?) |scope_node| {
                    try self.symbol_visit(scope_node, &head);
                }
            },
            .variable => |v| {
                switch (v.kind) {
                    .resolved => return,
                    .unresolved => |name| {
                        const kind = scope.lookup_kind(name);

                        if (kind == null) {
                            @panic("Kind not found!");
                        }

                        node.*.variable.kind = SymbolKind{ .resolved = kind.? };

                        // todo better
                        var symbol = node.*.variable.symbol orelse scope.lookup_symbol(name).?;
                        symbol.kind.? = SymbolKind{ .resolved = kind.? };
                    },
                }
            },
            .constant => |c| {
                switch (c.kind) {
                    .resolved => return,
                    .unresolved => |name| {
                        const kind = scope.lookup_kind(name);

                        if (kind == null) {
                            @panic("Kind not found!");
                        }

                        node.*.variable.kind = SymbolKind{ .resolved = kind.? };

                        // todo better
                        var symbol = node.*.constant.symbol orelse scope.lookup_symbol(name).?;
                        symbol.kind.? = SymbolKind{ .resolved = kind.? };
                    },
                }
            },
            .assignment => |a| {
                try self.symbol_visit(a.value, scope);
            },
            .statment => |v| {
                try self.symbol_visit(v.value, scope);
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
            .literal => {
                return;
            },
        }
    }
};
