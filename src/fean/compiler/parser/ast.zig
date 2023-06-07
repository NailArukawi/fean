const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const mod = @import("mod.zig");
const Token = mod.Token;
const Span = mod.Span;
const Symbol = @import("../symboltable.zig").Symbol;
const SymbolTable = @import("../symboltable.zig").SymbolTable;
const SymbolKind = @import("../symboltable.zig").SymbolKind;
const KindTable = @import("../kindtable.zig").KindTable;
const Kind = @import("../kindtable.zig").Kind;

pub const AST = struct {
    head: []*Node,
    symbols: ?*SymbolTable,
    kinds: ?*KindTable,
};

pub const Parameter = struct {
    name: []const u8,
    symbol: ?Symbol,
    kind: SymbolKind,
};

pub const Field = struct {
    name: []const u8,
    symbol: ?Symbol,
    kind: ?SymbolKind,
    value: ?*Node,
};

pub const FunctionBody = union {
    name: []const u8,
    body: *Node,
};

pub const FieldOrName = union(enum) {
    resolved: @import("../kindtable.zig").Field,
    unresolved: []const u8,
};

pub const Node = union(enum) {
    scope: struct {
        //span: Span,
        parent: ?*Node,
        statments: ?[]*Node,
        symbols: ?*SymbolTable,
        kinds: ?*KindTable,

        pub fn lookup_symbol(self: *@This(), name: []const u8) ?Symbol {
            if (self.symbols == null) {
                return null;
            }
            return self.symbols.?.lookup(name);
        }

        pub fn install_symbol(self: *@This(), to_insert: *Node, allocator: Allocator) !?Symbol {
            var name: []const u8 = "";

            // todo handle infered typing
            var kind: ?SymbolKind = null;
            var size: usize = 0;

            switch (to_insert.*) {
                .variable => |v| {
                    name = v.name;
                    kind = v.kind;

                    // todo maybe lookup early to resolve builtin types.
                    size = 0;
                },
                .constant => |c| {
                    name = c.name;
                    kind = c.kind;

                    // todo maybe lookup early to resolve builtin types.
                    size = 0;
                },
                else => @panic("Tried to pass non variable/const Node into install_symbol()"),
            }

            if (self.symbols == null) {
                self.symbols = try SymbolTable.create_head(allocator, name, kind, size);
                self.symbols.?.binding = 0;

                self.symbols.?.next = null;

                if (self.parent == null) {
                    self.symbols.?.global = true;
                }

                switch (to_insert.*) {
                    .variable => {
                        to_insert.variable.symbol = self.symbols;
                    },
                    .constant => {
                        to_insert.constant.symbol = self.symbols;
                    },
                    else => unreachable,
                }

                return self.symbols;
            }

            var result = try self.symbols.?.install(allocator, name, kind, size);

            if (self.parent == null) {
                result.global = true;
            }

            switch (to_insert.*) {
                .variable => {
                    to_insert.variable.symbol = result;
                },
                .constant => {
                    to_insert.constant.symbol = result;
                },
                else => unreachable,
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
    },
    structure: struct {
        //span: Span,
        name: []const u8,
        symbol: ?Symbol,
        this: ?Kind,
        fields: ?[]Field,
    },
    construct: struct {
        //span: Span,
        kind: ?SymbolKind,
        fields: ?[]Field,
    },
    variable: struct {
        //span: Span,
        name: []const u8,
        symbol: ?Symbol,
        kind: ?SymbolKind,
        value: ?*Node,
    },
    constant: struct {
        //span: Span,
        name: []const u8,
        symbol: ?Symbol,
        kind: ?SymbolKind,
        value: *Node,
    },
    assignment: struct {
        //span: Span,
        name: []const u8,
        symbol: ?Symbol,
        value: *Node,
    },
    function: struct {
        //span: Span,
        params: []Parameter,
        result: ?SymbolKind,
        body: FunctionBody,
        is_extern: bool,
    },
    statment: struct {
        //span: Span,
        kind: StatmentKind,
        value: ?*Node,
    },
    conditional_if: struct {
        //span: Span,
        condition: *Node,
        if_then: *Node,
        if_else: ?*Node,
    },
    conditional_while: struct {
        //span: Span,
        condition: *Node,
        body: *Node,
    },
    binary_expression: struct {
        //span: Span,
        kind: ?SymbolKind,
        lhs: *Node,
        op: Token,
        rhs: *Node,
    },
    unary_expression: struct {
        //span: Span,
        op: Token,
        value: *Node,
    },
    call: struct {
        //span: Span,
        name: []const u8,
        symbol: ?Symbol,
        arguments: ?[]*Node,
    },
    get: struct {
        //span: Span,
        field: FieldOrName,
        kind: ?SymbolKind,
        object: *Node,
    },
    set: struct {
        //span: Span,
        field: FieldOrName,
        kind: ?SymbolKind,
        object: *Node,
        value: *Node,
    },
    object: *Node,
    literal: Token,
};

pub const StatmentKind = enum {
    Expression,
    Return,
    ReturnRoot,
};

fn SymbolKind_helper(kind: ?Kind) ?SymbolKind {
    if (kind == null) {
        return null;
    }
    return SymbolKind{ .resolved = kind.? };
}
