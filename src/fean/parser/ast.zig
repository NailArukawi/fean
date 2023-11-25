const std = @import("std");
const List = std.ArrayList;
const Allocator = std.mem.Allocator;

const mod = @import("mod.zig");
const Token = mod.Token;
const Span = mod.Span;
const Symbol = @import("../compiler/symboltable.zig").Symbol;
const SymbolTable = @import("../compiler/symboltable.zig").SymbolTable;
const SymbolKind = @import("../compiler/symboltable.zig").SymbolKind;
const KindTable = @import("../compiler/kindtable.zig").KindTable;
const Kind = @import("../compiler/kindtable.zig").Kind;

pub const AST = struct {
    head: []*Node,
    symbols: *SymbolTable,
    kinds: *KindTable,
};

pub const Parameter = struct {
    name: []const u8,
    symbol: ?*Symbol,
    kind: SymbolKind,
};

pub const Field = struct {
    name: []const u8,
    symbol: ?*Symbol,
    kind: SymbolKind,
    value: ?*Node,
};

pub const FunctionBody = union {
    name: []const u8,
    body: *Node,
};

pub const FieldOrName = union(enum) {
    resolved: *@import("../compiler/kindtable.zig").Field,
    unresolved: []const u8,
};

pub const Node = union(enum) {
    scope: struct {
        //span: Span,
        parent: ?*Node,
        statments: ?[]*Node,
        symbols: *SymbolTable,
        kinds: *KindTable,

        pub fn create(allocator: Allocator) !*@This() {
            var this = try allocator.create(@This());
            this = .{};
            this.symbols = try SymbolKind.create(allocator);
            this.kinds = try KindTable.create(allocator);
            return this;
        }

        pub fn lookupSymbol(this: *@This(), name: []const u8) ?*Symbol {
            return this.symbols.lookup(name);
        }

        pub fn installSymbol(this: *@This(), insertee: *Node, allocator: Allocator) !?*Symbol {
            var name: []const u8 = "";

            // todo handle infered typing
            var kind: SymbolKind = .none;
            var size: usize = 0;

            switch (insertee.*) {
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

            var result = try this.symbols.install(allocator, name, kind, size);

            if (this.parent == null)
                result.global = true;

            switch (insertee.*) {
                .variable => {
                    insertee.variable.symbol = result;
                },
                .constant => {
                    insertee.constant.symbol = result;
                },
                else => unreachable,
            }

            return result;
        }

        pub fn lookupKind(this: *@This(), name: []const u8) ?*Kind {
            const result = this.kinds.lookup(name);
            if (result != null)
                return result;

            if (this.parent == null or this.parent != .scope)
                return null;

            return this.parent.?.scope.lookupKind(name);
        }
    },
    structure: struct {
        //span: Span,
        name: []const u8,
        symbol: ?*Symbol,
        this: ?*Kind,
        fields: ?[]Field,
    },
    impl: struct {
        //span: Span,
        this: SymbolKind,
        body: *Node,
    },
    construct: struct {
        //span: Span,
        kind: SymbolKind,
        fields: ?[]Field,
    },
    variable: struct {
        //span: Span,
        name: []const u8,
        symbol: ?*Symbol,
        kind: SymbolKind,
        value: ?*Node,
    },
    constant: struct {
        //span: Span,
        name: []const u8,
        symbol: ?*Symbol,
        kind: SymbolKind,
        value: *Node,
    },
    assignment: struct {
        //span: Span,
        name: []const u8,
        symbol: ?*Symbol,
        value: *Node,
    },
    method: struct {
        //span: Span,
        name: []const u8,
        params: []Parameter,
        result: SymbolKind,
        body: FunctionBody,
        is_extern: bool,
    },
    function: struct {
        //span: Span,
        params: []Parameter,
        result: SymbolKind,
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
        kind: SymbolKind,
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
        //callee: *Node,
        name: []const u8,
        symbol: ?*Symbol,
        arguments: ?[]*Node,
    },
    get: struct {
        //span: Span,
        field: FieldOrName,
        kind: SymbolKind,
        object: *Node,
    },
    set: struct {
        //span: Span,
        field: FieldOrName,
        kind: SymbolKind,
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
    return SymbolKind{ .resolved = kind orelse return null };
}
