pub const Span = packed struct {
    line: u32,
    pos: u32,
    len: u32,
    file_id: u32,

    pub inline fn newline(self: *@This()) void {
        self.line += 1;
        self.pos = 0;
    }

    pub inline fn reset(self: *@This()) void {
        self.pos += self.len;
        self.len = 0;
    }

    pub inline fn default() @This() {
        return @This(){
            .line = 1,
            .pos = 1,
            .len = 0,
            .file_id = 0,
        };
    }
};

// Token
const token = @import("token.zig");
pub const Token = token.Token;
pub const TokenKind = token.TokenKind;
pub const TokenData = token.TokenData;
pub const Symbol = token.Symbol;
pub const Keyword = token.Keyword;

// Scanner
const scanner = @import("scanner.zig");
pub const Scanner = scanner.Scanner;

// Ast
const ast = @import("ast.zig");
pub const AST = ast.AST;
pub const Node = ast.Node;
pub const Parameter = ast.Parameter;
pub const Field = ast.Field;
pub const FieldOrName = ast.FieldOrName;
pub const FunctionBody = ast.FunctionBody;
pub const StatmentKind = ast.StatmentKind;

// Parser
const parser = @import("parser.zig");
pub const Parser = parser.Parser;
