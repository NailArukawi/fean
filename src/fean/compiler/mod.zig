pub const parser = @import("../parser/mod.zig"); // todo remove

// Kindtable
const kindtable = @import("kindtable.zig");
pub const KindTable = kindtable.KindTable;
pub const Kind = kindtable.Kind;
pub const FieldList = kindtable.FieldList;
pub const Field = kindtable.Field;
pub const KIND_UNSET_SIZE = kindtable.UNSET_SIZE;
pub const KIND_FN_SIZE = kindtable.FN_SIZE;
pub const KIND_EXTERN_FN_SIZE = kindtable.EXTERN_FN_SIZE;

// Symboltable
const symboltable = @import("symboltable.zig");
pub const SymbolTable = symboltable.SymbolTable;
pub const Symbol = symboltable.Symbol;
pub const SymbolKind = symboltable.SymbolKind;

// Resolver
const resolver = @import("resolver.zig");
pub const Resolver = resolver.Resolver;

// Address
const address = @import("address.zig");
pub const AddressKind = address.AddressKind;
pub const AddressExtra = address.Extra;
pub const Address = address.Address;
pub const StructAccess = address.StructAccess;
pub const Load = address.Load;
pub const Arithmetic = address.Arithmetic;
pub const Instr = address.Instr;

// Compiler
pub const compiler = @import("compiler.zig");
pub const Compiler = compiler.Compiler;
pub const IR = compiler.IR;
pub const IRBlock = compiler.IRBlock;
pub const CompilerMeta = compiler.CompilerMeta;

// Assembler
pub const assembler = @import("assembler.zig");
pub const Assembler = assembler.Assembler;
pub const AssembledResult = assembler.AssembledResult;
