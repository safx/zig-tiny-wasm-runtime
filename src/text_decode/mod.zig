const std = @import("std");
const wasm_core = @import("wasm-core");

// Re-export from submodules
const lexer_mod = @import("./lexer.zig");
const parser_mod = @import("./parser.zig");

// Re-export lexer types
pub const TextDecodeError = lexer_mod.TextDecodeError;
pub const Token = lexer_mod.Token;
pub const Lexer = lexer_mod.Lexer;

// Re-export parser types
pub const ModuleBuilder = parser_mod.ModuleBuilder;
pub const Parser = parser_mod.Parser;

// Re-export parser functions
pub const parseWastModule = parser_mod.parseWastModule;
pub const parseWastScript = parser_mod.parseWastScript;
pub const freeCommand = parser_mod.freeCommand;
