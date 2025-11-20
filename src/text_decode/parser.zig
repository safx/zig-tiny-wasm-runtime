const std = @import("std");
const wasm_core = @import("wasm-core");
const lexer_mod = @import("./lexer.zig");
pub const wast = @import("./wast.zig");

pub const Lexer = lexer_mod.Lexer;
pub const Token = lexer_mod.Token;
pub const TextDecodeError = lexer_mod.TextDecodeError;

pub const ModuleBuilder = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(wasm_core.types.FuncType),
    funcs: std.ArrayList(wasm_core.types.Func),
    tables: std.ArrayList(wasm_core.types.TableType),
    memories: std.ArrayList(wasm_core.types.MemoryType),
    globals: std.ArrayList(wasm_core.types.Global),
    elements: std.ArrayList(wasm_core.types.Element),
    datas: std.ArrayList(wasm_core.types.Data),
    start: ?wasm_core.types.FuncIdx,
    imports: std.ArrayList(wasm_core.types.Import),
    exports: std.ArrayList(wasm_core.types.Export),

    pub fn init(allocator: std.mem.Allocator) ModuleBuilder {
        return ModuleBuilder{
            .allocator = allocator,
            .types = std.ArrayList(wasm_core.types.FuncType){},
            .funcs = std.ArrayList(wasm_core.types.Func){},
            .tables = std.ArrayList(wasm_core.types.TableType){},
            .memories = std.ArrayList(wasm_core.types.MemoryType){},
            .globals = std.ArrayList(wasm_core.types.Global){},
            .elements = std.ArrayList(wasm_core.types.Element){},
            .datas = std.ArrayList(wasm_core.types.Data){},
            .start = null,
            .imports = std.ArrayList(wasm_core.types.Import){},
            .exports = std.ArrayList(wasm_core.types.Export){},
        };
    }

    pub fn build(self: *ModuleBuilder) !wasm_core.types.Module {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        const arena_allocator = arena.allocator();

        return wasm_core.types.Module{
            .types = try arena_allocator.dupe(wasm_core.types.FuncType, self.types.items),
            .funcs = try arena_allocator.dupe(wasm_core.types.Func, self.funcs.items),
            .tables = try arena_allocator.dupe(wasm_core.types.TableType, self.tables.items),
            .memories = try arena_allocator.dupe(wasm_core.types.MemoryType, self.memories.items),
            .globals = try arena_allocator.dupe(wasm_core.types.Global, self.globals.items),
            .elements = try arena_allocator.dupe(wasm_core.types.Element, self.elements.items),
            .datas = try arena_allocator.dupe(wasm_core.types.Data, self.datas.items),
            .start = self.start,
            .imports = try arena_allocator.dupe(wasm_core.types.Import, self.imports.items),
            .exports = try arena_allocator.dupe(wasm_core.types.Export, self.exports.items),
            .arena = arena,
        };
    }

    pub fn deinit(self: *ModuleBuilder) void {
        self.types.deinit(self.allocator);
        self.funcs.deinit(self.allocator);
        self.tables.deinit(self.allocator);
        self.memories.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        self.elements.deinit(self.allocator);
        self.datas.deinit(self.allocator);
        self.imports.deinit(self.allocator);
        self.exports.deinit(self.allocator);
    }
};

pub const Parser = struct {
    lexer: Lexer,
    current_token: Token,
    allocator: std.mem.Allocator,
    // Name resolution for current function
    local_names: std.StringHashMap(u32),

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Parser {
        var lexer = Lexer.init(input);
        const current_token = try lexer.nextToken();

        return Parser{
            .lexer = lexer,
            .current_token = current_token,
            .allocator = allocator,
            .local_names = std.StringHashMap(u32).init(allocator),
        };
    }

    pub fn advance(self: *Parser) !void {
        self.current_token = try self.lexer.nextToken();
    }

    pub fn peekToken(self: *Parser) !Token {
        return try self.lexer.peekToken();
    }

    pub fn expectToken(self: *Parser, expected: std.meta.Tag(Token)) !void {
        if (std.meta.activeTag(self.current_token) != expected) {
            return TextDecodeError.UnexpectedToken;
        }
        try self.advance();
    }

    pub fn parseModule(self: *Parser) !wasm_core.types.Module {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            return TextDecodeError.InvalidModule;
        }
        try self.advance();

        var builder = ModuleBuilder.init(self.allocator);
        defer builder.deinit();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.parseModuleField(&builder);
        }

        try self.expectToken(.right_paren);
        return try builder.build();
    }

    fn parseModuleField(self: *Parser, builder: *ModuleBuilder) !void {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }

        const field_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, field_name, "memory")) {
            try self.parseMemory(builder);
        } else if (std.mem.eql(u8, field_name, "data")) {
            try self.parseData(builder);
        } else if (std.mem.eql(u8, field_name, "func")) {
            try self.parseFunction(builder);
        } else if (std.mem.eql(u8, field_name, "export")) {
            try self.parseExport(builder);
        } else {
            // Skip unknown fields
            var depth: u32 = 1;
            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                try self.advance();
            }
            return;
        }

        try self.expectToken(.right_paren);
    }

    fn parseMemory(self: *Parser, builder: *ModuleBuilder) !void {
        // Optional memory name (e.g., $mem0)
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance(); // skip the name
        }

        // Skip any inline exports or other nested expressions like (export "mem")
        while (self.current_token == .left_paren) {
            var depth: u32 = 1;
            try self.advance(); // consume '('
            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                if (depth > 0) {
                    try self.advance();
                }
            }
            if (self.current_token == .right_paren) {
                try self.advance(); // consume closing ')'
            }
        }

        // Optional memory type (i32 or i64 for memory64)
        if (self.current_token == .identifier and
            (std.mem.eql(u8, self.current_token.identifier, "i32") or
                std.mem.eql(u8, self.current_token.identifier, "i64")))
        {
            try self.advance(); // skip the memory type
        }

        // Skip any inline data or other nested expressions after memory type
        while (self.current_token == .left_paren) {
            var depth: u32 = 1;
            try self.advance(); // consume '('
            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                if (depth > 0) {
                    try self.advance();
                }
            }
            if (self.current_token == .right_paren) {
                try self.advance(); // consume closing ')'
            }
        }

        // Optional memory size (if not provided, default to 1 page)
        const min_pages: u32 = if (self.current_token == .number) blk: {
            // Parse as u64 first to handle large values, then truncate to u32
            const pages_64 = try std.fmt.parseInt(u64, self.current_token.number, 0);
            const pages: u32 = @intCast(@min(pages_64, std.math.maxInt(u32)));
            try self.advance();
            break :blk pages;
        } else 1;

        var max_pages: ?u32 = null;
        if (self.current_token == .number) {
            // Parse as u64 first to handle large values, then truncate to u32
            const pages_64 = try std.fmt.parseInt(u64, self.current_token.number, 0);
            max_pages = @intCast(@min(pages_64, std.math.maxInt(u32)));
            try self.advance();
        }

        const memory_type = wasm_core.types.MemoryType{
            .limits = wasm_core.types.Limits{
                .min = min_pages,
                .max = max_pages,
            },
        };

        try builder.memories.append(builder.allocator, memory_type);
    }

    fn parseData(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse: (data (i32.const offset) "string")

        // Parse offset expression
        var offset: wasm_core.types.InitExpression = .{ .i32_const = 0 };

        if (self.current_token == .left_paren) {
            try self.advance(); // consume '('
            if (self.current_token == .identifier) {
                const instr_name = self.current_token.identifier;
                try self.advance();

                if (std.mem.eql(u8, instr_name, "i32.const")) {
                    if (self.current_token == .number) {
                        const val = try std.fmt.parseInt(i32, self.current_token.number, 10);
                        offset = .{ .i32_const = val };
                        try self.advance();
                    }
                } else if (std.mem.eql(u8, instr_name, "i64.const")) {
                    if (self.current_token == .number) {
                        const val = try std.fmt.parseInt(i64, self.current_token.number, 10);
                        offset = .{ .i64_const = val };
                        try self.advance();
                    }
                }
            }
            try self.expectRightParen();
        }

        // Parse data string
        var data_bytes: []const u8 = "";
        if (self.current_token == .string) {
            data_bytes = self.current_token.string;
            try self.advance();
        }

        // Create Data structure
        const data = wasm_core.types.Data{
            .init = data_bytes,
            .mode = .{
                .active = .{
                    .mem_idx = 0,
                    .offset = offset,
                },
            },
        };

        try builder.datas.append(builder.allocator, data);
    }

    fn parseFunction(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse function: (func [name] [(export "name")] [(param ...)] [(result ...)] [...body])

        var func_name: ?[]const u8 = null;
        var export_name: ?[]const u8 = null;
        var params: std.ArrayList(wasm_core.types.ValueType) = .{};
        defer params.deinit(self.allocator);
        var results: std.ArrayList(wasm_core.types.ValueType) = .{};
        defer results.deinit(self.allocator);
        var locals: std.ArrayList(wasm_core.types.ValueType) = .{};
        defer locals.deinit(self.allocator);
        var instructions: std.ArrayList(wasm_core.types.Instruction) = .{};
        defer instructions.deinit(self.allocator);

        // Check for function name ($name)
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (id.len > 0 and id[0] == '$') {
                func_name = id;
                try self.advance();
            }
        }

        // Parse function attributes (export, param, result, local)
        while (self.current_token == .left_paren) {
            // Peek to see if next token is an attribute
            const next = try self.peekToken();
            if (next == .identifier) {
                const keyword = next.identifier;
                const is_attribute = std.mem.eql(u8, keyword, "export") or
                    std.mem.eql(u8, keyword, "param") or
                    std.mem.eql(u8, keyword, "result") or
                    std.mem.eql(u8, keyword, "local");
                if (!is_attribute) {
                    // Not an attribute, so it's the function body
                    break;
                }
            } else {
                // Not an identifier, so it's the function body
                break;
            }

            try self.advance(); // consume '('

            if (self.current_token != .identifier) break;

            const keyword = self.current_token.identifier;

            if (std.mem.eql(u8, keyword, "export")) {
                try self.advance(); // consume 'export'
                if (self.current_token == .string) {
                    export_name = self.current_token.string;
                    try self.advance();
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, keyword, "param")) {
                try self.advance(); // consume 'param'
                // Parse parameter types
                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (self.current_token == .identifier) {
                        const type_name = self.current_token.identifier;
                        // Record parameter names ($x)
                        if (type_name.len > 0 and type_name[0] == '$') {
                            const param_idx: u32 = @intCast(params.items.len);
                            try self.local_names.put(type_name, param_idx);
                            try self.advance();
                            continue;
                        }
                        // Parse type
                        if (try self.parseValueType()) |vtype| {
                            try params.append(self.allocator, vtype);
                        }
                    } else {
                        try self.advance();
                    }
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, keyword, "result")) {
                try self.advance(); // consume 'result'
                // Parse result types
                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (try self.parseValueType()) |vtype| {
                        try results.append(self.allocator, vtype);
                    }
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, keyword, "local")) {
                try self.advance(); // consume 'local'
                // Parse local variable types
                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (self.current_token == .identifier) {
                        const type_name = self.current_token.identifier;
                        // Record local names ($x)
                        if (type_name.len > 0 and type_name[0] == '$') {
                            const local_idx: u32 = @intCast(params.items.len + locals.items.len);
                            try self.local_names.put(type_name, local_idx);
                            try self.advance();
                            continue;
                        }
                        // Parse type
                        if (try self.parseValueType()) |vtype| {
                            try locals.append(self.allocator, vtype);
                        }
                    } else {
                        try self.advance();
                    }
                }
                try self.expectRightParen();
            }
        }

        // Parse remaining function body (instructions) - any instructions after attributes
        try self.parseInstructions(&instructions);

        // Add implicit 'end' instruction at the end of function body
        try instructions.append(self.allocator, .end);

        // Create function type
        const func_type_idx = builder.types.items.len;
        const func_type = wasm_core.types.FuncType{
            .parameter_types = try self.allocator.dupe(wasm_core.types.ValueType, params.items),
            .result_types = try self.allocator.dupe(wasm_core.types.ValueType, results.items),
        };
        try builder.types.append(self.allocator, func_type);

        // Create function
        const func = wasm_core.types.Func{
            .type = @intCast(func_type_idx),
            .locals = try self.allocator.dupe(wasm_core.types.ValueType, locals.items),
            .body = try self.allocator.dupe(wasm_core.types.Instruction, instructions.items),
        };
        try builder.funcs.append(self.allocator, func);

        // Add export if present
        if (export_name) |name| {
            const func_idx: u32 = @intCast(builder.funcs.items.len - 1);
            const exp = wasm_core.types.Export{
                .name = name,
                .desc = .{ .function = func_idx },
            };
            try builder.exports.append(self.allocator, exp);
        }

        // TODO: handle named functions (func_name)
        // Named functions would be stored in a map for later reference
        if (func_name) |_| {
            // For now, just ignore the function name
        }

        // Clear local names for next function
        self.local_names.clearRetainingCapacity();
    }

    fn parseExport(self: *Parser, builder: *ModuleBuilder) !void {
        // Simplified export parsing - just skip for now
        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.advance();
        }
        _ = builder;
    }

    /// Parse a value type (i32, i64, f32, f64, v128, funcref, externref)
    fn parseValueType(self: *Parser) !?wasm_core.types.ValueType {
        if (self.current_token != .identifier) return null;

        const type_name = self.current_token.identifier;
        const vtype: wasm_core.types.ValueType = if (std.mem.eql(u8, type_name, "i32"))
            .i32
        else if (std.mem.eql(u8, type_name, "i64"))
            .i64
        else if (std.mem.eql(u8, type_name, "f32"))
            .f32
        else if (std.mem.eql(u8, type_name, "f64"))
            .f64
        else if (std.mem.eql(u8, type_name, "v128"))
            .v128
        else if (std.mem.eql(u8, type_name, "funcref"))
            .func_ref
        else if (std.mem.eql(u8, type_name, "externref"))
            .extern_ref
        else
            return null;

        try self.advance();
        return vtype;
    }

    /// Parse block type: checks for (result ...) and returns appropriate BlockType
    fn parseBlockType(self: *Parser) !wasm_core.types.Instruction.BlockType {
        // Check if next token is '(' followed by 'result'
        if (self.current_token != .left_paren) {
            return .empty;
        }

        // Save lexer state for potential backtrack
        const saved_lexer_pos = self.lexer.pos;
        const saved_lexer_char = self.lexer.current_char;
        const saved_token = self.current_token;

        try self.advance(); // consume '('

        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "result")) {
            try self.advance(); // consume 'result'

            // Parse the result type(s)
            var result_types = std.ArrayList(wasm_core.types.ValueType){};
            defer result_types.deinit(self.allocator);

            while (self.current_token != .right_paren) {
                if (try self.parseValueType()) |vtype| {
                    try result_types.append(self.allocator, vtype);
                } else {
                    break;
                }
            }

            try self.expectRightParen(); // consume ')'

            // Return appropriate BlockType based on result count
            if (result_types.items.len == 0) {
                return .empty;
            } else if (result_types.items.len == 1) {
                return .{ .value_type = result_types.items[0] };
            } else {
                // Multiple results - would need type_index, but for now return first type
                // TODO: properly handle multiple result types with type indices
                return .{ .value_type = result_types.items[0] };
            }
        } else {
            // Not a (result ...), restore lexer state
            self.lexer.pos = saved_lexer_pos;
            self.lexer.current_char = saved_lexer_char;
            self.current_token = saved_token;
            return .empty;
        }
    }

    /// Expect and consume a right paren
    fn expectRightParen(self: *Parser) !void {
        if (self.current_token != .right_paren) {
            return TextDecodeError.UnexpectedToken;
        }
        try self.advance();
    }

    /// Parse instructions in function body
    fn parseInstructions(self: *Parser, instructions: *std.ArrayList(wasm_core.types.Instruction)) !void {
        while (self.current_token != .eof and self.current_token != .right_paren) {
            if (self.current_token == .left_paren) {
                // S-expression instruction like (i32.add) or (i32.load offset=0 ...)
                try self.advance(); // consume '('
                try self.parseSExpression(instructions);
                try self.expectRightParen();
            } else if (self.current_token == .identifier) {
                // Bare instruction like nop or i32.add
                if (try self.parseInstruction()) |instr| {
                    try instructions.append(self.allocator, instr);
                }
            } else {
                // Skip unknown tokens
                try self.advance();
            }
        }
    }

    /// Parse a single instruction
    fn parseInstruction(self: *Parser) !?wasm_core.types.Instruction {
        if (self.current_token != .identifier) return null;

        const instr_name = self.current_token.identifier;
        try self.advance();

        // Special keywords (not instructions)
        if (std.mem.eql(u8, instr_name, "result")) {
            // 'result' is a type declaration, not an instruction - skip it
            return null;
        } else if (std.mem.eql(u8, instr_name, "then")) {
            // 'then' is part of if structure, not a standalone instruction - skip it
            return null;
        }

        // Control instructions
        if (std.mem.eql(u8, instr_name, "nop")) {
            return .nop;
        } else if (std.mem.eql(u8, instr_name, "unreachable")) {
            return .@"unreachable";
        } else if (std.mem.eql(u8, instr_name, "block")) {
            const block_type = try self.parseBlockType();
            return .{ .block = .{ .type = block_type, .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "loop")) {
            const block_type = try self.parseBlockType();
            return .{ .loop = .{ .type = block_type, .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "if")) {
            const block_type = try self.parseBlockType();
            return .{ .@"if" = .{ .type = block_type, .@"else" = null, .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "else")) {
            return .@"else";
        } else if (std.mem.eql(u8, instr_name, "end")) {
            return .end;
        } else if (std.mem.eql(u8, instr_name, "return")) {
            return .@"return";
        }

        // Branch instructions
        else if (std.mem.eql(u8, instr_name, "br")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .br = idx };
        } else if (std.mem.eql(u8, instr_name, "br_if")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .br_if = idx };
        } else if (std.mem.eql(u8, instr_name, "br_table")) {
            // Parse br_table: (br_table label1 label2 ... default)
            var label_idxs = std.ArrayList(u32){};
            defer label_idxs.deinit(self.allocator);

            // Parse all label indices (last one is the default)
            while (self.current_token == .number or self.current_token == .identifier) {
                const idx = try self.parseU32OrIdentifier();
                try label_idxs.append(self.allocator, idx);
            }

            // Last label is the default, others are the table
            if (label_idxs.items.len == 0) {
                return TextDecodeError.InvalidFormat;
            }

            const default_label = label_idxs.items[label_idxs.items.len - 1];
            const table_labels = if (label_idxs.items.len > 1)
                try self.allocator.dupe(u32, label_idxs.items[0 .. label_idxs.items.len - 1])
            else
                &[_]u32{};

            return .{ .br_table = .{
                .label_idxs = table_labels,
                .default_label_idx = default_label,
            } };
        }

        // Parametric instructions
        else if (std.mem.eql(u8, instr_name, "drop")) {
            return .drop;
        } else if (std.mem.eql(u8, instr_name, "select")) {
            return .select;
        }

        // Call instructions
        else if (std.mem.eql(u8, instr_name, "call")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .call = idx };
        } else if (std.mem.eql(u8, instr_name, "call_indirect")) {
            // Parse (type idx) and optional (table idx)
            var type_idx: u32 = 0;
            var table_idx: u32 = 0;

            // Expect (type idx)
            if (self.current_token == .left_paren) {
                try self.advance(); // consume '('
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
                    try self.advance(); // consume 'type'
                    type_idx = try self.parseU32OrIdentifier();
                    try self.expectRightParen();
                } else {
                    return TextDecodeError.UnexpectedToken;
                }
            } else {
                return TextDecodeError.UnexpectedToken;
            }

            // Optional (table idx)
            if (self.current_token == .left_paren) {
                const saved_lexer_pos = self.lexer.pos;
                const saved_lexer_char = self.lexer.current_char;
                const saved_token = self.current_token;

                try self.advance(); // consume '('
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "table")) {
                    try self.advance(); // consume 'table'
                    table_idx = try self.parseU32OrIdentifier();
                    try self.expectRightParen();
                } else {
                    // Not a table declaration, restore state
                    self.lexer.pos = saved_lexer_pos;
                    self.lexer.current_char = saved_lexer_char;
                    self.current_token = saved_token;
                }
            }

            return .{ .call_indirect = .{
                .type_idx = type_idx,
                .table_idx = table_idx,
            } };
        }

        // reference instructions

        // parametric instructions
        else if (std.mem.eql(u8, instr_name, "drop")) {
            return .drop;
        }

        // Variable instructions
        else if (std.mem.eql(u8, instr_name, "local.get")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .local_get = idx };
        } else if (std.mem.eql(u8, instr_name, "local.set")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .local_set = idx };
        } else if (std.mem.eql(u8, instr_name, "local.tee")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .local_tee = idx };
        } else if (std.mem.eql(u8, instr_name, "global.get")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .global_get = idx };
        } else if (std.mem.eql(u8, instr_name, "global.set")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .global_set = idx };
        }
        // table instructions

        // Memory instructions
        else if (std.mem.eql(u8, instr_name, "i32.load")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_load = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load = memarg };
        } else if (std.mem.eql(u8, instr_name, "f32.load")) {
            const memarg = try self.parseMemArg();
            return .{ .f32_load = memarg };
        } else if (std.mem.eql(u8, instr_name, "f64.load")) {
            const memarg = try self.parseMemArg();
            return .{ .f64_load = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.load8_s")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_load8_s = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.load8_u")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_load8_u = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.load16_s")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_load16_s = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.load16_u")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_load16_u = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load8_s")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load8_s = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load8_u")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load8_u = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load16_s")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load16_s = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load16_u")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load16_u = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load32_s")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load32_s = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.load32_u")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_load32_u = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.store")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_store = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.store")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_store = memarg };
        } else if (std.mem.eql(u8, instr_name, "f32.store")) {
            const memarg = try self.parseMemArg();
            return .{ .f32_store = memarg };
        } else if (std.mem.eql(u8, instr_name, "f64.store")) {
            const memarg = try self.parseMemArg();
            return .{ .f64_store = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.store8")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_store8 = memarg };
        } else if (std.mem.eql(u8, instr_name, "i32.store16")) {
            const memarg = try self.parseMemArg();
            return .{ .i32_store16 = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.store8")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_store8 = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.store16")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_store16 = memarg };
        } else if (std.mem.eql(u8, instr_name, "i64.store32")) {
            const memarg = try self.parseMemArg();
            return .{ .i64_store32 = memarg };
        } else if (std.mem.eql(u8, instr_name, "memory.size")) {
            return .memory_size;
        } else if (std.mem.eql(u8, instr_name, "memory.grow")) {
            return .memory_grow;
        } else if (std.mem.eql(u8, instr_name, "memory.init")) {
            const idx = try self.parseU32OrIdentifier();
            return .{ .memory_init = idx };
        } else if (std.mem.eql(u8, instr_name, "memory.copy")) {
            return .memory_copy;
        } else if (std.mem.eql(u8, instr_name, "memory.fill")) {
            return .memory_fill;
        }

        // Numeric const instructions
        else if (std.mem.eql(u8, instr_name, "i32.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.InvalidNumber;
            }
            const number_str = self.current_token.number;

            // Remove underscores
            var buf: [256]u8 = undefined;
            var buf_idx: usize = 0;
            for (number_str) |c| {
                if (c != '_') {
                    if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                    buf[buf_idx] = c;
                    buf_idx += 1;
                }
            }
            const clean_str = buf[0..buf_idx];

            const val: i32 = if (clean_str.len > 0 and clean_str[0] == '-') blk: {
                break :blk try std.fmt.parseInt(i32, clean_str, 0);
            } else blk: {
                const unsigned_value = try std.fmt.parseInt(u32, clean_str, 0);
                break :blk @bitCast(unsigned_value);
            };
            try self.advance();
            return .{ .i32_const = val };
        } else if (std.mem.eql(u8, instr_name, "i64.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.InvalidNumber;
            }
            const number_str = self.current_token.number;

            // Remove underscores
            var buf: [256]u8 = undefined;
            var buf_idx: usize = 0;
            for (number_str) |c| {
                if (c != '_') {
                    if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                    buf[buf_idx] = c;
                    buf_idx += 1;
                }
            }
            const clean_str = buf[0..buf_idx];

            const val: i64 = if (clean_str.len > 0 and clean_str[0] == '-') blk: {
                break :blk try std.fmt.parseInt(i64, clean_str, 0);
            } else blk: {
                const unsigned_value = try std.fmt.parseInt(u64, clean_str, 0);
                break :blk @bitCast(unsigned_value);
            };
            try self.advance();
            return .{ .i64_const = val };
        } else if (std.mem.eql(u8, instr_name, "f32.const")) {
            const val = try self.parseFloat(f32);
            return .{ .f32_const = val };
        } else if (std.mem.eql(u8, instr_name, "f64.const")) {
            const val = try self.parseFloat(f64);
            return .{ .f64_const = val };
        } else if (std.mem.eql(u8, instr_name, "v128.const")) {
            const val = try self.parseInt(i128);
            return .{ .v128_const = val };
        }

        // i32 comparison
        else if (std.mem.eql(u8, instr_name, "i32.eqz")) {
            return .i32_eqz;
        } else if (std.mem.eql(u8, instr_name, "i32.eq")) {
            return .i32_eq;
        } else if (std.mem.eql(u8, instr_name, "i32.ne")) {
            return .i32_ne;
        } else if (std.mem.eql(u8, instr_name, "i32.lt_s")) {
            return .i32_lt_s;
        } else if (std.mem.eql(u8, instr_name, "i32.lt_u")) {
            return .i32_lt_u;
        } else if (std.mem.eql(u8, instr_name, "i32.gt_s")) {
            return .i32_gt_s;
        } else if (std.mem.eql(u8, instr_name, "i32.gt_u")) {
            return .i32_gt_u;
        } else if (std.mem.eql(u8, instr_name, "i32.le_s")) {
            return .i32_le_s;
        } else if (std.mem.eql(u8, instr_name, "i32.le_u")) {
            return .i32_le_u;
        } else if (std.mem.eql(u8, instr_name, "i32.ge_s")) {
            return .i32_ge_s;
        } else if (std.mem.eql(u8, instr_name, "i32.ge_u")) {
            return .i32_ge_u;
        }

        // i64 comparison
        else if (std.mem.eql(u8, instr_name, "i64.eqz")) {
            return .i64_eqz;
        } else if (std.mem.eql(u8, instr_name, "i64.eq")) {
            return .i64_eq;
        } else if (std.mem.eql(u8, instr_name, "i64.ne")) {
            return .i64_ne;
        } else if (std.mem.eql(u8, instr_name, "i64.lt_s")) {
            return .i64_lt_s;
        } else if (std.mem.eql(u8, instr_name, "i64.lt_u")) {
            return .i64_lt_u;
        } else if (std.mem.eql(u8, instr_name, "i64.gt_s")) {
            return .i64_gt_s;
        } else if (std.mem.eql(u8, instr_name, "i64.gt_u")) {
            return .i64_gt_u;
        } else if (std.mem.eql(u8, instr_name, "i64.le_s")) {
            return .i64_le_s;
        } else if (std.mem.eql(u8, instr_name, "i64.le_u")) {
            return .i64_le_u;
        } else if (std.mem.eql(u8, instr_name, "i64.ge_s")) {
            return .i64_ge_s;
        } else if (std.mem.eql(u8, instr_name, "i64.ge_u")) {
            return .i64_ge_u;
        }

        // f32 comparison
        else if (std.mem.eql(u8, instr_name, "f32.eq")) {
            return .f32_eq;
        } else if (std.mem.eql(u8, instr_name, "f32.ne")) {
            return .f32_ne;
        } else if (std.mem.eql(u8, instr_name, "f32.lt")) {
            return .f32_lt;
        } else if (std.mem.eql(u8, instr_name, "f32.gt")) {
            return .f32_gt;
        } else if (std.mem.eql(u8, instr_name, "f32.le")) {
            return .f32_le;
        } else if (std.mem.eql(u8, instr_name, "f32.ge")) {
            return .f32_ge;
        }

        // f64 comparison
        else if (std.mem.eql(u8, instr_name, "f64.eq")) {
            return .f64_eq;
        } else if (std.mem.eql(u8, instr_name, "f64.ne")) {
            return .f64_ne;
        } else if (std.mem.eql(u8, instr_name, "f64.lt")) {
            return .f64_lt;
        } else if (std.mem.eql(u8, instr_name, "f64.gt")) {
            return .f64_gt;
        } else if (std.mem.eql(u8, instr_name, "f64.le")) {
            return .f64_le;
        } else if (std.mem.eql(u8, instr_name, "f64.ge")) {
            return .f64_ge;
        }

        // i32 arithmetic
        else if (std.mem.eql(u8, instr_name, "i32.clz")) {
            return .i32_clz;
        } else if (std.mem.eql(u8, instr_name, "i32.ctz")) {
            return .i32_ctz;
        } else if (std.mem.eql(u8, instr_name, "i32.popcnt")) {
            return .i32_popcnt;
        } else if (std.mem.eql(u8, instr_name, "i32.add")) {
            return .i32_add;
        } else if (std.mem.eql(u8, instr_name, "i32.sub")) {
            return .i32_sub;
        } else if (std.mem.eql(u8, instr_name, "i32.mul")) {
            return .i32_mul;
        } else if (std.mem.eql(u8, instr_name, "i32.div_s")) {
            return .i32_div_s;
        } else if (std.mem.eql(u8, instr_name, "i32.div_u")) {
            return .i32_div_u;
        } else if (std.mem.eql(u8, instr_name, "i32.rem_s")) {
            return .i32_rem_s;
        } else if (std.mem.eql(u8, instr_name, "i32.rem_u")) {
            return .i32_rem_u;
        } else if (std.mem.eql(u8, instr_name, "i32.and")) {
            return .i32_and;
        } else if (std.mem.eql(u8, instr_name, "i32.or")) {
            return .i32_or;
        } else if (std.mem.eql(u8, instr_name, "i32.xor")) {
            return .i32_xor;
        } else if (std.mem.eql(u8, instr_name, "i32.shl")) {
            return .i32_shl;
        } else if (std.mem.eql(u8, instr_name, "i32.shr_s")) {
            return .i32_shr_s;
        } else if (std.mem.eql(u8, instr_name, "i32.shr_u")) {
            return .i32_shr_u;
        } else if (std.mem.eql(u8, instr_name, "i32.rotl")) {
            return .i32_rotl;
        } else if (std.mem.eql(u8, instr_name, "i32.rotr")) {
            return .i32_rotr;
        }

        // i64 arithmetic
        else if (std.mem.eql(u8, instr_name, "i64.clz")) {
            return .i64_clz;
        } else if (std.mem.eql(u8, instr_name, "i64.ctz")) {
            return .i64_ctz;
        } else if (std.mem.eql(u8, instr_name, "i64.popcnt")) {
            return .i64_popcnt;
        } else if (std.mem.eql(u8, instr_name, "i64.add")) {
            return .i64_add;
        } else if (std.mem.eql(u8, instr_name, "i64.sub")) {
            return .i64_sub;
        } else if (std.mem.eql(u8, instr_name, "i64.mul")) {
            return .i64_mul;
        } else if (std.mem.eql(u8, instr_name, "i64.div_s")) {
            return .i64_div_s;
        } else if (std.mem.eql(u8, instr_name, "i64.div_u")) {
            return .i64_div_u;
        } else if (std.mem.eql(u8, instr_name, "i64.rem_s")) {
            return .i64_rem_s;
        } else if (std.mem.eql(u8, instr_name, "i64.rem_u")) {
            return .i64_rem_u;
        } else if (std.mem.eql(u8, instr_name, "i64.and")) {
            return .i64_and;
        } else if (std.mem.eql(u8, instr_name, "i64.or")) {
            return .i64_or;
        } else if (std.mem.eql(u8, instr_name, "i64.xor")) {
            return .i64_xor;
        } else if (std.mem.eql(u8, instr_name, "i64.shl")) {
            return .i64_shl;
        } else if (std.mem.eql(u8, instr_name, "i64.shr_s")) {
            return .i64_shr_s;
        } else if (std.mem.eql(u8, instr_name, "i64.shr_u")) {
            return .i64_shr_u;
        } else if (std.mem.eql(u8, instr_name, "i64.rotl")) {
            return .i64_rotl;
        } else if (std.mem.eql(u8, instr_name, "i64.rotr")) {
            return .i64_rotr;
        }

        // f32 arithmetic
        else if (std.mem.eql(u8, instr_name, "f32.abs")) {
            return .f32_abs;
        } else if (std.mem.eql(u8, instr_name, "f32.neg")) {
            return .f32_neg;
        } else if (std.mem.eql(u8, instr_name, "f32.ceil")) {
            return .f32_ceil;
        } else if (std.mem.eql(u8, instr_name, "f32.floor")) {
            return .f32_floor;
        } else if (std.mem.eql(u8, instr_name, "f32.trunc")) {
            return .f32_trunc;
        } else if (std.mem.eql(u8, instr_name, "f32.nearest")) {
            return .f32_nearest;
        } else if (std.mem.eql(u8, instr_name, "f32.sqrt")) {
            return .f32_sqrt;
        } else if (std.mem.eql(u8, instr_name, "f32.add")) {
            return .f32_add;
        } else if (std.mem.eql(u8, instr_name, "f32.sub")) {
            return .f32_sub;
        } else if (std.mem.eql(u8, instr_name, "f32.mul")) {
            return .f32_mul;
        } else if (std.mem.eql(u8, instr_name, "f32.div")) {
            return .f32_div;
        } else if (std.mem.eql(u8, instr_name, "f32.min")) {
            return .f32_min;
        } else if (std.mem.eql(u8, instr_name, "f32.max")) {
            return .f32_max;
        } else if (std.mem.eql(u8, instr_name, "f32.copysign")) {
            return .f32_copy_sign;
        }

        // f64 arithmetic
        else if (std.mem.eql(u8, instr_name, "f64.abs")) {
            return .f64_abs;
        } else if (std.mem.eql(u8, instr_name, "f64.neg")) {
            return .f64_neg;
        } else if (std.mem.eql(u8, instr_name, "f64.ceil")) {
            return .f64_ceil;
        } else if (std.mem.eql(u8, instr_name, "f64.floor")) {
            return .f64_floor;
        } else if (std.mem.eql(u8, instr_name, "f64.trunc")) {
            return .f64_trunc;
        } else if (std.mem.eql(u8, instr_name, "f64.nearest")) {
            return .f64_nearest;
        } else if (std.mem.eql(u8, instr_name, "f64.sqrt")) {
            return .f64_sqrt;
        } else if (std.mem.eql(u8, instr_name, "f64.add")) {
            return .f64_add;
        } else if (std.mem.eql(u8, instr_name, "f64.sub")) {
            return .f64_sub;
        } else if (std.mem.eql(u8, instr_name, "f64.mul")) {
            return .f64_mul;
        } else if (std.mem.eql(u8, instr_name, "f64.div")) {
            return .f64_div;
        } else if (std.mem.eql(u8, instr_name, "f64.min")) {
            return .f64_min;
        } else if (std.mem.eql(u8, instr_name, "f64.max")) {
            return .f64_max;
        } else if (std.mem.eql(u8, instr_name, "f64.copysign")) {
            return .f64_copy_sign;
        }

        // Numeric conversion
        else if (std.mem.eql(u8, instr_name, "i32.wrap_i64")) {
            return .i32_wrap_i64;
        } else if (std.mem.eql(u8, instr_name, "i32.trunc_f32_s")) {
            return .i32_trunc_f32_s;
        } else if (std.mem.eql(u8, instr_name, "i32.trunc_f32_u")) {
            return .i32_trunc_f32_u;
        } else if (std.mem.eql(u8, instr_name, "i32.trunc_f64_s")) {
            return .i32_trunc_f64_s;
        } else if (std.mem.eql(u8, instr_name, "i32.trunc_f64_u")) {
            return .i32_trunc_f64_u;
        } else if (std.mem.eql(u8, instr_name, "i64.extend_i32_s")) {
            return .i64_extend_i32_s;
        } else if (std.mem.eql(u8, instr_name, "i64.extend_i32_u")) {
            return .i64_extend_i32_u;
        } else if (std.mem.eql(u8, instr_name, "i64.trunc_f32_s")) {
            return .i64_trunc_f32_s;
        } else if (std.mem.eql(u8, instr_name, "i64.trunc_f32_u")) {
            return .i64_trunc_f32_u;
        } else if (std.mem.eql(u8, instr_name, "i64.trunc_f64_s")) {
            return .i64_trunc_f64_s;
        } else if (std.mem.eql(u8, instr_name, "i64.trunc_f64_u")) {
            return .i64_trunc_f64_u;
        } else if (std.mem.eql(u8, instr_name, "f32.convert_i32_s")) {
            return .f32_convert_i32_s;
        } else if (std.mem.eql(u8, instr_name, "f32.convert_i32_u")) {
            return .f32_convert_i32_u;
        } else if (std.mem.eql(u8, instr_name, "f32.convert_i64_s")) {
            return .f32_convert_i64_s;
        } else if (std.mem.eql(u8, instr_name, "f32.convert_i64_u")) {
            return .f32_convert_i64_u;
        } else if (std.mem.eql(u8, instr_name, "f32.demote_f64")) {
            return .f32_demote_f64;
        } else if (std.mem.eql(u8, instr_name, "f64.convert_i32_s")) {
            return .f64_convert_i32_s;
        } else if (std.mem.eql(u8, instr_name, "f64.convert_i32_u")) {
            return .f64_convert_i32_u;
        } else if (std.mem.eql(u8, instr_name, "f64.convert_i64_s")) {
            return .f64_convert_i64_s;
        } else if (std.mem.eql(u8, instr_name, "f64.convert_i64_u")) {
            return .f64_convert_i64_u;
        } else if (std.mem.eql(u8, instr_name, "f64.promote_f32")) {
            return .f64_promote_f32;
        } else if (std.mem.eql(u8, instr_name, "i32.reinterpret_f32")) {
            return .i32_reinterpret_f32;
        } else if (std.mem.eql(u8, instr_name, "i64.reinterpret_f64")) {
            return .i64_reinterpret_f64;
        } else if (std.mem.eql(u8, instr_name, "f32.reinterpret_i32")) {
            return .f32_reinterpret_i32;
        } else if (std.mem.eql(u8, instr_name, "f64.reinterpret_i64")) {
            return .f64_reinterpret_i64;
        }

        // Unknown instruction
        std.debug.print("Unknown instruction: {s}\n", .{instr_name});
        unreachable;
    }

    /// Parse S-expression recursively: (instr arg1 arg2 ...)
    fn parseSExpression(self: *Parser, instructions: *std.ArrayList(wasm_core.types.Instruction)) !void {
        // Parse the instruction
        const instr = try self.parseInstruction() orelse return;

        // Special handling for 'block' instruction with folded form: (block (result) ...)
        if (std.meta.activeTag(instr) == .block) {
            // Add 'block' instruction first
            try instructions.append(self.allocator, instr);

            // Parse instructions in block body (inline to avoid circular dependency)
            while (self.current_token != .eof and self.current_token != .right_paren) {
                if (self.current_token == .left_paren) {
                    try self.advance(); // consume '('
                    try self.parseSExpression(instructions);
                    try self.expectRightParen();
                } else if (self.current_token == .identifier) {
                    if (try self.parseInstruction()) |instr_block| {
                        try instructions.append(self.allocator, instr_block);
                    }
                } else {
                    try self.advance();
                }
            }

            // Add 'end' instruction
            try instructions.append(self.allocator, .end);
            return;
        }

        // Special handling for 'loop' instruction with folded form: (loop (result) ...)
        if (std.meta.activeTag(instr) == .loop) {
            // Add 'loop' instruction first
            try instructions.append(self.allocator, instr);

            // Parse instructions in loop body (inline to avoid circular dependency)
            while (self.current_token != .eof and self.current_token != .right_paren) {
                if (self.current_token == .left_paren) {
                    try self.advance(); // consume '('
                    try self.parseSExpression(instructions);
                    try self.expectRightParen();
                } else if (self.current_token == .identifier) {
                    if (try self.parseInstruction()) |instr_loop| {
                        try instructions.append(self.allocator, instr_loop);
                    }
                } else {
                    try self.advance();
                }
            }

            // Add 'end' instruction
            try instructions.append(self.allocator, .end);
            return;
        }

        // Special handling for 'if' instruction with folded form: (if (result) (cond) (then ...) (else ...))
        if (std.meta.activeTag(instr) == .@"if") {
            // Parse condition expression
            if (self.current_token == .left_paren) {
                try self.advance(); // consume '('
                try self.parseSExpression(instructions);
                try self.expectRightParen();
            }

            // Add 'if' instruction
            try instructions.append(self.allocator, instr);

            // Parse (then ...) block
            if (self.current_token == .left_paren) {
                try self.advance(); // consume '('
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "then")) {
                    try self.advance(); // consume 'then'
                    // Parse instructions in then block (inline parseInstructions to avoid circular dependency)
                    while (self.current_token != .eof and self.current_token != .right_paren) {
                        if (self.current_token == .left_paren) {
                            try self.advance(); // consume '('
                            try self.parseSExpression(instructions);
                            try self.expectRightParen();
                        } else if (self.current_token == .identifier) {
                            if (try self.parseInstruction()) |instr_then| {
                                try instructions.append(self.allocator, instr_then);
                            }
                        } else {
                            try self.advance();
                        }
                    }
                    try self.expectRightParen();
                } else {
                    return TextDecodeError.UnexpectedToken;
                }
            }

            // Parse optional (else ...) block
            if (self.current_token == .left_paren) {
                const saved_lexer_pos = self.lexer.pos;
                const saved_lexer_char = self.lexer.current_char;
                const saved_token = self.current_token;

                try self.advance(); // consume '('
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "else")) {
                    try self.advance(); // consume 'else'
                    // Add 'else' instruction
                    try instructions.append(self.allocator, .@"else");
                    // Parse instructions in else block (inline parseInstructions to avoid circular dependency)
                    while (self.current_token != .eof and self.current_token != .right_paren) {
                        if (self.current_token == .left_paren) {
                            try self.advance(); // consume '('
                            try self.parseSExpression(instructions);
                            try self.expectRightParen();
                        } else if (self.current_token == .identifier) {
                            if (try self.parseInstruction()) |instr_else| {
                                try instructions.append(self.allocator, instr_else);
                            }
                        } else {
                            try self.advance();
                        }
                    }
                    try self.expectRightParen();
                } else {
                    // Not an else block, restore state
                    self.lexer.pos = saved_lexer_pos;
                    self.lexer.current_char = saved_lexer_char;
                    self.current_token = saved_token;
                }
            }

            // Add 'end' instruction
            try instructions.append(self.allocator, .end);
            return;
        }

        // Recursively parse child expressions (operands)
        while (self.current_token == .left_paren) {
            try self.advance(); // consume '('
            try self.parseSExpression(instructions); // recursive call
            try self.expectRightParen();
        }

        // Add parent instruction last (children are already added, so order is correct)
        try instructions.append(self.allocator, instr);
    }

    /// Parse u32 or identifier (for indices)
    fn parseU32OrIdentifier(self: *Parser) !u32 {
        if (self.current_token == .number) {
            const num = try std.fmt.parseInt(u32, self.current_token.number, 10);
            try self.advance();
            return num;
        } else if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            try self.advance();
            // Resolve named index
            if (self.local_names.get(id)) |idx| {
                return idx;
            }
            // If not found, return 0 as fallback
            return 0;
        }
        return TextDecodeError.UnexpectedToken;
    }

    /// Parse memory argument (offset and align)
    fn parseMemArg(self: *Parser) !wasm_core.types.Instruction.MemArg {
        var offset: u32 = 0;
        var alignment: u32 = 0;

        // Parse optional offset=N and align=N
        while (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (std.mem.startsWith(u8, id, "offset=")) {
                offset = try std.fmt.parseInt(u32, id[7..], 0);
                try self.advance();
            } else if (std.mem.startsWith(u8, id, "align=")) {
                alignment = try std.fmt.parseInt(u32, id[6..], 0);
                try self.advance();
            } else {
                break;
            }
        }

        return wasm_core.types.Instruction.MemArg{
            .@"align" = alignment,
            .offset = offset,
        };
    }

    /// Parse integer
    fn parseInt(self: *Parser, comptime T: type) !T {
        if (self.current_token != .number) {
            return TextDecodeError.InvalidNumber;
        }

        // Remove underscores from the number string (WASM allows underscores in numeric literals)
        var buf: [256]u8 = undefined;
        var buf_idx: usize = 0;
        for (self.current_token.number) |c| {
            if (c != '_') {
                if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                buf[buf_idx] = c;
                buf_idx += 1;
            }
        }

        const val = try std.fmt.parseInt(T, buf[0..buf_idx], 0);
        try self.advance();
        return val;
    }

    /// Parse float
    fn parseFloat(self: *Parser, comptime T: type) !T {
        const val: T = if (self.current_token == .identifier) blk: {
            const id = self.current_token.identifier;
            if (std.mem.startsWith(u8, id, "nan") or std.mem.startsWith(u8, id, "-nan")) {
                const is_negative = std.mem.startsWith(u8, id, "-");
                const nan_start: usize = if (is_negative) 4 else 3;

                if (id.len > nan_start and id[nan_start] == ':') {
                    const payload_str = id[nan_start + 1 ..];
                    var buf: [256]u8 = undefined;
                    var buf_idx: usize = 0;
                    for (payload_str) |c| {
                        if (c != '_') {
                            if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                            buf[buf_idx] = c;
                            buf_idx += 1;
                        }
                    }

                    if (T == f32) {
                        const payload = try std.fmt.parseInt(u32, buf[0..buf_idx], 0);
                        const sign_bit: u32 = if (is_negative) 1 << 31 else 0;
                        const exponent: u32 = 0xFF << 23;
                        const mantissa: u32 = payload & 0x7FFFFF;
                        const bits = sign_bit | exponent | mantissa;
                        break :blk @bitCast(bits);
                    } else {
                        const payload = try std.fmt.parseInt(u64, buf[0..buf_idx], 0);
                        const sign_bit: u64 = if (is_negative) 1 << 63 else 0;
                        const exponent: u64 = 0x7FF << 52;
                        const mantissa: u64 = payload & 0xFFFFFFFFFFFFF;
                        const bits = sign_bit | exponent | mantissa;
                        break :blk @bitCast(bits);
                    }
                } else {
                    const sign: T = if (is_negative) -1.0 else 1.0;
                    break :blk sign * std.math.nan(T);
                }
            } else if (std.mem.eql(u8, id, "inf")) {
                break :blk std.math.inf(T);
            } else if (std.mem.eql(u8, id, "-inf")) {
                break :blk -std.math.inf(T);
            } else {
                return TextDecodeError.InvalidNumber;
            }
        } else if (self.current_token == .number) blk: {
            break :blk try std.fmt.parseFloat(T, self.current_token.number);
        } else {
            return TextDecodeError.InvalidNumber;
        };

        try self.advance();
        return val;
    }

    /// Skip to the closing paren of the current s-expression
    fn skipToClosingParen(self: *Parser) !void {
        var depth: u32 = 1;
        while (depth > 0 and self.current_token != .eof) {
            if (self.current_token == .left_paren) {
                depth += 1;
            } else if (self.current_token == .right_paren) {
                depth -= 1;
            }
            if (depth > 0) {
                try self.advance();
            }
        }
        if (self.current_token == .right_paren) {
            try self.advance();
        }
    }

    /// Parse module command: (module ...)
    fn parseModuleCommand(self: *Parser) !wast.ModuleCommand {
        // current token is "module"
        try self.advance();

        // Skip optional "definition" keyword
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "definition")) {
            try self.advance();
        }

        // Check for optional module name (can be $name or just name)
        var name: ?[]const u8 = null;
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            // Check if it's NOT a known field name (and not "binary", "quote", or "instance")
            const is_field = std.mem.eql(u8, id, "func") or
                std.mem.eql(u8, id, "table") or
                std.mem.eql(u8, id, "memory") or
                std.mem.eql(u8, id, "data") or
                std.mem.eql(u8, id, "export") or
                std.mem.eql(u8, id, "type") or
                std.mem.eql(u8, id, "import") or
                std.mem.eql(u8, id, "global") or
                std.mem.eql(u8, id, "binary") or
                std.mem.eql(u8, id, "quote") or
                std.mem.eql(u8, id, "instance");
            if (!is_field) {
                name = try self.allocator.dupe(u8, self.current_token.identifier);
                try self.advance();
            }
        }

        // Check for binary format: (module binary "...")
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "binary")) {
            try self.advance(); // consume "binary"
            // Skip all following string literals
            while (self.current_token == .string) {
                try self.advance();
            }
            try self.expectToken(.right_paren);
            // Return a placeholder module for binary format
            var builder = ModuleBuilder.init(self.allocator);
            defer builder.deinit();
            return wast.ModuleCommand{
                .name = name,
                .module = try builder.build(),
            };
        }

        // Check for quote format: (module quote "..." "..." ...)
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "quote")) {
            try self.advance(); // consume "quote"
            // Skip all following string literals
            while (self.current_token == .string) {
                try self.advance();
            }
            try self.expectToken(.right_paren);
            // Return a placeholder module for quote format
            var builder = ModuleBuilder.init(self.allocator);
            defer builder.deinit();
            return wast.ModuleCommand{
                .name = name,
                .module = try builder.build(),
            };
        }

        // Check for instance format: (module instance $instance_name $module_name)
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "instance")) {
            try self.advance(); // consume "instance"
            // Skip instance name (e.g., $I1)
            if (self.current_token == .identifier) {
                try self.advance();
            }
            // Skip module name (e.g., $M)
            if (self.current_token == .identifier) {
                try self.advance();
            }
            try self.expectToken(.right_paren);
            // Return a placeholder module for instance
            var builder = ModuleBuilder.init(self.allocator);
            defer builder.deinit();
            return wast.ModuleCommand{
                .name = name,
                .module = try builder.build(),
            };
        }

        // Parse the module itself (text format)
        var builder = ModuleBuilder.init(self.allocator);
        defer builder.deinit();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.parseModuleField(&builder);
        }

        try self.expectToken(.right_paren);

        return wast.ModuleCommand{
            .name = name,
            .module = try builder.build(),
        };
    }

    /// Parse assert_return: (assert_return (invoke "func" ...) (result...))
    fn parseAssertReturn(self: *Parser) !wast.AssertReturn {
        // current token is "assert_return"
        try self.advance();

        // Parse action
        const action = try self.parseAction();

        // Parse expected results
        var expected: std.ArrayList(wast.Value) = .{};
        defer expected.deinit(self.allocator);

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const value = try self.parseValue();
            try expected.append(self.allocator, value);
        }

        try self.expectToken(.right_paren);

        return wast.AssertReturn{
            .action = action,
            .expected = try expected.toOwnedSlice(self.allocator),
        };
    }

    /// Parse assert_trap: (assert_trap (invoke ...) "message") or (assert_trap (module ...) "message")
    fn parseAssertTrap(self: *Parser) !wast.AssertTrap {
        // current token is "assert_trap"
        try self.advance();

        // Peek to see if it's a module or an action
        try self.expectToken(.left_paren);

        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "module")) {
            // It's an inline module - skip the entire module
            var depth: u32 = 1; // Already consumed '('
            try self.advance(); // consume "module"
            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                if (depth > 0) {
                    try self.advance();
                }
            }
            if (self.current_token == .right_paren) {
                try self.advance(); // consume closing ')'
            }
        } else {
            // It's an action - skip it by counting parentheses
            var depth: u32 = 1; // Already consumed '('
            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                if (depth > 0) {
                    try self.advance();
                }
            }
            if (self.current_token == .right_paren) {
                try self.advance(); // consume closing ')'
            }
        }

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        // Return a placeholder action
        return wast.AssertTrap{
            .action = wast.Action{ .invoke = wast.Invoke{
                .module_name = null,
                .func_name = "",
                .args = &[_]wast.Value{},
            } },
            .failure = failure,
        };
    }

    /// Parse assert_invalid: (assert_invalid (module ...) "message")
    fn parseAssertInvalid(self: *Parser) !wast.AssertInvalid {
        // current token is "assert_invalid"
        try self.advance();

        // Expect (module ...)
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            return TextDecodeError.UnexpectedToken;
        }

        // Skip the module section by counting nested parentheses
        var depth: u32 = 1; // We're already inside (module
        try self.advance(); // consume "module"

        while (depth > 0 and self.current_token != .eof) {
            if (self.current_token == .left_paren) {
                depth += 1;
            } else if (self.current_token == .right_paren) {
                depth -= 1;
            }
            try self.advance();
        }

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return wast.AssertInvalid{
            .module_text = "", // We don't need to store the module text for now
            .failure = failure,
        };
    }

    /// Parse register: (register "name" $module)
    fn parseRegister(self: *Parser) !wast.Register {
        // current token is "register"
        try self.advance();

        // Parse name
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        // Parse optional module name
        var module_name: ?[]const u8 = null;
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            module_name = try self.allocator.dupe(u8, self.current_token.identifier);
            try self.advance();
        }

        try self.expectToken(.right_paren);

        return wast.Register{
            .name = name,
            .module_name = module_name,
        };
    }

    /// Parse an action (invoke or get)
    fn parseAction(self: *Parser) !wast.Action {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }

        const action_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, action_name, "invoke")) {
            return wast.Action{ .invoke = try self.parseInvoke() };
        } else if (std.mem.eql(u8, action_name, "get")) {
            return wast.Action{ .get = try self.parseGet() };
        } else {
            return TextDecodeError.UnexpectedToken;
        }
    }

    /// Parse invoke: (invoke "func" args...)
    fn parseInvoke(self: *Parser) !wast.Invoke {
        // Optional module name
        var module_name: ?[]const u8 = null;
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            module_name = try self.allocator.dupe(u8, self.current_token.identifier);
            try self.advance();
        }

        // Function name
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const func_name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        // Parse arguments
        var args: std.ArrayList(wast.Value) = .{};
        defer args.deinit(self.allocator);

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const value = try self.parseValue();
            try args.append(self.allocator, value);
        }

        try self.expectToken(.right_paren);

        return wast.Invoke{
            .module_name = module_name,
            .func_name = func_name,
            .args = try args.toOwnedSlice(self.allocator),
        };
    }

    /// Parse get: (get "global")
    fn parseGet(self: *Parser) !wast.Get {
        // Optional module name
        var module_name: ?[]const u8 = null;
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            module_name = try self.allocator.dupe(u8, self.current_token.identifier);
            try self.advance();
        }

        // Global name
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const global_name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return wast.Get{
            .module_name = module_name,
            .global_name = global_name,
        };
    }

    /// Parse a value: (i32.const 42), (f64.const 3.14), etc.
    fn parseValue(self: *Parser) !wast.Value {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }

        const type_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, type_name, "i32.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.UnexpectedToken;
            }
            const number_str = self.current_token.number;

            // Remove underscores from number string
            var buf: [256]u8 = undefined;
            var buf_idx: usize = 0;
            for (number_str) |c| {
                if (c != '_') {
                    if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                    buf[buf_idx] = c;
                    buf_idx += 1;
                }
            }
            const clean_str = buf[0..buf_idx];

            const value: i32 = if (clean_str.len > 0 and clean_str[0] == '-') blk: {
                // Negative number: parse directly as i32
                break :blk try std.fmt.parseInt(i32, clean_str, 0);
            } else blk: {
                // Positive or hex number: parse as u32 then bitcast
                const unsigned_value = try std.fmt.parseInt(u32, clean_str, 0);
                break :blk @bitCast(unsigned_value);
            };
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.UnexpectedToken;
            }
            const number_str = self.current_token.number;

            // Remove underscores from number string
            var buf: [256]u8 = undefined;
            var buf_idx: usize = 0;
            for (number_str) |c| {
                if (c != '_') {
                    if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                    buf[buf_idx] = c;
                    buf_idx += 1;
                }
            }
            const clean_str = buf[0..buf_idx];

            const value: i64 = if (clean_str.len > 0 and clean_str[0] == '-') blk: {
                // Negative number: parse directly as i64
                break :blk try std.fmt.parseInt(i64, clean_str, 0);
            } else blk: {
                // Positive or hex number: parse as u64 then bitcast
                const unsigned_value = try std.fmt.parseInt(u64, clean_str, 0);
                break :blk @bitCast(unsigned_value);
            };
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .i64 = value };
        } else if (std.mem.eql(u8, type_name, "f32.const")) {
            const value: f32 = if (self.current_token == .identifier) blk: {
                const id = self.current_token.identifier;
                if (std.mem.startsWith(u8, id, "nan") or std.mem.startsWith(u8, id, "-nan")) {
                    // NaN (with optional bit pattern and optional sign)
                    const is_negative = std.mem.startsWith(u8, id, "-");
                    const nan_start: usize = if (is_negative) 4 else 3; // Skip "-nan" or "nan"

                    // Check for payload: nan:0x<hex>
                    if (id.len > nan_start and id[nan_start] == ':') {
                        const payload_str = id[nan_start + 1 ..];
                        // Remove underscores from payload
                        var buf: [256]u8 = undefined;
                        var buf_idx: usize = 0;
                        for (payload_str) |c| {
                            if (c != '_') {
                                if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                                buf[buf_idx] = c;
                                buf_idx += 1;
                            }
                        }
                        const payload = try std.fmt.parseInt(u32, buf[0..buf_idx], 0);

                        // Build NaN bit pattern: sign(1) | exponent(8 bits all 1) | mantissa(23 bits with payload)
                        const sign_bit: u32 = if (is_negative) 1 << 31 else 0;
                        const exponent: u32 = 0xFF << 23; // All 1s in exponent
                        const mantissa: u32 = payload & 0x7FFFFF; // 23 bits
                        const bits = sign_bit | exponent | mantissa;
                        break :blk @bitCast(bits);
                    } else {
                        // Plain nan without payload
                        const sign: f32 = if (is_negative) -1.0 else 1.0;
                        break :blk sign * std.math.nan(f32);
                    }
                } else if (std.mem.eql(u8, id, "inf")) {
                    break :blk std.math.inf(f32);
                } else if (std.mem.eql(u8, id, "-inf")) {
                    break :blk -std.math.inf(f32);
                } else {
                    return TextDecodeError.UnexpectedToken;
                }
            } else if (self.current_token == .number) blk: {
                break :blk try std.fmt.parseFloat(f32, self.current_token.number);
            } else {
                return TextDecodeError.UnexpectedToken;
            };
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .f32 = value };
        } else if (std.mem.eql(u8, type_name, "f64.const")) {
            const value: f64 = if (self.current_token == .identifier) blk: {
                const id = self.current_token.identifier;
                if (std.mem.startsWith(u8, id, "nan") or std.mem.startsWith(u8, id, "-nan")) {
                    // NaN (with optional bit pattern and optional sign)
                    const is_negative = std.mem.startsWith(u8, id, "-");
                    const nan_start: usize = if (is_negative) 4 else 3; // Skip "-nan" or "nan"

                    // Check for payload: nan:0x<hex>
                    if (id.len > nan_start and id[nan_start] == ':') {
                        const payload_str = id[nan_start + 1 ..];
                        // Remove underscores from payload
                        var buf: [256]u8 = undefined;
                        var buf_idx: usize = 0;
                        for (payload_str) |c| {
                            if (c != '_') {
                                if (buf_idx >= buf.len) return TextDecodeError.InvalidNumber;
                                buf[buf_idx] = c;
                                buf_idx += 1;
                            }
                        }
                        const payload = try std.fmt.parseInt(u64, buf[0..buf_idx], 0);

                        // Build NaN bit pattern: sign(1) | exponent(11 bits all 1) | mantissa(52 bits with payload)
                        const sign_bit: u64 = if (is_negative) 1 << 63 else 0;
                        const exponent: u64 = 0x7FF << 52; // All 1s in exponent (11 bits)
                        const mantissa: u64 = payload & 0xFFFFFFFFFFFFF; // 52 bits
                        const bits = sign_bit | exponent | mantissa;
                        break :blk @bitCast(bits);
                    } else {
                        // Plain nan without payload
                        const sign: f64 = if (is_negative) -1.0 else 1.0;
                        break :blk sign * std.math.nan(f64);
                    }
                } else if (std.mem.eql(u8, id, "inf")) {
                    break :blk std.math.inf(f64);
                } else if (std.mem.eql(u8, id, "-inf")) {
                    break :blk -std.math.inf(f64);
                } else {
                    return TextDecodeError.UnexpectedToken;
                }
            } else if (self.current_token == .number) blk: {
                break :blk try std.fmt.parseFloat(f64, self.current_token.number);
            } else {
                return TextDecodeError.UnexpectedToken;
            };
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .f64 = value };
        } else {
            // For now, skip unknown value types
            try self.skipToClosingParen();
            return wast.Value{ .i32 = 0 }; // placeholder
        }
    }
};

pub fn parseWastModule(allocator: std.mem.Allocator, input: []const u8) !wasm_core.types.Module {
    var parser = try Parser.init(allocator, input);
    return parser.parseModule();
}

/// Parse a complete .wast script file
pub fn parseWastScript(allocator: std.mem.Allocator, input: []const u8) !wast.WastScript {
    var script = wast.WastScript.init(allocator);
    errdefer script.deinit();

    var parser = try Parser.init(allocator, input);

    while (parser.current_token != .eof) {
        // Skip any whitespace or comments
        if (parser.current_token == .eof) break;

        // Expect left paren for each command
        if (parser.current_token != .left_paren) {
            try parser.advance();
            continue;
        }

        try parser.advance(); // consume '('

        if (parser.current_token != .identifier) {
            // Skip malformed commands
            try parser.skipToClosingParen();
            continue;
        }

        const cmd_name = parser.current_token.identifier;

        if (std.mem.eql(u8, cmd_name, "module")) {
            const module_cmd = try parser.parseModuleCommand();
            try script.addCommand(.{ .module = module_cmd });
        } else if (std.mem.eql(u8, cmd_name, "assert_return")) {
            const assert_cmd = try parser.parseAssertReturn();
            try script.addCommand(.{ .assert_return = assert_cmd });
        } else if (std.mem.eql(u8, cmd_name, "assert_trap")) {
            const assert_cmd = try parser.parseAssertTrap();
            try script.addCommand(.{ .assert_trap = assert_cmd });
        } else if (std.mem.eql(u8, cmd_name, "assert_invalid")) {
            const assert_cmd = try parser.parseAssertInvalid();
            try script.addCommand(.{ .assert_invalid = assert_cmd });
        } else if (std.mem.eql(u8, cmd_name, "register")) {
            const reg_cmd = try parser.parseRegister();
            try script.addCommand(.{ .register = reg_cmd });
        } else {
            // Unknown command, skip it
            try parser.skipToClosingParen();
        }
    }

    return script;
}

// Test function
