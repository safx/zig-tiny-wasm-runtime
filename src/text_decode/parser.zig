const std = @import("std");
const wasm_core = @import("wasm-core");
const lexer_mod = @import("./lexer.zig");
const spec_types = @import("spec-types");
const numeric_parser = @import("./numeric_parser.zig");

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
    func_names: std.StringHashMap(u32),
    memory_names: std.StringHashMap(u32),
    type_names: std.StringHashMap(u32),

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
            .func_names = std.StringHashMap(u32).init(allocator),
            .memory_names = std.StringHashMap(u32).init(allocator),
            .type_names = std.StringHashMap(u32).init(allocator),
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
        self.func_names.deinit();
        self.memory_names.deinit();
        self.type_names.deinit();
    }

    pub fn countImportFuncs(self: *const ModuleBuilder) u32 {
        var count: u32 = 0;
        for (self.imports.items) |imp| {
            if (imp.desc == .function) count += 1;
        }
        return count;
    }

    pub fn countImportTables(self: *const ModuleBuilder) u32 {
        var count: u32 = 0;
        for (self.imports.items) |imp| {
            if (imp.desc == .table) count += 1;
        }
        return count;
    }

    pub fn countImportMemories(self: *const ModuleBuilder) u32 {
        var count: u32 = 0;
        for (self.imports.items) |imp| {
            if (imp.desc == .memory) count += 1;
        }
        return count;
    }

    pub fn countImportGlobals(self: *const ModuleBuilder) u32 {
        var count: u32 = 0;
        for (self.imports.items) |imp| {
            if (imp.desc == .global) count += 1;
        }
        return count;
    }

    pub fn clear(self: *ModuleBuilder) void {
        self.types.clearRetainingCapacity();
        self.funcs.clearRetainingCapacity();
        self.tables.clearRetainingCapacity();
        self.memories.clearRetainingCapacity();
        self.globals.clearRetainingCapacity();
        self.elements.clearRetainingCapacity();
        self.datas.clearRetainingCapacity();
        self.imports.clearRetainingCapacity();
        self.exports.clearRetainingCapacity();
        self.func_names.clearRetainingCapacity();
        self.memory_names.clearRetainingCapacity();
        self.type_names.clearRetainingCapacity();
        self.start = null;
    }
};

pub const Parser = struct {
    lexer: Lexer,
    current_token: Token,
    allocator: std.mem.Allocator,
    input: []const u8,
    // Name resolution for current function
    local_names: std.StringHashMap(u32),
    label_stack: std.ArrayList(?[]const u8),
    builder: *ModuleBuilder,

    pub fn init(allocator: std.mem.Allocator, input: []const u8, builder: *ModuleBuilder) !Parser {
        var lexer = Lexer.init(input);
        const current_token = try lexer.nextToken();

        return Parser{
            .lexer = lexer,
            .current_token = current_token,
            .allocator = allocator,
            .input = input,
            .local_names = std.StringHashMap(u32).init(allocator),
            .label_stack = std.ArrayList(?[]const u8){},
            .builder = builder,
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
            std.debug.print("Warning: expectToken: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
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

        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.parseModuleField(self.builder);
        }

        try self.expectToken(.right_paren);
        return try self.builder.build();
    }

    fn parseModuleField(self: *Parser, builder: *ModuleBuilder) !void {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            std.debug.print("Error: parseModuleField: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        const field_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, field_name, "type")) {
            try self.parseType(builder);
        } else if (std.mem.eql(u8, field_name, "memory")) {
            try self.parseMemory(builder);
        } else if (std.mem.eql(u8, field_name, "data")) {
            try self.parseData(builder);
        } else if (std.mem.eql(u8, field_name, "table")) {
            try self.parseTable(builder);
        } else if (std.mem.eql(u8, field_name, "elem")) {
            try self.parseElem(builder);
        } else if (std.mem.eql(u8, field_name, "import")) {
            try self.parseImport(builder);
        } else if (std.mem.eql(u8, field_name, "global")) {
            try self.parseGlobal(builder);
        } else if (std.mem.eql(u8, field_name, "func")) {
            try self.parseFunction(builder);
        } else if (std.mem.eql(u8, field_name, "export")) {
            try self.parseExport(builder);
        } else if (std.mem.eql(u8, field_name, "start")) {
            const idx = try self.parseU32OrIdentifier();
            builder.start = idx;
        } else if (field_name.len > 0 and field_name[0] == '@') {
            // WAT annotation (@name ...) - skip raw content until matching closing paren
            // Must work at raw character level because annotation content may contain
            // characters that the lexer cannot tokenize (e.g., commas, brackets, braces)
            var depth: u32 = 0;
            while (self.lexer.pos < self.lexer.input.len) {
                const ch = self.lexer.input[self.lexer.pos];
                if (ch == '(') {
                    depth += 1;
                } else if (ch == ')') {
                    if (depth == 0) break;
                    depth -= 1;
                } else if (ch == '"') {
                    // Skip string literal content
                    self.lexer.pos += 1;
                    while (self.lexer.pos < self.lexer.input.len and self.lexer.input[self.lexer.pos] != '"') {
                        if (self.lexer.input[self.lexer.pos] == '\\') self.lexer.pos += 1; // skip escape
                        self.lexer.pos += 1;
                    }
                    if (self.lexer.pos < self.lexer.input.len) self.lexer.pos += 1; // skip closing "
                    continue;
                } else if (ch == ';' and self.lexer.pos + 1 < self.lexer.input.len and self.lexer.input[self.lexer.pos + 1] == ';') {
                    // Skip line comment
                    while (self.lexer.pos < self.lexer.input.len and self.lexer.input[self.lexer.pos] != '\n') {
                        self.lexer.pos += 1;
                    }
                    continue;
                } else if (ch == '\n') {
                    self.lexer.line += 1;
                }
                self.lexer.pos += 1;
            }
            // Position is now at ')' or end — update lexer state and advance past ')'
            if (self.lexer.pos < self.lexer.input.len) {
                self.lexer.pos += 1; // skip ')'
                self.lexer.current_char = if (self.lexer.pos < self.lexer.input.len) self.lexer.input[self.lexer.pos] else null;
            } else {
                self.lexer.current_char = null;
            }
            self.current_token = try self.lexer.nextToken();
            return;
        } else {
            // Unknown module field (e.g., tag, rec) - skip content to allow parsing remaining fields
            while (self.current_token != .right_paren and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    try self.advance();
                    try self.skipToClosingParen();
                } else {
                    try self.advance();
                }
            }
            // Falls through to expectToken(.right_paren) on line 232
        }

        try self.expectToken(.right_paren);
    }

    fn parseElem(self: *Parser, builder: *ModuleBuilder) !void {
        // Skip optional name
        _ = try self.parseOptionalName();

        const ref_type: wasm_core.types.RefType = .funcref;
        var table_idx: u32 = 0;
        var offset: wasm_core.types.InitExpression = .{ .i32_const = 0 };
        var mode: wasm_core.types.ElementMode = .passive;

        // Check for (table $t) or (table idx) or (offset-expr) or (i32.const ...)
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "table")) {
                try self.advance();
                if (self.current_token == .number) {
                    table_idx = try std.fmt.parseInt(u32, self.current_token.number, 0);
                    try self.advance();
                } else if (self.current_token == .identifier) {
                    // Table name - skip
                    try self.advance();
                }
                try self.expectRightParen();

                // Parse offset
                if (self.current_token == .left_paren) {
                    const saved_pos2 = self.lexer.pos;
                    const saved_char2 = self.lexer.current_char;
                    const saved_token2 = self.current_token;

                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "offset")) {
                        try self.advance();
                        offset = try self.parseInitExpression();
                        try self.expectRightParen(); // close offset
                    } else {
                        // Restore and parse as init expression
                        self.lexer.pos = saved_pos2;
                        self.lexer.current_char = saved_char2;
                        self.current_token = saved_token2;
                        offset = try self.parseInitExpression();
                    }
                }

                mode = .{ .active = .{ .table_idx = table_idx, .offset = offset } };
            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "offset")) {
                // (offset (i32.const 0))
                try self.advance();
                offset = try self.parseInitExpression();
                try self.expectRightParen(); // close offset
                mode = .{ .active = .{ .table_idx = 0, .offset = offset } };
            } else {
                // Direct offset expression like (i32.const 0)
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
                offset = try self.parseInitExpression();
                mode = .{ .active = .{ .table_idx = 0, .offset = offset } };
            }
        }

        // Check for 'declare' keyword
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "declare")) {
            mode = .declarative;
            try self.advance();
        }

        // Skip ref type keyword if present (func, funcref, externref, etc.)
        if (self.current_token == .identifier) {
            const kw = self.current_token.identifier;
            if (std.mem.eql(u8, kw, "func") or std.mem.eql(u8, kw, "funcref") or
                std.mem.eql(u8, kw, "externref") or std.mem.eql(u8, kw, "extern"))
            {
                try self.advance();
            }
        }

        // Parse function references
        var init_list = std.ArrayList(wasm_core.types.InitExpression){};
        defer init_list.deinit(self.allocator);

        while (self.current_token != .right_paren and self.current_token != .eof) {
            if (self.current_token == .left_paren) {
                try self.advance();
                if (self.current_token == .identifier) {
                    if (std.mem.eql(u8, self.current_token.identifier, "ref")) {
                        // (ref func) - type specification, skip
                        try self.advance();
                        if (self.current_token == .identifier) {
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, self.current_token.identifier, "ref.func")) {
                        try self.advance();
                        const func_idx: u32 = if (self.current_token == .number)
                            try std.fmt.parseInt(u32, self.current_token.number, 0)
                        else
                            0;
                        try init_list.append(self.allocator, .{ .ref_func = func_idx });
                        try self.advance();
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, self.current_token.identifier, "item")) {
                        try self.advance();
                        if (self.current_token == .left_paren) {
                            try self.advance();
                            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref.func")) {
                                try self.advance();
                                const func_idx: u32 = if (self.current_token == .number)
                                    try std.fmt.parseInt(u32, self.current_token.number, 0)
                                else
                                    0;
                                try init_list.append(self.allocator, .{ .ref_func = func_idx });
                                try self.advance();
                                try self.expectRightParen();
                                try self.expectRightParen();
                            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref.null")) {
                                try self.advance();
                                try init_list.append(self.allocator, .{ .ref_null = .funcref });
                                try self.advance();
                                try self.expectRightParen();
                                try self.expectRightParen();
                            }
                        } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref.func")) {
                            try self.advance();
                            const func_idx: u32 = if (self.current_token == .number)
                                try std.fmt.parseInt(u32, self.current_token.number, 0)
                            else
                                0;
                            try init_list.append(self.allocator, .{ .ref_func = func_idx });
                            try self.advance();
                            try self.expectRightParen();
                        }
                    } else if (std.mem.eql(u8, self.current_token.identifier, "ref.null")) {
                        try self.advance();
                        try init_list.append(self.allocator, .{ .ref_null = .funcref });
                        // Consume the type argument if present
                        if (self.current_token != .right_paren) try self.advance();
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, self.current_token.identifier, "global.get")) {
                        try self.advance();
                        const global_idx = try self.parseU32OrIdentifier();
                        try init_list.append(self.allocator, .{ .global_get = global_idx });
                        try self.expectRightParen();
                    } else {
                        // Unknown expression - skip all tokens until closing paren
                        while (self.current_token != .right_paren and self.current_token != .eof) {
                            try self.advance();
                        }
                        try self.expectRightParen();
                    }
                }
            } else if (self.current_token == .identifier) {
                // Function name or index
                if (std.mem.startsWith(u8, self.current_token.identifier, "$")) {
                    const func_idx = builder.func_names.get(self.current_token.identifier) orelse 0;
                    try init_list.append(self.allocator, .{ .ref_func = func_idx });
                } else {
                    // Try to parse as number
                    const func_idx = std.fmt.parseInt(u32, self.current_token.identifier, 0) catch 0;
                    try init_list.append(self.allocator, .{ .ref_func = func_idx });
                }
                try self.advance();
            } else if (self.current_token == .number) {
                const func_idx = try std.fmt.parseInt(u32, self.current_token.number, 0);
                try init_list.append(self.allocator, .{ .ref_func = func_idx });
                try self.advance();
            } else {
                break;
            }
        }

        try builder.elements.append(self.allocator, .{
            .type = ref_type,
            .init = try init_list.toOwnedSlice(self.allocator),
            .mode = mode,
        });
    }

    fn parseType(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse optional type name
        const type_name = try self.parseOptionalName();

        // Expect (func ...)
        try self.expectToken(.left_paren);
        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "func")) {
            std.debug.print("Error: Expected 'func' in type definition\n", .{});
            return TextDecodeError.UnexpectedToken;
        }
        try self.advance();

        var params: std.ArrayList(wasm_core.types.ValueType) = .{};
        defer params.deinit(self.allocator);
        var results: std.ArrayList(wasm_core.types.ValueType) = .{};
        defer results.deinit(self.allocator);

        // Parse (param ...) and (result ...)
        while (self.current_token == .left_paren) {
            try self.advance();
            if (self.current_token != .identifier) break;

            const keyword = self.current_token.identifier;
            if (std.mem.eql(u8, keyword, "param")) {
                try self.advance();
                while (self.current_token != .right_paren and self.current_token != .eof) {
                    // Skip parameter name if present (e.g., $x)
                    if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                        try self.advance();
                    }
                    if (try self.parseValueType()) |vtype| {
                        try params.append(self.allocator, vtype);
                    } else {
                        std.debug.print("Warning: Unknown parameter type in type definition\n", .{});
                        return TextDecodeError.UnexpectedToken;
                    }
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, keyword, "result")) {
                try self.advance();
                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (try self.parseValueType()) |vtype| {
                        try results.append(self.allocator, vtype);
                    } else {
                        std.debug.print("Error: Unknown result type in type definition\n", .{});
                        return TextDecodeError.UnexpectedToken;
                    }
                }
                try self.expectRightParen();
            } else {
                break;
            }
        }

        try self.expectRightParen(); // close (func ...)

        const type_idx: u32 = @intCast(builder.types.items.len);
        const func_type = wasm_core.types.FuncType{
            .parameter_types = try self.allocator.dupe(wasm_core.types.ValueType, params.items),
            .result_types = try self.allocator.dupe(wasm_core.types.ValueType, results.items),
        };
        try builder.types.append(self.allocator, func_type);

        // Register type name if present
        if (type_name) |name| {
            try builder.type_names.put(name, type_idx);
        }
    }

    fn parseGlobal(self: *Parser, builder: *ModuleBuilder) !void {
        // Skip optional name
        _ = try self.parseOptionalName();

        // Check if it's an import
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "import")) {
                try self.advance(); // consume 'import'

                // Parse module name
                if (self.current_token != .string) return TextDecodeError.UnexpectedToken;
                const module_name = try self.allocator.dupe(u8, self.current_token.string);
                try self.advance();

                // Parse field name
                if (self.current_token != .string) return TextDecodeError.UnexpectedToken;
                const field_name = try self.allocator.dupe(u8, self.current_token.string);
                try self.advance();

                try self.expectRightParen();

                // Parse global type: plain identifier, (mut type), or (ref ...)
                var value_type: wasm_core.types.ValueType = .i32;
                var mutability: wasm_core.types.Mutability = .immutable;

                if (self.current_token == .left_paren) {
                    const saved2 = .{ self.lexer.pos, self.lexer.current_char, self.current_token };
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "mut")) {
                        mutability = .mutable;
                        try self.advance();
                        if (try self.parseValueType()) |vtype| {
                            value_type = vtype;
                        }
                        try self.expectRightParen();
                    } else {
                        // Not (mut ...), restore and try parseValueType for (ref ...)
                        self.lexer.pos = saved2[0];
                        self.lexer.current_char = saved2[1];
                        self.current_token = saved2[2];
                        if (try self.parseValueType()) |vtype| {
                            value_type = vtype;
                        }
                    }
                } else if (self.current_token == .identifier) {
                    value_type = try self.parseValueType() orelse .i32;
                }

                // Add to imports
                try builder.imports.append(self.allocator, .{
                    .module_name = module_name,
                    .name = field_name,
                    .desc = .{ .global = .{ .value_type = value_type, .mutability = mutability } },
                });
                return;
            } else {
                // Not import, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Process optional export
        var export_name: ?[]const u8 = null;
        while (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "export")) {
                try self.advance(); // skip 'export'
                if (self.current_token == .string) {
                    export_name = try self.allocator.dupe(u8, self.current_token.string);
                    try self.advance();
                }
                try self.expectRightParen();
            } else {
                // Not export, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
                break;
            }
        }

        // Parse type
        var value_type: wasm_core.types.ValueType = .i32;
        var mutability: wasm_core.types.Mutability = .immutable;

        // Check for (mut type) or (ref ...) or plain type
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "mut")) {
                mutability = .mutable;
                try self.advance();
                // Parse type inside (mut ...) - can be plain identifier or (ref ...)
                if (try self.parseValueType()) |vtype| {
                    value_type = vtype;
                }
                try self.expectRightParen();
            } else {
                // Not (mut ...), restore and try parseValueType which handles (ref ...)
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;

                if (try self.parseValueType()) |vtype| {
                    value_type = vtype;
                }
            }
        } else if (self.current_token == .identifier) {
            value_type = try self.parseValueType() orelse .i32;
        }

        // Parse init expression
        var init_expr: wasm_core.types.InitExpression = .{ .i32_const = 0 };

        if (self.current_token == .left_paren) {
            init_expr = try self.parseInitExpression();
        }

        const global_idx: u32 = builder.countImportGlobals() + @as(u32, @intCast(builder.globals.items.len));
        try builder.globals.append(self.allocator, .{
            .type = .{ .value_type = value_type, .mutability = mutability },
            .init = init_expr,
        });

        // Add export if present
        if (export_name) |name| {
            try builder.exports.append(self.allocator, .{
                .name = name,
                .desc = .{ .global = global_idx },
            });
        }
    }

    fn parseImport(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse module name string
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const module_name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        // Parse field name string
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const field_name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        // Parse import descriptor: (func ...), (table ...), (memory ...), (global ...)
        if (self.current_token != .left_paren) {
            return TextDecodeError.UnexpectedToken;
        }
        try self.advance();

        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }

        const desc_type = self.current_token.identifier;
        try self.advance();

        const import_desc = if (std.mem.eql(u8, desc_type, "func")) blk: {
            // Parse function type index or signature
            var type_idx: u32 = 0;

            // Check for (type $name) or (type idx)
            if (self.current_token == .left_paren) {
                const saved_pos_type = self.lexer.pos;
                const saved_char_type = self.lexer.current_char;
                const saved_token_type = self.current_token;
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
                    try self.advance();
                    if (self.current_token == .number) {
                        type_idx = try std.fmt.parseInt(u32, self.current_token.number, 0);
                        try self.advance();
                    } else if (self.current_token == .identifier) {
                        // Type name reference
                        if (self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                            if (self.builder.type_names.get(self.current_token.identifier)) |idx| {
                                type_idx = idx;
                            }
                        }
                        try self.advance();
                    }
                    try self.expectRightParen();
                } else {
                    // Not (type ...), restore position
                    self.lexer.pos = saved_pos_type;
                    self.lexer.current_char = saved_char_type;
                    self.current_token = saved_token_type;
                }
            }

            // Skip remaining signature
            while (self.current_token != .right_paren and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    var depth: u32 = 1;
                    try self.advance();
                    while (depth > 0 and self.current_token != .eof) {
                        if (self.current_token == .left_paren) depth += 1 else if (self.current_token == .right_paren) depth -= 1;
                        if (depth > 0) try self.advance();
                    }
                    if (self.current_token == .right_paren) try self.advance();
                } else {
                    try self.advance();
                }
            }
            break :blk wasm_core.types.ImportDesc{ .function = type_idx };
        } else if (std.mem.eql(u8, desc_type, "global")) blk: {
            // Skip optional $name
            if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                try self.advance();
            }

            // Parse global type: plain identifier, (mut type), or (ref ...)
            var value_type: wasm_core.types.ValueType = .i32;
            var mutability: wasm_core.types.Mutability = .immutable;

            if (self.current_token == .left_paren) {
                const saved_pos2 = self.lexer.pos;
                const saved_char2 = self.lexer.current_char;
                const saved_token2 = self.current_token;
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "mut")) {
                    mutability = .mutable;
                    try self.advance();
                    // Parse type inside (mut ...) - can be plain identifier or (ref ...)
                    if (try self.parseValueType()) |vtype| {
                        value_type = vtype;
                    }
                    try self.expectRightParen();
                } else {
                    // Not (mut ...), restore and try parseValueType for (ref ...)
                    self.lexer.pos = saved_pos2;
                    self.lexer.current_char = saved_char2;
                    self.current_token = saved_token2;
                    if (try self.parseValueType()) |vtype| {
                        value_type = vtype;
                    }
                }
            } else if (self.current_token == .identifier) {
                value_type = try self.parseValueType() orelse .i32;
            }

            break :blk wasm_core.types.ImportDesc{ .global = .{ .value_type = value_type, .mutability = mutability } };
        } else if (std.mem.eql(u8, desc_type, "table")) blk: {
            // Skip optional $name
            if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                try self.advance();
            }
            // Skip optional address type (i32/i64 for table64)
            if (self.current_token == .identifier and
                (std.mem.eql(u8, self.current_token.identifier, "i32") or
                std.mem.eql(u8, self.current_token.identifier, "i64")))
            {
                try self.advance();
            }

            // Parse table type: min [max] reftype
            var min: u32 = 0;
            var max: ?u32 = null;
            var ref_type: wasm_core.types.RefType = .funcref;

            if (self.current_token == .number) {
                min = try std.fmt.parseInt(u32, self.current_token.number, 0);
                try self.advance();
            }

            if (self.current_token == .number) {
                max = try std.fmt.parseInt(u32, self.current_token.number, 0);
                try self.advance();
            }

            if (self.current_token == .identifier) {
                ref_type = try self.parseRefType();
            } else if (self.current_token == .left_paren) {
                // Handle (ref null func) style ref type
                const saved_pos4 = self.lexer.pos;
                const saved_char4 = self.lexer.current_char;
                const saved_token4 = self.current_token;
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref")) {
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "null")) {
                        try self.advance();
                    }
                    if (self.current_token == .identifier) {
                        ref_type = try self.parseRefType();
                    } else if (self.current_token == .number) {
                        // Numeric type index
                        try self.advance();
                    }
                    try self.expectRightParen();
                } else {
                    self.lexer.pos = saved_pos4;
                    self.lexer.current_char = saved_char4;
                    self.current_token = saved_token4;
                }
            }

            break :blk wasm_core.types.ImportDesc{ .table = .{ .limits = .{ .min = min, .max = max }, .ref_type = ref_type } };
        } else if (std.mem.eql(u8, desc_type, "memory")) blk: {
            // Skip optional $name
            if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                try self.advance();
            }
            // Skip optional address type (i32/i64 for memory64)
            if (self.current_token == .identifier and
                (std.mem.eql(u8, self.current_token.identifier, "i32") or
                std.mem.eql(u8, self.current_token.identifier, "i64")))
            {
                try self.advance();
            }

            // Parse memory type: min [max]
            var min: u32 = 1;
            var max: ?u32 = null;

            if (self.current_token == .number) {
                min = try std.fmt.parseInt(u32, self.current_token.number, 0);
                try self.advance();
            }

            if (self.current_token == .number) {
                max = try std.fmt.parseInt(u32, self.current_token.number, 0);
                try self.advance();
            }

            break :blk wasm_core.types.ImportDesc{ .memory = .{ .limits = .{ .min = min, .max = max } } };
        } else if (std.mem.eql(u8, desc_type, "tag")) blk: {
            // Exception handling tags - skip content, create dummy function import
            while (self.current_token != .right_paren and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    var depth2: u32 = 1;
                    try self.advance();
                    while (depth2 > 0 and self.current_token != .eof) {
                        if (self.current_token == .left_paren) depth2 += 1 else if (self.current_token == .right_paren) depth2 -= 1;
                        if (depth2 > 0) try self.advance();
                    }
                    if (self.current_token == .right_paren) try self.advance();
                } else {
                    try self.advance();
                }
            }
            // Create an empty function type for the tag import
            const func_type = wasm_core.types.FuncType{
                .parameter_types = &.{},
                .result_types = &.{},
            };
            const tag_type_idx: u32 = @intCast(builder.types.items.len);
            try builder.types.append(self.allocator, func_type);
            break :blk wasm_core.types.ImportDesc{ .function = tag_type_idx };
        } else {
            return TextDecodeError.UnexpectedToken;
        };

        try self.expectRightParen();

        try builder.imports.append(self.allocator, .{
            .module_name = module_name,
            .name = field_name,
            .desc = import_desc,
        });
    }

    fn parseTable(self: *Parser, builder: *ModuleBuilder) !void {
        // Skip table name if present
        _ = try self.parseOptionalName();

        // Check for inline import: (table (import "mod" "name") ...)
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "import")) {
                try self.advance(); // consume 'import'

                var module_name: []const u8 = "";
                var field_name: []const u8 = "";

                if (self.current_token == .string) {
                    module_name = self.current_token.string;
                    try self.advance();
                }
                if (self.current_token == .string) {
                    field_name = self.current_token.string;
                    try self.advance();
                }
                try self.expectRightParen();

                // Parse table type after import
                var min: u32 = 0;
                var max: ?u32 = null;
                var ref_type: wasm_core.types.RefType = .funcref;

                // Optional table address type (i32 or i64 for table64)
                if (self.current_token == .identifier and
                    (std.mem.eql(u8, self.current_token.identifier, "i32") or
                    std.mem.eql(u8, self.current_token.identifier, "i64")))
                {
                    try self.advance();
                }

                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
                    min = std.math.cast(u32, val) orelse std.math.maxInt(u32);
                    try self.advance();
                }

                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
                    max = std.math.cast(u32, val) orelse std.math.maxInt(u32);
                    try self.advance();
                }

                if (self.current_token == .identifier) {
                    ref_type = try self.parseRefType();
                } else if (self.current_token == .left_paren) {
                    // Handle (ref null func) etc.
                    const sp = .{ self.lexer.pos, self.lexer.current_char, self.current_token };
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref")) {
                        try self.advance();
                        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "null")) {
                            try self.advance();
                        }
                        if (self.current_token == .identifier) {
                            ref_type = try self.parseRefType();
                        } else if (self.current_token == .number) {
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else {
                        self.lexer.pos = sp[0];
                        self.lexer.current_char = sp[1];
                        self.current_token = sp[2];
                    }
                }

                const table_type = wasm_core.types.TableType{
                    .limits = .{ .min = min, .max = max },
                    .ref_type = ref_type,
                };

                const import_desc = wasm_core.types.ImportDesc{
                    .table = table_type,
                };
                const import = wasm_core.types.Import{
                    .module_name = try self.allocator.dupe(u8, module_name),
                    .name = try self.allocator.dupe(u8, field_name),
                    .desc = import_desc,
                };
                try builder.imports.append(self.allocator, import);

                return;
            } else {
                // Not import, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Process optional export
        var export_name: ?[]const u8 = null;
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "export")) {
                try self.advance(); // skip 'export'
                if (self.current_token == .string) {
                    export_name = try self.allocator.dupe(u8, self.current_token.string);
                    try self.advance();
                }
                try self.expectRightParen();
            } else {
                // Not export, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Check for (import ...) after (export ...) — e.g., (table (export "t") (import "m" "n") 2 funcref)
        if (export_name != null and self.current_token == .left_paren) {
            const saved_pos3 = self.lexer.pos;
            const saved_char3 = self.lexer.current_char;
            const saved_token3 = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "import")) {
                try self.advance(); // consume 'import'

                var module_name: []const u8 = "";
                var field_name2: []const u8 = "";

                if (self.current_token == .string) {
                    module_name = self.current_token.string;
                    try self.advance();
                }
                if (self.current_token == .string) {
                    field_name2 = self.current_token.string;
                    try self.advance();
                }
                try self.expectRightParen();

                // Parse table type after import
                var min2: u32 = 0;
                var max2: ?u32 = null;
                var ref_type2: wasm_core.types.RefType = .funcref;

                // Skip optional address type
                if (self.current_token == .identifier and
                    (std.mem.eql(u8, self.current_token.identifier, "i32") or
                    std.mem.eql(u8, self.current_token.identifier, "i64")))
                {
                    try self.advance();
                }

                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
                    min2 = std.math.cast(u32, val) orelse std.math.maxInt(u32);
                    try self.advance();
                }
                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
                    max2 = std.math.cast(u32, val) orelse std.math.maxInt(u32);
                    try self.advance();
                }
                if (self.current_token == .identifier) {
                    ref_type2 = try self.parseRefType();
                }

                const table_type = wasm_core.types.TableType{
                    .limits = .{ .min = min2, .max = max2 },
                    .ref_type = ref_type2,
                };

                try builder.imports.append(self.allocator, .{
                    .module_name = try self.allocator.dupe(u8, module_name),
                    .name = try self.allocator.dupe(u8, field_name2),
                    .desc = .{ .table = table_type },
                });

                // Register the export for this imported table
                const table_idx: u32 = builder.countImportTables() - 1;
                try builder.exports.append(self.allocator, .{
                    .name = export_name.?,
                    .desc = .{ .table = table_idx },
                });

                return;
            } else {
                // Not import, restore
                self.lexer.pos = saved_pos3;
                self.lexer.current_char = saved_char3;
                self.current_token = saved_token3;
            }
        }

        // Parse table limits and type
        var min: u32 = 0;
        var max: ?u32 = null;
        var ref_type: wasm_core.types.RefType = .funcref;

        // Optional table address type (i32 or i64 for table64)
        if (self.current_token == .identifier and
            (std.mem.eql(u8, self.current_token.identifier, "i32") or
            std.mem.eql(u8, self.current_token.identifier, "i64")))
        {
            try self.advance();
        }

        // Parse min (use u64 to handle table64 large values, clamp to u32)
        if (self.current_token == .number) {
            const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
            min = std.math.cast(u32, val) orelse std.math.maxInt(u32);
            try self.advance();
        }

        // Parse optional max
        if (self.current_token == .number) {
            const val = try std.fmt.parseInt(u64, self.current_token.number, 0);
            max = std.math.cast(u32, val) orelse std.math.maxInt(u32);
            try self.advance();
        }

        // Parse ref type (funcref, externref, or (ref null ...))
        if (self.current_token == .identifier) {
            ref_type = try self.parseRefType();
        } else if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref")) {
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "null")) {
                    try self.advance();
                    // Parse ref type or type index
                    if (self.current_token == .identifier) {
                        // Try to parse as ref type (func/funcref/extern/externref)
                        const is_ref_type = std.mem.eql(u8, self.current_token.identifier, "func") or
                            std.mem.eql(u8, self.current_token.identifier, "funcref") or
                            std.mem.eql(u8, self.current_token.identifier, "extern") or
                            std.mem.eql(u8, self.current_token.identifier, "externref");
                        if (is_ref_type) {
                            ref_type = try self.parseRefType();
                        } else {
                            // Type index ($t) - treat as funcref
                            ref_type = .funcref;
                            try self.advance();
                        }
                    } else if (self.current_token == .number) {
                        // Type index (number) - treat as funcref
                        ref_type = .funcref;
                        try self.advance();
                    }
                    try self.expectRightParen();
                } else if (self.current_token == .identifier) {
                    // (ref func) / (ref extern) / (ref $type) without null
                    ref_type = try self.parseRefType();
                    try self.expectRightParen();
                } else if (self.current_token == .number) {
                    // (ref 0) - numeric type index
                    ref_type = .funcref;
                    try self.advance();
                    try self.expectRightParen();
                } else {
                    // Not a recognized (ref ...), restore
                    self.lexer.pos = saved_pos;
                    self.lexer.current_char = saved_char;
                    self.current_token = saved_token;
                }
            } else {
                // Not (ref ...), restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Parse inline elem if present: (elem ...) or (ref.null ...)
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "elem")) {
                try self.advance();

                // Parse element initializers
                var init_list = std.ArrayList(wasm_core.types.InitExpression){};
                defer init_list.deinit(self.allocator);

                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (self.current_token == .identifier) {
                        if (self.builder.func_names.get(self.current_token.identifier)) |idx| {
                            try init_list.append(self.allocator, .{ .ref_func = idx });
                        }
                        try self.advance();
                    } else if (self.current_token == .number) {
                        const idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
                        try init_list.append(self.allocator, .{ .ref_func = idx });
                        try self.advance();
                    } else if (self.current_token == .left_paren) {
                        // Handle (ref.func $f) and (ref.null func) expressions
                        try self.advance();
                        if (self.current_token == .identifier) {
                            const expr_kw = self.current_token.identifier;
                            if (std.mem.eql(u8, expr_kw, "ref.func")) {
                                try self.advance();
                                const idx = try self.parseU32OrIdentifier();
                                try init_list.append(self.allocator, .{ .ref_func = idx });
                            } else if (std.mem.eql(u8, expr_kw, "ref.null")) {
                                try self.advance();
                                // Consume the type argument (func/extern/etc.)
                                _ = try self.parseRefType();
                                try init_list.append(self.allocator, .{ .ref_null = .funcref });
                            } else {
                                // Unknown expression, skip content
                                try self.advance();
                            }
                        }
                        try self.expectRightParen();
                    } else {
                        break;
                    }
                }

                try self.expectRightParen();

                // Update limits if not specified
                if (min == 0 and max == null) {
                    min = @intCast(init_list.items.len);
                    max = min;
                }

                // Create table
                _ = @as(u32, @intCast(builder.tables.items.len));
                try builder.tables.append(self.allocator, .{
                    .ref_type = ref_type,
                    .limits = .{ .min = min, .max = max },
                });

                // Create element segment
            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref")) {
                // (ref null func) or (ref.null func)
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "null")) {
                    try self.advance();
                }
                const init_ref_type = try self.parseRefType();
                try self.expectRightParen();

                // Single ref.null initializer
                if (min == 0) min = 1;

                const table_idx: u32 = builder.countImportTables() + @as(u32, @intCast(builder.tables.items.len));
                try builder.tables.append(self.allocator, .{
                    .ref_type = ref_type,
                    .limits = .{ .min = min, .max = max },
                });

                const element = wasm_core.types.Element{
                    .type = init_ref_type,
                    .init = &.{.{ .ref_null = init_ref_type }},
                    .mode = .{ .active = .{ .table_idx = table_idx, .offset = .{ .i32_const = 0 } } },
                };
                try builder.elements.append(self.allocator, element);

                if (export_name) |name| {
                    try builder.exports.append(self.allocator, .{
                        .name = try self.allocator.dupe(u8, name),
                        .desc = .{ .table = table_idx },
                    });
                }
                return;
            } else {
                // Not elem, restore and skip
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;

                var depth: u32 = 1;
                try self.advance();
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
            }
        }

        const table_idx: u32 = builder.countImportTables() + @as(u32, @intCast(builder.tables.items.len));
        try builder.tables.append(self.allocator, .{
            .ref_type = ref_type,
            .limits = .{ .min = min, .max = max },
        });

        // Add export if present
        if (export_name) |name| {
            try builder.exports.append(self.allocator, .{
                .name = name,
                .desc = .{ .table = table_idx },
            });
        }
    }

    fn parseMemory(self: *Parser, builder: *ModuleBuilder) !void {
        // Optional memory name (e.g., $mem0)
        const memory_name = try self.parseOptionalName();

        // Check if it's an import
        if (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance();
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "import")) {
                try self.advance(); // consume 'import'

                // Parse module name
                if (self.current_token != .string) return TextDecodeError.UnexpectedToken;
                const module_name = try self.allocator.dupe(u8, self.current_token.string);
                try self.advance();

                // Parse field name
                if (self.current_token != .string) return TextDecodeError.UnexpectedToken;
                const field_name = try self.allocator.dupe(u8, self.current_token.string);
                try self.advance();

                try self.expectRightParen();

                // Parse memory limits (min max)
                // Two formats:
                //   (memory $name (import ...) <limits>)  - limits directly after import
                //   (memory $name (import ...) (memory <limits>))  - nested memory form
                var min: u32 = 1;
                var max: ?u32 = null;
                var need_inner_close = false;

                if (self.current_token == .left_paren) {
                    try self.advance(); // consume '('
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "memory")) {
                        try self.advance(); // consume 'memory'
                        need_inner_close = true;
                    }
                }

                // Optional memory type (i32 or i64 for memory64)
                var is_64 = false;
                if (self.current_token == .identifier and
                    (std.mem.eql(u8, self.current_token.identifier, "i32") or
                    std.mem.eql(u8, self.current_token.identifier, "i64")))
                {
                    is_64 = std.mem.eql(u8, self.current_token.identifier, "i64");
                    try self.advance();
                }

                if (self.current_token == .number) {
                    min = @intCast(try std.fmt.parseInt(u64, self.current_token.number, 0));
                    try self.advance();
                }
                if (self.current_token == .number) {
                    max = @intCast(try std.fmt.parseInt(u64, self.current_token.number, 0));
                    try self.advance();
                }

                if (need_inner_close) {
                    try self.expectRightParen();
                }

                // Add to imports
                try builder.imports.append(self.allocator, .{
                    .module_name = module_name,
                    .name = field_name,
                    .desc = .{ .memory = .{ .limits = .{ .min = min, .max = max }, .is_64 = is_64 } },
                });
                return;
            } else {
                // Not import, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Process inline exports like (export "mem")
        var export_name: ?[]const u8 = null;
        while (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance(); // consume '('
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "export")) {
                try self.advance(); // consume 'export'
                if (self.current_token == .string) {
                    export_name = try self.allocator.dupe(u8, self.current_token.string);
                    try self.advance();
                }
                try self.expectRightParen();
            } else {
                // Not export, restore and skip
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;

                var depth: u32 = 1;
                try self.advance();
                while (depth > 0 and self.current_token != .eof) {
                    if (self.current_token == .left_paren) depth += 1 else if (self.current_token == .right_paren) depth -= 1;
                    if (depth > 0) try self.advance();
                }
                if (self.current_token == .right_paren) try self.advance();
            }
        }

        // Optional memory type (i32 or i64 for memory64)
        var is_64 = false;
        if (self.current_token == .identifier and
            (std.mem.eql(u8, self.current_token.identifier, "i32") or
                std.mem.eql(u8, self.current_token.identifier, "i64")))
        {
            is_64 = std.mem.eql(u8, self.current_token.identifier, "i64");
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
            .is_64 = is_64,
        };

        const mem_idx: u32 = builder.countImportMemories() + @as(u32, @intCast(builder.memories.items.len));
        try builder.memories.append(builder.allocator, memory_type);

        // Register memory name if present
        if (memory_name) |name| {
            try builder.memory_names.put(name, mem_idx);
        }

        // Add export if present
        if (export_name) |name| {
            try builder.exports.append(self.allocator, .{
                .name = name,
                .desc = .{ .memory = mem_idx },
            });
        }
    }

    fn parseData(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse: (data (i32.const offset) "string" "string" ...)
        // or: (data $name (i32.const offset) "string" ...)
        // or: (data (memory idx) (i32.const offset) "string" ...)
        // or: (data (offset (i32.const offset)) "string" ...)

        // Skip optional data name ($name)
        _ = try self.parseOptionalName();

        var offset: wasm_core.types.InitExpression = .{ .i32_const = 0 };
        var mem_idx: u32 = 0;

        // Check for (memory idx) or (offset ...) or offset expression
        while (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;

            try self.advance(); // consume '('

            if (self.current_token != .identifier) {
                // Restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
                break;
            }

            const instr_name = self.current_token.identifier;

            if (std.mem.eql(u8, instr_name, "memory")) {
                try self.advance();
                // Parse memory index
                if (self.current_token == .number) {
                    mem_idx = try std.fmt.parseInt(u32, self.current_token.number, 0);
                    try self.advance();
                } else if (self.current_token == .identifier) {
                    if (self.builder.memory_names.get(self.current_token.identifier)) |idx| {
                        mem_idx = idx;
                    }
                    try self.advance();
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, instr_name, "offset")) {
                try self.advance();
                offset = try self.parseInitExpression();
                try self.expectRightParen(); // close offset
                break;
            } else {
                // Restore and parse as init expression
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
                offset = try self.parseInitExpression();
                break;
            }
        }

        // Parse data strings (can be multiple)
        var data_list: std.ArrayList(u8) = .{};
        defer data_list.deinit(self.allocator);

        while (self.current_token == .string) {
            const unescaped = try self.unescapeString(self.current_token.string);
            defer self.allocator.free(unescaped);
            try data_list.appendSlice(self.allocator, unescaped);
            try self.advance();
        }

        const data = wasm_core.types.Data{
            .init = try data_list.toOwnedSlice(self.allocator),
            .mode = .{
                .active = .{
                    .mem_idx = mem_idx,
                    .offset = offset,
                },
            },
        };

        try builder.datas.append(builder.allocator, data);
    }

    fn unescapeString(self: *Parser, input: []const u8) ![]const u8 {
        var result: std.ArrayList(u8) = .{};
        var i: usize = 0;
        while (i < input.len) : (i += 1) {
            if (input[i] == '\\' and i + 1 < input.len) {
                i += 1;
                const next = input[i];
                const is_hex_digit = (next >= '0' and next <= '9') or
                    (next >= 'a' and next <= 'f') or
                    (next >= 'A' and next <= 'F');
                if (is_hex_digit and i + 1 < input.len) {
                    // Hex escape: \XX
                    const hex_str = input[i .. i + 2];
                    const byte = std.fmt.parseInt(u8, hex_str, 16) catch {
                        try result.append(self.allocator, '\\');
                        try result.append(self.allocator, next);
                        continue;
                    };
                    try result.append(self.allocator, byte);
                    i += 1;
                } else {
                    switch (next) {
                        'n' => try result.append(self.allocator, '\n'),
                        't' => try result.append(self.allocator, '\t'),
                        'r' => try result.append(self.allocator, '\r'),
                        '\\' => try result.append(self.allocator, '\\'),
                        '"' => try result.append(self.allocator, '"'),
                        else => {
                            try result.append(self.allocator, '\\');
                            try result.append(self.allocator, next);
                        },
                    }
                }
            } else {
                try result.append(self.allocator, input[i]);
            }
        }
        return result.toOwnedSlice(self.allocator);
    }

    fn parseFunction(self: *Parser, builder: *ModuleBuilder) !void {
        // Parse function: (func [name] [(export "name")] [(param ...)] [(result ...)] [...body])

        // Clear function-local state
        self.local_names.clearRetainingCapacity();
        self.label_stack.clearRetainingCapacity();

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

        // Parse function attributes (export, param, result, local, type)
        var explicit_type_idx: ?u32 = null;
        while (self.current_token == .left_paren) {
            // Peek to see if next token is an attribute
            const next = try self.peekToken();
            if (next == .identifier) {
                const keyword = next.identifier;
                const is_attribute = std.mem.eql(u8, keyword, "export") or
                    std.mem.eql(u8, keyword, "import") or
                    std.mem.eql(u8, keyword, "param") or
                    std.mem.eql(u8, keyword, "result") or
                    std.mem.eql(u8, keyword, "local") or
                    std.mem.eql(u8, keyword, "type");
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
            } else if (std.mem.eql(u8, keyword, "import")) {
                // Inline import: (func $name (import "mod" "name") ...)
                // Convert to regular import and return early
                try self.advance(); // consume 'import'

                var module_name: []const u8 = "";
                var field_name: []const u8 = "";

                if (self.current_token == .string) {
                    module_name = self.current_token.string;
                    try self.advance();
                }
                if (self.current_token == .string) {
                    field_name = self.current_token.string;
                    try self.advance();
                }
                try self.expectRightParen();

                // Continue parsing type/params/results after import
                // Reuses outer explicit_type_idx (this path returns early)
                while (self.current_token == .left_paren) {
                    const next_tok = try self.peekToken();
                    if (next_tok == .identifier) {
                        const kw = next_tok.identifier;
                        if (!std.mem.eql(u8, kw, "param") and !std.mem.eql(u8, kw, "result") and !std.mem.eql(u8, kw, "type")) {
                            break;
                        }
                    } else break;

                    try self.advance(); // consume '('
                    const kw = self.current_token.identifier;

                    if (std.mem.eql(u8, kw, "type")) {
                        try self.advance(); // consume 'type'
                        explicit_type_idx = try self.parseU32OrIdentifier();
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, kw, "param")) {
                        try self.advance();
                        while (self.current_token != .right_paren and self.current_token != .eof) {
                            if (self.current_token == .identifier) {
                                const type_name = self.current_token.identifier;
                                if (type_name.len > 0 and type_name[0] == '$') {
                                    const param_idx: u32 = @intCast(params.items.len);
                                    try self.local_names.put(type_name, param_idx);
                                    try self.advance();
                                    continue;
                                }
                            }
                            if (try self.parseValueType()) |vtype| {
                                try params.append(self.allocator, vtype);
                            } else {
                                try self.advance();
                            }
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, kw, "result")) {
                        try self.advance();
                        while (self.current_token != .right_paren and self.current_token != .eof) {
                            if (try self.parseValueType()) |vtype| {
                                try results.append(self.allocator, vtype);
                            } else {
                                try self.advance();
                            }
                        }
                        try self.expectRightParen();
                    }
                }

                // Use explicit type index if provided, otherwise create new type
                const type_idx: u32 = if (explicit_type_idx) |tidx| tidx else blk: {
                    const func_type = wasm_core.types.FuncType{
                        .parameter_types = try self.allocator.dupe(wasm_core.types.ValueType, params.items),
                        .result_types = try self.allocator.dupe(wasm_core.types.ValueType, results.items),
                    };
                    const idx: u32 = @intCast(builder.types.items.len);
                    try builder.types.append(self.allocator, func_type);
                    break :blk idx;
                };

                const import_desc = wasm_core.types.ImportDesc{
                    .function = type_idx,
                };
                const import = wasm_core.types.Import{
                    .module_name = try self.allocator.dupe(u8, module_name),
                    .name = try self.allocator.dupe(u8, field_name),
                    .desc = import_desc,
                };
                try builder.imports.append(self.allocator, import);

                if (func_name) |name| {
                    const func_idx: u32 = builder.countImportFuncs() - 1;
                    try builder.func_names.put(name, func_idx);
                }

                return;
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
                    }
                    // Parse type (handles both identifier and (ref ...) syntax)
                    if (try self.parseValueType()) |vtype| {
                        try params.append(self.allocator, vtype);
                    } else if (self.current_token == .identifier) {
                        std.debug.print("Error: Unknown parameter type: {s}\n", .{self.current_token.identifier});
                        return TextDecodeError.UnexpectedToken;
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
                    } else {
                        std.debug.print("Error: Unknown result type at line {d}\n", .{self.lexer.line});
                        return TextDecodeError.UnexpectedToken;
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
                    }
                    // Parse type (handles both identifier and (ref ...) syntax)
                    if (try self.parseValueType()) |vtype| {
                        try locals.append(self.allocator, vtype);
                    } else {
                        try self.advance();
                    }
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, keyword, "type")) {
                try self.advance(); // consume 'type'
                explicit_type_idx = try self.parseU32OrIdentifier();
                try self.expectRightParen();
            }
        }

        // Parse remaining function body (instructions) - any instructions after attributes
        try self.parseInstructions(&instructions);

        // Add implicit 'end' instruction at the end of function body
        try instructions.append(self.allocator, .end);

        // Fix block/loop/if end positions
        try self.fixBlockEnds(instructions.items);

        // Use explicit type index if provided, otherwise create new type
        const func_type_idx: u32 = if (explicit_type_idx) |tidx| tidx else blk: {
            const ft = wasm_core.types.FuncType{
                .parameter_types = try self.allocator.dupe(wasm_core.types.ValueType, params.items),
                .result_types = try self.allocator.dupe(wasm_core.types.ValueType, results.items),
            };
            const idx: u32 = @intCast(builder.types.items.len);
            try builder.types.append(self.allocator, ft);
            break :blk idx;
        };

        // Create function
        const func = wasm_core.types.Func{
            .type = func_type_idx,
            .locals = try self.allocator.dupe(wasm_core.types.ValueType, locals.items),
            .body = try self.allocator.dupe(wasm_core.types.Instruction, instructions.items),
        };
        try builder.funcs.append(self.allocator, func);

        // Register function name if present
        if (func_name) |name| {
            const func_idx: u32 = builder.countImportFuncs() + @as(u32, @intCast(builder.funcs.items.len)) - 1;
            try builder.func_names.put(name, func_idx);
        }

        // Add export if present
        if (export_name) |name| {
            const func_idx: u32 = builder.countImportFuncs() + @as(u32, @intCast(builder.funcs.items.len)) - 1;
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

    /// Parse a value type (i32, i64, f32, f64, v128, funcref, externref, ref)
    fn parseValueType(self: *Parser) !?wasm_core.types.ValueType {
        // Handle (ref ...) syntax
        if (self.current_token == .left_paren) {
            try self.advance(); // consume '('
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "ref")) {
                try self.advance(); // consume 'ref'
                // Skip optional 'null' in (ref null <heaptype>)
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "null")) {
                    try self.advance();
                }
                if (self.current_token == .identifier) {
                    const ref_type = self.current_token.identifier;
                    try self.advance();
                    try self.expectRightParen();
                    // Map ref types to basic reference types
                    if (std.mem.eql(u8, ref_type, "extern")) {
                        return .extern_ref;
                    } else if (std.mem.eql(u8, ref_type, "func")) {
                        return .func_ref;
                    }
                    // For other ref types (GC types, $name), return externref as fallback
                    return .extern_ref;
                } else if (self.current_token == .number) {
                    // Numeric type index (ref null 0) - treat as funcref
                    try self.advance();
                    try self.expectRightParen();
                    return .func_ref;
                }
            }
            return null;
        }

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
        else if (std.mem.eql(u8, type_name, "anyref"))
            .extern_ref
        else if (std.mem.eql(u8, type_name, "exnref"))
            .extern_ref
        else if (std.mem.eql(u8, type_name, "nullexnref"))
            .extern_ref
        else if (std.mem.eql(u8, type_name, "nullfuncref"))
            .func_ref
        else if (std.mem.eql(u8, type_name, "nullexternref"))
            .extern_ref
        else if (std.mem.eql(u8, type_name, "nullref"))
            .extern_ref
        else
            return null;

        try self.advance();
        return vtype;
    }

    /// Parse block type: checks for (type ...), (param ...), (result ...) and returns appropriate BlockType.
    /// Handles all valid combinations including (type $idx) followed by trailing (param ...)/(result ...) annotations.
    fn parseBlockType(self: *Parser) !wasm_core.types.Instruction.BlockType {
        // Skip label if present (e.g., $l)
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
        }

        // Check if next token is '('
        if (self.current_token != .left_paren) {
            return .empty;
        }

        // Save lexer state for potential backtrack (before any '(' consumption)
        const original_lexer_pos = self.lexer.pos;
        const original_lexer_char = self.lexer.current_char;
        const original_token = self.current_token;

        // Try to parse (type ...) first
        var has_type_index: ?u32 = null;

        try self.advance(); // consume '('

        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
            try self.advance(); // consume 'type'

            var type_idx: u32 = 0;
            if (self.current_token == .number) {
                type_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
                try self.advance();
            } else if (self.current_token == .identifier) {
                if (self.builder.type_names.get(self.current_token.identifier)) |idx| {
                    type_idx = idx;
                }
                try self.advance();
            }

            try self.expectRightParen(); // consume ')'
            has_type_index = type_idx;
            // Fall through to consume trailing (param ...)/(result ...) annotations
        } else {
            // Not (type ...) - restore state before param/result loop
            self.lexer.pos = original_lexer_pos;
            self.lexer.current_char = original_lexer_char;
            self.current_token = original_token;
        }

        // Parse zero or more (param ...) clauses
        var param_types = std.ArrayList(wasm_core.types.ValueType){};
        defer param_types.deinit(self.allocator);

        while (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_tok = self.current_token;

            try self.advance(); // consume '('
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "param")) {
                try self.advance(); // consume 'param'

                // Skip optional param name ($name)
                if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
                    try self.advance();
                }

                while (self.current_token != .right_paren) {
                    if (try self.parseValueType()) |vtype| {
                        try param_types.append(self.allocator, vtype);
                    } else {
                        break;
                    }
                }
                try self.expectRightParen(); // consume ')'
            } else {
                // Not a (param ...), restore and break
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_tok;
                break;
            }
        }

        // Parse zero or more (result ...) clauses
        var result_types = std.ArrayList(wasm_core.types.ValueType){};
        defer result_types.deinit(self.allocator);

        while (self.current_token == .left_paren) {
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_tok = self.current_token;

            try self.advance(); // consume '('
            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "result")) {
                try self.advance(); // consume 'result'
                while (self.current_token != .right_paren) {
                    if (try self.parseValueType()) |vtype| {
                        try result_types.append(self.allocator, vtype);
                    } else {
                        break;
                    }
                }
                try self.expectRightParen(); // consume ')'
            } else {
                // Not a (result ...), restore and break
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_tok;
                break;
            }
        }

        // Return appropriate BlockType
        if (has_type_index) |type_idx| {
            // (type $idx) was present - trailing annotations already consumed
            return .{ .type_index = type_idx };
        }

        if (param_types.items.len == 0 and result_types.items.len == 0) {
            // No type annotations found - restore original state
            self.lexer.pos = original_lexer_pos;
            self.lexer.current_char = original_lexer_char;
            self.current_token = original_token;
            return .empty;
        }

        if (param_types.items.len == 0 and result_types.items.len == 1) {
            return .{ .value_type = result_types.items[0] };
        }

        // Multiple params/results or params with results - create a function type
        const func_type = wasm_core.types.FuncType{
            .parameter_types = if (param_types.items.len > 0)
                try self.allocator.dupe(wasm_core.types.ValueType, param_types.items)
            else
                &.{},
            .result_types = if (result_types.items.len > 0)
                try self.allocator.dupe(wasm_core.types.ValueType, result_types.items)
            else
                &.{},
        };
        const type_idx: u32 = @intCast(self.builder.types.items.len);
        try self.builder.types.append(self.allocator, func_type);
        return .{ .type_index = type_idx };
    }

    /// Expect and consume a right paren
    fn expectRightParen(self: *Parser) !void {
        if (self.current_token != .right_paren) {
            std.debug.print("Error: expectRightParen: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
        try self.advance();
    }

    /// Parse init expression for globals, data, and element offsets
    fn parseInitExpression(self: *Parser) !wasm_core.types.InitExpression {
        var instrs = std.ArrayList(wasm_core.types.Instruction){};
        defer instrs.deinit(self.allocator);

        // Parse instructions until we hit right paren or string
        while (self.current_token != .right_paren and self.current_token != .eof and self.current_token != .string) {
            if (self.current_token == .left_paren) {
                try self.advance();
                if (self.current_token == .identifier) {
                    const instr_name = self.current_token.identifier;
                    try self.advance();

                    if (std.mem.eql(u8, instr_name, "i32.const")) {
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            try instrs.append(self.allocator, .{ .i32_const = val });
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "i64.const")) {
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(i64, self.current_token.number, 0);
                            try instrs.append(self.allocator, .{ .i64_const = val });
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "f32.const")) {
                        if (self.current_token == .number) {
                            const val = try numeric_parser.parseFloat(f32, self.current_token.number);
                            try instrs.append(self.allocator, .{ .f32_const = val });
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "f64.const")) {
                        if (self.current_token == .number) {
                            const val = try numeric_parser.parseFloat(f64, self.current_token.number);
                            try instrs.append(self.allocator, .{ .f64_const = val });
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "v128.const")) {
                        const val = try self.parseV128Const();
                        try instrs.append(self.allocator, .{ .v128_const = val });
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "ref.func")) {
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(u32, self.current_token.number, 10);
                            try instrs.append(self.allocator, .{ .ref_func = val });
                            try self.advance();
                        } else if (self.current_token == .identifier) {
                            // Named function reference - resolve to index
                            const name = self.current_token.identifier;
                            const func_idx = self.builder.func_names.get(name) orelse 0;
                            try instrs.append(self.allocator, .{ .ref_func = func_idx });
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "ref.null")) {
                        var rt: wasm_core.types.RefType = .funcref;
                        if (self.current_token == .identifier) {
                            const type_name = self.current_token.identifier;
                            if (std.mem.eql(u8, type_name, "extern") or std.mem.eql(u8, type_name, "externref")) {
                                rt = .externref;
                            }
                            try self.advance();
                        }
                        try instrs.append(self.allocator, .{ .ref_null = rt });
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "global.get")) {
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(u32, self.current_token.number, 10);
                            try instrs.append(self.allocator, .{ .global_get = val });
                            try self.advance();
                        } else if (self.current_token == .identifier) {
                            // TODO: Named global resolution
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (std.mem.eql(u8, instr_name, "i32.add") or
                        std.mem.eql(u8, instr_name, "i32.sub") or
                        std.mem.eql(u8, instr_name, "i32.mul") or
                        std.mem.eql(u8, instr_name, "i64.add") or
                        std.mem.eql(u8, instr_name, "i64.sub") or
                        std.mem.eql(u8, instr_name, "i64.mul"))
                    {
                        // Recursively parse operands
                        const sub_expr = try self.parseInitExpression();
                        // Flatten sub-expression instructions
                        switch (sub_expr) {
                            .instructions => |sub_instrs| {
                                try instrs.appendSlice(self.allocator, sub_instrs);
                            },
                            .i32_const => |v| try instrs.append(self.allocator, .{ .i32_const = v }),
                            .i64_const => |v| try instrs.append(self.allocator, .{ .i64_const = v }),
                            .f32_const => |v| try instrs.append(self.allocator, .{ .f32_const = v }),
                            .f64_const => |v| try instrs.append(self.allocator, .{ .f64_const = v }),
                            .v128_const => |v| try instrs.append(self.allocator, .{ .v128_const = v }),
                            .ref_null => |v| try instrs.append(self.allocator, .{ .ref_null = v }),
                            .ref_func => |v| try instrs.append(self.allocator, .{ .ref_func = v }),
                            .global_get => |v| try instrs.append(self.allocator, .{ .global_get = v }),
                        }

                        // Add the operation
                        if (std.mem.eql(u8, instr_name, "i32.add")) {
                            try instrs.append(self.allocator, .i32_add);
                        } else if (std.mem.eql(u8, instr_name, "i32.sub")) {
                            try instrs.append(self.allocator, .i32_sub);
                        } else if (std.mem.eql(u8, instr_name, "i32.mul")) {
                            try instrs.append(self.allocator, .i32_mul);
                        } else if (std.mem.eql(u8, instr_name, "i64.add")) {
                            try instrs.append(self.allocator, .i64_add);
                        } else if (std.mem.eql(u8, instr_name, "i64.sub")) {
                            try instrs.append(self.allocator, .i64_sub);
                        } else if (std.mem.eql(u8, instr_name, "i64.mul")) {
                            try instrs.append(self.allocator, .i64_mul);
                        }

                        try self.expectRightParen();
                    } else {
                        // Unknown init expression instruction - skip tokens until closing paren
                        var depth: u32 = 1;
                        while (depth > 0 and self.current_token != .eof) {
                            if (self.current_token == .left_paren) {
                                depth += 1;
                            } else if (self.current_token == .right_paren) {
                                depth -= 1;
                            }
                            if (depth > 0) try self.advance();
                        }
                        if (self.current_token == .right_paren) try self.advance();
                    }
                }
            } else {
                break;
            }
        }

        // If single instruction, return as simple init expression
        if (instrs.items.len == 1) {
            const instr = instrs.items[0];
            return switch (instr) {
                .i32_const => |v| .{ .i32_const = v },
                .i64_const => |v| .{ .i64_const = v },
                .f32_const => |v| .{ .f32_const = v },
                .f64_const => |v| .{ .f64_const = v },
                .v128_const => |v| .{ .v128_const = v },
                .ref_null => |v| .{ .ref_null = v },
                .ref_func => |v| .{ .ref_func = v },
                .global_get => |v| .{ .global_get = v },
                else => .{ .instructions = try self.allocator.dupe(wasm_core.types.Instruction, instrs.items) },
            };
        }

        // Multiple instructions
        return .{ .instructions = try self.allocator.dupe(wasm_core.types.Instruction, instrs.items) };
    }

    /// Fix block/loop/if end positions after parsing
    fn fixBlockEnds(self: *Parser, instructions: []wasm_core.types.Instruction) !void {
        var nested_blocks = std.ArrayList(u32){};
        defer nested_blocks.deinit(self.allocator);

        for (instructions, 0..) |*inst, i| {
            const pos: u32 = @intCast(i);

            if (inst.* == .block or inst.* == .loop or inst.* == .@"if") {
                try nested_blocks.append(self.allocator, pos);
            } else if (inst.* == .@"else") {
                if (nested_blocks.items.len > 0) {
                    const idx = nested_blocks.getLast();
                    if (instructions[idx] == .@"if") {
                        instructions[idx].@"if".@"else" = pos;
                    }
                }
            } else if (inst.* == .end) {
                if (nested_blocks.items.len > 0) {
                    const idx = nested_blocks.pop().?;
                    switch (instructions[idx]) {
                        .block => instructions[idx].block.end = pos,
                        .loop => instructions[idx].loop.end = pos,
                        .@"if" => instructions[idx].@"if".end = pos,
                        else => {},
                    }
                }
            }
        }
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
                // Skip $-prefixed label identifiers that appear as bare tokens
                if (self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                    try self.advance();
                    continue;
                }
                // Check if this is a block/loop/if instruction and extract label name
                const instr_name = self.current_token.identifier;
                const is_block_instr = std.mem.eql(u8, instr_name, "block") or
                    std.mem.eql(u8, instr_name, "loop") or
                    std.mem.eql(u8, instr_name, "if");

                var label_name: ?[]const u8 = null;
                if (is_block_instr) {
                    // Peek ahead to check for label name
                    const saved_pos = self.lexer.pos;
                    const saved_char = self.lexer.current_char;
                    const saved_token = self.current_token;

                    try self.advance(); // skip instruction name
                    if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                        label_name = self.current_token.identifier;
                    }

                    // Restore position
                    self.lexer.pos = saved_pos;
                    self.lexer.current_char = saved_char;
                    self.current_token = saved_token;

                    // Push label to stack
                    try self.label_stack.append(self.allocator, label_name);
                }

                // Bare instruction like nop or i32.add
                if (try self.parseInstruction()) |instr| {
                    try instructions.append(self.allocator, instr);

                    // Pop label stack on 'end' instruction
                    if (std.meta.activeTag(instr) == .end and self.label_stack.items.len > 0) {
                        _ = self.label_stack.pop();
                    }
                }
            } else {
                std.debug.print("Error: Unexpected token in instruction sequence at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
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
        } else if (std.mem.eql(u8, instr_name, "param")) {
            // 'param' is a type declaration, not an instruction - skip it
            return null;
        } else if (std.mem.eql(u8, instr_name, "type")) {
            // 'type' is a type reference, not an instruction - skip it
            return null;
        } else if (std.mem.eql(u8, instr_name, "then")) {
            // 'then' is part of if structure, not a standalone instruction - skip it
            return null;
        } else if (std.mem.eql(u8, instr_name, "local")) {
            // 'local' is a local declaration, not an instruction - skip it
            return null;
        }

        // Validate instruction name using instruction_map
        // This provides early error detection for unknown instructions
        const instruction_map = @import("./instruction_map.zig");
        const opcode = instruction_map.opcodeFromName(instr_name) orelse {
            // Unknown instruction (e.g., call_ref, return_call, throw, try_table)
            // Return null to skip - callers handle this gracefully
            return null;
        };

        // Control instructions
        switch (opcode) {
            .nop => return .nop,
            .@"unreachable" => return .@"unreachable",
            .block => {
                _ = try self.parseOptionalName();
                return .{ .block = .{ .type = try self.parseBlockType(), .end = 0 } };
            },
            .loop => {
                _ = try self.parseOptionalName();
                return .{ .loop = .{ .type = try self.parseBlockType(), .end = 0 } };
            },
            .@"if" => {
                _ = try self.parseOptionalName();
                return .{ .@"if" = .{ .type = try self.parseBlockType(), .@"else" = null, .end = 0 } };
            },
            .@"else" => return .@"else",
            .end => return .end,
            .@"return" => return .@"return",

            // Branch instructions
            .br => return .{ .br = try self.parseLabelIndex() },
            .br_if => return .{ .br_if = try self.parseLabelIndex() },
            .br_table => {
                // Parse br_table: (br_table label1 label2 ... default)
                var label_idxs = std.ArrayList(u32){};
                defer label_idxs.deinit(self.allocator);

                // Parse all label indices (last one is the default)
                while (self.current_token == .number or self.current_token == .identifier) {
                    const idx = try self.parseLabelIndex();
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
            },

            // Parametric instructions
            .drop => return .drop,
            .select => {
                // Check for optional (result ...)
                // We need to peek ahead to see if it's (result ...) without consuming tokens
                const next = try self.peekToken();
                if (next == .left_paren) {
                    // Save lexer state to peek further
                    const saved_pos = self.lexer.pos;
                    const saved_char = self.lexer.current_char;

                    // Peek past the '('
                    _ = try self.lexer.nextToken(); // consume '(' in lexer
                    const after_paren = try self.lexer.peekToken();

                    // Restore lexer state
                    self.lexer.pos = saved_pos;
                    self.lexer.current_char = saved_char;

                    // Check if it's 'result'
                    if (after_paren == .identifier and std.mem.eql(u8, after_paren.identifier, "result")) {
                        // It's (result ...), so parse it
                        try self.advance(); // consume '('
                        try self.advance(); // consume 'result'

                        var types = std.ArrayList(wasm_core.types.ValueType){};
                        defer types.deinit(self.allocator);

                        while (self.current_token != .right_paren and self.current_token != .eof) {
                            if (try self.parseValueType()) |vtype| {
                                try types.append(self.allocator, vtype);
                            } else {
                                break;
                            }
                        }

                        try self.expectRightParen();

                        const types_slice = try self.allocator.dupe(wasm_core.types.ValueType, types.items);
                        return .{ .selectv = types_slice };
                    }
                    // Not (result ...), so it's a plain select followed by another S-expression
                }
                return .select;
            },
            .selectv => {
                // Parse types for selectv
                var types = std.ArrayList(wasm_core.types.ValueType){};
                defer types.deinit(self.allocator);

                while (self.current_token != .right_paren and self.current_token != .eof) {
                    if (try self.parseValueType()) |vtype| {
                        try types.append(self.allocator, vtype);
                    } else {
                        break;
                    }
                }

                const types_slice = try self.allocator.dupe(wasm_core.types.ValueType, types.items);
                return .{ .selectv = types_slice };
            },

            // Call instructions
            .call => return .{ .call = try self.parseU32OrIdentifier() },
            .call_indirect => {
                // Parse optional table name, (type idx) or inline (param)/(result)
                var type_idx: u32 = 0;
                var table_idx: u32 = 0;

                // Check for table name or number first: call_indirect $table (type ...)
                if (self.isName() or self.current_token == .number) {
                    table_idx = try self.parseU32OrIdentifier();
                }

                // Check for (type idx) or (param)/(result) or (table idx)
                if (self.current_token == .left_paren) {
                    const saved_lexer_pos = self.lexer.pos;
                    const saved_lexer_char = self.lexer.current_char;
                    const saved_token = self.current_token;

                    try self.advance(); // consume '('
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
                        try self.advance(); // consume 'type'

                        // Parse type index or name
                        if (self.current_token == .number) {
                            type_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
                            try self.advance();
                        } else if (self.current_token == .identifier) {
                            // Resolve type name
                            if (self.builder.type_names.get(self.current_token.identifier)) |idx| {
                                type_idx = idx;
                            }
                            try self.advance();
                        }

                        try self.expectRightParen();

                        // Consume trailing (param ...)/(result ...) annotations after (type ...)
                        while (self.current_token == .left_paren) {
                            const trail_pos = self.lexer.pos;
                            const trail_char = self.lexer.current_char;
                            const trail_tok = self.current_token;

                            try self.advance(); // consume '('
                            if (self.current_token == .identifier and
                                (std.mem.eql(u8, self.current_token.identifier, "param") or
                                std.mem.eql(u8, self.current_token.identifier, "result")))
                            {
                                // Skip to closing paren
                                try self.advance(); // consume 'param'/'result'
                                // Skip optional param name
                                if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
                                    try self.advance();
                                }
                                while (self.current_token != .right_paren) {
                                    try self.advance();
                                }
                                try self.expectRightParen(); // consume ')'
                            } else {
                                // Not param/result, restore and break
                                self.lexer.pos = trail_pos;
                                self.lexer.current_char = trail_char;
                                self.current_token = trail_tok;
                                break;
                            }
                        }
                    } else if (self.current_token == .identifier and
                        (std.mem.eql(u8, self.current_token.identifier, "param") or
                            std.mem.eql(u8, self.current_token.identifier, "result")))
                    {
                        // Inline function type - parse as BlockType and create a type
                        self.lexer.pos = saved_lexer_pos;
                        self.lexer.current_char = saved_lexer_char;
                        self.current_token = saved_token;

                        const block_type = try self.parseBlockType();
                        if (block_type == .type_index) {
                            type_idx = block_type.type_index;
                        } else {
                            // Create a new type for this inline signature
                            const func_type = switch (block_type) {
                                .empty => wasm_core.types.FuncType{ .parameter_types = &.{}, .result_types = &.{} },
                                .value_type => |vt| wasm_core.types.FuncType{
                                    .parameter_types = &.{},
                                    .result_types = try self.allocator.dupe(wasm_core.types.ValueType, &.{vt}),
                                },
                                .type_index => unreachable,
                            };
                            type_idx = @intCast(self.builder.types.items.len);
                            try self.builder.types.append(self.allocator, func_type);
                        }
                    } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "table")) {
                        // (table idx) without type specification - use type 0
                        try self.advance(); // consume 'table'
                        table_idx = try self.parseU32OrIdentifier();
                        try self.expectRightParen();
                    } else {
                        // No type specification, restore and use type 0
                        self.lexer.pos = saved_lexer_pos;
                        self.lexer.current_char = saved_lexer_char;
                        self.current_token = saved_token;
                    }
                }

                // Optional (table idx) if not already parsed
                if (table_idx == 0 and self.current_token == .left_paren) {
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
            },

            // reference instructions
            .ref_null => return .{ .ref_null = try self.parseRefType() },
            .ref_is_null => return .ref_is_null,
            .ref_func => return .{ .ref_func = try self.parseU32OrIdentifier() },

            // Variable instructions
            .local_get => return .{ .local_get = try self.parseU32OrIdentifier() },
            .local_set => return .{ .local_set = try self.parseU32OrIdentifier() },
            .local_tee => return .{ .local_tee = try self.parseU32OrIdentifier() },
            .global_get => return .{ .global_get = try self.parseU32OrIdentifier() },
            .global_set => return .{ .global_set = try self.parseU32OrIdentifier() },
            // table instructions
            .table_get => return .{ .table_get = try self.parseOptionalTableIdx() },
            .table_set => return .{ .table_set = try self.parseOptionalTableIdx() },
            .table_size => return .{ .table_size = try self.parseOptionalTableIdx() },
            .table_grow => return .{ .table_grow = try self.parseOptionalTableIdx() },
            .table_fill => return .{ .table_fill = try self.parseOptionalTableIdx() },
            .table_copy => {
                const dst = try self.parseOptionalTableIdx();
                const src = try self.parseOptionalTableIdx();
                return .{ .table_copy = .{ .table_idx_dst = dst, .table_idx_src = src } };
            },
            .table_init => {
                const first_idx = try self.parseU32OrIdentifier();
                const second_idx = try self.parseOptionalTableIdx();
                // If second index exists, first is table_idx; otherwise first is elem_idx
                return if (second_idx != 0 or self.current_token == .number)
                    .{ .table_init = .{ .table_idx = first_idx, .elem_idx = second_idx } }
                else
                    .{ .table_init = .{ .table_idx = 0, .elem_idx = first_idx } };
            },
            .elem_drop => return .{ .elem_drop = try self.parseU32OrIdentifier() },

            // Memory instructions
            .i32_load => return .{ .i32_load = try self.parseMemArg() },
            .i64_load => return .{ .i64_load = try self.parseMemArg() },
            .f32_load => return .{ .f32_load = try self.parseMemArg() },
            .f64_load => return .{ .f64_load = try self.parseMemArg() },
            .i32_load8_s => return .{ .i32_load8_s = try self.parseMemArg() },
            .i32_load8_u => return .{ .i32_load8_u = try self.parseMemArg() },
            .i32_load16_s => return .{ .i32_load16_s = try self.parseMemArg() },
            .i32_load16_u => return .{ .i32_load16_u = try self.parseMemArg() },
            .i64_load8_s => return .{ .i64_load8_s = try self.parseMemArg() },
            .i64_load8_u => return .{ .i64_load8_u = try self.parseMemArg() },
            .i64_load16_s => return .{ .i64_load16_s = try self.parseMemArg() },
            .i64_load16_u => return .{ .i64_load16_u = try self.parseMemArg() },
            .i64_load32_s => return .{ .i64_load32_s = try self.parseMemArg() },
            .i64_load32_u => return .{ .i64_load32_u = try self.parseMemArg() },
            .i32_store => return .{ .i32_store = try self.parseMemArg() },
            .i64_store => return .{ .i64_store = try self.parseMemArg() },
            .f32_store => return .{ .f32_store = try self.parseMemArg() },
            .f64_store => return .{ .f64_store = try self.parseMemArg() },
            .i32_store8 => return .{ .i32_store8 = try self.parseMemArg() },
            .i32_store16 => return .{ .i32_store16 = try self.parseMemArg() },
            .i64_store8 => return .{ .i64_store8 = try self.parseMemArg() },
            .i64_store16 => return .{ .i64_store16 = try self.parseMemArg() },
            .i64_store32 => return .{ .i64_store32 = try self.parseMemArg() },
            .memory_size => return .{ .memory_size = try self.parseOptionalMemIdx() },
            .memory_grow => return .{ .memory_grow = try self.parseOptionalMemIdx() },
            .memory_init => {
                const data_idx = try self.parseU32OrIdentifier();
                return .{ .memory_init = .{ .data_idx = data_idx, .mem_idx = try self.parseOptionalMemIdx() } };
            },
            .memory_copy => {
                const mem_idx_dst = try self.parseOptionalMemIdx();
                const mem_idx_src = try self.parseOptionalMemIdx();
                return .{ .memory_copy = .{ .mem_idx_dst = mem_idx_dst, .mem_idx_src = mem_idx_src } };
            },
            .memory_fill => return .{ .memory_fill = try self.parseOptionalMemIdx() },
            .data_drop => return .{ .data_drop = try self.parseU32OrIdentifier() },

            // Numeric const instructions
            .i32_const => {
                if (self.current_token != .number) {
                    return TextDecodeError.InvalidNumber;
                }
                const val = try numeric_parser.parseInteger(i32, self.current_token.number);
                try self.advance();
                return .{ .i32_const = val };
            },
            .i64_const => {
                if (self.current_token != .number) {
                    return TextDecodeError.InvalidNumber;
                }
                const val = try numeric_parser.parseInteger(i64, self.current_token.number);
                try self.advance();
                return .{ .i64_const = val };
            },
            .f32_const => return .{ .f32_const = try self.parseFloat(f32) },
            .f64_const => return .{ .f64_const = try self.parseFloat(f64) },
            .v128_const => {
                const result = try self.parseV128Const();
                return .{ .v128_const = result };
            },

            // i32 comparison
            .i32_eqz => return .i32_eqz,
            .i32_eq => return .i32_eq,
            .i32_ne => return .i32_ne,
            .i32_lt_s => return .i32_lt_s,
            .i32_lt_u => return .i32_lt_u,
            .i32_gt_s => return .i32_gt_s,
            .i32_gt_u => return .i32_gt_u,
            .i32_le_s => return .i32_le_s,
            .i32_le_u => return .i32_le_u,
            .i32_ge_s => return .i32_ge_s,
            .i32_ge_u => return .i32_ge_u,

            // i64 comparison
            .i64_eqz => return .i64_eqz,
            .i64_eq => return .i64_eq,
            .i64_ne => return .i64_ne,
            .i64_lt_s => return .i64_lt_s,
            .i64_lt_u => return .i64_lt_u,
            .i64_gt_s => return .i64_gt_s,
            .i64_gt_u => return .i64_gt_u,
            .i64_le_s => return .i64_le_s,
            .i64_le_u => return .i64_le_u,
            .i64_ge_s => return .i64_ge_s,
            .i64_ge_u => return .i64_ge_u,

            // f32 comparison
            .f32_eq => return .f32_eq,
            .f32_ne => return .f32_ne,
            .f32_lt => return .f32_lt,
            .f32_gt => return .f32_gt,
            .f32_le => return .f32_le,
            .f32_ge => return .f32_ge,

            // f64 comparison
            .f64_eq => return .f64_eq,
            .f64_ne => return .f64_ne,
            .f64_lt => return .f64_lt,
            .f64_gt => return .f64_gt,
            .f64_le => return .f64_le,
            .f64_ge => return .f64_ge,

            // i32 arithmetic
            .i32_clz => return .i32_clz,
            .i32_ctz => return .i32_ctz,
            .i32_popcnt => return .i32_popcnt,
            .i32_add => return .i32_add,
            .i32_sub => return .i32_sub,
            .i32_mul => return .i32_mul,
            .i32_div_s => return .i32_div_s,
            .i32_div_u => return .i32_div_u,
            .i32_rem_s => return .i32_rem_s,
            .i32_rem_u => return .i32_rem_u,
            .i32_and => return .i32_and,
            .i32_or => return .i32_or,
            .i32_xor => return .i32_xor,
            .i32_shl => return .i32_shl,
            .i32_shr_s => return .i32_shr_s,
            .i32_shr_u => return .i32_shr_u,
            .i32_rotl => return .i32_rotl,
            .i32_rotr => return .i32_rotr,

            // i64 arithmetic
            .i64_clz => return .i64_clz,
            .i64_ctz => return .i64_ctz,
            .i64_popcnt => return .i64_popcnt,
            .i64_add => return .i64_add,
            .i64_sub => return .i64_sub,
            .i64_mul => return .i64_mul,
            .i64_div_s => return .i64_div_s,
            .i64_div_u => return .i64_div_u,
            .i64_rem_s => return .i64_rem_s,
            .i64_rem_u => return .i64_rem_u,
            .i64_and => return .i64_and,
            .i64_or => return .i64_or,
            .i64_xor => return .i64_xor,
            .i64_shl => return .i64_shl,
            .i64_shr_s => return .i64_shr_s,
            .i64_shr_u => return .i64_shr_u,
            .i64_rotl => return .i64_rotl,
            .i64_rotr => return .i64_rotr,

            // f32 arithmetic
            .f32_abs => return .f32_abs,
            .f32_neg => return .f32_neg,
            .f32_ceil => return .f32_ceil,
            .f32_floor => return .f32_floor,
            .f32_trunc => return .f32_trunc,
            .f32_nearest => return .f32_nearest,
            .f32_sqrt => return .f32_sqrt,
            .f32_add => return .f32_add,
            .f32_sub => return .f32_sub,
            .f32_mul => return .f32_mul,
            .f32_div => return .f32_div,
            .f32_min => return .f32_min,
            .f32_max => return .f32_max,
            .f32_copy_sign => return .f32_copy_sign,

            // f64 arithmetic
            .f64_abs => return .f64_abs,
            .f64_neg => return .f64_neg,
            .f64_ceil => return .f64_ceil,
            .f64_floor => return .f64_floor,
            .f64_trunc => return .f64_trunc,
            .f64_nearest => return .f64_nearest,
            .f64_sqrt => return .f64_sqrt,
            .f64_add => return .f64_add,
            .f64_sub => return .f64_sub,
            .f64_mul => return .f64_mul,
            .f64_div => return .f64_div,
            .f64_min => return .f64_min,
            .f64_max => return .f64_max,
            .f64_copy_sign => return .f64_copy_sign,

            // Numeric conversion
            .i32_wrap_i64 => return .i32_wrap_i64,
            .i32_trunc_f32_s => return .i32_trunc_f32_s,
            .i32_trunc_f32_u => return .i32_trunc_f32_u,
            .i32_trunc_f64_s => return .i32_trunc_f64_s,
            .i32_trunc_f64_u => return .i32_trunc_f64_u,
            .i32_trunc_sat_f32_s => return .i32_trunc_sat_f32_s,
            .i32_trunc_sat_f32_u => return .i32_trunc_sat_f32_u,
            .i32_trunc_sat_f64_s => return .i32_trunc_sat_f64_s,
            .i32_trunc_sat_f64_u => return .i32_trunc_sat_f64_u,
            .i64_extend_i32_s => return .i64_extend_i32_s,
            .i64_extend_i32_u => return .i64_extend_i32_u,
            .i64_trunc_f32_s => return .i64_trunc_f32_s,
            .i64_trunc_f32_u => return .i64_trunc_f32_u,
            .i64_trunc_f64_s => return .i64_trunc_f64_s,
            .i64_trunc_f64_u => return .i64_trunc_f64_u,
            .i64_trunc_sat_f32_s => return .i64_trunc_sat_f32_s,
            .i64_trunc_sat_f32_u => return .i64_trunc_sat_f32_u,
            .i64_trunc_sat_f64_s => return .i64_trunc_sat_f64_s,
            .i64_trunc_sat_f64_u => return .i64_trunc_sat_f64_u,
            .f32_convert_i32_s => return .f32_convert_i32_s,
            .f32_convert_i32_u => return .f32_convert_i32_u,
            .f32_convert_i64_s => return .f32_convert_i64_s,
            .f32_convert_i64_u => return .f32_convert_i64_u,
            .f32_demote_f64 => return .f32_demote_f64,
            .f64_convert_i32_s => return .f64_convert_i32_s,
            .f64_convert_i32_u => return .f64_convert_i32_u,
            .f64_convert_i64_s => return .f64_convert_i64_s,
            .f64_convert_i64_u => return .f64_convert_i64_u,
            .f64_promote_f32 => return .f64_promote_f32,
            .i32_reinterpret_f32 => return .i32_reinterpret_f32,
            .i64_reinterpret_f64 => return .i64_reinterpret_f64,
            .f32_reinterpret_i32 => return .f32_reinterpret_i32,
            .f64_reinterpret_i64 => return .f64_reinterpret_i64,

            // Sign extension instructions (WebAssembly 2.0)
            .i32_extend8_s => return .i32_extend8_s,
            .i32_extend16_s => return .i32_extend16_s,
            .i64_extend8_s => return .i64_extend8_s,
            .i64_extend16_s => return .i64_extend16_s,
            .i64_extend32_s => return .i64_extend32_s,

            // v128 memory instructions
            .v128_load => return .{ .v128_load = try self.parseMemArg() },
            .v128_load8x8_s => return .{ .v128_load8x8_s = try self.parseMemArg() },
            .v128_load8x8_u => return .{ .v128_load8x8_u = try self.parseMemArg() },
            .v128_load16x4_s => return .{ .v128_load16x4_s = try self.parseMemArg() },
            .v128_load16x4_u => return .{ .v128_load16x4_u = try self.parseMemArg() },
            .v128_load32x2_s => return .{ .v128_load32x2_s = try self.parseMemArg() },
            .v128_load32x2_u => return .{ .v128_load32x2_u = try self.parseMemArg() },
            .v128_load8_splat => return .{ .v128_load8_splat = try self.parseMemArg() },
            .v128_load16_splat => return .{ .v128_load16_splat = try self.parseMemArg() },
            .v128_load32_splat => return .{ .v128_load32_splat = try self.parseMemArg() },
            .v128_load64_splat => return .{ .v128_load64_splat = try self.parseMemArg() },
            .v128_store => return .{ .v128_store = try self.parseMemArg() },
            .v128_load8_lane => return .{ .v128_load8_lane = try self.parseMemArgWithLaneIdx() },
            .v128_load16_lane => return .{ .v128_load16_lane = try self.parseMemArgWithLaneIdx() },
            .v128_load32_lane => return .{ .v128_load32_lane = try self.parseMemArgWithLaneIdx() },
            .v128_load64_lane => return .{ .v128_load64_lane = try self.parseMemArgWithLaneIdx() },
            .v128_store8_lane => return .{ .v128_store8_lane = try self.parseMemArgWithLaneIdx() },
            .v128_store16_lane => return .{ .v128_store16_lane = try self.parseMemArgWithLaneIdx() },
            .v128_store32_lane => return .{ .v128_store32_lane = try self.parseMemArgWithLaneIdx() },
            .v128_store64_lane => return .{ .v128_store64_lane = try self.parseMemArgWithLaneIdx() },
            .v128_load32_zero => return .{ .v128_load32_zero = try self.parseMemArg() },
            .v128_load64_zero => return .{ .v128_load64_zero = try self.parseMemArg() },

            // v128 element manipulation instructions
            .i8x16_shuffle => return .{ .i8x16_shuffle = try self.parseLaneIdx16() },
            .i8x16_swizzle => return .i8x16_swizzle,
            .i8x16_splat => return .i8x16_splat,
            .i16x8_splat => return .i16x8_splat,
            .i32x4_splat => return .i32x4_splat,
            .i64x2_splat => return .i64x2_splat,
            .f32x4_splat => return .f32x4_splat,
            .f64x2_splat => return .f64x2_splat,
            .i8x16_extract_lane_s => return .{ .i8x16_extract_lane_s = try self.parseLaneIdx() },
            .i8x16_extract_lane_u => return .{ .i8x16_extract_lane_u = try self.parseLaneIdx() },
            .i8x16_replace_lane => return .{ .i8x16_replace_lane = try self.parseLaneIdx() },
            .i16x8_extract_lane_s => return .{ .i16x8_extract_lane_s = try self.parseLaneIdx() },
            .i16x8_extract_lane_u => return .{ .i16x8_extract_lane_u = try self.parseLaneIdx() },
            .i16x8_replace_lane => return .{ .i16x8_replace_lane = try self.parseLaneIdx() },
            .i32x4_extract_lane => return .{ .i32x4_extract_lane = try self.parseLaneIdx() },
            .i32x4_replace_lane => return .{ .i32x4_replace_lane = try self.parseLaneIdx() },
            .i64x2_extract_lane => return .{ .i64x2_extract_lane = try self.parseLaneIdx() },
            .i64x2_replace_lane => return .{ .i64x2_replace_lane = try self.parseLaneIdx() },
            .f32x4_extract_lane => return .{ .f32x4_extract_lane = try self.parseLaneIdx() },
            .f32x4_replace_lane => return .{ .f32x4_replace_lane = try self.parseLaneIdx() },
            .f64x2_extract_lane => return .{ .f64x2_extract_lane = try self.parseLaneIdx() },
            .f64x2_replace_lane => return .{ .f64x2_replace_lane = try self.parseLaneIdx() },

            // v128 comparison instructions - i8x16
            .i8x16_eq => return .i8x16_eq,
            .i8x16_ne => return .i8x16_ne,
            .i8x16_lt_s => return .i8x16_lt_s,
            .i8x16_lt_u => return .i8x16_lt_u,
            .i8x16_gt_s => return .i8x16_gt_s,
            .i8x16_gt_u => return .i8x16_gt_u,
            .i8x16_le_s => return .i8x16_le_s,
            .i8x16_le_u => return .i8x16_le_u,
            .i8x16_ge_s => return .i8x16_ge_s,
            .i8x16_ge_u => return .i8x16_ge_u,

            // i16x8
            .i16x8_eq => return .i16x8_eq,
            .i16x8_ne => return .i16x8_ne,
            .i16x8_lt_s => return .i16x8_lt_s,
            .i16x8_lt_u => return .i16x8_lt_u,
            .i16x8_gt_s => return .i16x8_gt_s,
            .i16x8_gt_u => return .i16x8_gt_u,
            .i16x8_le_s => return .i16x8_le_s,
            .i16x8_le_u => return .i16x8_le_u,
            .i16x8_ge_s => return .i16x8_ge_s,
            .i16x8_ge_u => return .i16x8_ge_u,

            // i32x4
            .i32x4_eq => return .i32x4_eq,
            .i32x4_ne => return .i32x4_ne,
            .i32x4_lt_s => return .i32x4_lt_s,
            .i32x4_lt_u => return .i32x4_lt_u,
            .i32x4_gt_s => return .i32x4_gt_s,
            .i32x4_gt_u => return .i32x4_gt_u,
            .i32x4_le_s => return .i32x4_le_s,
            .i32x4_le_u => return .i32x4_le_u,
            .i32x4_ge_s => return .i32x4_ge_s,
            .i32x4_ge_u => return .i32x4_ge_u,

            // i64x2
            .i64x2_eq => return .i64x2_eq,
            .i64x2_ne => return .i64x2_ne,
            .i64x2_lt_s => return .i64x2_lt_s,
            .i64x2_gt_s => return .i64x2_gt_s,
            .i64x2_le_s => return .i64x2_le_s,
            .i64x2_ge_s => return .i64x2_ge_s,

            // f32x4
            .f32x4_eq => return .f32x4_eq,
            .f32x4_ne => return .f32x4_ne,
            .f32x4_lt => return .f32x4_lt,
            .f32x4_gt => return .f32x4_gt,
            .f32x4_le => return .f32x4_le,
            .f32x4_ge => return .f32x4_ge,

            // f64x2
            .f64x2_eq => return .f64x2_eq,
            .f64x2_ne => return .f64x2_ne,
            .f64x2_lt => return .f64x2_lt,
            .f64x2_gt => return .f64x2_gt,
            .f64x2_le => return .f64x2_le,
            .f64x2_ge => return .f64x2_ge,

            // v128 logical operations
            .v128_not => return .v128_not,
            .v128_and => return .v128_and,
            .v128_andnot => return .v128_andnot,
            .v128_or => return .v128_or,
            .v128_xor => return .v128_xor,
            .v128_bitselect => return .v128_bitselect,

            // v128 test instructions
            .v128_any_true => return .v128_any_true,
            .i8x16_all_true => return .i8x16_all_true,
            .i16x8_all_true => return .i16x8_all_true,
            .i32x4_all_true => return .i32x4_all_true,
            .i64x2_all_true => return .i64x2_all_true,
            .i8x16_bitmask => return .i8x16_bitmask,
            .i16x8_bitmask => return .i16x8_bitmask,
            .i32x4_bitmask => return .i32x4_bitmask,
            .i64x2_bitmask => return .i64x2_bitmask,

            // v128 integer arithmetic - unary operations
            .i8x16_abs => return .i8x16_abs,
            .i16x8_abs => return .i16x8_abs,
            .i32x4_abs => return .i32x4_abs,
            .i64x2_abs => return .i64x2_abs,
            .i8x16_neg => return .i8x16_neg,
            .i16x8_neg => return .i16x8_neg,
            .i32x4_neg => return .i32x4_neg,
            .i64x2_neg => return .i64x2_neg,
            .i8x16_popcnt => return .i8x16_popcnt,

            // shift operations
            .i8x16_shl => return .i8x16_shl,
            .i16x8_shl => return .i16x8_shl,
            .i32x4_shl => return .i32x4_shl,
            .i64x2_shl => return .i64x2_shl,
            .i8x16_shr_s => return .i8x16_shr_s,
            .i16x8_shr_s => return .i16x8_shr_s,
            .i32x4_shr_s => return .i32x4_shr_s,
            .i64x2_shr_s => return .i64x2_shr_s,
            .i8x16_shr_u => return .i8x16_shr_u,
            .i16x8_shr_u => return .i16x8_shr_u,
            .i32x4_shr_u => return .i32x4_shr_u,
            .i64x2_shr_u => return .i64x2_shr_u,

            // add/sub operations
            .i8x16_add => return .i8x16_add,
            .i16x8_add => return .i16x8_add,
            .i32x4_add => return .i32x4_add,
            .i64x2_add => return .i64x2_add,
            .i8x16_sub => return .i8x16_sub,
            .i16x8_sub => return .i16x8_sub,
            .i32x4_sub => return .i32x4_sub,
            .i64x2_sub => return .i64x2_sub,

            // saturating add/sub
            .i8x16_add_sat_s => return .i8x16_add_sat_s,
            .i16x8_add_sat_s => return .i16x8_add_sat_s,
            .i8x16_add_sat_u => return .i8x16_add_sat_u,
            .i16x8_add_sat_u => return .i16x8_add_sat_u,
            .i8x16_sub_sat_s => return .i8x16_sub_sat_s,
            .i16x8_sub_sat_s => return .i16x8_sub_sat_s,
            .i8x16_sub_sat_u => return .i8x16_sub_sat_u,
            .i16x8_sub_sat_u => return .i16x8_sub_sat_u,

            // mul operations
            .i16x8_mul => return .i16x8_mul,
            .i32x4_mul => return .i32x4_mul,
            .i64x2_mul => return .i64x2_mul,

            // min/max operations
            .i8x16_min_s => return .i8x16_min_s,
            .i16x8_min_s => return .i16x8_min_s,
            .i32x4_min_s => return .i32x4_min_s,
            .i8x16_min_u => return .i8x16_min_u,
            .i16x8_min_u => return .i16x8_min_u,
            .i32x4_min_u => return .i32x4_min_u,
            .i8x16_max_s => return .i8x16_max_s,
            .i16x8_max_s => return .i16x8_max_s,
            .i32x4_max_s => return .i32x4_max_s,
            .i8x16_max_u => return .i8x16_max_u,
            .i16x8_max_u => return .i16x8_max_u,
            .i32x4_max_u => return .i32x4_max_u,

            // avgr operations
            .i8x16_avgr_u => return .i8x16_avgr_u,
            .i16x8_avgr_u => return .i16x8_avgr_u,

            // special operations
            .i16x8_q15mulr_sat_s => return .i16x8_q15mulr_sat_s,
            .i32x4_dot_i16x8_s => return .i32x4_dot_i16x8_s,

            // extmul operations
            .i16x8_extmul_low_i8x16_s => return .i16x8_extmul_low_i8x16_s,
            .i16x8_extmul_high_i8x16_s => return .i16x8_extmul_high_i8x16_s,
            .i16x8_extmul_low_i8x16_u => return .i16x8_extmul_low_i8x16_u,
            .i16x8_extmul_high_i8x16_u => return .i16x8_extmul_high_i8x16_u,
            .i32x4_extmul_low_i16x8_s => return .i32x4_extmul_low_i16x8_s,
            .i32x4_extmul_high_i16x8_s => return .i32x4_extmul_high_i16x8_s,
            .i32x4_extmul_low_i16x8_u => return .i32x4_extmul_low_i16x8_u,
            .i32x4_extmul_high_i16x8_u => return .i32x4_extmul_high_i16x8_u,
            .i64x2_extmul_low_i32x4_s => return .i64x2_extmul_low_i32x4_s,
            .i64x2_extmul_high_i32x4_s => return .i64x2_extmul_high_i32x4_s,
            .i64x2_extmul_low_i32x4_u => return .i64x2_extmul_low_i32x4_u,
            .i64x2_extmul_high_i32x4_u => return .i64x2_extmul_high_i32x4_u,

            // extadd_pairwise operations
            .i16x8_extadd_pairwise_i8x16_s => return .i16x8_extadd_pairwise_i8x16_s,
            .i16x8_extadd_pairwise_i8x16_u => return .i16x8_extadd_pairwise_i8x16_u,
            .i32x4_extadd_pairwise_i16x8_s => return .i32x4_extadd_pairwise_i16x8_s,
            .i32x4_extadd_pairwise_i16x8_u => return .i32x4_extadd_pairwise_i16x8_u,

            // v128 floating-point arithmetic - unary operations
            .f32x4_abs => return .f32x4_abs,
            .f64x2_abs => return .f64x2_abs,
            .f32x4_neg => return .f32x4_neg,
            .f64x2_neg => return .f64x2_neg,
            .f32x4_sqrt => return .f32x4_sqrt,
            .f64x2_sqrt => return .f64x2_sqrt,
            .f32x4_ceil => return .f32x4_ceil,
            .f64x2_ceil => return .f64x2_ceil,
            .f32x4_floor => return .f32x4_floor,
            .f64x2_floor => return .f64x2_floor,
            .f32x4_trunc => return .f32x4_trunc,
            .f64x2_trunc => return .f64x2_trunc,
            .f32x4_nearest => return .f32x4_nearest,
            .f64x2_nearest => return .f64x2_nearest,

            // binary operations
            .f32x4_add => return .f32x4_add,
            .f64x2_add => return .f64x2_add,
            .f32x4_sub => return .f32x4_sub,
            .f64x2_sub => return .f64x2_sub,
            .f32x4_mul => return .f32x4_mul,
            .f64x2_mul => return .f64x2_mul,
            .f32x4_div => return .f32x4_div,
            .f64x2_div => return .f64x2_div,
            .f32x4_min => return .f32x4_min,
            .f64x2_min => return .f64x2_min,
            .f32x4_max => return .f32x4_max,
            .f64x2_max => return .f64x2_max,
            .f32x4_pmin => return .f32x4_pmin,
            .f64x2_pmin => return .f64x2_pmin,
            .f32x4_pmax => return .f32x4_pmax,
            .f64x2_pmax => return .f64x2_pmax,

            // v128 type conversion instructions
            .i32x4_trunc_sat_f32x4_s => return .i32x4_trunc_sat_f32x4_s,
            .i32x4_trunc_sat_f32x4_u => return .i32x4_trunc_sat_f32x4_u,
            .i32x4_trunc_sat_f64x2_s_zero => return .i32x4_trunc_sat_f64x2_s_zero,
            .i32x4_trunc_sat_f64x2_u_zero => return .i32x4_trunc_sat_f64x2_u_zero,
            .f32x4_convert_i32x4_s => return .f32x4_convert_i32x4_s,
            .f32x4_convert_i32x4_u => return .f32x4_convert_i32x4_u,
            .f64x2_convert_low_i32x4_s => return .f64x2_convert_low_i32x4_s,
            .f64x2_convert_low_i32x4_u => return .f64x2_convert_low_i32x4_u,
            .f32x4_demote_f64x2_zero => return .f32x4_demote_f64x2_zero,
            .f64x2_promote_low_f32x4 => return .f64x2_promote_low_f32x4,
            .i8x16_narrow_i16x8_s => return .i8x16_narrow_i16x8_s,
            .i8x16_narrow_i16x8_u => return .i8x16_narrow_i16x8_u,
            .i16x8_narrow_i32x4_s => return .i16x8_narrow_i32x4_s,
            .i16x8_narrow_i32x4_u => return .i16x8_narrow_i32x4_u,
            .i16x8_extend_low_i8x16_s => return .i16x8_extend_low_i8x16_s,
            .i16x8_extend_low_i8x16_u => return .i16x8_extend_low_i8x16_u,
            .i16x8_extend_high_i8x16_s => return .i16x8_extend_high_i8x16_s,
            .i16x8_extend_high_i8x16_u => return .i16x8_extend_high_i8x16_u,
            .i32x4_extend_low_i16x8_s => return .i32x4_extend_low_i16x8_s,
            .i32x4_extend_low_i16x8_u => return .i32x4_extend_low_i16x8_u,
            .i32x4_extend_high_i16x8_s => return .i32x4_extend_high_i16x8_s,
            .i32x4_extend_high_i16x8_u => return .i32x4_extend_high_i16x8_u,
            .i64x2_extend_low_i32x4_s => return .i64x2_extend_low_i32x4_s,
            .i64x2_extend_low_i32x4_u => return .i64x2_extend_low_i32x4_u,
            .i64x2_extend_high_i32x4_s => return .i64x2_extend_high_i32x4_s,
            .i64x2_extend_high_i32x4_u => return .i64x2_extend_high_i32x4_u,

            // Relaxed SIMD instructions (WebAssembly 2.0)
            .i8x16_relaxed_swizzle => return .i8x16_relaxed_swizzle,
            .i32x4_relaxed_trunc_f32x4_s => return .i32x4_relaxed_trunc_f32x4_s,
            .i32x4_relaxed_trunc_f32x4_u => return .i32x4_relaxed_trunc_f32x4_u,
            .i32x4_relaxed_trunc_f64x2_s_zero => return .i32x4_relaxed_trunc_f64x2_s_zero,
            .i32x4_relaxed_trunc_f64x2_u_zero => return .i32x4_relaxed_trunc_f64x2_u_zero,
            .f32x4_relaxed_madd => return .f32x4_relaxed_madd,
            .f32x4_relaxed_nmadd => return .f32x4_relaxed_nmadd,
            .f64x2_relaxed_madd => return .f64x2_relaxed_madd,
            .f64x2_relaxed_nmadd => return .f64x2_relaxed_nmadd,
            .i8x16_relaxed_laneselect => return .i8x16_relaxed_laneselect,
            .i16x8_relaxed_laneselect => return .i16x8_relaxed_laneselect,
            .i32x4_relaxed_laneselect => return .i32x4_relaxed_laneselect,
            .i64x2_relaxed_laneselect => return .i64x2_relaxed_laneselect,
            .f32x4_relaxed_min => return .f32x4_relaxed_min,
            .f32x4_relaxed_max => return .f32x4_relaxed_max,
            .f64x2_relaxed_min => return .f64x2_relaxed_min,
            .f64x2_relaxed_max => return .f64x2_relaxed_max,
            .i16x8_relaxed_q15mulr_s => return .i16x8_relaxed_q15mulr_s,
            .i16x8_relaxed_dot_i8x16_i7x16_s => return .i16x8_relaxed_dot_i8x16_i7x16_s,
            .i32x4_relaxed_dot_i8x16_i7x16_add_s => return .i32x4_relaxed_dot_i8x16_i7x16_add_s,
            .f32x4_relaxed_dot_bf16x8_add_f32x4 => return .f32x4_relaxed_dot_bf16x8_add_f32x4,
        }
    }

    /// Parse S-expression recursively: (instr arg1 arg2 ...)
    fn parseSExpression(self: *Parser, instructions: *std.ArrayList(wasm_core.types.Instruction)) !void {
        // Check if this is a block/loop/if instruction and extract label name
        var label_name: ?[]const u8 = null;
        var is_block_instr = false;
        if (self.current_token == .identifier) {
            const instr_name = self.current_token.identifier;
            is_block_instr = std.mem.eql(u8, instr_name, "block") or
                std.mem.eql(u8, instr_name, "loop") or
                std.mem.eql(u8, instr_name, "if");

            if (is_block_instr) {
                // Peek ahead to check for label name
                const saved_pos = self.lexer.pos;
                const saved_char = self.lexer.current_char;
                const saved_token = self.current_token;

                try self.advance(); // skip instruction name
                if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
                    label_name = self.current_token.identifier;
                }

                // Restore position
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;

                // Push label to stack
                try self.label_stack.append(self.allocator, label_name);
            }
        }

        // Parse the instruction
        const instr = try self.parseInstruction() orelse {
            // Skip to matching closing paren, tracking nested parentheses
            var depth: u32 = 0;
            while (self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    if (depth == 0) break;
                    depth -= 1;
                }
                try self.advance();
            }
            // Pop label if we pushed one
            if (is_block_instr and self.label_stack.items.len > 0) {
                _ = self.label_stack.pop();
            }
            return;
        };

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
            // Pop label stack
            if (self.label_stack.items.len > 0) {
                _ = self.label_stack.pop();
            }
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
            // Pop label stack
            if (self.label_stack.items.len > 0) {
                _ = self.label_stack.pop();
            }
            return;
        }

        // Special handling for 'if' instruction with folded form: (if (result) (cond) (then ...) (else ...))
        if (std.meta.activeTag(instr) == .@"if") {
            // Parse condition expression (only if not followed by 'then')
            while (self.current_token == .left_paren) {
                // Peek ahead to check if it's (then ...)
                const saved_pos = self.lexer.pos;
                const saved_char = self.lexer.current_char;
                const saved_token = self.current_token;

                try self.advance(); // consume '('
                const is_then = self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "then");

                // Restore position
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;

                if (is_then) break;

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
                    std.debug.print("Error: parseSExpression: Expected 'then' block at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
            // Pop label stack
            if (self.label_stack.items.len > 0) {
                _ = self.label_stack.pop();
            }
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

    /// Parse label index for br/br_if instructions
    fn parseLabelIndex(self: *Parser) !u32 {
        if (self.current_token == .number) {
            const num = try std.fmt.parseInt(u32, self.current_token.number, 10);
            try self.advance();
            return num;
        } else if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            try self.advance();
            // Search label stack from top (innermost) to bottom (outermost)
            var depth: u32 = 0;
            var i = self.label_stack.items.len;
            while (i > 0) {
                i -= 1;
                if (self.label_stack.items[i]) |label_name| {
                    if (std.mem.eql(u8, label_name, id)) {
                        return depth;
                    }
                }
                depth += 1;
            }
            // If not found, return 0 as fallback
            return 0;
        }
        std.debug.print("Error: parseLabelIndex: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
        return TextDecodeError.UnexpectedToken;
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
            // Resolve named index - check locals first, then functions, then memories
            if (self.local_names.get(id)) |idx| {
                return idx;
            }
            if (self.builder.func_names.get(id)) |idx| {
                return idx;
            }
            if (self.builder.memory_names.get(id)) |idx| {
                return idx;
            }
            if (self.builder.type_names.get(id)) |idx| {
                return idx;
            }
            // If not found, return 0 as fallback
            return 0;
        }
        std.debug.print("Error: parseU32OrIdentifier: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
        return TextDecodeError.UnexpectedToken;
    }

    /// Check if current token is a name (identifier starting with '$')
    fn isName(self: *Parser) bool {
        return self.current_token == .identifier and
            self.current_token.identifier.len > 0 and
            self.current_token.identifier[0] == '$';
    }

    /// Parse optional name ($identifier), returns name and advances if present
    fn parseOptionalName(self: *Parser) !?[]const u8 {
        if (self.isName()) {
            const name = self.current_token.identifier;
            try self.advance();
            return name;
        }
        return null;
    }

    /// Parse ref type (funcref/externref or func/extern)
    fn parseRefType(self: *Parser) !wasm_core.types.RefType {
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (std.mem.eql(u8, id, "funcref")) {
                try self.advance();
                return .funcref;
            } else if (std.mem.eql(u8, id, "externref") or std.mem.eql(u8, id, "anyref")) {
                try self.advance();
                return .externref;
            } else if (std.mem.eql(u8, id, "func") or std.mem.eql(u8, id, "nofunc") or std.mem.eql(u8, id, "nullfuncref")) {
                try self.advance();
                return .funcref;
            } else if (std.mem.eql(u8, id, "extern") or std.mem.eql(u8, id, "noextern") or std.mem.eql(u8, id, "nullexternref")) {
                try self.advance();
                return .externref;
            } else if (std.mem.eql(u8, id, "any") or std.mem.eql(u8, id, "none") or
                std.mem.eql(u8, id, "exn") or std.mem.eql(u8, id, "noexn") or
                std.mem.eql(u8, id, "exnref") or std.mem.eql(u8, id, "nullexnref"))
            {
                try self.advance();
                return .funcref;
            } else if (id.len > 0 and id[0] == '$') {
                // Named type reference ($t) - treat as funcref
                try self.advance();
                return .funcref;
            }
        }
        return .funcref; // default
    }

    /// Parse optional table index (number or $name), returns 0 if not present
    fn parseOptionalTableIdx(self: *Parser) !u32 {
        if (self.current_token == .number or self.isName()) {
            return try self.parseU32OrIdentifier();
        }
        return 0;
    }

    /// Parse optional memory index (number or identifier), returns 0 if not present
    fn parseOptionalMemIdx(self: *Parser) !u32 {
        if (self.current_token == .number or self.current_token == .identifier) {
            return try self.parseU32OrIdentifier();
        }
        return 0;
    }

    /// Parse memory argument (offset and align)
    fn parseMemArg(self: *Parser) !wasm_core.types.Instruction.MemArg {
        var offset: u64 = 0;
        var alignment: u32 = 0;
        var mem_idx: u32 = 0;

        // Parse optional memory index (e.g., $mem1 or 1)
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (!std.mem.startsWith(u8, id, "offset=") and !std.mem.startsWith(u8, id, "align=")) {
                // This is a memory index
                if (self.builder.memory_names.get(id)) |idx| {
                    mem_idx = idx;
                }
                try self.advance();
            }
        } else if (self.current_token == .number) {
            mem_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
            try self.advance();
        }

        // Parse optional offset=N and align=N
        while (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (std.mem.startsWith(u8, id, "offset=")) {
                offset = try std.fmt.parseInt(u64, id[7..], 0);
                try self.advance();
            } else if (std.mem.startsWith(u8, id, "align=")) {
                const align_bytes = try std.fmt.parseInt(u32, id[6..], 0);
                // Alignment must be a power of 2 and at least 1
                if (align_bytes == 0 or (align_bytes & (align_bytes - 1)) != 0) {
                    return TextDecodeError.InvalidFormat;
                }
                // Convert byte alignment to log2 (e.g., 4 bytes -> 2)
                alignment = @ctz(align_bytes);
                try self.advance();
            } else {
                break;
            }
        }

        return wasm_core.types.Instruction.MemArg{
            .@"align" = alignment,
            .offset = offset,
            .mem_idx = mem_idx,
        };
    }

    /// Parse memory argument with lane index (offset, align, and lane)
    fn parseMemArgWithLaneIdx(self: *Parser) !wasm_core.types.Instruction.MemArgWithLaneIdx {
        var offset: u64 = 0;
        var alignment: u32 = 0;
        var lane_idx: u8 = 0;
        var mem_idx: u32 = 0;

        // Parse optional memory index (e.g., $mem1 or numeric index)
        // Must distinguish from lane_idx: if a number is followed by offset=/align=/another number, it's mem_idx
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (!std.mem.startsWith(u8, id, "offset=") and !std.mem.startsWith(u8, id, "align=")) {
                // This is a memory index name
                if (self.builder.memory_names.get(id)) |idx| {
                    mem_idx = idx;
                }
                try self.advance();
            }
        } else if (self.current_token == .number) {
            // Could be mem_idx or lane_idx - need lookahead
            const saved_pos = self.lexer.pos;
            const saved_char = self.lexer.current_char;
            const saved_token = self.current_token;
            const potential_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
            try self.advance();

            if (self.current_token == .identifier and
                (std.mem.startsWith(u8, self.current_token.identifier, "offset=") or
                std.mem.startsWith(u8, self.current_token.identifier, "align=")))
            {
                // Followed by memarg keywords -> this number is mem_idx
                mem_idx = potential_idx;
            } else if (self.current_token == .number) {
                // Followed by another number -> this number is mem_idx, next is lane_idx
                mem_idx = potential_idx;
            } else {
                // Not followed by memarg/number -> this is lane_idx, not mem_idx
                // Restore and let existing lane_idx parsing handle it
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }

        // Parse optional offset=N and align=N
        while (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            if (std.mem.startsWith(u8, id, "offset=")) {
                offset = try std.fmt.parseInt(u64, id[7..], 0);
                try self.advance();
            } else if (std.mem.startsWith(u8, id, "align=")) {
                const align_bytes = try std.fmt.parseInt(u32, id[6..], 0);
                // Alignment must be a power of 2 and at least 1
                if (align_bytes == 0 or (align_bytes & (align_bytes - 1)) != 0) {
                    return TextDecodeError.InvalidFormat;
                }
                // Convert byte alignment to log2 (e.g., 4 bytes -> 2)
                alignment = @ctz(align_bytes);
                try self.advance();
            } else {
                break;
            }
        }

        // Parse lane index (required)
        if (self.current_token == .number) {
            lane_idx = try std.fmt.parseInt(u8, self.current_token.number, 0);
            try self.advance();
        }

        return wasm_core.types.Instruction.MemArgWithLaneIdx{
            .@"align" = alignment,
            .offset = offset,
            .lane_idx = lane_idx,
            .mem_idx = mem_idx,
        };
    }

    /// Parse single lane index
    fn parseLaneIdx(self: *Parser) !u8 {
        if (self.current_token == .number) {
            const idx = try std.fmt.parseInt(u8, self.current_token.number, 0);
            try self.advance();
            return idx;
        }
        return TextDecodeError.InvalidNumber;
    }

    /// Parse 16 lane indices for i8x16.shuffle
    fn parseLaneIdx16(self: *Parser) ![16]u8 {
        var indices: [16]u8 = undefined;
        var i: usize = 0;
        while (i < 16) : (i += 1) {
            if (self.current_token == .number) {
                indices[i] = try std.fmt.parseInt(u8, self.current_token.number, 0);
                try self.advance();
            } else {
                return TextDecodeError.InvalidNumber;
            }
        }
        return indices;
    }

    /// Parse float
    fn parseFloat(self: *Parser, comptime T: type) !T {
        const input = if (self.current_token == .identifier)
            self.current_token.identifier
        else if (self.current_token == .number)
            self.current_token.number
        else
            return TextDecodeError.InvalidNumber;

        const val = try numeric_parser.parseFloat(T, input);
        try self.advance();
        return val;
    }

    /// Parse v128.const value: shape and values
    fn parseV128Const(self: *Parser) !i128 {
        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }
        const shape = self.current_token.identifier;
        try self.advance();

        var bytes: [16]u8 = undefined;

        if (std.mem.eql(u8, shape, "i8x16")) {
            for (0..16) |i| {
                const val = try numeric_parser.parseInteger(i8, self.current_token.number);
                bytes[i] = @bitCast(val);
                try self.advance();
            }
        } else if (std.mem.eql(u8, shape, "i16x8")) {
            for (0..8) |i| {
                const val = try numeric_parser.parseInteger(i16, self.current_token.number);
                const val_bytes: [2]u8 = @bitCast(val);
                bytes[i * 2] = val_bytes[0];
                bytes[i * 2 + 1] = val_bytes[1];
                try self.advance();
            }
        } else if (std.mem.eql(u8, shape, "i32x4")) {
            for (0..4) |i| {
                const val = try numeric_parser.parseInteger(i32, self.current_token.number);
                const val_bytes: [4]u8 = @bitCast(val);
                @memcpy(bytes[i * 4 .. i * 4 + 4], &val_bytes);
                try self.advance();
            }
        } else if (std.mem.eql(u8, shape, "i64x2")) {
            for (0..2) |i| {
                const val = try numeric_parser.parseInteger(i64, self.current_token.number);
                const val_bytes: [8]u8 = @bitCast(val);
                @memcpy(bytes[i * 8 .. i * 8 + 8], &val_bytes);
                try self.advance();
            }
        } else if (std.mem.eql(u8, shape, "f32x4")) {
            for (0..4) |i| {
                const val = try self.parseFloat(f32);
                const val_bytes: [4]u8 = @bitCast(val);
                @memcpy(bytes[i * 4 .. i * 4 + 4], &val_bytes);
            }
        } else if (std.mem.eql(u8, shape, "f64x2")) {
            for (0..2) |i| {
                const val = try self.parseFloat(f64);
                const val_bytes: [8]u8 = @bitCast(val);
                @memcpy(bytes[i * 8 .. i * 8 + 8], &val_bytes);
            }
        } else {
            return TextDecodeError.UnexpectedToken;
        }

        return @bitCast(bytes);
    }

    /// Parse a float value for Result context — handles nan:canonical/nan:arithmetic keywords
    fn parseFloatResult(self: *Parser, comptime T: type) !spec_types.command.FloatType(T) {
        const input = if (self.current_token == .identifier)
            self.current_token.identifier
        else if (self.current_token == .number)
            self.current_token.number
        else
            return TextDecodeError.UnexpectedToken;

        if (std.mem.eql(u8, input, "nan:canonical")) {
            try self.advance();
            return .nan_canonical;
        } else if (std.mem.eql(u8, input, "nan:arithmetic")) {
            try self.advance();
            return .nan_arithmetic;
        }

        const F = if (T == u32) f32 else f64;
        const val = try numeric_parser.parseFloat(F, input);
        try self.advance();
        return .{ .value = @bitCast(val) };
    }

    /// Parse v128.const for Result context — returns vec_f32/vec_f64 when NaN lanes are present
    fn parseV128ConstResult(self: *Parser) !spec_types.command.Result {
        if (self.current_token != .identifier) {
            return TextDecodeError.UnexpectedToken;
        }
        const shape = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, shape, "f32x4")) {
            var lanes: [4]spec_types.command.FloatType(u32) = undefined;
            var has_nan = false;
            for (0..4) |i| {
                lanes[i] = try self.parseFloatResult(u32);
                if (lanes[i] == .nan_canonical or lanes[i] == .nan_arithmetic) has_nan = true;
            }
            if (has_nan) {
                return .{ .vec_f32 = lanes };
            }
            var bytes: [16]u8 = undefined;
            for (0..4) |i| {
                const val_bytes: [4]u8 = @bitCast(lanes[i].value);
                @memcpy(bytes[i * 4 .. i * 4 + 4], &val_bytes);
            }
            return .{ .v128 = @bitCast(bytes) };
        } else if (std.mem.eql(u8, shape, "f64x2")) {
            var lanes: [2]spec_types.command.FloatType(u64) = undefined;
            var has_nan = false;
            for (0..2) |i| {
                lanes[i] = try self.parseFloatResult(u64);
                if (lanes[i] == .nan_canonical or lanes[i] == .nan_arithmetic) has_nan = true;
            }
            if (has_nan) {
                return .{ .vec_f64 = lanes };
            }
            var bytes: [16]u8 = undefined;
            for (0..2) |i| {
                const val_bytes: [8]u8 = @bitCast(lanes[i].value);
                @memcpy(bytes[i * 8 .. i * 8 + 8], &val_bytes);
            }
            return .{ .v128 = @bitCast(bytes) };
        } else {
            // Integer shapes — parse directly
            var bytes: [16]u8 = undefined;
            if (std.mem.eql(u8, shape, "i8x16")) {
                for (0..16) |i| {
                    const val = try numeric_parser.parseInteger(i8, self.current_token.number);
                    bytes[i] = @bitCast(val);
                    try self.advance();
                }
            } else if (std.mem.eql(u8, shape, "i16x8")) {
                for (0..8) |i| {
                    const val = try numeric_parser.parseInteger(i16, self.current_token.number);
                    const val_bytes: [2]u8 = @bitCast(val);
                    bytes[i * 2] = val_bytes[0];
                    bytes[i * 2 + 1] = val_bytes[1];
                    try self.advance();
                }
            } else if (std.mem.eql(u8, shape, "i32x4")) {
                for (0..4) |i| {
                    const val = try numeric_parser.parseInteger(i32, self.current_token.number);
                    const val_bytes: [4]u8 = @bitCast(val);
                    @memcpy(bytes[i * 4 .. i * 4 + 4], &val_bytes);
                    try self.advance();
                }
            } else if (std.mem.eql(u8, shape, "i64x2")) {
                for (0..2) |i| {
                    const val = try numeric_parser.parseInteger(i64, self.current_token.number);
                    const val_bytes: [8]u8 = @bitCast(val);
                    @memcpy(bytes[i * 8 .. i * 8 + 8], &val_bytes);
                    try self.advance();
                }
            } else {
                return TextDecodeError.UnexpectedToken;
            }
            return .{ .v128 = @bitCast(bytes) };
        }
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
    fn parseModuleCommand(self: *Parser) !spec_types.command.Command {
        const line = self.lexer.line;
        // current token is "module"
        // Need to include the '(' before "module"
        const module_start_pos = self.lexer.pos - "module".len - 1;
        try self.advance();

        // Skip optional "definition" keyword
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "definition")) {
            try self.advance();
        }

        // Check for optional module name
        var name: ?[]const u8 = null;
        if (self.current_token == .identifier) {
            const id = self.current_token.identifier;
            const is_field = std.mem.eql(u8, id, "func") or
                std.mem.eql(u8, id, "table") or
                std.mem.eql(u8, id, "memory") or
                std.mem.eql(u8, id, "data") or
                std.mem.eql(u8, id, "elem") or
                std.mem.eql(u8, id, "start") or
                std.mem.eql(u8, id, "export") or
                std.mem.eql(u8, id, "type") or
                std.mem.eql(u8, id, "import") or
                std.mem.eql(u8, id, "global") or
                std.mem.eql(u8, id, "tag") or
                std.mem.eql(u8, id, "rec") or
                std.mem.eql(u8, id, "binary") or
                std.mem.eql(u8, id, "quote") or
                std.mem.eql(u8, id, "instance");
            if (!is_field) {
                name = try self.allocator.dupe(u8, self.current_token.identifier);
                try self.advance();
            }
        }

        // Check for binary/quote/instance format
        if (self.current_token == .identifier) {
            const format_type = self.current_token.identifier;
            if (std.mem.eql(u8, format_type, "binary") or
                std.mem.eql(u8, format_type, "quote") or
                std.mem.eql(u8, format_type, "instance"))
            {
                try self.advance();

                if (std.mem.eql(u8, format_type, "binary")) {
                    // Collect binary data
                    var binary_data = std.ArrayList(u8){};
                    defer binary_data.deinit(self.allocator);

                    while (self.current_token == .string) {
                        const bytes = try parseBinaryString(self.allocator, self.current_token.string);
                        defer self.allocator.free(bytes);
                        try binary_data.appendSlice(self.allocator, bytes);
                        try self.advance();
                    }

                    try self.expectToken(.right_paren);

                    return spec_types.command.Command{
                        .module = .{
                            .line = line,
                            .file_name = try self.allocator.dupe(u8, ""),
                            .name = name,
                            .module_data = null,
                            .module_binary = try binary_data.toOwnedSlice(self.allocator),
                        },
                    };
                }

                // quote or instance format
                while (self.current_token == .string or self.current_token == .identifier) {
                    try self.advance();
                }
                try self.expectToken(.right_paren);

                if (std.mem.eql(u8, format_type, "quote")) {
                    return spec_types.command.Command{ .module_quote = {} };
                }

                return spec_types.command.Command{
                    .module = .{
                        .line = line,
                        .file_name = try self.allocator.dupe(u8, ""),
                        .name = name,
                        .module_data = null,
                        .module_binary = null,
                    },
                };
            }
        }

        // Parse text format module
        self.builder.clear();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.parseModuleField(self.builder);
        }

        try self.expectToken(.right_paren);
        const module_end_pos = self.lexer.pos;

        _ = try self.builder.build();

        const module_text = self.input[module_start_pos..module_end_pos];
        const module_data = try self.allocator.dupe(u8, module_text);

        return spec_types.command.Command{
            .module = .{
                .line = line,
                .file_name = try self.allocator.dupe(u8, ""),
                .name = name,
                .module_data = module_data,
                .module_binary = null,
            },
        };
    }

    /// Parse assert_return: (assert_return (invoke "func" ...) (result...))
    fn parseAssertReturn(self: *Parser) !spec_types.command.Command {
        // current token is "assert_return"
        const line = self.lexer.line;
        try self.advance();

        // Parse action
        const action = try self.parseAction();

        // Parse expected results
        var expected: std.ArrayList(spec_types.command.Result) = .{};
        defer expected.deinit(self.allocator);

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const result = try self.parseResult();
            try expected.append(self.allocator, result);
        }

        try self.expectToken(.right_paren);

        return spec_types.command.Command{
            .assert_return = .{
                .line = line,
                .action = action,
                .expected = try expected.toOwnedSlice(self.allocator),
            },
        };
    }

    /// Parse assert_trap: (assert_trap (invoke ...) "message") or (assert_trap (module ...) "message")
    fn parseAssertTrap(self: *Parser) !spec_types.command.Command {
        // current token is "assert_trap"
        const line = self.lexer.line;
        try self.advance();

        try self.expectToken(.left_paren);

        var action: spec_types.command.Action = undefined;
        var has_action = false;

        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "module")) {
            // Skip module
            var depth: u32 = 1;
            try self.advance();
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
        } else {
            // Parse action
            action = try self.parseActionInner();
            has_action = true;
            try self.expectToken(.right_paren);
        }

        // Parse failure message
        if (self.current_token != .string) {
            std.debug.print("Error: parseAssertTrap: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        if (!has_action) {
            action = spec_types.command.Action{
                .invoke = .{
                    .field = try self.allocator.dupe(u8, ""),
                    .args = &[_]spec_types.command.Value{},
                    .module = null,
                },
            };
        }

        return spec_types.command.Command{
            .assert_trap = .{
                .line = line,
                .action = action,
                .error_text = failure,
            },
        };
    }

    /// Parse assert_exhaustion: (assert_exhaustion (invoke ...) "message")
    fn parseAssertExhaustion(self: *Parser) !spec_types.command.Command {
        // current token is "assert_exhaustion"
        const line = self.lexer.line;
        try self.advance();

        // Parse action
        const action = try self.parseAction();

        // Parse failure message
        if (self.current_token != .string) {
            std.debug.print("Error: parseAssertExhaustion: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return spec_types.command.Command{
            .assert_exhaustion = .{
                .line = line,
                .action = action,
                .error_text = failure,
            },
        };
    }

    /// Parse assert_unlinkable/assert_uninstantiable: (assert_unlinkable (module ...) "message")
    fn parseAssertModuleError(self: *Parser, comptime tag: enum { assert_unlinkable, assert_uninstantiable }) !spec_types.command.Command {
        const line = self.lexer.line;
        try self.advance();

        // Expect (module ...) - skip the module content
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            return TextDecodeError.UnexpectedToken;
        }

        // Skip module content by counting parentheses
        var depth: u32 = 1;
        try self.advance();
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

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return switch (tag) {
            .assert_unlinkable => spec_types.command.Command{
                .assert_unlinkable = .{
                    .line = line,
                    .file_name = try self.allocator.dupe(u8, ""),
                    .error_text = failure,
                },
            },
            .assert_uninstantiable => spec_types.command.Command{
                .assert_uninstantiable = .{
                    .line = line,
                    .file_name = try self.allocator.dupe(u8, ""),
                    .error_text = failure,
                },
            },
        };
    }

    /// Convert escaped string to binary data
    fn parseBinaryString(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
        var result = std.ArrayList(u8){};
        var i: usize = 0;
        while (i < str.len) {
            if (str[i] == '\\' and i + 1 < str.len) {
                i += 1;
                const c = str[i];
                if (c >= '0' and c <= '9') {
                    // Hex escape: \XX
                    if (i + 1 < str.len) {
                        const hex_str = str[i .. i + 2];
                        const byte = try std.fmt.parseInt(u8, hex_str, 16);
                        try result.append(allocator, byte);
                        i += 2;
                    } else {
                        i += 1;
                    }
                } else if ((c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
                    // Hex escape: \XX
                    if (i + 1 < str.len) {
                        const hex_str = str[i .. i + 2];
                        const byte = try std.fmt.parseInt(u8, hex_str, 16);
                        try result.append(allocator, byte);
                        i += 2;
                    } else {
                        i += 1;
                    }
                } else {
                    // Other escapes
                    const byte: u8 = switch (c) {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        else => c,
                    };
                    try result.append(allocator, byte);
                    i += 1;
                }
            } else {
                try result.append(allocator, str[i]);
                i += 1;
            }
        }
        return result.toOwnedSlice(allocator);
    }

    /// Parse assert_invalid: (assert_invalid (module ...) "message")
    fn parseAssertInvalid(self: *Parser) !spec_types.command.Command {
        const line = self.lexer.line;
        // current token is "assert_invalid"
        try self.advance();

        // Expect (module ...)
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            std.debug.print("Error: parseAssertInvalid: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        try self.advance();

        // Check for format type
        var module_text: ?[]const u8 = null;
        var module_binary: ?[]const u8 = null;

        if (self.current_token == .identifier) {
            const format_type = self.current_token.identifier;
            if (std.mem.eql(u8, format_type, "quote")) {
                // module quote format: collect string literals
                try self.advance();
                var text_parts = std.ArrayList(u8){};
                defer text_parts.deinit(self.allocator);

                while (self.current_token == .string) {
                    if (text_parts.items.len > 0) {
                        try text_parts.append(self.allocator, ' ');
                    }
                    try text_parts.appendSlice(self.allocator, self.current_token.string);
                    try self.advance();
                }

                module_text = try text_parts.toOwnedSlice(self.allocator);
                try self.expectToken(.right_paren);
            } else if (std.mem.eql(u8, format_type, "binary")) {
                // module binary format: collect and convert strings
                try self.advance();
                var binary_data = std.ArrayList(u8){};
                defer binary_data.deinit(self.allocator);

                while (self.current_token == .string) {
                    const bytes = try parseBinaryString(self.allocator, self.current_token.string);
                    defer self.allocator.free(bytes);
                    try binary_data.appendSlice(self.allocator, bytes);
                    try self.advance();
                }

                module_binary = try binary_data.toOwnedSlice(self.allocator);
                try self.expectToken(.right_paren);
            } else {
                // Regular module format: extract text
                const module_start = self.lexer.pos;
                var depth: u32 = 1;

                while (depth > 0 and self.current_token != .eof) {
                    if (self.current_token == .left_paren) {
                        depth += 1;
                    } else if (self.current_token == .right_paren) {
                        depth -= 1;
                    }
                    try self.advance();
                }

                const module_end = self.lexer.pos;
                module_text = try self.allocator.dupe(u8, self.lexer.input[module_start..module_end]);
            }
        } else {
            // Regular module format: extract text
            const module_start = self.lexer.pos;
            var depth: u32 = 1;

            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                try self.advance();
            }

            const module_end = self.lexer.pos;
            module_text = try self.allocator.dupe(u8, self.lexer.input[module_start..module_end]);
        }

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return spec_types.command.Command{
            .assert_invalid = .{
                .line = line,
                .file_name = try self.allocator.dupe(u8, ""),
                .module_data = module_text,
                .module_binary = module_binary,
                .error_text = failure,
            },
        };
    }

    /// Parse assert_malformed: (assert_malformed (module ...) "message")
    fn parseAssertMalformed(self: *Parser) !spec_types.command.Command {
        const line = self.lexer.line;
        // current token is "assert_malformed"
        try self.advance();

        // Expect (module ...)
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            std.debug.print("Error: parseAssertMalformed: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        try self.advance();

        // Check for format type
        var module_text: ?[]const u8 = null;
        var module_binary: ?[]const u8 = null;

        if (self.current_token == .identifier) {
            const format_type = self.current_token.identifier;
            if (std.mem.eql(u8, format_type, "quote")) {
                // module quote format: collect string literals
                try self.advance();
                var text_parts = std.ArrayList(u8){};
                defer text_parts.deinit(self.allocator);

                while (self.current_token == .string) {
                    if (text_parts.items.len > 0) {
                        try text_parts.append(self.allocator, ' ');
                    }
                    try text_parts.appendSlice(self.allocator, self.current_token.string);
                    try self.advance();
                }

                module_text = try text_parts.toOwnedSlice(self.allocator);
                try self.expectToken(.right_paren);
            } else if (std.mem.eql(u8, format_type, "binary")) {
                // module binary format: collect and convert strings
                try self.advance();
                var binary_data = std.ArrayList(u8){};
                defer binary_data.deinit(self.allocator);

                while (self.current_token == .string) {
                    const bytes = try parseBinaryString(self.allocator, self.current_token.string);
                    defer self.allocator.free(bytes);
                    try binary_data.appendSlice(self.allocator, bytes);
                    try self.advance();
                }

                module_binary = try binary_data.toOwnedSlice(self.allocator);
                try self.expectToken(.right_paren);
            } else {
                // Regular module format: extract text
                const module_start = self.lexer.pos;
                var depth: u32 = 1;

                while (depth > 0 and self.current_token != .eof) {
                    if (self.current_token == .left_paren) {
                        depth += 1;
                    } else if (self.current_token == .right_paren) {
                        depth -= 1;
                    }
                    try self.advance();
                }

                const module_end = self.lexer.pos;
                module_text = try self.allocator.dupe(u8, self.lexer.input[module_start..module_end]);
            }
        } else {
            // Regular module format: extract text
            const module_start = self.lexer.pos;
            var depth: u32 = 1;

            while (depth > 0 and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    depth += 1;
                } else if (self.current_token == .right_paren) {
                    depth -= 1;
                }
                try self.advance();
            }

            const module_end = self.lexer.pos;
            module_text = try self.allocator.dupe(u8, self.lexer.input[module_start..module_end]);
        }

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return spec_types.command.Command{
            .assert_malformed = .{
                .line = line,
                .file_name = try self.allocator.dupe(u8, ""),
                .module_data = module_text,
                .module_binary = module_binary,
                .error_text = failure,
            },
        };
    }

    /// Parse register: (register "name" $module)
    fn parseRegister(self: *Parser) !spec_types.command.Command {
        // current token is "register"
        try self.advance();

        // Parse name
        if (self.current_token != .string) {
            std.debug.print("Error: parseRegister: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
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

        return spec_types.command.Command{
            .register = .{
                .as_name = name,
                .name = module_name,
            },
        };
    }

    /// Parse an action (invoke or get)
    fn parseAction(self: *Parser) !spec_types.command.Action {
        try self.expectToken(.left_paren);
        const action = try self.parseActionInner();
        try self.expectToken(.right_paren);
        return action;
    }

    fn parseActionInner(self: *Parser) !spec_types.command.Action {
        if (self.current_token != .identifier) {
            std.debug.print("Error: parseActionInner: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        const action_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, action_name, "invoke")) {
            return spec_types.command.Action{ .invoke = try self.parseInvoke() };
        } else if (std.mem.eql(u8, action_name, "get")) {
            return spec_types.command.Action{ .get = try self.parseGet() };
        } else {
            std.debug.print("Error: parseActionInner: Unknown action '{s}' at line {d}: col {d}: {s}\n", .{ action_name, self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
    }

    /// Parse invoke: (invoke "func" args...)
    fn parseInvoke(self: *Parser) !spec_types.command.InvokeCommandArg {
        // Optional module name
        var module_name: ?[]const u8 = null;
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            module_name = try self.allocator.dupe(u8, self.current_token.identifier);
            try self.advance();
        }

        // Function name
        if (self.current_token != .string) {
            std.debug.print("Error: parseInvoke: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
        const func_name = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        // Parse arguments
        var args: std.ArrayList(spec_types.command.Value) = .{};
        defer args.deinit(self.allocator);

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const value = try self.parseValue();
            try args.append(self.allocator, value);
        }

        return spec_types.command.InvokeCommandArg{
            .field = func_name,
            .args = try args.toOwnedSlice(self.allocator),
            .module = module_name,
        };
    }

    /// Parse get: (get "global")
    fn parseGet(self: *Parser) !spec_types.command.GetCommandArg {
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

        return spec_types.command.GetCommandArg{
            .field = global_name,
            .module = module_name,
        };
    }

    /// Parse a value: (i32.const 42), (f64.const 3.14), etc.
    fn parseValue(self: *Parser) !spec_types.command.Value {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            std.debug.print("Error: parseValue: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        const type_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, type_name, "i32.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseValue: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i32, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseValue: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i64, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .i64 = value };
        } else if (std.mem.eql(u8, type_name, "f32.const")) {
            const input = if (self.current_token == .identifier)
                self.current_token.identifier
            else if (self.current_token == .number)
                self.current_token.number
            else {
                std.debug.print("Error: parseValue: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            };

            const value = try numeric_parser.parseFloat(f32, input);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .f32 = @bitCast(value) };
        } else if (std.mem.eql(u8, type_name, "f64.const")) {
            const input = if (self.current_token == .identifier)
                self.current_token.identifier
            else if (self.current_token == .number)
                self.current_token.number
            else {
                std.debug.print("Error: parseValue: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            };
            const value = try numeric_parser.parseFloat(f64, input);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .f64 = @bitCast(value) };
        } else if (std.mem.eql(u8, type_name, "v128.const")) {
            const value = try self.parseV128Const();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .v128 = value };
        } else if (std.mem.eql(u8, type_name, "ref.null")) {
            // Parse ref type: funcref, externref, func, extern, etc.
            var is_extern = false;
            if (self.current_token == .identifier) {
                const rt = self.current_token.identifier;
                if (std.mem.eql(u8, rt, "extern") or std.mem.eql(u8, rt, "externref")) {
                    is_extern = true;
                }
                try self.advance();
            }
            try self.expectToken(.right_paren);
            if (is_extern) {
                return spec_types.command.Value{ .extern_ref = null };
            } else {
                return spec_types.command.Value{ .func_ref = null };
            }
        } else if (std.mem.eql(u8, type_name, "ref.func")) {
            // Parse function index
            var func_idx: u32 = 0;
            if (self.current_token == .number) {
                func_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
                try self.advance();
            } else if (self.current_token == .identifier) {
                if (self.builder.func_names.get(self.current_token.identifier)) |idx| {
                    func_idx = idx;
                }
                try self.advance();
            }
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .func_ref = func_idx };
        } else if (std.mem.eql(u8, type_name, "ref.extern")) {
            // Parse external reference index
            var extern_idx: u32 = 0;
            if (self.current_token == .number) {
                extern_idx = try std.fmt.parseInt(u32, self.current_token.number, 10);
                try self.advance();
            }
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .extern_ref = extern_idx };
        } else {
            try self.skipToClosingParen();
            return spec_types.command.Value{ .i32 = 0 };
        }
    }

    /// Parse a result: (i32.const 42), (f32.const nan:canonical), etc.
    fn parseResult(self: *Parser) !spec_types.command.Result {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            std.debug.print("Error: parseResult: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }

        const type_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, type_name, "i32.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseResult: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i32, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseResult: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i64, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .i64 = value };
        } else if (std.mem.eql(u8, type_name, "f32.const")) {
            const input = if (self.current_token == .identifier)
                self.current_token.identifier
            else if (self.current_token == .number)
                self.current_token.number
            else {
                std.debug.print("Error: parseResult: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            };

            if (std.mem.eql(u8, input, "nan:canonical")) {
                try self.advance();
                try self.expectToken(.right_paren);
                return spec_types.command.Result{ .f32 = .nan_canonical };
            } else if (std.mem.eql(u8, input, "nan:arithmetic")) {
                try self.advance();
                try self.expectToken(.right_paren);
                return spec_types.command.Result{ .f32 = .nan_arithmetic };
            }

            const value = try numeric_parser.parseFloat(f32, input);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .f32 = .{ .value = @bitCast(value) } };
        } else if (std.mem.eql(u8, type_name, "f64.const")) {
            const input = if (self.current_token == .identifier)
                self.current_token.identifier
            else if (self.current_token == .number)
                self.current_token.number
            else {
                std.debug.print("Error: parseResult: Unexpected token at line {d}: col {d}: {s}\n", .{ self.lexer.line, self.lexer.getColumn(), self.lexer.getCurrentLine() });
                return TextDecodeError.UnexpectedToken;
            };

            if (std.mem.eql(u8, input, "nan:canonical")) {
                try self.advance();
                try self.expectToken(.right_paren);
                return spec_types.command.Result{ .f64 = .nan_canonical };
            } else if (std.mem.eql(u8, input, "nan:arithmetic")) {
                try self.advance();
                try self.expectToken(.right_paren);
                return spec_types.command.Result{ .f64 = .nan_arithmetic };
            }

            const value = try numeric_parser.parseFloat(f64, input);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .f64 = .{ .value = @bitCast(value) } };
        } else if (std.mem.eql(u8, type_name, "v128.const")) {
            const result = try self.parseV128ConstResult();
            try self.expectToken(.right_paren);
            return result;
        } else if (std.mem.eql(u8, type_name, "either")) {
            // Parse (either (result1) (result2) ...)
            var alternatives: std.ArrayList(spec_types.command.Result) = .{};
            defer alternatives.deinit(self.allocator);
            while (self.current_token != .right_paren and self.current_token != .eof) {
                const alt = try self.parseResult();
                try alternatives.append(self.allocator, alt);
            }
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .either = try alternatives.toOwnedSlice(self.allocator) };
        } else {
            try self.skipToClosingParen();
            return spec_types.command.Result{ .i32 = 0 };
        }
    }
};

pub fn parseWastModule(allocator: std.mem.Allocator, input: []const u8) !wasm_core.types.Module {
    var builder = ModuleBuilder.init(allocator);
    defer builder.deinit();
    var parser = try Parser.init(allocator, input, &builder);
    return parser.parseModule();
}

/// Parse a complete .wast script file
pub fn parseWastScript(allocator: std.mem.Allocator, input: []const u8) ![]spec_types.command.Command {
    var commands: std.ArrayList(spec_types.command.Command) = .{};
    errdefer {
        for (commands.items) |*cmd| {
            freeCommand(allocator, cmd);
        }
        commands.deinit(allocator);
    }

    var builder = ModuleBuilder.init(allocator);
    defer builder.deinit();
    var parser = try Parser.init(allocator, input, &builder);

    while (parser.current_token != .eof) {
        if (parser.current_token == .eof) break;

        if (parser.current_token != .left_paren) {
            try parser.advance();
            continue;
        }

        try parser.advance(); // consume '('

        if (parser.current_token != .identifier) {
            try parser.skipToClosingParen();
            continue;
        }

        const cmd_name = parser.current_token.identifier;

        if (std.mem.eql(u8, cmd_name, "module")) {
            const cmd = try parser.parseModuleCommand();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_return")) {
            const cmd = try parser.parseAssertReturn();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_trap")) {
            const cmd = try parser.parseAssertTrap();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_invalid")) {
            const cmd = try parser.parseAssertInvalid();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_malformed")) {
            const cmd = try parser.parseAssertMalformed();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "register")) {
            const cmd = try parser.parseRegister();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "invoke")) {
            try parser.advance(); // consume 'invoke'
            const action = try parser.parseInvoke();
            try parser.expectToken(.right_paren);
            const cmd = spec_types.command.Command{
                .action = .{
                    .action = .{ .invoke = action },
                    .line = parser.lexer.line,
                },
            };
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "get")) {
            try parser.advance(); // consume 'get'
            const get_action = try parser.parseGet();
            try parser.expectToken(.right_paren);
            const cmd = spec_types.command.Command{
                .action = .{
                    .action = .{ .get = get_action },
                    .line = parser.lexer.line,
                },
            };
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_exhaustion")) {
            const cmd = try parser.parseAssertExhaustion();
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_unlinkable")) {
            const cmd = try parser.parseAssertModuleError(.assert_unlinkable);
            try commands.append(allocator, cmd);
        } else if (std.mem.eql(u8, cmd_name, "assert_uninstantiable")) {
            const cmd = try parser.parseAssertModuleError(.assert_uninstantiable);
            try commands.append(allocator, cmd);
        } else {
            try parser.skipToClosingParen();
        }
    }

    return commands.toOwnedSlice(allocator);
}

pub fn freeCommand(allocator: std.mem.Allocator, cmd: *spec_types.command.Command) void {
    switch (cmd.*) {
        .module => |m| {
            if (m.name) |n| allocator.free(n);
            allocator.free(m.file_name);
            if (m.module_data) |data| {
                allocator.free(data);
            }
            if (m.module_binary) |data| {
                allocator.free(data);
            }
        },
        .register => |r| {
            allocator.free(r.as_name);
            if (r.name) |n| allocator.free(n);
        },
        .assert_return => |ar| {
            freeAction(allocator, @constCast(&ar.action));
            for (ar.expected) |*exp| {
                freeResult(allocator, exp);
            }
            allocator.free(ar.expected);
        },
        .assert_trap => |at| {
            freeAction(allocator, @constCast(&at.action));
            allocator.free(at.error_text);
        },
        .assert_invalid => |ai| {
            allocator.free(ai.file_name);
            if (ai.module_data) |data| allocator.free(data);
            if (ai.module_binary) |data| allocator.free(data);
            allocator.free(ai.error_text);
        },
        .assert_malformed => |am| {
            allocator.free(am.file_name);
            if (am.module_data) |data| allocator.free(data);
            if (am.module_binary) |data| allocator.free(data);
            allocator.free(am.error_text);
        },
        else => {},
    }
}

fn freeAction(allocator: std.mem.Allocator, action: *spec_types.command.Action) void {
    switch (action.*) {
        .invoke => |inv| {
            allocator.free(inv.field);
            if (inv.module) |m| allocator.free(m);
            allocator.free(inv.args);
        },
        .get => |g| {
            allocator.free(g.field);
            if (g.module) |m| allocator.free(m);
        },
    }
}

fn freeResult(allocator: std.mem.Allocator, result: *const spec_types.command.Result) void {
    switch (result.*) {
        .either => |alternatives| {
            for (alternatives) |*alt| {
                freeResult(allocator, alt);
            }
            allocator.free(alternatives);
        },
        else => {}, // Other results don't own any memory
    }
}

// Test function
