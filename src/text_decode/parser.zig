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
    input: []const u8,
    // Name resolution for current function
    local_names: std.StringHashMap(u32),

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Parser {
        var lexer = Lexer.init(input);
        const current_token = try lexer.nextToken();

        return Parser{
            .lexer = lexer,
            .current_token = current_token,
            .allocator = allocator,
            .input = input,
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
            std.debug.print("Error: expectToken: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
            std.debug.print("Error: parseModuleField: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
        } else {
            std.debug.print("Error: Unknown module field: {s}\n", .{field_name});
            return TextDecodeError.UnexpectedToken;
        }

        try self.expectToken(.right_paren);
    }

    fn parseElem(self: *Parser, builder: *ModuleBuilder) !void {
        // Skip optional name
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
        }
        
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
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "offset")) {
                        try self.advance();
                        if (self.current_token == .left_paren) {
                            try self.advance();
                            if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                                try self.advance();
                                if (self.current_token == .number) {
                                    const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                                    offset = .{ .i32_const = val };
                                    try self.advance();
                                }
                                try self.expectRightParen();
                            }
                        }
                        try self.expectRightParen();
                    } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                        try self.advance();
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            offset = .{ .i32_const = val };
                            try self.advance();
                        }
                        try self.expectRightParen();
                    }
                }
                
                mode = .{ .active = .{ .table_idx = table_idx, .offset = offset } };
            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "offset")) {
                // (offset (i32.const 0))
                try self.advance();
                if (self.current_token == .left_paren) {
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                        try self.advance();
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            offset = .{ .i32_const = val };
                            try self.advance();
                        }
                        try self.expectRightParen();
                    } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "global.get")) {
                        try self.advance();
                        const val: u32 = if (self.current_token == .number)
                            try std.fmt.parseInt(u32, self.current_token.number, 0)
                        else
                            0;
                        offset = .{ .global_get = val };
                        try self.advance();
                        try self.expectRightParen();
                    }
                }
                try self.expectRightParen();
                mode = .{ .active = .{ .table_idx = 0, .offset = offset } };
            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                // Direct offset expression: (i32.const 0)
                try self.advance();
                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                    offset = .{ .i32_const = val };
                    try self.advance();
                }
                try self.expectRightParen();
                mode = .{ .active = .{ .table_idx = 0, .offset = offset } };
            } else if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "global.get")) {
                // Direct offset expression: (global.get 0)
                try self.advance();
                const val: u32 = if (self.current_token == .number)
                    try std.fmt.parseInt(u32, self.current_token.number, 0)
                else
                    0;
                offset = .{ .global_get = val };
                try self.advance();
                try self.expectRightParen();
                mode = .{ .active = .{ .table_idx = 0, .offset = offset } };
            } else {
                // Not table or offset, restore
                self.lexer.pos = saved_pos;
                self.lexer.current_char = saved_char;
                self.current_token = saved_token;
            }
        }
        
        // Check for 'declare' keyword
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "declare")) {
            mode = .declarative;
            try self.advance();
        }
        
        // Skip 'func' keyword if present
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "func")) {
            try self.advance();
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
                        try self.advance();
                        try self.expectRightParen();
                    } else {
                        try self.advance();
                        try self.expectRightParen();
                    }
                }
            } else if (self.current_token == .identifier) {
                // Function name or index
                if (std.mem.startsWith(u8, self.current_token.identifier, "$")) {
                    // Function name - skip for now, would need name resolution
                    try init_list.append(self.allocator, .{ .ref_func = 0 });
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
        // Skip optional type name
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
        }

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
                    if (try self.parseValueType()) |vtype| {
                        try params.append(self.allocator, vtype);
                    } else {
                        std.debug.print("Error: Unknown parameter type in type definition\n", .{});
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

        const func_type = wasm_core.types.FuncType{
            .parameter_types = try self.allocator.dupe(wasm_core.types.ValueType, params.items),
            .result_types = try self.allocator.dupe(wasm_core.types.ValueType, results.items),
        };
        try builder.types.append(self.allocator, func_type);
    }

    fn parseGlobal(self: *Parser, builder: *ModuleBuilder) !void {
        // Skip optional name
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
        }
        
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
                
                // Parse global type
                var value_type: wasm_core.types.ValueType = .i32;
                if (self.current_token == .identifier) {
                    value_type = try self.parseValueType() orelse .i32;
                }
                
                // Add to imports
                try builder.imports.append(self.allocator, .{
                    .module_name = module_name,
                    .name = field_name,
                    .desc = .{ .global = .{ .value_type = value_type, .mutability = .immutable } },
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
        const mutability: wasm_core.types.Mutability = .immutable;
        
        if (self.current_token == .identifier) {
            value_type = try self.parseValueType() orelse .i32;
        }
        
        // Parse init expression
        var init_expr: wasm_core.types.InitExpression = .{ .i32_const = 0 };
        
        if (self.current_token == .left_paren) {
            try self.advance();
            if (self.current_token == .identifier) {
                const instr = self.current_token.identifier;
                try self.advance();
                
                if (std.mem.eql(u8, instr, "i32.const")) {
                    if (self.current_token == .number) {
                        const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                        init_expr = .{ .i32_const = val };
                        try self.advance();
                    }
                } else if (std.mem.eql(u8, instr, "i64.const")) {
                    if (self.current_token == .number) {
                        const val = try std.fmt.parseInt(i64, self.current_token.number, 0);
                        init_expr = .{ .i64_const = val };
                        try self.advance();
                    }
                } else if (std.mem.eql(u8, instr, "f32.const")) {
                    if (self.current_token == .number) {
                        const val = try numeric_parser.parseFloat(f32, self.current_token.number);
                        init_expr = .{ .f32_const = val };
                        try self.advance();
                    }
                } else if (std.mem.eql(u8, instr, "f64.const")) {
                    if (self.current_token == .number) {
                        const val = try numeric_parser.parseFloat(f64, self.current_token.number);
                        init_expr = .{ .f64_const = val };
                        try self.advance();
                    }
                }
            }
            try self.expectRightParen();
        }
        
        const global_idx: u32 = @intCast(builder.globals.items.len);
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
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
                    try self.advance();
                    if (self.current_token == .number) {
                        type_idx = try std.fmt.parseInt(u32, self.current_token.number, 0);
                        try self.advance();
                    } else if (self.current_token == .identifier) {
                        // Type name - skip for now
                        try self.advance();
                    }
                    try self.expectRightParen();
                }
            }
            
            // Skip remaining signature
            while (self.current_token != .right_paren and self.current_token != .eof) {
                if (self.current_token == .left_paren) {
                    var depth: u32 = 1;
                    try self.advance();
                    while (depth > 0 and self.current_token != .eof) {
                        if (self.current_token == .left_paren) depth += 1
                        else if (self.current_token == .right_paren) depth -= 1;
                        if (depth > 0) try self.advance();
                    }
                    if (self.current_token == .right_paren) try self.advance();
                } else {
                    try self.advance();
                }
            }
            break :blk wasm_core.types.ImportDesc{ .function = type_idx };
        } else if (std.mem.eql(u8, desc_type, "global")) blk: {
            // Parse global type
            var value_type: wasm_core.types.ValueType = .i32;
            var mutability: wasm_core.types.Mutability = .immutable;
            
            // Check for (mut type)
            if (self.current_token == .left_paren) {
                try self.advance();
                if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "mut")) {
                    mutability = .mutable;
                    try self.advance();
                    if (self.current_token == .identifier) {
                        value_type = try self.parseValueType() orelse .i32;
                    }
                    try self.expectRightParen();
                }
            } else if (self.current_token == .identifier) {
                value_type = try self.parseValueType() orelse .i32;
            }
            
            break :blk wasm_core.types.ImportDesc{ .global = .{ .value_type = value_type, .mutability = mutability } };
        } else if (std.mem.eql(u8, desc_type, "table")) blk: {
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
                if (std.mem.eql(u8, self.current_token.identifier, "funcref")) {
                    ref_type = .funcref;
                } else if (std.mem.eql(u8, self.current_token.identifier, "externref")) {
                    ref_type = .externref;
                }
                try self.advance();
            }
            
            break :blk wasm_core.types.ImportDesc{ .table = .{ .limits = .{ .min = min, .max = max }, .ref_type = ref_type } };
        } else if (std.mem.eql(u8, desc_type, "memory")) blk: {
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
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
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

        // Parse table limits and type
        var min: u32 = 0;
        var max: ?u32 = null;
        var ref_type: wasm_core.types.RefType = .funcref;

        // Parse min
        if (self.current_token == .number) {
            min = try std.fmt.parseInt(u32, self.current_token.number, 0);
            try self.advance();
        }

        // Parse optional max
        if (self.current_token == .number) {
            max = try std.fmt.parseInt(u32, self.current_token.number, 0);
            try self.advance();
        }

        // Parse ref type (funcref or externref)
        if (self.current_token == .identifier) {
            if (std.mem.eql(u8, self.current_token.identifier, "funcref")) {
                ref_type = .funcref;
                try self.advance();
            } else if (std.mem.eql(u8, self.current_token.identifier, "externref")) {
                ref_type = .externref;
                try self.advance();
            }
        }

        // Skip inline elem if present: (elem ...)
        if (self.current_token == .left_paren) {
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

        const table_idx: u32 = @intCast(builder.tables.items.len);
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
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance(); // skip the name
        }

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
                if (self.current_token != .left_paren) return TextDecodeError.UnexpectedToken;
                try self.advance(); // consume '('
                if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "memory")) {
                    return TextDecodeError.UnexpectedToken;
                }
                try self.advance(); // consume 'memory'
                
                var min: u32 = 1;
                var max: ?u32 = null;
                if (self.current_token == .number) {
                    min = @intCast(try std.fmt.parseInt(u64, self.current_token.number, 0));
                    try self.advance();
                }
                if (self.current_token == .number) {
                    max = @intCast(try std.fmt.parseInt(u64, self.current_token.number, 0));
                    try self.advance();
                }
                
                try self.expectRightParen();
                
                // Add to imports
                try builder.imports.append(self.allocator, .{
                    .module_name = module_name,
                    .name = field_name,
                    .desc = .{ .memory = .{ .limits = .{ .min = min, .max = max } } },
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
                    if (self.current_token == .left_paren) depth += 1
                    else if (self.current_token == .right_paren) depth -= 1;
                    if (depth > 0) try self.advance();
                }
                if (self.current_token == .right_paren) try self.advance();
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

        const mem_idx: u32 = @intCast(builder.memories.items.len);
        try builder.memories.append(builder.allocator, memory_type);
        
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
        if (self.current_token == .identifier and self.current_token.identifier.len > 0 and self.current_token.identifier[0] == '$') {
            try self.advance();
        }

        var offset: wasm_core.types.InitExpression = .{ .i32_const = 0 };

        // Check for (memory idx) or (offset ...) or (i32.const ...)
        while (self.current_token == .left_paren) {
            try self.advance(); // consume '('
            
            if (self.current_token != .identifier) break;
            
            const instr_name = self.current_token.identifier;
            try self.advance();
            
            if (std.mem.eql(u8, instr_name, "memory")) {
                // Skip memory index
                if (self.current_token == .number or self.current_token == .identifier) {
                    try self.advance();
                }
                try self.expectRightParen();
            } else if (std.mem.eql(u8, instr_name, "offset")) {
                // Parse (offset (i32.const ...))
                if (self.current_token == .left_paren) {
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                        try self.advance();
                        if (self.current_token == .number) {
                            const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            offset = .{ .i32_const = val };
                            try self.advance();
                        }
                    }
                    try self.expectRightParen();
                }
                try self.expectRightParen();
                break;
            } else if (std.mem.eql(u8, instr_name, "i32.const")) {
                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(i32, self.current_token.number, 0);
                    offset = .{ .i32_const = val };
                    try self.advance();
                }
                try self.expectRightParen();
                break;
            } else if (std.mem.eql(u8, instr_name, "i64.const")) {
                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(i64, self.current_token.number, 0);
                    offset = .{ .i64_const = val };
                    try self.advance();
                }
                try self.expectRightParen();
                break;
            } else if (std.mem.eql(u8, instr_name, "global.get")) {
                if (self.current_token == .number) {
                    const val = try std.fmt.parseInt(u32, self.current_token.number, 10);
                    offset = .{ .global_get = val };
                    try self.advance();
                } else if (self.current_token == .identifier) {
                    // Named global - skip for now (would need name resolution)
                    try self.advance();
                }
                try self.expectRightParen();
                break;
            } else if (std.mem.eql(u8, instr_name, "i32.add")) {
                // Parse (i32.add (i32.const a) (i32.const b)) -> a + b
                var val1: i32 = 0;
                var val2: i32 = 0;
                
                // Parse first operand
                if (self.current_token == .left_paren) {
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                        try self.advance();
                        if (self.current_token == .number) {
                            val1 = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            try self.advance();
                        }
                    }
                    try self.expectRightParen();
                }
                
                // Parse second operand
                if (self.current_token == .left_paren) {
                    try self.advance();
                    if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "i32.const")) {
                        try self.advance();
                        if (self.current_token == .number) {
                            val2 = try std.fmt.parseInt(i32, self.current_token.number, 0);
                            try self.advance();
                        }
                    }
                    try self.expectRightParen();
                }
                
                offset = .{ .i32_const = val1 + val2 };
                try self.expectRightParen();
                break;
            } else {
                std.debug.print("Error: Unknown instruction in data offset: {s}\n", .{self.current_token.identifier});
                return TextDecodeError.UnexpectedToken;
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
                    .mem_idx = 0,
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
                        } else {
                            std.debug.print("Error: Unknown parameter type: {s}\n", .{type_name});
                            return TextDecodeError.UnexpectedToken;
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
        // Skip label if present (e.g., $l)
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            try self.advance();
        }
        
        // Check if next token is '(' followed by 'result'
        if (self.current_token != .left_paren) {
            return .empty;
        }

        // Save lexer state for potential backtrack
        const saved_lexer_pos = self.lexer.pos;
        const saved_lexer_char = self.lexer.current_char;
        const saved_token = self.current_token;

        try self.advance(); // consume '('

        // Skip (type ...) if present
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "type")) {
            try self.advance(); // consume 'type'
            if (self.current_token == .identifier) {
                try self.advance(); // skip type identifier
            }
            if (self.current_token == .right_paren) {
                try self.advance();
            }
            
            // Check for (result ...) or (param ...) after (type ...)
            if (self.current_token != .left_paren) {
                return .empty;
            }
            try self.advance(); // consume '('
        }

        // Skip (param ...) if present
        if (self.current_token == .identifier and std.mem.eql(u8, self.current_token.identifier, "param")) {
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
            
            // Check for (result ...) after (param ...)
            if (self.current_token == .left_paren) {
                try self.advance();
            } else {
                return .empty;
            }
        }

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
            std.debug.print("Error: expectRightParen: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
                std.debug.print("Error: Unexpected token in instruction sequence at line {d}\n", .{self.lexer.line});
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
            return .{ .block = .{ .type = try self.parseBlockType(), .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "loop")) {
            return .{ .loop = .{ .type = try self.parseBlockType(), .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "if")) {
            return .{ .@"if" = .{ .type = try self.parseBlockType(), .@"else" = null, .end = 0 } };
        } else if (std.mem.eql(u8, instr_name, "else")) {
            return .@"else";
        } else if (std.mem.eql(u8, instr_name, "end")) {
            return .end;
        } else if (std.mem.eql(u8, instr_name, "return")) {
            return .@"return";
        }

        // Branch instructions
        else if (std.mem.eql(u8, instr_name, "br")) {
            return .{ .br = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "br_if")) {
            return .{ .br_if = try self.parseU32OrIdentifier() };
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
            return .{ .call = try self.parseU32OrIdentifier() };
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
                    std.debug.print("Error: parseCallIndirect: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
                    return TextDecodeError.UnexpectedToken;
                }
            } else {
                std.debug.print("Error: parseCallIndirect: Expected '(' for type declaration at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
        else if (std.mem.eql(u8, instr_name, "ref.null")) {
            // Parse ref type (funcref or externref)
            if (self.current_token == .identifier) {
                if (std.mem.eql(u8, self.current_token.identifier, "funcref")) {
                    try self.advance();
                    return .{ .ref_null = .funcref };
                } else if (std.mem.eql(u8, self.current_token.identifier, "externref")) {
                    try self.advance();
                    return .{ .ref_null = .externref };
                } else if (std.mem.eql(u8, self.current_token.identifier, "func")) {
                    try self.advance();
                    return .{ .ref_null = .funcref };
                } else if (std.mem.eql(u8, self.current_token.identifier, "extern")) {
                    try self.advance();
                    return .{ .ref_null = .externref };
                }
            }
            return .{ .ref_null = .funcref };
        } else if (std.mem.eql(u8, instr_name, "ref.is_null")) {
            return .ref_is_null;
        } else if (std.mem.eql(u8, instr_name, "ref.func")) {
            return .{ .ref_func = try self.parseU32OrIdentifier() };
        }

        // parametric instructions
        else if (std.mem.eql(u8, instr_name, "drop")) {
            return .drop;
        }

        // Variable instructions
        else if (std.mem.eql(u8, instr_name, "local.get")) {
            return .{ .local_get = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "local.set")) {
            return .{ .local_set = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "local.tee")) {
            return .{ .local_tee = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "global.get")) {
            return .{ .global_get = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "global.set")) {
            return .{ .global_set = try self.parseU32OrIdentifier() };
        }
        // table instructions
        else if (std.mem.eql(u8, instr_name, "table.get")) {
            const idx = if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")))
                try self.parseU32OrIdentifier()
            else
                0;
            return .{ .table_get = idx };
        } else if (std.mem.eql(u8, instr_name, "table.set")) {
            const idx = if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")))
                try self.parseU32OrIdentifier()
            else
                0;
            return .{ .table_set = idx };
        } else if (std.mem.eql(u8, instr_name, "table.size")) {
            const idx = if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")))
                try self.parseU32OrIdentifier()
            else
                0;
            return .{ .table_size = idx };
        } else if (std.mem.eql(u8, instr_name, "table.grow")) {
            const idx = if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")))
                try self.parseU32OrIdentifier()
            else
                0;
            return .{ .table_grow = idx };
        } else if (std.mem.eql(u8, instr_name, "table.fill")) {
            const idx = if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")))
                try self.parseU32OrIdentifier()
            else
                0;
            return .{ .table_fill = idx };
        } else if (std.mem.eql(u8, instr_name, "table.copy")) {
            // Check if there are table indices
            if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$"))) {
                const dst = try self.parseU32OrIdentifier();
                const src = try self.parseU32OrIdentifier();
                return .{ .table_copy = .{ .table_idx_dst = dst, .table_idx_src = src } };
            } else {
                return .{ .table_copy = .{ .table_idx_dst = 0, .table_idx_src = 0 } };
            }
        } else if (std.mem.eql(u8, instr_name, "table.init")) {
            const first_idx = try self.parseU32OrIdentifier();
            // Check if there's a second index (table elem) or just one (elem, table=0)
            if (self.current_token == .number or (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$"))) {
                const second_idx = try self.parseU32OrIdentifier();
                return .{ .table_init = .{ .table_idx = first_idx, .elem_idx = second_idx } };
            } else {
                return .{ .table_init = .{ .table_idx = 0, .elem_idx = first_idx } };
            }
        } else if (std.mem.eql(u8, instr_name, "elem.drop")) {
            return .{ .elem_drop = try self.parseU32OrIdentifier() };
        }

        // Memory instructions
        else if (std.mem.eql(u8, instr_name, "i32.load")) {
            return .{ .i32_load = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load")) {
            return .{ .i64_load = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "f32.load")) {
            return .{ .f32_load = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "f64.load")) {
            return .{ .f64_load = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.load8_s")) {
            return .{ .i32_load8_s = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.load8_u")) {
            return .{ .i32_load8_u = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.load16_s")) {
            return .{ .i32_load16_s = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.load16_u")) {
            return .{ .i32_load16_u = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load8_s")) {
            return .{ .i64_load8_s = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load8_u")) {
            return .{ .i64_load8_u = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load16_s")) {
            return .{ .i64_load16_s = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load16_u")) {
            return .{ .i64_load16_u = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load32_s")) {
            return .{ .i64_load32_s = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.load32_u")) {
            return .{ .i64_load32_u = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.store")) {
            return .{ .i32_store = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.store")) {
            return .{ .i64_store = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "f32.store")) {
            return .{ .f32_store = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "f64.store")) {
            return .{ .f64_store = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.store8")) {
            return .{ .i32_store8 = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i32.store16")) {
            return .{ .i32_store16 = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.store8")) {
            return .{ .i64_store8 = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.store16")) {
            return .{ .i64_store16 = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "i64.store32")) {
            return .{ .i64_store32 = try self.parseMemArg() };
        } else if (std.mem.eql(u8, instr_name, "memory.size")) {
            return .memory_size;
        } else if (std.mem.eql(u8, instr_name, "memory.grow")) {
            return .memory_grow;
        } else if (std.mem.eql(u8, instr_name, "memory.init")) {
            return .{ .memory_init = try self.parseU32OrIdentifier() };
        } else if (std.mem.eql(u8, instr_name, "memory.copy")) {
            return .memory_copy;
        } else if (std.mem.eql(u8, instr_name, "memory.fill")) {
            return .memory_fill;
        } else if (std.mem.eql(u8, instr_name, "data.drop")) {
            return .{ .data_drop = try self.parseU32OrIdentifier() };
        }

        // Numeric const instructions
        else if (std.mem.eql(u8, instr_name, "i32.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.InvalidNumber;
            }
            const val = try numeric_parser.parseInteger(i32, self.current_token.number);
            try self.advance();
            return .{ .i32_const = val };
        } else if (std.mem.eql(u8, instr_name, "i64.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.InvalidNumber;
            }
            const val = try numeric_parser.parseInteger(i64, self.current_token.number);
            try self.advance();
            return .{ .i64_const = val };
        } else if (std.mem.eql(u8, instr_name, "f32.const")) {
            return .{ .f32_const = try self.parseFloat(f32) };
        } else if (std.mem.eql(u8, instr_name, "f64.const")) {
            return .{ .f64_const = try self.parseFloat(f64) };
        } else if (std.mem.eql(u8, instr_name, "v128.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.InvalidNumber;
            }
            const val = try numeric_parser.parseInteger(i128, self.current_token.number);
            try self.advance();
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

        // Sign extension instructions (WebAssembly 2.0)
        else if (std.mem.eql(u8, instr_name, "i32.extend8_s")) {
            return .i32_extend8_s;
        } else if (std.mem.eql(u8, instr_name, "i32.extend16_s")) {
            return .i32_extend16_s;
        } else if (std.mem.eql(u8, instr_name, "i64.extend8_s")) {
            return .i64_extend8_s;
        } else if (std.mem.eql(u8, instr_name, "i64.extend16_s")) {
            return .i64_extend16_s;
        } else if (std.mem.eql(u8, instr_name, "i64.extend32_s")) {
            return .i64_extend32_s;
        }

        // Skip non-instruction keywords
        if (std.mem.eql(u8, instr_name, "type") or 
            std.mem.eql(u8, instr_name, "param") or
            std.mem.eql(u8, instr_name, "result")) {
            return null;
        }

        // Unknown instruction
        std.debug.print("Error: Unknown instruction: {s}\n", .{instr_name});
        return TextDecodeError.UnexpectedToken;
    }

    /// Parse S-expression recursively: (instr arg1 arg2 ...)
    fn parseSExpression(self: *Parser, instructions: *std.ArrayList(wasm_core.types.Instruction)) !void {
        // Parse the instruction
        const instr = try self.parseInstruction() orelse {
            // Skip to closing paren for non-instruction keywords
            while (self.current_token != .right_paren and self.current_token != .eof) {
                try self.advance();
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
            // Parse condition expression (only if not followed by 'then')
            if (self.current_token == .left_paren) {
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
                
                if (!is_then) {
                    try self.advance(); // consume '('
                    try self.parseSExpression(instructions);
                    try self.expectRightParen();
                }
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
        std.debug.print("Error: parseU32OrIdentifier: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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

        // Check for binary/quote/instance format
        if (self.current_token == .identifier) {
            const format_type = self.current_token.identifier;
            if (std.mem.eql(u8, format_type, "binary") or
                std.mem.eql(u8, format_type, "quote") or
                std.mem.eql(u8, format_type, "instance"))
            {
                try self.advance();
                while (self.current_token == .string or self.current_token == .identifier) {
                    try self.advance();
                }
                try self.expectToken(.right_paren);

                if (std.mem.eql(u8, format_type, "quote")) {
                    return spec_types.command.Command{ .module_quote = {} };
                }

                return spec_types.command.Command{
                    .module = .{
                        .line = 0,
                        .file_name = try self.allocator.dupe(u8, ""),
                        .name = name,
                        .module_data = null,
                    },
                };
            }
        }

        // Parse text format module
        var builder = ModuleBuilder.init(self.allocator);
        defer builder.deinit();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.parseModuleField(&builder);
        }

        try self.expectToken(.right_paren);
        const module_end_pos = self.lexer.pos;

        _ = try builder.build();

        const module_text = self.input[module_start_pos..module_end_pos];
        const module_data = try self.allocator.dupe(u8, module_text);

        return spec_types.command.Command{
            .module = .{
                .line = 0,
                .file_name = try self.allocator.dupe(u8, ""),
                .name = name,
                .module_data = module_data,
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
            std.debug.print("Error: parseAssertTrap: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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

    /// Parse assert_invalid: (assert_invalid (module ...) "message")
    fn parseAssertInvalid(self: *Parser) !spec_types.command.Command {
        // current token is "assert_invalid"
        try self.advance();

        // Expect (module ...)
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier or !std.mem.eql(u8, self.current_token.identifier, "module")) {
            std.debug.print("Error: parseAssertInvalid: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
            return TextDecodeError.UnexpectedToken;
        }

        // Skip the module section
        var depth: u32 = 1;
        try self.advance();

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

        return spec_types.command.Command{
            .assert_invalid = .{
                .line = 0,
                .file_name = try self.allocator.dupe(u8, ""),
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
            std.debug.print("Error: parseRegister: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
            std.debug.print("Error: parseActionInner: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
            return TextDecodeError.UnexpectedToken;
        }

        const action_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, action_name, "invoke")) {
            return spec_types.command.Action{ .invoke = try self.parseInvoke() };
        } else if (std.mem.eql(u8, action_name, "get")) {
            return spec_types.command.Action{ .get = try self.parseGet() };
        } else {
            std.debug.print("Error: parseActionInner: Unknown action '{s}' at line {d} char [{c}]\n", .{ action_name, self.lexer.line, self.lexer.current_char.? });
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
            std.debug.print("Error: parseInvoke: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
            std.debug.print("Error: parseValue: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
            return TextDecodeError.UnexpectedToken;
        }

        const type_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, type_name, "i32.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseValue: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i32, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseValue: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
                std.debug.print("Error: parseValue: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
                std.debug.print("Error: parseValue: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
                return TextDecodeError.UnexpectedToken;
            };
            const value = try numeric_parser.parseFloat(f64, input);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Value{ .f64 = @bitCast(value) };
        } else {
            try self.skipToClosingParen();
            return spec_types.command.Value{ .i32 = 0 };
        }
    }

    /// Parse a result: (i32.const 42), (f32.const nan:canonical), etc.
    fn parseResult(self: *Parser) !spec_types.command.Result {
        try self.expectToken(.left_paren);

        if (self.current_token != .identifier) {
            std.debug.print("Error: parseResult: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
            return TextDecodeError.UnexpectedToken;
        }

        const type_name = self.current_token.identifier;
        try self.advance();

        if (std.mem.eql(u8, type_name, "i32.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseResult: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
                return TextDecodeError.UnexpectedToken;
            }
            const value = try numeric_parser.parseInteger(i32, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return spec_types.command.Result{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                std.debug.print("Error: parseResult: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
                std.debug.print("Error: parseResult: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
                std.debug.print("Error: parseResult: Unexpected token at line {d} char [{c}]\n", .{ self.lexer.line, self.lexer.current_char.? });
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
        } else {
            try self.skipToClosingParen();
            return spec_types.command.Result{ .i32 = 0 };
        }
    }
};

pub fn parseWastModule(allocator: std.mem.Allocator, input: []const u8) !wasm_core.types.Module {
    var parser = try Parser.init(allocator, input);
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

    var parser = try Parser.init(allocator, input);

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
        } else if (std.mem.eql(u8, cmd_name, "register")) {
            const cmd = try parser.parseRegister();
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
            allocator.free(ai.error_text);
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

fn freeResult(_: std.mem.Allocator, _: *const spec_types.command.Result) void {
    // Results don't own any memory
}

// Test function
