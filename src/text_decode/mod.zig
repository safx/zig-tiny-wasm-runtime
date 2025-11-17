const std = @import("std");
const wasm_core = @import("wasm-core");
pub const wast = @import("./wast.zig");

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

pub const TextDecodeError = error{
    InvalidFormat,
    UnexpectedToken,
    InvalidNumber,
    InvalidString,
    OutOfMemory,
    UnknownInstruction,
    InvalidModule,
};

pub const Token = union(enum) {
    left_paren,
    right_paren,
    identifier: []const u8,
    string: []const u8,
    number: []const u8,
    eof,
};

pub const Lexer = struct {
    input: []const u8,
    pos: usize,
    current_char: ?u8,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .pos = 0,
            .current_char = null,
        };
        if (input.len > 0) {
            lexer.current_char = input[0];
        }
        return lexer;
    }

    pub fn advance(self: *Lexer) void {
        self.pos += 1;
        if (self.pos >= self.input.len) {
            self.current_char = null;
        } else {
            self.current_char = self.input[self.pos];
        }
    }

    pub fn skipWhitespace(self: *Lexer) void {
        while (self.current_char) |char| {
            if (char == ' ' or char == '\t' or char == '\n' or char == '\r') {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn skipComment(self: *Lexer) void {
        if (self.current_char == ';' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';') {
            while (self.current_char) |char| {
                self.advance();
                if (char == '\n') break;
            }
        } else if (self.current_char == '(' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';') {
            self.advance();
            self.advance();
            var depth: u32 = 1;
            while (self.current_char != null and depth > 0) {
                const char = self.current_char.?;
                if (char == '(' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';') {
                    depth += 1;
                    self.advance();
                    self.advance();
                } else if (char == ';' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ')') {
                    depth -= 1;
                    self.advance();
                    self.advance();
                } else {
                    self.advance();
                }
            }
        }
    }

    pub fn nextToken(self: *Lexer) !Token {
        while (true) {
            self.skipWhitespace();
            
            if (self.current_char == null) {
                return Token.eof;
            }

            if (self.current_char == ';' or 
                (self.current_char == '(' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';')) {
                self.skipComment();
                continue;
            }

            if (self.current_char == '(') {
                self.advance();
                return Token.left_paren;
            }

            if (self.current_char == ')') {
                self.advance();
                return Token.right_paren;
            }

            if (self.current_char == '"') {
                return try self.readString();
            }

            if (self.isAlphaNumeric(self.current_char.?)) {
                return try self.readIdentifierOrNumber();
            }

            return TextDecodeError.UnexpectedToken;
        }
    }

    fn readString(self: *Lexer) !Token {
        const start = self.pos;
        self.advance(); // skip opening quote
        
        while (self.current_char) |char| {
            if (char == '"') {
                const result = self.input[start + 1..self.pos];
                self.advance(); // skip closing quote
                return Token{ .string = result };
            }
            if (char == '\\') {
                self.advance(); // skip escape character
                if (self.current_char == null) break;
            }
            self.advance();
        }
        
        return TextDecodeError.InvalidString;
    }

    fn readIdentifierOrNumber(self: *Lexer) !Token {
        const start = self.pos;
        
        while (self.current_char) |char| {
            if (!self.isAlphaNumeric(char)) break;
            self.advance();
        }
        
        const text = self.input[start..self.pos];
        
        // Check if it's a number
        if (self.isDigit(text[0]) or 
            (text.len > 1 and (text[0] == '+' or text[0] == '-') and self.isDigit(text[1]))) {
            return Token{ .number = text };
        }
        
        return Token{ .identifier = text };
    }

    fn isAlphaNumeric(self: *Lexer, char: u8) bool {
        _ = self;
        return (char >= 'a' and char <= 'z') or
               (char >= 'A' and char <= 'Z') or
               (char >= '0' and char <= '9') or
               char == '_' or char == '.' or char == '+' or char == '-' or char == '$';
    }

    fn isDigit(self: *Lexer, char: u8) bool {
        _ = self;
        return char >= '0' and char <= '9';
    }
};

pub const Parser = struct {
    lexer: Lexer,
    current_token: Token,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Parser {
        var lexer = Lexer.init(input);
        const current_token = try lexer.nextToken();
        
        return Parser{
            .lexer = lexer,
            .current_token = current_token,
            .allocator = allocator,
        };
    }

    pub fn advance(self: *Parser) !void {
        self.current_token = try self.lexer.nextToken();
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
        if (self.current_token != .number) {
            return TextDecodeError.UnexpectedToken;
        }
        
        const min_pages = try std.fmt.parseInt(u32, self.current_token.number, 10);
        try self.advance();
        
        var max_pages: ?u32 = null;
        if (self.current_token == .number) {
            max_pages = try std.fmt.parseInt(u32, self.current_token.number, 10);
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
        // Simplified data parsing - just skip for now
        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.advance();
        }
        _ = builder;
    }

    fn parseFunction(self: *Parser, builder: *ModuleBuilder) !void {
        // Simplified function parsing - just skip for now
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
        _ = builder;
    }

    fn parseExport(self: *Parser, builder: *ModuleBuilder) !void {
        // Simplified export parsing - just skip for now
        while (self.current_token != .right_paren and self.current_token != .eof) {
            try self.advance();
        }
        _ = builder;
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

        // Check for optional module name
        var name: ?[]const u8 = null;
        if (self.current_token == .identifier and std.mem.startsWith(u8, self.current_token.identifier, "$")) {
            name = try self.allocator.dupe(u8, self.current_token.identifier);
            try self.advance();
        }

        // Parse the module itself
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
        var expected = std.ArrayList(wast.Value).init(self.allocator);
        defer expected.deinit();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const value = try self.parseValue();
            try expected.append(value);
        }

        try self.expectToken(.right_paren);

        return wast.AssertReturn{
            .action = action,
            .expected = try expected.toOwnedSlice(),
        };
    }

    /// Parse assert_trap: (assert_trap (invoke ...) "message")
    fn parseAssertTrap(self: *Parser) !wast.AssertTrap {
        // current token is "assert_trap"
        try self.advance();

        // Parse action
        const action = try self.parseAction();

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return wast.AssertTrap{
            .action = action,
            .failure = failure,
        };
    }

    /// Parse assert_invalid: (assert_invalid (module ...) "message")
    fn parseAssertInvalid(self: *Parser) !wast.AssertInvalid {
        // current token is "assert_invalid"
        try self.advance();

        // For now, just capture the module text and skip it
        const start_pos = self.lexer.pos;
        try self.skipToClosingParen();
        const end_pos = self.lexer.pos;
        const module_text = try self.allocator.dupe(u8, self.lexer.input[start_pos..end_pos]);

        // Parse failure message
        if (self.current_token != .string) {
            return TextDecodeError.UnexpectedToken;
        }
        const failure = try self.allocator.dupe(u8, self.current_token.string);
        try self.advance();

        try self.expectToken(.right_paren);

        return wast.AssertInvalid{
            .module_text = module_text,
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
        var args = std.ArrayList(wast.Value).init(self.allocator);
        defer args.deinit();

        while (self.current_token != .right_paren and self.current_token != .eof) {
            const value = try self.parseValue();
            try args.append(value);
        }

        try self.expectToken(.right_paren);

        return wast.Invoke{
            .module_name = module_name,
            .func_name = func_name,
            .args = try args.toOwnedSlice(),
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
            const value = try std.fmt.parseInt(i32, self.current_token.number, 0);
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .i32 = value };
        } else if (std.mem.eql(u8, type_name, "i64.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.UnexpectedToken;
            }
            const value = try std.fmt.parseInt(i64, self.current_token.number, 0);
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .i64 = value };
        } else if (std.mem.eql(u8, type_name, "f32.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.UnexpectedToken;
            }
            const value = try std.fmt.parseFloat(f32, self.current_token.number);
            try self.advance();
            try self.expectToken(.right_paren);
            return wast.Value{ .f32 = value };
        } else if (std.mem.eql(u8, type_name, "f64.const")) {
            if (self.current_token != .number) {
                return TextDecodeError.UnexpectedToken;
            }
            const value = try std.fmt.parseFloat(f64, self.current_token.number);
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
test "lexer basic tokens" {
    var lexer = Lexer.init("(module (memory 1))");
    
    const tokens = [_]std.meta.Tag(Token){
        .left_paren, .identifier, .left_paren, .identifier, .number, .right_paren, .right_paren, .eof
    };
    
    for (tokens) |expected| {
        const token = try lexer.nextToken();
        try std.testing.expect(std.meta.activeTag(token) == expected);
    }
}