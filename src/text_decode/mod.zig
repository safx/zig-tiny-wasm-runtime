const std = @import("std");
const wasm_core = @import("wasm-core");

pub const ModuleBuilder = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(wasm_core.FuncType),
    funcs: std.ArrayList(wasm_core.Func),
    tables: std.ArrayList(wasm_core.TableType),
    memories: std.ArrayList(wasm_core.MemoryType),
    globals: std.ArrayList(wasm_core.Global),
    elements: std.ArrayList(wasm_core.Element),
    datas: std.ArrayList(wasm_core.Data),
    start: ?wasm_core.FuncIdx,
    imports: std.ArrayList(wasm_core.Import),
    exports: std.ArrayList(wasm_core.Export),

    pub fn init(allocator: std.mem.Allocator) ModuleBuilder {
        return ModuleBuilder{
            .allocator = allocator,
            .types = std.ArrayList(wasm_core.FuncType).init(allocator),
            .funcs = std.ArrayList(wasm_core.Func).init(allocator),
            .tables = std.ArrayList(wasm_core.TableType).init(allocator),
            .memories = std.ArrayList(wasm_core.MemoryType).init(allocator),
            .globals = std.ArrayList(wasm_core.Global).init(allocator),
            .elements = std.ArrayList(wasm_core.Element).init(allocator),
            .datas = std.ArrayList(wasm_core.Data).init(allocator),
            .start = null,
            .imports = std.ArrayList(wasm_core.Import).init(allocator),
            .exports = std.ArrayList(wasm_core.Export).init(allocator),
        };
    }

    pub fn build(self: *ModuleBuilder) !wasm_core.Module {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        const arena_allocator = arena.allocator();
        
        return wasm_core.Module{
            .types = try arena_allocator.dupe(wasm_core.FuncType, self.types.items),
            .funcs = try arena_allocator.dupe(wasm_core.Func, self.funcs.items),
            .tables = try arena_allocator.dupe(wasm_core.TableType, self.tables.items),
            .memories = try arena_allocator.dupe(wasm_core.MemoryType, self.memories.items),
            .globals = try arena_allocator.dupe(wasm_core.Global, self.globals.items),
            .elements = try arena_allocator.dupe(wasm_core.Element, self.elements.items),
            .datas = try arena_allocator.dupe(wasm_core.Data, self.datas.items),
            .start = self.start,
            .imports = try arena_allocator.dupe(wasm_core.Import, self.imports.items),
            .exports = try arena_allocator.dupe(wasm_core.Export, self.exports.items),
            .arena = arena,
        };
    }
    
    pub fn deinit(self: *ModuleBuilder) void {
        self.types.deinit();
        self.funcs.deinit();
        self.tables.deinit();
        self.memories.deinit();
        self.globals.deinit();
        self.elements.deinit();
        self.datas.deinit();
        self.imports.deinit();
        self.exports.deinit();
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

    pub fn parseModule(self: *Parser) !wasm_core.Module {
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
        
        const memory_type = wasm_core.MemoryType{
            .limits = wasm_core.Limits{
                .min = min_pages,
                .max = max_pages,
            },
        };
        
        try builder.memories.append(memory_type);
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
};

pub fn parseWastModule(allocator: std.mem.Allocator, input: []const u8) !wasm_core.Module {
    var parser = try Parser.init(allocator, input);
    return parser.parseModule();
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