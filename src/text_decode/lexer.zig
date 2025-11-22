const std = @import("std");

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
    line: u32,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .pos = 0,
            .current_char = null,
            .line = 1,
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
            if (char == ' ' or char == '\t' or char == '\r') {
                self.advance();
            } else if (char == '\n') {
                self.line += 1;
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn skipComment(self: *Lexer) void {
        if (self.current_char == ';' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';') {
            while (self.current_char) |char| {
                if (char == '\n') {
                    self.line += 1;
                    self.advance();
                    break;
                }
                self.advance();
            }
        } else if (self.current_char == '(' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';') {
            self.advance();
            self.advance();
            var depth: u32 = 1;
            while (self.current_char != null and depth > 0) {
                const char = self.current_char.?;
                if (char == '\n') {
                    self.line += 1;
                }
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
                (self.current_char == '(' and self.pos + 1 < self.input.len and self.input[self.pos + 1] == ';'))
            {
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

            std.debug.print("Error: Unexpected token at line {d}: col {d}: {s}\n", .{ self.line, self.getColumn(), self.getCurrentLine() });
            return TextDecodeError.UnexpectedToken;
        }
    }

    pub fn getCurrentLine(self: *Lexer) []const u8 {
        var line_start: usize = 0;
        var line_end: usize = self.input.len;

        // Find start of the current line
        var i: usize = self.pos;
        while (i > 0) : (i -= 1) {
            if (self.input[i - 1] == '\n') {
                line_start = i;
                break;
            }
        }

        // Find end of the current line
        i = self.pos;
        while (i < self.input.len) : (i += 1) {
            if (self.input[i] == '\n') {
                line_end = i;
                break;
            }
        }

        const line = self.input[line_start..line_end];
        return line;
    }

    pub fn getColumn(self: *Lexer) usize {
        var line_start: usize = 0;

        // Find start of the current line
        var i: usize = self.pos;
        while (i > 0) : (i -= 1) {
            if (self.input[i - 1] == '\n') {
                line_start = i;
                break;
            }
        }

        return self.pos - line_start;
    }

    pub fn peekToken(self: *Lexer) !Token {
        // Save current state
        const saved_pos = self.pos;
        const saved_char = self.current_char;

        // Get next token
        const token = try self.nextToken();

        // Restore state
        self.pos = saved_pos;
        self.current_char = saved_char;

        return token;
    }

    fn readString(self: *Lexer) !Token {
        const start = self.pos;
        self.advance(); // skip opening quote

        while (self.current_char) |char| {
            if (char == '"') {
                const result = self.input[start + 1 .. self.pos];
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
            (text.len > 1 and (text[0] == '+' or text[0] == '-') and self.isDigit(text[1])))
        {
            return Token{ .number = text };
        }

        return Token{ .identifier = text };
    }

    fn isAlphaNumeric(self: *Lexer, char: u8) bool {
        _ = self;
        return (char >= 'a' and char <= 'z') or
            (char >= 'A' and char <= 'Z') or
            (char >= '0' and char <= '9') or
            char == '_' or char == '.' or char == '+' or char == '-' or char == '$' or
            char == '=' or char == ':' or char == '>' or char == '<' or char == '@' or
            char == '!' or char == '#' or char == '*' or char == '^' or char == '&' or
            char == '?' or char == '%' or char == '\'' or char == '`' or
            char == '|' or char == '/' or char == '\\' or char == '~';
    }

    fn isDigit(self: *Lexer, char: u8) bool {
        _ = self;
        return char >= '0' and char <= '9';
    }
};
