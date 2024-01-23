const std = @import("std");
const utils = @import("./utils.zig");
const EOF = @import("./errors.zig").Error.EOF;

pub const BinaryReader = struct {
    const Self = @This();

    buffer: []const u8,
    position: usize,
    original_offset: usize,

    pub fn new(data: []const u8) BinaryReader {
        return .{ .buffer = data, .position = 0, .original_offset = 0 };
    }

    pub fn newWithOffset(data: []const u8, offset: usize) BinaryReader {
        return .{ .buffer = data, .position = 0, .original_offset = offset };
    }

    pub fn eof(self: Self) bool {
        return self.position >= self.buffer.len;
    }

    pub fn originalPosition(self: Self) usize {
        return self.original_offset + self.position;
    }

    pub fn peek(self: Self) error{EOF}!u8 {
        if (self.eof()) {
            return EOF;
        }
        return self.buffer[self.position];
    }

    pub fn ensureHasBytes(self: Self, len: usize) error{EOF}!void {
        if (self.position + len > self.buffer.len) {
            return EOF;
        }
    }

    pub fn readBytes(self: *Self, size: usize) error{EOF}![]const u8 {
        try self.ensureHasBytes(size);
        const start = self.position;
        self.position += size;
        return self.buffer[start..self.position];
    }

    pub fn readU32(self: *Self) error{EOF}!u32 {
        return try self.readNumber(u32);
    }

    pub fn readU8(self: *Self) error{EOF}!u8 {
        try self.ensureHasBytes(1);
        defer self.position += 1;
        return self.buffer[self.position];
    }

    pub fn readF32(self: *Self) error{EOF}!f32 {
        return try self.readNumber(f32);
    }

    pub fn readF64(self: *Self) error{EOF}!f64 {
        return try self.readNumber(f64);
    }

    fn readNumber(self: *Self, comptime T: type) error{EOF}!T {
        const buffer = try self.readBytes(@sizeOf(T));
        return utils.safeNumCast(T, buffer);
    }

    pub fn readVarU32(self: *Self) error{EOF}!u32 {
        return self.readLeb(u32, u5, false);
    }

    pub fn readVarI32(self: *Self) error{EOF}!i32 {
        return self.readLeb(i32, u5, true);
    }

    pub fn readVarU64(self: *Self) error{EOF}!u64 {
        return self.readLeb(u64, u6, false);
    }

    pub fn readVarI64(self: *Self) error{EOF}!i64 {
        return self.readLeb(i64, u6, true);
    }

    fn readLeb(self: *Self, comptime NumType: type, comptime ShiftType: type, comptime arith_shift: bool) error{EOF}!NumType {
        var result: NumType = 0;
        var shift: ShiftType = 0;
        while (true) {
            const byte = try self.readU8();
            const num = @as(NumType, byte);
            result |= (num & 0x7f) << shift;
            if (byte & 0x80 == 0)
                break;
            shift += 7;
        }
        if (arith_shift and shift > 0) {
            // avoiding `error: type 'u5' cannot represent integer value '32'`
            const ashift = (31 - shift) + 1;
            return (result << ashift) >> ashift;
        } else {
            return result;
        }
    }
};
