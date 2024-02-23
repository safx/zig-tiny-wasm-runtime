const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

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

    pub fn peek(self: Self) error{UnexpectedEndOfBuffer}!u8 {
        if (self.eof()) {
            return Error.UnexpectedEndOfBuffer;
        }
        return self.buffer[self.position];
    }

    pub fn ensureHasBytes(self: Self, len: usize) error{UnexpectedEndOfBuffer}!void {
        if (self.position + len > self.buffer.len) {
            return Error.UnexpectedEndOfBuffer;
        }
    }

    pub fn readBytes(self: *Self, size: usize) error{UnexpectedEndOfBuffer}![]const u8 {
        try self.ensureHasBytes(size);
        const start = self.position;
        self.position += size;
        return self.buffer[start..self.position];
    }

    pub fn readU32(self: *Self) error{UnexpectedEndOfBuffer}!u32 {
        return try self.readNumber(u32);
    }

    pub fn readU8(self: *Self) error{UnexpectedEndOfBuffer}!u8 {
        try self.ensureHasBytes(1);
        const ch = self.buffer[self.position];
        self.position += 1;
        return ch;
    }

    pub fn readF32(self: *Self) error{UnexpectedEndOfBuffer}!f32 {
        return try self.readNumber(f32);
    }

    pub fn readF64(self: *Self) error{UnexpectedEndOfBuffer}!f64 {
        return try self.readNumber(f64);
    }

    fn readNumber(self: *Self, comptime T: type) error{UnexpectedEndOfBuffer}!T {
        const buffer = try self.readBytes(@sizeOf(T));
        return utils.safeNumCast(T, buffer);
    }

    pub fn readVarU32(self: *Self) Error!u32 {
        return self.readLeb(u32);
    }

    pub fn readVarI32(self: *Self) Error!i32 {
        return self.readLeb(i32);
    }

    pub fn readVarU64(self: *Self) Error!u64 {
        return self.readLeb(u64);
    }

    pub fn readVarI64(self: *Self) Error!i64 {
        return self.readLeb(i64);
    }

    fn readLeb(self: *Self, comptime NumType: type) Error!NumType {
        if (NumType != i32 and NumType != u32 and NumType != i64 and NumType != u64) {
            @compileError("Unknown Number Type");
        }

        const bit_size = @bitSizeOf(NumType);
        const max_shift = if (bit_size == 32) 28 else 63;
        const BaseType = if (bit_size == 32) u32 else u64;
        const ShiftType = if (bit_size == 32) u5 else u6;

        var result: BaseType = 0;
        var shift: ShiftType = 0;
        while (true) {
            const byte = try self.readU8();

            if (shift == max_shift) {
                if (byte & 0x80 > 0)
                    return Error.IntegerRepresentationTooLong;

                switch (NumType) {
                    u32 => if (byte & 0xf0 > 0) return Error.IntegerTooLarge,
                    u64 => if (byte & 0xfe > 0) return Error.IntegerTooLarge,
                    i32 => {
                        const sign_bit = byte & 0x08 > 0;
                        const top_bits = byte & 0xf0;
                        if ((sign_bit and top_bits != 0x70) or (!sign_bit and top_bits != 0)) {
                            return Error.IntegerTooLarge;
                        }
                    },
                    i64 => {
                        const sign_bit = byte & 0x01 > 0;
                        const top_bits = byte & 0x7e;
                        if ((sign_bit and top_bits != 0x7e) or (!sign_bit and top_bits != 0)) {
                            return Error.IntegerTooLarge;
                        }
                    },
                    else => unreachable,
                }
            }

            const num = @as(BaseType, byte);
            result |= (num & 0x7f) << shift;

            if (byte & 0x80 == 0)
                break;

            shift += 7;
        }

        const signed_integer_type = NumType == i32 or NumType == i64;
        if (signed_integer_type) {
            const value: NumType = @bitCast(result);
            if (shift == max_shift) {
                return value;
            }
            const top_bit = value & @as(NumType, 1) << (shift + 6);
            return (value ^ top_bit) - top_bit;
        } else {
            return result;
        }
    }
};

test BinaryReader {
    const std = @import("std");
    const expectEqual = std.testing.expectEqual;

    {
        const data: []const u8 = &.{ 0xcd, 0x00 };
        var b = BinaryReader.new(data);
        try expectEqual(@as(i32, 77), try b.readVarI32());
    }

    {
        const data: []const u8 = &.{0x7f};
        var b = BinaryReader.new(data);
        try expectEqual(@as(i32, -1), try b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0xff, 0xff, 0xff, 0xff, 0x7f };
        var b = BinaryReader.new(data);
        try expectEqual(@as(i32, -1), try b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0xef, 0xf9, 0xbe, 0xef, 0x9a, 0xf1, 0xd9, 0x92, 0x01 };
        var b = BinaryReader.new(data);
        try expectEqual(@as(i64, 0x0125_6789_adef_bcef), try b.readVarI64());
    }

    {
        const data: []const u8 = &.{ 0xdf, 0xf7, 0xbf, 0xd6, 0x79 };
        var b = BinaryReader.new(data);
        const v: u32 = 0x9acf_fbdf;
        try expectEqual(@as(i32, @bitCast(v)), try b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0x80, 0x80, 0x80, 0x80, 0x70 };
        var b = BinaryReader.new(data);
        try std.testing.expectError(Error.IntegerTooLarge, b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0x80, 0x80, 0x80, 0x80, 0x80, 0x00 };
        var b = BinaryReader.new(data);
        try std.testing.expectError(Error.IntegerRepresentationTooLong, b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0x80, 0x80, 0x80, 0x80, 0x10 };
        var b = BinaryReader.new(data);
        try std.testing.expectError(Error.IntegerTooLarge, b.readVarU32());
    }

    {
        const data: []const u8 = &.{ 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x02 };
        var b = BinaryReader.new(data);
        try std.testing.expectError(Error.IntegerTooLarge, b.readVarU64());
    }
}
