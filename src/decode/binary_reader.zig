const std = @import("std");
const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

pub const BinaryReader = struct {
    const Self = @This();

    buffer: []const u8,
    position: usize,

    pub fn new(data: []const u8) BinaryReader {
        return .{ .buffer = data, .position = 0 };
    }

    pub fn eof(self: Self) bool {
        return self.position >= self.buffer.len;
    }

    pub fn peek(self: Self) error{UnexpectedEndOfBuffer}!u8 {
        if (self.eof())
            return Error.UnexpectedEndOfBuffer;

        return self.buffer[self.position];
    }

    fn ensureHasBytes(self: Self, len: usize) error{UnexpectedEndOfBuffer}!void {
        if (self.position + len > self.buffer.len)
            return Error.UnexpectedEndOfBuffer;
    }

    pub fn readBytes(self: *Self, size: usize) error{UnexpectedEndOfBuffer}![]const u8 {
        try self.ensureHasBytes(size);
        const start = self.position;
        self.position += size;
        return self.buffer[start..self.position];
    }

    pub fn readI128(reader: *BinaryReader) Error!i128 {
        const buf = try reader.readBytes(16);
        var val: i128 = std.mem.readInt(i128, @as(*[16]u8, @ptrCast(@constCast(&buf[0]))), .Little);
        return val;
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
        if (NumType != i32 and NumType != u32 and NumType != i64 and NumType != u64)
            @compileError("Unknown Number Type");

        const bit_size = @bitSizeOf(NumType);
        const max_shift = if (bit_size == 32) 28 else 63;
        const signed_integer_type = NumType == i32 or NumType == i64;
        const ShiftType = if (bit_size == 32) u5 else u6;

        var result: NumType = 0;
        var shift: ShiftType = 0;
        while (true) : (shift += 7) {
            const byte = try self.readU8();

            if (shift == max_shift)
                try checkIntegerTooLarge(NumType, byte);

            const num = @as(NumType, byte) & 0x7f;
            result |= num << shift;

            if (byte & 0x80 == 0) {
                if (signed_integer_type and shift < max_shift and 0x40 != 0) {
                    const top_bit = result & @as(NumType, 1) << (shift + 6);
                    return (result ^ top_bit) - top_bit;
                } else {
                    return result;
                }
            }
        }
        unreachable;
    }
};

inline fn checkIntegerTooLarge(comptime NumType: type, byte: u8) (error{IntegerRepresentationTooLong} || error{IntegerTooLarge})!void {
    const too_large = switch (NumType) {
        u32 => byte & 0xf0 > 0,
        u64 => byte & 0xfe > 0,
        i32 => blk: {
            const sign_bit = byte & 0x08 != 0;
            const top_bits = byte & 0xf0;
            break :blk (sign_bit and top_bits != 0x70) or (!sign_bit and top_bits != 0);
        },
        i64 => blk: {
            const sign_bit = byte & 0x01 != 0;
            const top_bits = byte & 0x7e;
            break :blk (sign_bit and top_bits != 0x7e) or (!sign_bit and top_bits != 0);
        },
        else => unreachable,
    };

    if (byte & 0x80 > 0)
        return Error.IntegerRepresentationTooLong;

    if (too_large)
        return Error.IntegerTooLarge;
}

test BinaryReader {
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
