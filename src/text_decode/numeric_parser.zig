const std = @import("std");
const TextDecodeError = @import("lexer.zig").TextDecodeError;

const libc = @cImport({
    @cInclude("stdlib.h");
});

/// Helper function to remove underscores from numeric literals
/// WASM allows underscores in numeric literals (e.g., 1_000_000)
fn removeUnderscores(input: []const u8, buffer: []u8) ![]u8 {
    var idx: usize = 0;
    for (input) |c| {
        if (c != '_') {
            if (idx >= buffer.len) return TextDecodeError.InvalidNumber;
            buffer[idx] = c;
            idx += 1;
        }
    }
    return buffer[0..idx];
}

/// Parse floating point number from string
/// Supports:
/// - Regular decimal numbers (e.g., "3.14", "-2.5")
/// - Special values: "inf", "-inf", "nan", "-nan"
/// - NaN with payload: "nan:0x123", "-nan:0x456"
pub fn parseFloat(comptime T: type, input: []const u8) !T {
    if (@typeInfo(T) != .float) {
        @compileError("parseFloat only supports float types");
    }

    // Check for special identifiers (inf, -inf, nan, -nan with optional payload)
    if (std.mem.startsWith(u8, input, "nan") or std.mem.startsWith(u8, input, "-nan")) {
        const is_negative = std.mem.startsWith(u8, input, "-");
        const nan_start: usize = if (is_negative) 4 else 3; // Skip "-nan" or "nan"

        // Check for payload: nan:0x<hex>
        if (input.len > nan_start and input[nan_start] == ':') {
            const payload_str = input[nan_start + 1 ..];
            // Remove underscores from payload
            var buffer: [256]u8 = undefined;
            const clean_payload = try removeUnderscores(payload_str, &buffer);

            if (T == f32) {
                const payload = try std.fmt.parseInt(u32, clean_payload, 0);
                const sign_bit: u32 = if (is_negative) 1 << 31 else 0;
                const exponent: u32 = 0xFF << 23; // All 1s in exponent (8 bits)
                const mantissa: u32 = payload & 0x7FFFFF; // 23 bits
                const bits = sign_bit | exponent | mantissa;
                return @bitCast(bits);
            } else {
                const payload = try std.fmt.parseInt(u64, clean_payload, 0);
                const sign_bit: u64 = if (is_negative) 1 << 63 else 0;
                const exponent: u64 = 0x7FF << 52; // All 1s in exponent (11 bits)
                const mantissa: u64 = payload & 0xFFFFFFFFFFFFF; // 52 bits
                const bits = sign_bit | exponent | mantissa;
                return @bitCast(bits);
            }
        } else {
            // Plain nan without payload
            const sign: T = if (is_negative) -1.0 else 1.0;
            return sign * std.math.nan(T);
        }
    } else if (std.mem.eql(u8, input, "inf")) {
        return std.math.inf(T);
    } else if (std.mem.eql(u8, input, "-inf")) {
        return -std.math.inf(T);
    } else {
        // Regular number - parse as float
        var buffer: [512]u8 = undefined;
        const clean = try removeUnderscores(input, &buffer);
        
        // Use C library for correct IEEE 754 rounding
        var c_buffer: [512]u8 = undefined;
        const c_str = try std.fmt.bufPrintZ(&c_buffer, "{s}", .{clean});
        
        if (T == f32) {
            const result = libc.strtof(c_str.ptr, null);
            return result;
        } else {
            const result = libc.strtod(c_str.ptr, null);
            return result;
        }
    }
}

/// Parse integer from string with underscore support
/// For signed types (i32, i64): handles negative numbers directly
/// For positive values that should be interpreted as unsigned but stored in signed type,
/// this function parses as unsigned and bitcasts to signed (WebAssembly pattern)
pub fn parseInteger(comptime T: type, input: []const u8) !T {
    // Remove underscores
    var buffer: [256]u8 = undefined;
    const clean = try removeUnderscores(input, &buffer);

    // Determine if this is a negative number
    const is_negative = clean.len > 0 and clean[0] == '-';

    return switch (@typeInfo(T)) {
        .int => |info| {
            if (info.signedness == .signed) {
                // For signed types
                if (is_negative) {
                    // Parse as signed
                    return try std.fmt.parseInt(T, clean, 0);
                } else {
                    // Parse as unsigned and bitcast (WebAssembly pattern)
                    const UnsignedT = @Type(.{ .int = .{
                        .signedness = .unsigned,
                        .bits = info.bits,
                    } });
                    const unsigned_value = try std.fmt.parseInt(UnsignedT, clean, 0);
                    return @bitCast(unsigned_value);
                }
            } else {
                // For unsigned types, parse directly
                return try std.fmt.parseInt(T, clean, 0);
            }
        },
        else => @compileError("parseInteger only supports integer types"),
    };
}

// ============================================================================
// Tests
// ============================================================================

test "removeUnderscores - simple number without underscores" {
    var buffer: [256]u8 = undefined;
    const result = try removeUnderscores("12345", &buffer);
    try std.testing.expectEqualStrings("12345", result);
}

test "removeUnderscores - number with underscores" {
    var buffer: [256]u8 = undefined;
    const result = try removeUnderscores("1_000_000", &buffer);
    try std.testing.expectEqualStrings("1000000", result);
}

test "removeUnderscores - hex number with underscores" {
    var buffer: [256]u8 = undefined;
    const result = try removeUnderscores("0x_FF_FF", &buffer);
    try std.testing.expectEqualStrings("0xFFFF", result);
}

test "removeUnderscores - negative number with underscores" {
    var buffer: [256]u8 = undefined;
    const result = try removeUnderscores("-1_234", &buffer);
    try std.testing.expectEqualStrings("-1234", result);
}

test "removeUnderscores - buffer too small" {
    var buffer: [3]u8 = undefined;
    const result = removeUnderscores("12345", &buffer);
    try std.testing.expectError(TextDecodeError.InvalidNumber, result);
}

// parseInteger tests

test "parseInteger - i32 positive decimal" {
    const result = try parseInteger(i32, "42");
    try std.testing.expectEqual(@as(i32, 42), result);
}

test "parseInteger - i32 negative decimal" {
    const result = try parseInteger(i32, "-42");
    try std.testing.expectEqual(@as(i32, -42), result);
}

test "parseInteger - i32 hex with underscores" {
    const result = try parseInteger(i32, "0xFF_FF");
    try std.testing.expectEqual(@as(i32, @bitCast(@as(u32, 0xFFFF))), result);
}

test "parseInteger - i32 large positive (unsigned pattern)" {
    // 0xFFFFFFFF as u32 = 4294967295, when bitcast to i32 = -1
    const result = try parseInteger(i32, "0xFFFFFFFF");
    try std.testing.expectEqual(@as(i32, -1), result);
}

test "parseInteger - i64 positive decimal with underscores" {
    const result = try parseInteger(i64, "1_000_000");
    try std.testing.expectEqual(@as(i64, 1000000), result);
}

test "parseInteger - i64 negative decimal" {
    const result = try parseInteger(i64, "-9223372036854775808");
    try std.testing.expectEqual(@as(i64, -9223372036854775808), result);
}

test "parseInteger - i64 large hex (unsigned pattern)" {
    const result = try parseInteger(i64, "0xFFFFFFFFFFFFFFFF");
    try std.testing.expectEqual(@as(i64, -1), result);
}

test "parseInteger - u32 positive decimal" {
    const result = try parseInteger(u32, "4294967295");
    try std.testing.expectEqual(@as(u32, 4294967295), result);
}

test "parseInteger - u32 hex with underscores" {
    const result = try parseInteger(u32, "0xFF_FF_FF_FF");
    try std.testing.expectEqual(@as(u32, 0xFFFFFFFF), result);
}

test "parseInteger - u64 positive decimal" {
    const result = try parseInteger(u64, "18446744073709551615");
    try std.testing.expectEqual(@as(u64, 18446744073709551615), result);
}

// parseFloat tests

test "parseFloat - f32 simple decimal" {
    const result = try parseFloat(f32, "3.14");
    try std.testing.expectApproxEqRel(@as(f32, 3.14), result, 0.0001);
}

test "parseFloat - f32 negative decimal" {
    const result = try parseFloat(f32, "-2.5");
    try std.testing.expectApproxEqRel(@as(f32, -2.5), result, 0.0001);
}

test "parseFloat - f32 positive infinity" {
    const result = try parseFloat(f32, "inf");
    try std.testing.expect(std.math.isPositiveInf(result));
}

test "parseFloat - f32 negative infinity" {
    const result = try parseFloat(f32, "-inf");
    try std.testing.expect(std.math.isNegativeInf(result));
}

test "parseFloat - f32 positive nan" {
    const result = try parseFloat(f32, "nan");
    try std.testing.expect(std.math.isNan(result));
}

test "parseFloat - f32 negative nan" {
    const result = try parseFloat(f32, "-nan");
    try std.testing.expect(std.math.isNan(result));
}

test "parseFloat - f32 nan with payload" {
    const result = try parseFloat(f32, "nan:0x123");
    try std.testing.expect(std.math.isNan(result));
    // Verify payload bits are preserved
    const bits: u32 = @bitCast(result);
    const mantissa = bits & 0x7FFFFF;
    try std.testing.expectEqual(@as(u32, 0x123), mantissa);
}

test "parseFloat - f32 negative nan with payload" {
    const result = try parseFloat(f32, "-nan:0x456");
    try std.testing.expect(std.math.isNan(result));
    const bits: u32 = @bitCast(result);
    const sign_bit = (bits >> 31) & 1;
    try std.testing.expectEqual(@as(u32, 1), sign_bit);
    const mantissa = bits & 0x7FFFFF;
    try std.testing.expectEqual(@as(u32, 0x456), mantissa);
}

test "parseFloat - f64 simple decimal" {
    const result = try parseFloat(f64, "3.141592653589793");
    try std.testing.expectApproxEqRel(@as(f64, 3.141592653589793), result, 0.0000001);
}

test "parseFloat - f64 positive infinity" {
    const result = try parseFloat(f64, "inf");
    try std.testing.expect(std.math.isPositiveInf(result));
}

test "parseFloat - f64 negative infinity" {
    const result = try parseFloat(f64, "-inf");
    try std.testing.expect(std.math.isNegativeInf(result));
}

test "parseFloat - f64 nan with payload" {
    const result = try parseFloat(f64, "nan:0x123456");
    try std.testing.expect(std.math.isNan(result));
    const bits: u64 = @bitCast(result);
    const mantissa = bits & 0xFFFFFFFFFFFFF;
    try std.testing.expectEqual(@as(u64, 0x123456), mantissa);
}

// Hexfloat tests (C99/C++11 format)

test "parseFloat - f32 hexfloat simple" {
    const result = try parseFloat(f32, "0x1p0");
    try std.testing.expectApproxEqRel(@as(f32, 1.0), result, 0.0001);
}

test "parseFloat - f32 hexfloat with exponent" {
    const result = try parseFloat(f32, "0x1p127");
    try std.testing.expectApproxEqRel(@as(f32, 1.7014118346046923e+38), result, 0.0001);
}

test "parseFloat - f32 hexfloat with fraction" {
    const result = try parseFloat(f32, "0x1.fffffep127");
    try std.testing.expectApproxEqRel(@as(f32, 3.4028234663852886e+38), result, 0.0001);
}

test "parseFloat - f32 hexfloat negative" {
    const result = try parseFloat(f32, "-0x1.fffffep127");
    try std.testing.expectApproxEqRel(@as(f32, -3.4028234663852886e+38), result, 0.0001);
}

test "parseFloat - f64 hexfloat simple" {
    const result = try parseFloat(f64, "0x1p0");
    try std.testing.expectApproxEqRel(@as(f64, 1.0), result, 0.0000001);
}

test "parseFloat - f64 hexfloat with exponent" {
    const result = try parseFloat(f64, "0x1p1023");
    try std.testing.expect(std.math.isFinite(result));
}

test "parseFloat - f64 hexfloat with fraction" {
    const result = try parseFloat(f64, "0x1.fffffffffffffp1023");
    try std.testing.expect(std.math.isFinite(result));
}
