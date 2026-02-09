const std = @import("std");
const spec_types = @import("spec-types");
const runtime = @import("wasm-runtime");

pub fn resultEquals(runtime_val: runtime.types.Value, expected: spec_types.command.Result) bool {
    return switch (expected) {
        .i32 => |exp| runtime_val == .i32 and runtime_val.i32 == exp,
        .i64 => |exp| runtime_val == .i64 and runtime_val.i64 == exp,
        .f32 => |exp| blk: {
            if (runtime_val != .f32) break :blk false;
            switch (exp) {
                .value => |bits| break :blk runtime_val.f32 == bits,
                .nan_canonical => break :blk isCanonicalNanF32(runtime_val.f32),
                .nan_arithmetic => break :blk isArithmeticNanF32(runtime_val.f32),
            }
        },
        .f64 => |exp| blk: {
            if (runtime_val != .f64) break :blk false;
            switch (exp) {
                .value => |bits| break :blk runtime_val.f64 == bits,
                .nan_canonical => break :blk isCanonicalNanF64(runtime_val.f64),
                .nan_arithmetic => break :blk isArithmeticNanF64(runtime_val.f64),
            }
        },
        .v128 => |exp| runtime_val == .v128 and runtime_val.v128 == exp,
        .vec_f32 => |lanes| blk: {
            if (runtime_val != .v128) break :blk false;
            const v128_val = runtime_val.v128;
            for (lanes, 0..) |lane, i| {
                const lane_bits = extractF32Lane(v128_val, i);
                const match = switch (lane) {
                    .value => |bits| lane_bits == bits,
                    .nan_canonical => isCanonicalNanF32(lane_bits),
                    .nan_arithmetic => isArithmeticNanF32(lane_bits),
                };
                if (!match) break :blk false;
            }
            break :blk true;
        },
        .vec_f64 => |lanes| blk: {
            if (runtime_val != .v128) break :blk false;
            const v128_val = runtime_val.v128;
            for (lanes, 0..) |lane, i| {
                const lane_bits = extractF64Lane(v128_val, i);
                const match = switch (lane) {
                    .value => |bits| lane_bits == bits,
                    .nan_canonical => isCanonicalNanF64(lane_bits),
                    .nan_arithmetic => isArithmeticNanF64(lane_bits),
                };
                if (!match) break :blk false;
            }
            break :blk true;
        },
        .func_ref => |exp| runtime_val == .func_ref and runtime_val.func_ref == exp,
        .extern_ref => |exp| runtime_val == .extern_ref and runtime_val.extern_ref == exp,
        .either => |alternatives| blk: {
            for (alternatives) |alt| {
                if (resultEquals(runtime_val, alt)) break :blk true;
            }
            break :blk false;
        },
    };
}

pub fn isCanonicalNanF32(bits: u32) bool {
    return bits == 0x7fc00000 or bits == 0xffc00000;
}

pub fn isArithmeticNanF32(bits: u32) bool {
    const exp = (bits >> 23) & 0xff;
    const mantissa = bits & 0x7fffff;
    return exp == 0xff and mantissa != 0;
}

pub fn isCanonicalNanF64(bits: u64) bool {
    return bits == 0x7ff8000000000000 or bits == 0xfff8000000000000;
}

pub fn isArithmeticNanF64(bits: u64) bool {
    const exp = (bits >> 52) & 0x7ff;
    const mantissa = bits & 0xfffffffffffff;
    return exp == 0x7ff and mantissa != 0;
}

fn extractF32Lane(v128: i128, lane: usize) u32 {
    const shift = @as(u7, @intCast(lane * 32));
    return @truncate(@as(u128, @bitCast(v128)) >> shift);
}

fn extractF64Lane(v128: i128, lane: usize) u64 {
    const shift = @as(u7, @intCast(lane * 64));
    return @truncate(@as(u128, @bitCast(v128)) >> shift);
}

test "canonical NaN f32" {
    try std.testing.expect(isCanonicalNanF32(0x7fc00000));
    try std.testing.expect(isCanonicalNanF32(0xffc00000));
    try std.testing.expect(!isCanonicalNanF32(0x7fc00001));
    try std.testing.expect(!isCanonicalNanF32(0x7f800000));
}

test "arithmetic NaN f32" {
    try std.testing.expect(isArithmeticNanF32(0x7fc00000));
    try std.testing.expect(isArithmeticNanF32(0x7fc00001));
    try std.testing.expect(isArithmeticNanF32(0xffc00000));
    try std.testing.expect(!isArithmeticNanF32(0x7f800000));
    try std.testing.expect(!isArithmeticNanF32(0x3f800000));
}

test "canonical NaN f64" {
    try std.testing.expect(isCanonicalNanF64(0x7ff8000000000000));
    try std.testing.expect(isCanonicalNanF64(0xfff8000000000000));
    try std.testing.expect(!isCanonicalNanF64(0x7ff8000000000001));
    try std.testing.expect(!isCanonicalNanF64(0x7ff0000000000000));
}

test "arithmetic NaN f64" {
    try std.testing.expect(isArithmeticNanF64(0x7ff8000000000000));
    try std.testing.expect(isArithmeticNanF64(0x7ff8000000000001));
    try std.testing.expect(isArithmeticNanF64(0xfff8000000000000));
    try std.testing.expect(!isArithmeticNanF64(0x7ff0000000000000));
    try std.testing.expect(!isArithmeticNanF64(0x3ff0000000000000));
}
