const std = @import("std");
const command = @import("./command.zig");

test "spec-types: Value creation" {
    const v1 = command.Value{ .i32 = 42 };
    const v2 = command.Value{ .f32 = 0x3f800000 }; // 1.0 bit pattern
    
    try std.testing.expect(v1.i32 == 42);
    try std.testing.expect(v2.f32 == 0x3f800000);
}

test "spec-types: Float type with NaN" {
    const f1 = command.FloatType(u32){ .value = 0x3f800000 };
    const f2 = command.FloatType(u32){ .nan_canonical = {} };
    const f3 = command.FloatType(u32){ .nan_arithmetic = {} };
    
    try std.testing.expect(f1 == .value);
    try std.testing.expect(f2 == .nan_canonical);
    try std.testing.expect(f3 == .nan_arithmetic);
}

test "spec-types: Result with vec_f32" {
    const vec = [4]command.FloatType(u32){
        .{ .value = 0x3f800000 },
        .{ .nan_canonical = {} },
        .{ .value = 0x40000000 },
        .{ .nan_arithmetic = {} },
    };
    
    const result = command.Result{ .vec_f32 = vec };
    try std.testing.expect(result == .vec_f32);
    try std.testing.expect(result.vec_f32[0] == .value);
    try std.testing.expect(result.vec_f32[1] == .nan_canonical);
}

test "spec-types: Command with null module" {
    const cmd = command.Command{
        .register = .{
            .as_name = "test",
            .name = null, // Current module
        },
    };
    try std.testing.expect(cmd.register.name == null);
}

test "spec-types: Action with module name" {
    const action = command.Action{
        .invoke = .{
            .field = "add",
            .args = &.{},
            .module = "math",
        },
    };
    try std.testing.expect(action == .invoke);
    try std.testing.expectEqualStrings("math", action.invoke.module.?);
}

test "spec-types: module_quote command" {
    const cmd = command.Command{ .module_quote = {} };
    try std.testing.expect(cmd == .module_quote);
}

test "spec-types: v128 SIMD value" {
    const v128_val: i128 = 0x0102030405060708090a0b0c0d0e0f10;
    const val = command.Value{ .v128 = v128_val };
    
    try std.testing.expect(val.v128 == v128_val);
    
    // Lane extraction test
    const bytes: [16]u8 = @bitCast(@as(u128, @bitCast(v128_val)));
    try std.testing.expect(bytes[0] == 0x10); // Little endian
    try std.testing.expect(bytes[15] == 0x01);
}
