const std = @import("std");
const spec_types = @import("spec-types");
const runtime = @import("wasm-runtime");

pub fn toRuntimeValue(spec_val: spec_types.command.Value) runtime.types.Value {
    return switch (spec_val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}

pub fn toSpecResult(runtime_val: runtime.types.Value) spec_types.command.Result {
    return switch (runtime_val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = .{ .value = v } },
        .f64 => |v| .{ .f64 = .{ .value = v } },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}

test "toRuntimeValue i32" {
    const spec_val = spec_types.command.Value{ .i32 = 42 };
    const runtime_val = toRuntimeValue(spec_val);
    try std.testing.expectEqual(runtime.types.Value{ .i32 = 42 }, runtime_val);
}

test "toRuntimeValue f32" {
    const spec_val = spec_types.command.Value{ .f32 = 0x3f800000 };
    const runtime_val = toRuntimeValue(spec_val);
    try std.testing.expectEqual(runtime.types.Value{ .f32 = 0x3f800000 }, runtime_val);
}

test "toSpecResult i64" {
    const runtime_val = runtime.types.Value{ .i64 = 123456 };
    const spec_result = toSpecResult(runtime_val);
    try std.testing.expectEqual(spec_types.command.Result{ .i64 = 123456 }, spec_result);
}

test "toSpecResult f64" {
    const runtime_val = runtime.types.Value{ .f64 = 0x3ff0000000000000 };
    const spec_result = toSpecResult(runtime_val);
    try std.testing.expectEqual(spec_types.command.Result{ .f64 = .{ .value = 0x3ff0000000000000 } }, spec_result);
}
