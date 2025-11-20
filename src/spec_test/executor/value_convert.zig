const std = @import("std");
const spec_types = @import("../types.zig");
const runtime = @import("wasm-runtime");

/// spec_types.Value → runtime.types.Value
pub fn toRuntimeValue(val: spec_types.Value) runtime.types.Value {
    return switch (val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },  // Bit pattern as-is
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}

/// runtime.types.Value → spec_types.Value
pub fn fromRuntimeValue(val: runtime.types.Value) spec_types.Value {
    return switch (val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}
