const std = @import("std");
const core = @import("wasm-core");
const Error = @import("./errors.zig").Error || error{OutOfMemory};

// Type aliases for convenience
const ValueType = core.types.ValueType;

pub const ValidationType = enum(u8) {
    i32 = val(.i32),
    i64 = val(.i64),
    f32 = val(.f32),
    f64 = val(.f64),
    v128 = val(.v128),
    func_ref = val(.func_ref),
    extern_ref = val(.extern_ref),

    any = 0,

    fn val(comptime v: ValueType) u8 {
        return @intFromEnum(v);
    }

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const TypeStack = struct {
    const Self = @This();
    const Stack = std.array_list.Managed(ValidationType);

    array: Stack,
    polymophic: bool = false,
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        return .{
            .array = Stack.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn push(self: *Self, val_type: ValidationType) error{OutOfMemory}!void {
        try self.array.append(val_type);
    }

    pub fn pushValueType(self: *Self, value_type: ValueType) error{OutOfMemory}!void {
        try self.push(validationTypeFromValueType(value_type));
    }

    pub fn appendValueType(self: *Self, value_types: []const ValueType) error{OutOfMemory}!void {
        for (value_types) |v|
            try self.pushValueType(v);
    }

    pub fn pop(self: *Self) error{TypeMismatch}!ValidationType {
        if (self.isEmpty())
            return Error.TypeMismatch;
        return self.array.pop().?;
    }

    pub fn polymophicPop(self: *Self) error{TypeMismatch}!ValidationType {
        if (self.polymophic and self.isEmpty())
            return .any;
        return try self.pop();
    }

    pub fn isEmpty(self: *Self) bool {
        return self.array.items.len == 0;
    }

    pub fn setPolymophic(self: *Self) error{OutOfMemory}!void {
        try self.array.resize(0);
        self.polymophic = true;
    }

    pub fn popWithChecking(self: *Self, expected_validate_type: ValidationType) error{TypeMismatch}!void {
        if (self.polymophic and self.isEmpty())
            return;

        const popped = try self.pop();
        if (popped != .any and popped != expected_validate_type)
            return Error.TypeMismatch;
    }

    pub fn popWithCheckingValueType(self: *Self, expected_value_type: ValueType) error{TypeMismatch}!void {
        try self.popWithChecking(validationTypeFromValueType(expected_value_type));
    }

    pub fn popValuesWithCheckingValueType(self: *Self, expected_value_types: []const ValueType) error{TypeMismatch}!void {
        var i = expected_value_types.len;
        while (i > 0) : (i -= 1) {
            try self.popWithCheckingValueType(expected_value_types[i - 1]);
        }
    }
};

fn validationTypeFromValueType(ty: ValueType) ValidationType {
    return @enumFromInt(@intFromEnum(ty));
}
