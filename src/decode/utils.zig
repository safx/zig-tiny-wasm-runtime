const std = @import("std");
const types = @import("wasm-core");

pub fn sectionFromNum(section_id: u8) ?std.wasm.Section {
    return if (0 <= section_id and section_id <= 12) @enumFromInt(section_id) else null;
}

pub fn valueTypeFromNum(byte: u8) ?types.ValueType {
    return switch (byte) {
        0x6f...0x70 => @enumFromInt(byte),
        0x7b...0x7f => @enumFromInt(byte),
        else => null,
    };
}

pub fn refTypeFromNum(byte: u8) ?types.RefType {
    return switch (byte) {
        0x6f...0x70 => @enumFromInt(byte),
        else => null,
    };
}

pub fn safeNumCast(comptime T: type, buffer: []const u8) T {
    const p: *const [@sizeOf(T)]u8 = @ptrCast(&buffer[0]);
    return @bitCast(p.*);
}
