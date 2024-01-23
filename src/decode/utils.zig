const std = @import("std");
const wa = @import("wasm-core");

pub fn sectionFromNum(section_id: u8) ?std.wasm.Section {
    return if (0 <= section_id and section_id <= 12) @enumFromInt(section_id) else null;
}

pub fn valueTypeFromNum(byte: u8) ?wa.ValueType {
    return switch (byte) {
        0x7b...0x7f => @enumFromInt(byte),
        0x6f...0x70 => @enumFromInt(byte),
        else => null,
    };
}

pub fn refTypeFromNum(byte: u8) ?wa.RefType {
    return switch (byte) {
        0x6f...0x70 => @enumFromInt(byte),
        else => null,
    };
}

pub fn safeNumCast(comptime T: type, num_buffer: []const u8) T {
    const addr = @intFromPtr(&num_buffer[0]);
    if (addr % @alignOf(T) == 0) {
        const num_p: *const T = @ptrFromInt(addr);
        return num_p.*;
    } else {
        // copy to buffer for avoiding `panic: incorrect alignment`
        var buffer: [@sizeOf(T)]u8 align(@alignOf(T)) = undefined;
        std.mem.copyForwards(u8, &buffer, num_buffer);
        const num_p: *const T = @alignCast(@ptrCast(&buffer));
        return num_p.*;
    }
}
