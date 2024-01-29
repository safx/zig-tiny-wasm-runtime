pub const Loader = @import("./loader.zig").ModuleLoader;
pub const Error = @import("./errors.zig").Error;
pub const BinaryReader = @import("./binary_reader.zig").BinaryReader;
pub const safeNumCast = @import("./utils.zig").safeNumCast;

test "fixture for BinaryReader" {
    const std = @import("std");
    const expectEqual = std.testing.expectEqual;

    // i32.const 77
    {
        const data: []const u8 = &.{ 0xcd, 0x00 };
        var b = BinaryReader.new(data);
        try expectEqual(@as(i32, 77), try b.readVarI32());
    }

    {
        const data: []const u8 = &.{ 0xef, 0xf9, 0xbe, 0xef, 0x9a, 0xf1, 0xd9, 0x92, 0x01 };
        var b = BinaryReader.new(data);
        try expectEqual(@as(i64, 82586009202572527), try b.readVarI64());
    }
}
