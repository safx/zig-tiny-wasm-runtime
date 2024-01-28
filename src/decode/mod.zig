pub const Loader = @import("./loader.zig").ModuleLoader;
pub const Error = @import("./errors.zig").Error;
pub const BinaryReader = @import("./binary_reader.zig").BinaryReader;
pub const safeNumCast = @import("./utils.zig").safeNumCast;

test "fixture for i32.const 77" {
    const std = @import("std");
    const expectEqual = std.testing.expectEqual;

    const data: []const u8 = &.{ 0xcd, 0x00 };
    var b = BinaryReader.new(data);
    try expectEqual(@as(i32, 77), try b.readVarI32());
}
