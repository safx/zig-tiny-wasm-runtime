pub const Loader = @import("./loader.zig").ModuleLoader;
pub const Error = @import("./errors.zig").Error;
pub const BinaryReader = @import("./binary_reader.zig").BinaryReader;
pub const safeNumCast = @import("./utils.zig").safeNumCast;

test {
    const std = @import("std");
    std.testing.refAllDecls(@This());
}
