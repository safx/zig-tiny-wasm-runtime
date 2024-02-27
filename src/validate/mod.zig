const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
};
const Context = @import("./context.zig").Context;
pub const Error = @import("./errors.zig").Error;

pub fn validateModule(module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    const context = try Context.new(module, allocator);
    defer context.deinit(allocator);
}
