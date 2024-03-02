const std = @import("std");
const Module = @import("wasm-core").Module;
const ModuleValidator = @import("./validator.zig").ModuleValidator;
pub const Error = @import("./errors.zig").Error;

pub fn validateModule(module: Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    var validator = ModuleValidator.new(allocator);
    try validator.validate(module);
}
