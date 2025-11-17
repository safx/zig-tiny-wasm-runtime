const std = @import("std");
const core = @import("wasm-core");
const Module = core.types.Module;
const ModuleValidator = @import("./validator.zig").ModuleValidator;
pub const Error = @import("./errors.zig").Error;

pub fn validateModule(module: Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    var validator = ModuleValidator.new(allocator);
    try validator.validate(module);
}
