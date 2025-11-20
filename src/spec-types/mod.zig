pub const command = @import("./command.zig");

// Re-export commonly used types
pub const Command = command.Command;
pub const Action = command.Action;
pub const Value = command.Value;
pub const Result = command.Result;
pub const FuncAddr = command.FuncAddr;
pub const ExternAddr = command.ExternAddr;
pub const FloatType = command.FloatType;

test {
    _ = @import("./test.zig");
}
