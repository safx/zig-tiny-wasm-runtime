const std = @import("std");
const spec_types = @import("spec-types");
const text_decode = @import("wasm-text-decode");

pub fn readWastFromFile(file_path: []const u8, allocator: std.mem.Allocator) ![]spec_types.command.Command {
    const file_content = try std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024);
    defer allocator.free(file_content);

    return try text_decode.parseWastScript(allocator, file_content);
}

pub fn freeCommands(commands: []spec_types.command.Command, allocator: std.mem.Allocator) void {
    for (commands) |*cmd| {
        text_decode.freeCommand(allocator, cmd);
    }
    allocator.free(commands);
}
