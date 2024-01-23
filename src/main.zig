const std = @import("std");
const spec = @import("wasm-spec-test");

pub fn main() !void {
    for (std.os.argv, 0..) |arg, i| {
        if (i == 0) continue;
        const file_name: []const u8 = std.mem.span(arg);
        try spec.execSpecTestsFromFile(file_name);
    }
}
