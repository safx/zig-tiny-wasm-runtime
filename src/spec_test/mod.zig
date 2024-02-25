const std = @import("std");
pub const SpecTestRunneer = @import("./test_runner.zig").SpecTestRunner;

test "Wasm spec test" {
    // try doWasmSpecTest();
}

pub fn doWasmSpecTest() !void {
    var buf: [4096]u8 = undefined;

    const cwd = std.fs.cwd();
    try std.os.chdir(try cwd.realpath("spec_test", &buf));

    var dir = try cwd.openIterableDir(".", .{});
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        const ext = getExtention(entry.name);
        if (std.mem.eql(u8, ext, "json")) {
            //try execSpecTestsFromFile(entry.name, 2);
        }
    }
}

fn getExtention(filename: []const u8) []const u8 {
    var parts = std.mem.split(u8, filename, ".");
    var elem: []const u8 = "";
    while (parts.next()) |p| {
        elem = p;
    }
    return elem;
}
