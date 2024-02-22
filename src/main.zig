const std = @import("std");
const spec = @import("wasm-spec-test");

pub fn main() !void {
    var verbose: u8 = 1;
    var file_name: ?[]const u8 = null;

    for (std.os.argv, 0..) |argv, i| {
        if (i == 0) continue;

        const arg: []const u8 = std.mem.span(argv);
        if (std.mem.eql(u8, arg, "-s")) {
            verbose = 0;
        } else if (std.mem.eql(u8, arg, "-v")) {
            verbose = 2;
        } else {
            file_name = arg;
        }
    }
    try spec.execSpecTestsFromFile(file_name.?, verbose);
}
