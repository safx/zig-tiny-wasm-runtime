const std = @import("std");
const spec = @import("wasm-spec-test");

pub fn main() !void {
    var verbose: u8 = 1;
    var file_name: ?[]const u8 = null;

    for (std.os.argv[1..]) |argv| {
        const arg: []const u8 = std.mem.span(argv);
        if (std.mem.eql(u8, arg, "-s")) {
            verbose = 0;
        } else if (std.mem.eql(u8, arg, "-v")) {
            verbose = 2;
        } else {
            file_name = arg;
        }
    }

    if (file_name) |n| {
        var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var runner = try spec.SpecTestRunneer.new(allocator, "spec_test", verbose);
        try runner.execFromFile(n);
    } else {
        std.debug.print("file expected", .{});
    }
}
