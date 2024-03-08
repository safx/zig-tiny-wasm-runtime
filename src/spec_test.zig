const std = @import("std");
const spec = @import("wasm-spec-test");

pub fn main() !void {
    var verbose: u8 = 1;
    var file_name: ?[]const u8 = null;

    var pos: usize = 1;
    while (pos < std.os.argv.len) : (pos += 1) {
        const arg: []const u8 = std.mem.span(std.os.argv[pos]);
        if (std.mem.eql(u8, arg, "-s")) {
            verbose = 0;
        } else if (std.mem.eql(u8, arg, "-v")) {
            verbose = 2;
        } else if (std.mem.eql(u8, arg, "-b")) {
            pos += 1;
            const base_dir = std.mem.span(std.os.argv[pos]);
            var buf: [4096]u8 = undefined;
            const cwd = std.fs.cwd();
            try std.os.chdir(try cwd.realpath(base_dir, &buf));
        } else {
            file_name = arg;
        }
    }

    if (file_name) |n| {
        var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var runner = try spec.SpecTestRunneer.new(allocator, verbose);
        try runner.execFromFile(n);
    } else {
        std.debug.print("file expected", .{});
    }
}
