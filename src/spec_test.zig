const std = @import("std");
const spec = @import("wasm-spec-test");

/// Main entry point for spec test runner
/// Supports both .wast/.wat text format and .json spec test format
pub fn main() !void {
    const args = try parseArgs();
    
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (args.file_name) |file_path| {
        var runner = try spec.SpecTestRunneer.new(allocator, args.verbose);
        try runner.execFromFile(file_path);
    } else {
        std.debug.print("Usage: spec_test [-s|-v] <file.wast|file.json>\n", .{});
        std.debug.print("  -s: silent mode\n", .{});
        std.debug.print("  -v: verbose mode\n", .{});
        std.process.exit(1);
    }
}

const Args = struct {
    verbose: u8,
    file_name: ?[]const u8,
};

fn parseArgs() !Args {
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
            try std.posix.chdir(try cwd.realpath(base_dir, &buf));
        } else {
            file_name = arg;
        }
    }

    return Args{ .verbose = verbose, .file_name = file_name };
}
