const std = @import("std");
const Engine = @import("wasm-runtime").Engine;

pub fn main() !void {
    var verbose: bool = false;

    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var engine = Engine.new(allocator, verbose);

    for (std.os.argv[1..]) |argv| {
        const arg: []const u8 = std.mem.span(argv);
        if (std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else {
            _ = try engine.loadModuleFromPath(arg, arg);
        }
    }
}
