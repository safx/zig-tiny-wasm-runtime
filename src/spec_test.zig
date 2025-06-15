const std = @import("std");
const spec = @import("wasm-spec-test");
const text_decode = @import("wasm-text-decode");
const runtime = @import("wasm-runtime");

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
            try std.posix.chdir(try cwd.realpath(base_dir, &buf));
        } else {
            file_name = arg;
        }
    }

    if (file_name) |n| {
        var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        if (std.mem.endsWith(u8, n, ".wast") or std.mem.endsWith(u8, n, ".wat")) {
            // Handle WebAssembly Text Format files
            try runWastFile(allocator, n, verbose);
        } else {
            // Handle JSON spec test files
            var runner = try spec.SpecTestRunneer.new(allocator, verbose);
            try runner.execFromFile(n);
        }
    } else {
        std.debug.print("file expected", .{});
    }
}

fn runWastFile(allocator: std.mem.Allocator, file_path: []const u8, verbose: u8) !void {
    if (verbose >= 1) {
        std.debug.print("Loading .wast file: {s}\n", .{file_path});
    }
    
    // Read the file content (increased limit for large test files)
    const file_content = try std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024);
    defer allocator.free(file_content);
    
    // Parse the WAST file
    const module = text_decode.parseWastModule(allocator, file_content) catch |err| {
        std.debug.print("Error parsing WAST file: {}\n", .{err});
        return;
    };
    defer module.deinit();
    
    // Create a runtime engine
    var engine = runtime.Engine.new(allocator, verbose >= 2);
    
    // Load the module
    const inst = engine.loadModule(module, file_path) catch |err| {
        std.debug.print("Error loading module: {}\n", .{err});
        return;
    };
    
    if (verbose >= 1) {
        std.debug.print("Module loaded successfully!\n", .{});
        if (verbose >= 2) {
            for (inst.exports, 0..) |exp, i| {
                std.debug.print("export[{}] = {s} ", .{ i, exp.name });
                if (exp.value == .function) {
                    std.debug.print("(func: {any})\n", .{engine.instance.store.funcs.items[exp.value.function].type});
                } else {
                    std.debug.print("({s})\n", .{@tagName(exp.value)});
                }
            }
        }
    }
    
    // For now, just validate that the module can be loaded
    // In the future, we could run specific test assertions here
}
