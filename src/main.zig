const std = @import("std");
const runtime = @import("wasm-runtime");

pub fn main() !void {
    var verbose: bool = false;

    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var wasm_files = std.ArrayList([]const u8).init(allocator);
    var func_name: ?[]const u8 = null;
    var wasm_args = std.ArrayList(runtime.Value).init(allocator);

    var pos: usize = 1;
    while (pos < std.os.argv.len) : (pos += 1) {
        const arg: []const u8 = std.mem.span(std.os.argv[pos]);
        if (strcmp(arg, "-v")) {
            verbose = true;
        } else if (strcmp(arg, "-r")) {
            pos += 1;
            func_name = std.mem.span(std.os.argv[pos]);
        } else if (strcmp(arg, "-a")) {
            pos += 1;
            const val = try parseValue(std.mem.span(std.os.argv[pos]));
            try wasm_args.append(val);
        } else {
            try wasm_files.append(std.mem.span(std.os.argv[pos]));
        }
    }

    var engine = runtime.Engine.new(allocator, verbose);
    for (wasm_files.items) |file| {
        _ = try engine.loadModuleFromPath(file, file);
    }
    if (func_name) |func| {
        const return_values = try engine.invokeFunctionByName(func, wasm_args.items);
        std.debug.print("=> {any}\n", .{return_values});
    } else {
        std.debug.print("usage: file.wasm -v -r func -a arg1 -a arg2\n", .{});
    }
}

fn parseValue(string: []const u8) !runtime.Value {
    var parts = std.mem.split(u8, string, ":");

    var ty: []const u8 = "";
    var num: []const u8 = "";
    var i: u32 = 0;
    while (parts.next()) |p| {
        switch (i) {
            0 => ty = p,
            1 => num = p,
            else => @panic("unknown value format"),
        }
        i += 1;
    }
    if (i == 1) {
        num = ty;
        ty = "";
    }

    if (strcmp(ty, "i32") or strcmp(ty, "")) {
        return runtime.Value.from(try std.fmt.parseInt(i32, num, 10));
    } else if (strcmp(ty, "u32")) {
        return runtime.Value.from(try std.fmt.parseInt(u32, num, 10));
    } else if (strcmp(ty, "i64")) {
        return runtime.Value.from(try std.fmt.parseInt(i64, num, 10));
    } else if (strcmp(ty, "u64")) {
        return runtime.Value.from(try std.fmt.parseInt(u64, num, 10));
    } else if (strcmp(ty, "f32")) {
        return runtime.Value.from(try std.fmt.parseFloat(f32, num));
    } else if (strcmp(ty, "f64")) {
        return runtime.Value.from(try std.fmt.parseFloat(f64, num));
    }

    @panic("unknown value type");
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
