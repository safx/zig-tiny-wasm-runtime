const std = @import("std");
const runtime = @import("wasm-runtime");

const Value = runtime.types.Value;

pub fn main() !void {
    var verbose: bool = false;

    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var wasm_files = std.array_list.Managed([]const u8).init(allocator);
    var func_name: ?[]const u8 = null;
    var wasm_args = std.array_list.Managed(Value).init(allocator);

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
        const inst = try engine.loadModuleFromPath(file, file);
        if (verbose) {
            for (inst.exports, 0..) |exp, i| {
                std.debug.print("export[{}] = {s} ", .{ i, exp.name });
                if (exp.value == .function) {
                    std.debug.print("(func: {any})\n", .{engine.instance.store.funcs.items[exp.value.function].type});
                } else {
                    std.debug.print("({s}) \n", .{@tagName(exp.value)});
                }
            }
        }
    }
    if (func_name) |func| {
        const return_values = try engine.invokeFunctionByName(func, wasm_args.items);
        defer allocator.free(return_values);
        std.debug.print("=> {any}\n", .{return_values});
    } else if (wasm_files.items.len == 0) {
        std.debug.print("usage: file.wasm -v -r func -a arg1 -a arg2\n", .{});
    }
}

fn parseValue(string: []const u8) !runtime.types.Value {
    var parts = std.mem.splitSequence(u8, string, ":");

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
        return Value.from(try std.fmt.parseInt(i32, num, 10));
    } else if (strcmp(ty, "u32")) {
        return Value.from(try std.fmt.parseInt(u32, num, 10));
    } else if (strcmp(ty, "i64")) {
        return Value.from(try std.fmt.parseInt(i64, num, 10));
    } else if (strcmp(ty, "u64")) {
        return Value.from(try std.fmt.parseInt(u64, num, 10));
    } else if (strcmp(ty, "f32")) {
        return Value.from(try std.fmt.parseFloat(f32, num));
    } else if (strcmp(ty, "f64")) {
        return Value.from(try std.fmt.parseFloat(f64, num));
    }

    @panic("unknown value type");
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
