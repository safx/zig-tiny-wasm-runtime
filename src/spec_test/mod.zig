const std = @import("std");
const wa = @import("wasm-core").types;
const runtime = @import("wasm-runtime");
const types = @import("./types.zig");
const reader = @import("./reader.zig");

test "Wasm spec test" {
    //try doWasmSpecTest();
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
            std.debug.print("=============================== {s}\n", .{entry.name});
            try execSpecTestsFromFile(entry.name);
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

pub fn execSpecTestsFromFile(file_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [4096]u8 = undefined;
    const cwd = std.fs.cwd();
    try std.os.chdir(try cwd.realpath("spec_test", &buf));
    const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
    defer file.close();

    const commands = try reader.readJsonFromFile(file, allocator);
    try execSpecTests(commands, allocator);
}

fn execSpecTests(commands: []const types.Command, allocator: std.mem.Allocator) !void {
    const decode = @import("wasm-decode");

    var engine = runtime.Engine.new(allocator);
    var loader = decode.Loader.new(allocator);

    for (commands) |cmd| {
        std.debug.print("---------------------------------------------------------------\n", .{});
        std.debug.print("{any}\n", .{cmd});
        //std.debug.print("{}\n", .{std.json.fmt(cmd, .{})});
        switch (cmd) {
            .module => |arg| {
                const file = try std.fs.cwd().openFile(arg.file_name, .{ .mode = .read_only });
                defer file.close();
                const data = try file.readToEndAlloc(allocator, 10_000_000);
                const module = try loader.parseAll(data);
                defer module.deinit();
                try engine.load(module);
            },
            .assert_return => |arg| {
                switch (arg.action) {
                    .invoke => |iarg| {
                        const func_args = try allocator.alloc(runtime.Value, iarg.args.len);
                        for (iarg.args, 0..) |a, i| {
                            func_args[i] = valueFromConst(a);
                        }
                        const ret = try engine.invokeFunctionByName(iarg.field, func_args);
                        defer allocator.free(ret);
                        if (ret.len != arg.expected.len) {
                            @panic("Test failed (length not match).");
                        }

                        for (ret, arg.expected) |rv, exp| {
                            const result = checkReturnValue(exp, rv);
                            if (!result) {
                                std.debug.print("====================\n", .{});
                                std.debug.print("\t  Test failed at line {}\n", .{arg.line});
                                std.debug.print("\t  return =  {any}\n", .{ret});
                                std.debug.print("\texpected = {any}\n", .{valueFromConst(exp.@"const")});
                                std.debug.print("====================\n", .{});
                                @panic("Test failed.");
                            }
                        }
                        std.debug.print("test pass (result = {any})\n", .{ret});
                    },
                    .get => unreachable,
                }
            },
            .assert_trap => |arg| {
                switch (arg.action) {
                    .invoke => |iarg| {
                        const func_args = try allocator.alloc(runtime.Value, iarg.args.len);
                        for (iarg.args, 0..) |a, i| {
                            func_args[i] = valueFromConst(a);
                        }
                        _ = engine.invokeFunctionByName(iarg.field, func_args) catch |err| {
                            const match_error = err == arg.trap;
                            if (match_error) {
                                std.debug.print("test pass (expected failure: {any})\n", .{arg.trap});
                                continue;
                            } else {
                                std.debug.print("====================\n", .{});
                                std.debug.print("\t  Test failed at line {}\n", .{arg.line});
                                std.debug.print("\n  actual failure: {}\n", .{err});
                                std.debug.print("\n  expected failure: {any}\n", .{arg.trap});
                                std.debug.print("====================\n", .{});
                                @panic("Test failed.");
                            }
                        };
                        std.debug.print("failure test NOT FAILED (expected failure: {any})\n", .{arg.trap});
                        @panic("Test failed.");
                    },
                    .get => unreachable,
                }
            },
            else => {},
        }
    }
}

fn valueFromConst(init_expr: types.Const) runtime.Value {
    return switch (init_expr) {
        .i32_const => |val| .{ .i32 = val },
        .i64_const => |val| .{ .i64 = val },
        .f32_const => |val| .{ .f32 = val },
        .f64_const => |val| .{ .f64 = val },
        else => unreachable,
    };
}

fn checkReturnValue(expected: types.Result, result: runtime.Value) bool {
    switch (expected) {
        .@"const" => |exp_const| {
            switch (exp_const) {
                .i32_const, .i64_const => |ev| switch (result) {
                    .i32, .i64 => |rv| return ev == rv,
                    else => return false,
                },
                .f32_const => |ev| switch (result) {
                    .i32, .i64 => |rv| {
                        const r: i32 = @intCast(rv);
                        return ev == r;
                    },
                    .f32 => |rv| {
                        const r: i32 = @bitCast(rv);
                        return ev == r;
                    },
                    else => return false,
                },
                .f64_const => |ev| switch (result) {
                    .i32, .i64 => |rv| {
                        const r: i64 = @intCast(rv);
                        return ev == r;
                    },
                    .f64 => |rv| {
                        const r: i64 = @bitCast(rv);
                        return ev == r;
                    },
                    else => return false,
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}
