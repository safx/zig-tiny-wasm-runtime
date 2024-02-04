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
    var engine = runtime.Engine.new(allocator);

    var module_insts = std.StringHashMap(*runtime.ModuleInst).init(allocator);
    var current_module: *runtime.ModuleInst = try engine.loadModuleFromPath("spectest.wasm");

    for (commands) |cmd| {
        std.debug.print("---------------------------------------------------------------\n", .{});
        std.debug.print("{any}\n", .{cmd});
        //std.debug.print("{}\n", .{std.json.fmt(cmd, .{})});

        switch (cmd) {
            .module => |arg| {
                current_module = try engine.loadModuleFromPath(arg.file_name);
                if (arg.name) |name| {
                    try module_insts.put(name, current_module);
                }
            },
            .assert_return => |arg| {
                switch (arg.action) {
                    .invoke => |iarg| {
                        if (iarg.module) |name| {
                            current_module = module_insts.get(name).?;
                        }

                        const func_args = try allocator.alloc(runtime.Value, iarg.args.len);
                        for (iarg.args, 0..) |a, i| {
                            func_args[i] = a;
                        }
                        const func_addr = try getFunctionByName(current_module, iarg.field);
                        const ret = try engine.invokeFunctionByAddr(func_addr.value.function, func_args);
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
                                std.debug.print("\texpected = {any}\n", .{exp});
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
                            func_args[i] = a;
                        }
                        const func_addr = try getFunctionByName(current_module, iarg.field);
                        _ = engine.invokeFunctionByAddr(func_addr.value.function, func_args) catch |err| {
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

fn checkReturnValue(expected: types.Result, result: runtime.Value) bool {
    switch (expected) {
        .@"const" => |exp_const| {
            switch (exp_const) {
                .i32, .i64 => return exp_const.asI64() == result.asI64(),
                .f32 => return exp_const.asI32() == result.asI32(),
                .f64 => return exp_const.asI64() == result.asI64(),
                .v128 => unreachable,
                .func_ref => |r| switch (result) {
                    .func_ref => |v| return r == v,
                    else => return false,
                },
                .extern_ref => |r| switch (result) {
                    .extern_ref => |v| return r == v,
                    else => return false,
                },
            }
        },
        .f32_nan_arithmetic => return isArithmeticNanF32(result.f32),
        .f32_nan_canonical => return isCanonicalNanF32(result.f32),
        .f64_nan_arithmetic => return isArithmeticNanF64(result.f64),
        .f64_nan_canonical => return isCanonicalNanF64(result.f64),
    }
}

/// Returns function name by searching from the latest instaitiated modules.
fn getFunctionByName(module: *runtime.ModuleInst, func_name: []const u8) error{ExportItemNotFound}!runtime.ExportInst {
    for (module.exports) |exp| {
        if (std.mem.eql(u8, exp.name, func_name)) {
            return exp;
        }
    }
    std.debug.print("ExportItemNotFound: {s}\n", .{func_name});
    return runtime.Error.ExportItemNotFound;
}

fn getExtention(filename: []const u8) []const u8 {
    var parts = std.mem.split(u8, filename, ".");
    var elem: []const u8 = "";
    while (parts.next()) |p| {
        elem = p;
    }
    return elem;
}

fn isCanonicalNanF32(num: u32) bool {
    return (num & 0x7fffffff) == 0x7fc00000;
}

fn isCanonicalNanF64(num: u64) bool {
    return (num & 0x7fffffffffffffff) == 0x7ff8000000000000;
}

fn isArithmeticNanF32(num: u64) bool {
    return (num & 0x00400000) == 0x00400000;
}

fn isArithmeticNanF64(num: u64) bool {
    return (num & 0x0008000000000000) == 0x0008000000000000;
}
