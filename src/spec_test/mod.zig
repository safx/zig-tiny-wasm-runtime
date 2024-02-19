const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("wasm-runtime");
    usingnamespace @import("./types.zig");
};
const Error = @import("./errors.zig").RuntimeError;
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
            try execSpecTestsFromFile(entry.name);
        }
    }
}

pub fn execSpecTestsFromFile(file_name: []const u8, verbose: bool) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [4096]u8 = undefined;
    const cwd = std.fs.cwd();
    try std.os.chdir(try cwd.realpath("spec_test", &buf));
    const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
    defer file.close();

    const commands = try reader.readJsonFromFile(file, allocator);
    var engine = types.Engine.new(allocator, verbose);
    try execSpecTests(&engine, commands, allocator);
}

fn execSpecTests(engine: *types.Engine, commands: []const types.Command, allocator: std.mem.Allocator) !void {
    var current_module: *types.ModuleInst = try engine.loadModuleFromPath("spectest.wasm", "spectest");

    for (commands) |cmd| {
        // std.debug.print("-" ** 75 ++ "\n", .{});
        // std.debug.print("{any}\n", .{cmd});
        //std.debug.print("{}\n", .{std.json.fmt(cmd, .{})});

        switch (cmd) {
            .module => |arg| {
                current_module = try engine.loadModuleFromPath(arg.file_name, arg.name);
            },
            .register => |arg| {
                try engine.registerModule(current_module, arg.as_name);
            },
            .action => |arg| {
                const ret = try doAction(arg.action, engine, current_module, allocator);
                defer allocator.free(ret);
            },
            .assert_return => |arg| {
                const ret = try doAction(arg.action, engine, current_module, allocator);
                validateResult(arg.expected, ret, arg.line);
                defer allocator.free(ret);
            },
            .assert_trap => |arg| {
                _ = doAction(arg.action, engine, current_module, allocator) catch |err| {
                    validateCatchedError(arg.trap, err, arg.line);
                    continue;
                };
                std.debug.print("failure test NOT FAILED (expected failure: {any})\n", .{arg.trap});
                @panic("Test failed.");
            },
            // .assert_exhaustion =>
            // .assert_malformed => |arg| try exepecError(engine, arg.file_name, arg.trap, arg.line),
            // .assert_invalid => |arg| try exepecError(engine, arg.file_name, arg.trap, arg.line),
            .assert_unlinkable => |arg| try exepecError(engine, arg.file_name, arg.trap, arg.line),
            .assert_uninstantiable => |arg| try exepecError(engine, arg.file_name, arg.trap, arg.line),
            else => {},
        }
    }
}

fn doAction(action: types.Action, engine: *types.Engine, current_module: *types.ModuleInst, allocator: std.mem.Allocator) ![]const types.Value {
    switch (action) {
        .invoke => |arg| {
            const mod = if (arg.module) |name| engine.getModuleInstByName(name) orelse current_module else current_module;
            const func_addr = try getFunctionByName(mod, arg.field);
            return try engine.invokeFunctionByAddr(func_addr.value.function, arg.args);
        },
        .get => |arg| {
            const mod = if (arg.module) |name| engine.getModuleInstByName(name) orelse current_module else current_module;
            const gval = engine.getValueFromGlobal(mod, arg.field).?;
            const array = try allocator.alloc(types.Value, 1);
            array[0] = gval;
            return array;
        },
    }
}

fn exepecError(engine: *types.Engine, file_name: []const u8, trap: anyerror, line: u32) !void {
    _ = engine.loadModuleFromPath(file_name, null) catch |err| {
        validateCatchedError(trap, err, line);
        return;
    };
    std.debug.print("failure test NOT FAILED (expected failure: {any})\n", .{trap});
    @panic("Test failed.");
}

fn validateResult(expected_value: []const types.Result, actual_result: []const types.Value, line: u32) void {
    if (actual_result.len != expected_value.len) {
        @panic("Test failed (length not match).");
    }

    for (expected_value, actual_result) |exp, res| {
        const result = checkReturnValue(exp, res);
        if (!result) {
            std.debug.print("====================\n", .{});
            std.debug.print("\t  Test failed at line {}\n", .{line});
            std.debug.print("\t  return =  {any}\n", .{actual_result});
            std.debug.print("\texpected = {any}\n", .{expected_value});
            std.debug.print("====================\n", .{});
            @panic("Test failed.");
        }
    }
    std.debug.print("test pass (result = {any})\n", .{actual_result});
}

fn checkReturnValue(expected: types.Result, result: types.Value) bool {
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

fn validateCatchedError(expected_error: anyerror, actual_error: anyerror, line: u32) void {
    if (actual_error != expected_error) {
        std.debug.print("====================\n", .{});
        std.debug.print("\t  Test failed at line {}\n", .{line});
        std.debug.print("\n  actual failure: {}\n", .{actual_error});
        std.debug.print("\n  expected failure: {any}\n", .{expected_error});
        std.debug.print("====================\n", .{});
        @panic("Test failed.");
    }
    std.debug.print("test pass (expected failure: {any})\n", .{expected_error});
}

/// Returns function name by searching from the latest instaitiated modules.
fn getFunctionByName(module: *types.ModuleInst, func_name: []const u8) error{ExportItemNotFound}!types.ExportInst {
    for (module.exports) |exp| {
        if (std.mem.eql(u8, exp.name, func_name)) {
            return exp;
        }
    }
    std.debug.print("ExportItemNotFound: {s}\n", .{func_name});
    return Error.ExportItemNotFound;
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
