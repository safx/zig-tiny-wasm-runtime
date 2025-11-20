const std = @import("std");
const core = @import("wasm-core");
const runtime = @import("wasm-runtime");
const spec_types = @import("./types.zig");
const Error = @import("./errors.zig").RuntimeError;
const reader = @import("./reader.zig");
const value_convert = @import("./executor/value_convert.zig");

pub const SpecTestRunner = struct {
    const Self = @This();

    engine: runtime.Engine,
    allocator: std.mem.Allocator,
    verbose_level: u32,

    pub fn new(allocator: std.mem.Allocator, verbose_level: u8) !Self {
        const engine = runtime.Engine.new(allocator, verbose_level >= 2);
        return .{ .engine = engine, .allocator = allocator, .verbose_level = verbose_level };
    }

    pub fn execFromFile(self: *Self, file_name: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
        defer file.close();

        const commands = try reader.readJsonFromFile(file, self.allocator);
        try self.execSpecTests(commands);
    }

    fn execSpecTests(self: *Self, commands: []const spec_types.Command) !void {
        var current_module: *runtime.types.ModuleInst = try self.engine.loadModuleFromPath("spectest.wasm", "spectest");

        for (commands) |cmd| {
            if (self.verbose_level >= 1) {
                self.debugPrint("-" ** 75 ++ "\n", .{});
                self.debugPrint("{any}\n", .{cmd});
            }

            switch (cmd) {
                .module => |arg| {
                    current_module = try self.engine.loadModuleFromPath(arg.file_name, arg.name);
                },
                .register => |arg| {
                    try self.engine.registerModule(current_module, arg.as_name);
                },
                .action => |arg| {
                    const ret = try self.doAction(arg.action, current_module);
                    self.allocator.free(ret);
                },
                .assert_return => |arg| {
                    const ret = try self.doAction(arg.action, current_module);
                    self.validateResult(arg.expected, ret, arg.line);
                    self.allocator.free(ret);
                },
                .assert_trap => |arg| {
                    _ = self.doAction(arg.action, current_module) catch |err| {
                        self.validateCatchedError(arg.trap, err, true, arg.line);
                        continue;
                    };
                    self.debugPrint("failure test NOT FAILED (expected failure: {any})\n", .{arg.trap});
                    @panic("Test failed.");
                },
                .assert_exhaustion => |arg| {
                    _ = self.doAction(arg.action, current_module) catch |err| {
                        self.validateCatchedError(arg.trap, err, true, arg.line);
                        continue;
                    };
                    self.debugPrint("failure test NOT FAILED (expected failure: {any})\n", .{arg.trap});
                    @panic("Test failed.");
                },
                .assert_malformed => |arg| try self.expectErrorWhileloadingModule(arg.file_name, arg.trap, false, arg.line),
                .assert_invalid => |arg| try self.expectErrorWhileloadingModule(arg.file_name, arg.trap, false, arg.line),
                .assert_unlinkable => |arg| try self.expectErrorWhileloadingModule(arg.file_name, arg.trap, true, arg.line),
                .assert_uninstantiable => |arg| try self.expectErrorWhileloadingModule(arg.file_name, arg.trap, true, arg.line),
                else => {},
            }
        }
    }

    fn doAction(self: *Self, action: spec_types.Action, current_module: *runtime.types.ModuleInst) ![]const runtime.types.Value {
        switch (action) {
            .invoke => |arg| {
                const mod = if (arg.module) |name| self.engine.getModuleInstByName(name) orelse current_module else current_module;
                const func_addr = try getFunctionByName(mod, arg.field);
                
                // Convert spec_types.Value → runtime.types.Value
                const runtime_args = try self.allocator.alloc(runtime.types.Value, arg.args.len);
                defer self.allocator.free(runtime_args);
                for (arg.args, 0..) |spec_arg, i| {
                    runtime_args[i] = value_convert.toRuntimeValue(spec_arg);
                }
                
                return try self.engine.invokeFunctionByAddr(func_addr.value.function, runtime_args);
            },
            .get => |arg| {
                const mod = if (arg.module) |name| self.engine.getModuleInstByName(name) orelse current_module else current_module;
                const gval = self.engine.getValueFromGlobal(mod, arg.field).?;
                const array = try self.allocator.alloc(runtime.types.Value, 1);
                array[0] = gval;
                return array;
            },
        }
    }

    fn expectErrorWhileloadingModule(self: *Self, file_name: []const u8, trap: anyerror, panic_when_check_failed: bool, line: u32) !void {
        _ = self.engine.loadModuleFromPath(file_name, null) catch |err| {
            self.validateCatchedError(trap, err, panic_when_check_failed, line);
            return;
        };
        self.debugPrint("failure test NOT FAILED (expected failure: {any})\n", .{trap});
        @panic("Test failed.");
    }

    fn validateResult(self: Self, expected_value: []const spec_types.Result, actual_result: []const runtime.types.Value, line: u32) void {
        if (actual_result.len != expected_value.len) {
            @panic("Test failed (length not match).");
        }

        for (expected_value, actual_result) |exp, res| {
            const result = checkReturnValue(exp, res);
            if (!result) {
                self.debugPrint("====================\n", .{});
                self.debugPrint("\t  Test failed at line {}\n", .{line});
                self.debugPrint("\t  return =  {any}\n", .{actual_result});
                self.debugPrint("\texpected = {any}\n", .{expected_value});
                self.debugPrint("====================\n", .{});
                @panic("Test failed.");
            }
        }
        self.debugPrint("test pass (result = {any})\n", .{actual_result});
    }

    fn validateCatchedError(self: Self, expected_error: anyerror, actual_error: anyerror, panic_when_check_failed: bool, line: u32) void {
        if (actual_error != expected_error) {
            if (panic_when_check_failed) {
                self.debugPrint("====================\n", .{});
                self.debugPrint("\t  Test failed at line {}\n", .{line});
                self.debugPrint("\n  actual failure: {}\n", .{actual_error});
                self.debugPrint("\n  expected failure: {any}\n", .{expected_error});
                self.debugPrint("====================\n", .{});
                @panic("Test failed.");
            }
        }
        self.debugPrint("test pass (actual failure: {any})\n", .{actual_error});
    }

    fn debugPrint(self: Self, comptime fmt: []const u8, args: anytype) void {
        if (self.verbose_level >= 1) {
            std.debug.print(fmt, args);
        }
    }
};

/// Returns function name by searching from the latest instaitiated modules.
fn getFunctionByName(module: *runtime.types.ModuleInst, func_name: []const u8) error{ExportItemNotFound}!runtime.types.ExportInst {
    for (module.exports) |exp| {
        if (std.mem.eql(u8, exp.name, func_name)) {
            return exp;
        }
    }
    std.debug.print("ExportItemNotFound: {s}\n", .{func_name});
    return Error.ExportItemNotFound;
}

fn checkReturnValue(expected: spec_types.Result, result: runtime.types.Value) bool {
    return switch (expected) {
        .i32 => |val| val == result.i32,
        .i64 => |val| val == result.i64,
        .f32 => |val| switch (val) {
            .value => |v| v == result.f32,
            .nan_canonical => isCanonicalNanF32(result.f32),
            .nan_arithmetic => isArithmeticNanF32(result.f32),
        },
        .f64 => |val| switch (val) {
            .value => |v| v == result.f64,
            .nan_canonical => isCanonicalNanF64(result.f64),
            .nan_arithmetic => isArithmeticNanF64(result.f64),
        },

        .func_ref => |val| val == result.func_ref,
        .extern_ref => |val| val == result.extern_ref,

        .v128 => |val| val == result.v128,
        .vec_f32 => |e_vec| blk: {
            const r_vec = result.asVec(@Vector(4, u32));
            for (e_vec, 0..) |ev, idx| {
                const rv = r_vec[idx];
                const match = switch (ev) {
                    .value => |v| v == rv,
                    .nan_canonical => isCanonicalNanF32(rv),
                    .nan_arithmetic => isArithmeticNanF32(rv),
                };
                if (!match) break :blk false;
            }
            break :blk true;
        },
        .vec_f64 => |e_vec| blk: {
            const r_vec = result.asVec(@Vector(2, u64));
            for (e_vec, 0..) |ev, idx| {
                const rv = r_vec[idx];
                const match = switch (ev) {
                    .value => |v| v == rv,
                    .nan_canonical => isCanonicalNanF64(rv),
                    .nan_arithmetic => isArithmeticNanF64(rv),
                };
                if (!match) break :blk false;
            }
            break :blk true;
        },
    };
}

fn valueEquals(expected: runtime.types.Value, result: runtime.types.Value) bool {
    const Tag = @typeInfo(runtime.types.Value).Union.tag_type.?;
    inline for (@typeInfo(Tag).Enum.fields) |field| {
        if (field.value == @intFromEnum(expected) and field.value == @intFromEnum(result)) {
            return @field(expected, field.name) == @field(result, field.name);
        }
    }
    return false;
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
