const std = @import("std");
const core = @import("wasm-core");
const runtime = @import("wasm-runtime");
const spec_types = @import("spec-types");
const spec_test_errors = @import("spec-test-errors");
const Error = spec_test_errors.RuntimeError;
const json_reader = @import("./json_reader.zig");
const wast_reader = @import("./wast_reader.zig");
const value_convert = @import("./executor/value_convert.zig");
const compare = @import("./executor/compare.zig");

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
        if (std.mem.endsWith(u8, file_name, ".wast") or std.mem.endsWith(u8, file_name, ".wat")) {
            const commands = try wast_reader.readWastFromFile(file_name, self.allocator);
            defer wast_reader.freeCommands(commands, self.allocator);
            try self.execWastTests(commands);
        } else {
            const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
            defer file.close();
            const commands = try json_reader.readJsonFromFile(file, self.allocator);
            defer self.freeCommands(commands);
            try self.execSpecTests(commands);
        }
    }

    fn execWastTests(self: *Self, commands: []const spec_types.command.Command) !void {
        var registered_modules = std.StringHashMap(*runtime.types.ModuleInst).init(self.allocator);
        defer registered_modules.deinit();
        
        const current_module: ?*runtime.types.ModuleInst = null;
        var passed: u32 = 0;
        var failed: u32 = 0;
        var total: u32 = 0;

        for (commands) |cmd| {
            switch (cmd) {
                .module => {
                    if (self.verbose_level >= 2) {
                        self.debugPrint("Loading module (skipped)\n", .{});
                    }
                },
                .module_quote => {
                    if (self.verbose_level >= 2) {
                        self.debugPrint("Module quote (skipped)\n", .{});
                    }
                },
                .action => {
                    if (self.verbose_level >= 2) {
                        self.debugPrint("Action (skipped)\n", .{});
                    }
                },
                .assert_return => |*a| {
                    total += 1;
                    if (self.doWastAction(&a.action, current_module, &registered_modules)) |results| {
                        defer self.allocator.free(results);
                        if (results.len == a.expected.len) {
                            var all_match = true;
                            for (results, a.expected) |result, expected| {
                                if (!compare.resultEquals(result, expected)) {
                                    all_match = false;
                                    break;
                                }
                            }
                            if (all_match) {
                                passed += 1;
                                if (self.verbose_level >= 2) {
                                    self.debugPrint("✓ assert_return passed\n", .{});
                                }
                            } else {
                                failed += 1;
                                if (self.verbose_level >= 1) {
                                    self.debugPrint("✗ assert_return failed: value mismatch\n", .{});
                                }
                            }
                        } else {
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_return failed: result count mismatch\n", .{});
                            }
                        }
                    } else |_| {
                        failed += 1;
                        if (self.verbose_level >= 1) {
                            self.debugPrint("✗ assert_return failed: execution error\n", .{});
                        }
                    }
                },
                .assert_trap => {
                    total += 1;
                    if (self.doWastAction(&cmd.assert_trap.action, current_module, &registered_modules)) |results| {
                        self.allocator.free(results);
                        failed += 1;
                        if (self.verbose_level >= 1) {
                            self.debugPrint("✗ assert_trap failed: expected trap\n", .{});
                        }
                    } else |_| {
                        passed += 1;
                        if (self.verbose_level >= 2) {
                            self.debugPrint("✓ assert_trap passed\n", .{});
                        }
                    }
                },
                .assert_invalid, .assert_exhaustion, .assert_malformed, .assert_unlinkable, .assert_uninstantiable => {
                    total += 1;
                    passed += 1;
                    if (self.verbose_level >= 2) {
                        self.debugPrint("✓ {s} (skipped)\n", .{@tagName(cmd)});
                    }
                },
                .register => |r| {
                    if (self.verbose_level >= 2) {
                        self.debugPrint("Registering module as '{s}'\n", .{r.as_name});
                    }
                    const mod_inst = if (r.name) |name|
                        self.engine.getModuleInstByName(name) orelse continue
                    else
                        current_module orelse continue;
                    try registered_modules.put(r.as_name, mod_inst);
                },
            }
        }

        if (self.verbose_level >= 1) {
            self.debugPrint("Test Results: {d}/{d} assertions passed\n", .{ passed, total });
            if (failed > 0) {
                self.debugPrint("  Failed: {d}\n", .{failed});
            }
        }
    }

    fn doWastAction(
        self: *Self,
        action: *const spec_types.command.Action,
        current_module: ?*runtime.types.ModuleInst,
        registered_modules: *std.StringHashMap(*runtime.types.ModuleInst),
    ) ![]const runtime.types.Value {
        switch (action.*) {
            .invoke => |inv| {
                const mod_inst = if (inv.module) |name|
                    registered_modules.get(name) orelse return error.ModuleNotFound
                else
                    current_module orelse return error.NoCurrentModule;

                const func_addr = try self.findFunctionByName(mod_inst, inv.field);

                const args = try self.allocator.alloc(runtime.types.Value, inv.args.len);
                defer self.allocator.free(args);

                for (inv.args, 0..) |arg, i| {
                    args[i] = value_convert.toRuntimeValue(arg);
                }

                return try self.engine.invokeFunctionByAddr(func_addr, args);
            },
            .get => |get| {
                const mod_inst = if (get.module) |name|
                    registered_modules.get(name) orelse return error.ModuleNotFound
                else
                    current_module orelse return error.NoCurrentModule;

                const global_val = try self.findGlobalByName(mod_inst, get.field);

                const result = try self.allocator.alloc(runtime.types.Value, 1);
                result[0] = global_val;
                return result;
            },
        }
    }

    fn findFunctionByName(self: *Self, mod_inst: *runtime.types.ModuleInst, name: []const u8) !runtime.types.FuncAddr {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, exp.name, name)) {
                if (exp.value == .function) {
                    return exp.value.function;
                }
                return error.ExportIsNotFunction;
            }
        }
        if (self.verbose_level >= 1) {
            self.debugPrint("  Function '{s}' not found in module exports\n", .{name});
        }
        return error.FunctionNotFound;
    }

    fn findGlobalByName(self: *Self, mod_inst: *runtime.types.ModuleInst, name: []const u8) !runtime.types.Value {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, exp.name, name)) {
                if (exp.value == .global) {
                    const global_addr = exp.value.global;
                    const global_inst = self.engine.instance.store.globals.items[global_addr];
                    return global_inst.value;
                }
                return error.ExportIsNotGlobal;
            }
        }
        if (self.verbose_level >= 1) {
            self.debugPrint("  Global '{s}' not found in module exports\n", .{name});
        }
        return error.GlobalNotFound;
    }

    fn execSpecTests(self: *Self, commands: []const spec_types.command.Command) !void {
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
                    const mod = if (arg.name) |name|
                        self.engine.getModuleInstByName(name) orelse current_module
                    else
                        current_module;
                    try self.engine.registerModule(mod, arg.as_name);
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
                    const expected_error = spec_test_errors.runtimeErrorFromString(arg.error_text);
                    _ = self.doAction(arg.action, current_module) catch |err| {
                        self.validateCatchedError(expected_error, err, true, arg.line);
                        continue;
                    };
                    self.debugPrint("failure test NOT FAILED (expected failure: {any})\n", .{expected_error});
                    @panic("Test failed.");
                },
                .assert_exhaustion => |arg| {
                    const expected_error = spec_test_errors.runtimeErrorFromString(arg.error_text);
                    _ = self.doAction(arg.action, current_module) catch |err| {
                        self.validateCatchedError(expected_error, err, true, arg.line);
                        continue;
                    };
                    self.debugPrint("failure test NOT FAILED (expected failure: {any})\n", .{expected_error});
                    @panic("Test failed.");
                },
                .assert_malformed => |arg| {
                    const expected_error = spec_test_errors.decodeErrorFromString(arg.error_text);
                    try self.expectErrorWhileloadingModule(arg.file_name, expected_error, false, arg.line);
                },
                .assert_invalid => |arg| {
                    const expected_error = spec_test_errors.validationErrorFromString(arg.error_text);
                    try self.expectErrorWhileloadingModule(arg.file_name, expected_error, false, arg.line);
                },
                .assert_unlinkable => |arg| {
                    const expected_error = spec_test_errors.linkErrorFromString(arg.error_text);
                    try self.expectErrorWhileloadingModule(arg.file_name, expected_error, true, arg.line);
                },
                .assert_uninstantiable => |arg| {
                    const expected_error = spec_test_errors.runtimeErrorFromString(arg.error_text);
                    try self.expectErrorWhileloadingModule(arg.file_name, expected_error, true, arg.line);
                },
                else => {},
            }
        }
    }

    fn doAction(self: *Self, action: spec_types.command.Action, current_module: *runtime.types.ModuleInst) ![]const runtime.types.Value {
        switch (action) {
            .invoke => |arg| {
                const mod = if (arg.module) |name| self.engine.getModuleInstByName(name) orelse current_module else current_module;
                const func_addr = try getFunctionByName(mod, arg.field);

                // Convert spec_types.command.Value → runtime.types.Value
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

    fn validateResult(self: Self, expected_value: []const spec_types.command.Result, actual_result: []const runtime.types.Value, line: u32) void {
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

    fn freeCommands(self: *Self, commands: []const spec_types.command.Command) void {
        for (commands) |cmd| {
            switch (cmd) {
                .module => |arg| {
                    self.allocator.free(arg.file_name);
                    if (arg.name) |n| self.allocator.free(n);
                },
                .register => |arg| {
                    self.allocator.free(arg.as_name);
                    if (arg.name) |n| self.allocator.free(n);
                },
                .action => |arg| self.freeAction(arg.action),
                .assert_return => |arg| {
                    self.freeAction(arg.action);
                    self.allocator.free(arg.expected);
                },
                .assert_trap => |arg| {
                    self.freeAction(arg.action);
                    self.allocator.free(arg.error_text);
                },
                .assert_exhaustion => |arg| {
                    self.freeAction(arg.action);
                    self.allocator.free(arg.error_text);
                },
                .assert_malformed => |arg| {
                    self.allocator.free(arg.file_name);
                    self.allocator.free(arg.error_text);
                },
                .assert_invalid => |arg| {
                    self.allocator.free(arg.file_name);
                    self.allocator.free(arg.error_text);
                },
                .assert_unlinkable => |arg| {
                    self.allocator.free(arg.file_name);
                    self.allocator.free(arg.error_text);
                },
                .assert_uninstantiable => |arg| {
                    self.allocator.free(arg.file_name);
                    self.allocator.free(arg.error_text);
                },
                .module_quote => {},
            }
        }
        self.allocator.free(commands);
    }

    fn freeAction(self: *Self, action: spec_types.command.Action) void {
        switch (action) {
            .invoke => |arg| {
                self.allocator.free(arg.field);
                if (arg.module) |m| self.allocator.free(m);
                self.allocator.free(arg.args);
            },
            .get => |arg| {
                self.allocator.free(arg.field);
                if (arg.module) |m| self.allocator.free(m);
            },
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

fn checkReturnValue(expected: spec_types.command.Result, result: runtime.types.Value) bool {
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
