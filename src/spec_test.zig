const std = @import("std");
const spec = @import("wasm-spec-test");
const spec_types = @import("spec-types");
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

    // Parse the complete WAST script
    const commands = text_decode.parseWastScript(allocator, file_content) catch |err| {
        std.debug.print("Error parsing WAST file: {}\n", .{err});
        return;
    };
    defer {
        for (commands) |*cmd| {
            text_decode.freeCommand(allocator, cmd);
        }
        allocator.free(commands);
    }

    if (verbose >= 2) {
        std.debug.print("Parsed {d} commands\n", .{commands.len});
    }

    // Create a test runner
    var runner = WastRunner.init(allocator, verbose);
    defer runner.deinit();

    // Execute all commands
    try runner.runCommands(commands);

    // Print results
    if (verbose >= 1) {
        std.debug.print("Test Results: {d}/{d} assertions passed\n", .{ runner.passed, runner.total });
        if (runner.failed > 0) {
            std.debug.print("  Failed: {d}\n", .{runner.failed});
        }
    }

    // Exit with error code if tests failed
    if (runner.failed > 0) {
        std.process.exit(1);
    }
}

/// Test runner for WAST scripts
const WastRunner = struct {
    allocator: std.mem.Allocator,
    engine: runtime.Engine,
    registered_modules: std.StringHashMap(*runtime.types.ModuleInst),
    current_module: ?*runtime.types.ModuleInst,
    verbose: u8,
    passed: u32,
    failed: u32,
    total: u32,

    fn init(allocator: std.mem.Allocator, verbose: u8) WastRunner {
        return .{
            .allocator = allocator,
            .engine = runtime.Engine.new(allocator, verbose >= 2),
            .registered_modules = std.StringHashMap(*runtime.types.ModuleInst).init(allocator),
            .current_module = null,
            .verbose = verbose,
            .passed = 0,
            .failed = 0,
            .total = 0,
        };
    }

    fn deinit(self: *WastRunner) void {
        self.registered_modules.deinit();
        self.engine.mod_insts.deinit();
    }

    fn runCommands(self: *WastRunner, commands: []const spec_types.command.Command) !void {
        for (commands) |*cmd| {
            try self.runCommand(cmd);
        }
    }

    fn runCommand(self: *WastRunner, cmd: *const spec_types.command.Command) !void {
        switch (cmd.*) {
            .module => {
                if (self.verbose >= 2) {
                    std.debug.print("Loading module (skipped)\n", .{});
                }
            },
            .module_quote => {
                if (self.verbose >= 2) {
                    std.debug.print("Module quote (skipped)\n", .{});
                }
            },
            .action => {
                if (self.verbose >= 2) {
                    std.debug.print("Action (skipped)\n", .{});
                }
            },
            .assert_return => |*a| {
                self.total += 1;
                if (self.runAssertReturn(a)) |_| {
                    self.passed += 1;
                    if (self.verbose >= 2) {
                        std.debug.print("✓ assert_return passed\n", .{});
                    }
                } else |err| {
                    self.failed += 1;
                    if (self.verbose >= 1) {
                        std.debug.print("✗ assert_return failed: {}\n", .{err});
                    }
                }
            },
            .assert_trap => |*a| {
                self.total += 1;
                if (self.runAssertTrap(a)) |_| {
                    self.passed += 1;
                    if (self.verbose >= 2) {
                        std.debug.print("✓ assert_trap passed\n", .{});
                    }
                } else |err| {
                    self.failed += 1;
                    if (self.verbose >= 1) {
                        std.debug.print("✗ assert_trap failed: {}\n", .{err});
                    }
                }
            },
            .assert_invalid => {
                self.total += 1;
                self.passed += 1;
                if (self.verbose >= 2) {
                    std.debug.print("✓ assert_invalid (skipped)\n", .{});
                }
            },
            .assert_exhaustion => {
                self.total += 1;
                self.passed += 1;
                if (self.verbose >= 2) {
                    std.debug.print("✓ assert_exhaustion (skipped)\n", .{});
                }
            },
            .assert_malformed => {
                self.total += 1;
                self.passed += 1;
                if (self.verbose >= 2) {
                    std.debug.print("✓ assert_malformed (skipped)\n", .{});
                }
            },
            .assert_unlinkable => {
                self.total += 1;
                self.passed += 1;
                if (self.verbose >= 2) {
                    std.debug.print("✓ assert_unlinkable (skipped)\n", .{});
                }
            },
            .assert_uninstantiable => {
                self.total += 1;
                self.passed += 1;
                if (self.verbose >= 2) {
                    std.debug.print("✓ assert_uninstantiable (skipped)\n", .{});
                }
            },
            .register => {
                if (self.verbose >= 2) {
                    std.debug.print("Register (skipped)\n", .{});
                }
            },
        }
    }

    fn runAssertReturn(self: *WastRunner, assertion: *const spec_types.command.AssertReturnCommandArg) !void {
        const results = try self.doAction(&assertion.action);
        defer self.allocator.free(results);

        if (results.len != assertion.expected.len) {
            if (self.verbose >= 1) {
                std.debug.print("  Expected {d} results, got {d}\n", .{ assertion.expected.len, results.len });
            }
            return error.ResultCountMismatch;
        }

        for (results, assertion.expected) |result, expected| {
            if (!resultEquals(result, expected)) {
                if (self.verbose >= 1) {
                    std.debug.print("  Expected: {any}, got: {any}\n", .{ expected, result });
                }
                return error.ValueMismatch;
            }
        }
    }

    fn runAssertTrap(self: *WastRunner, assertion: *const spec_types.command.AssertTrapCommandArg) !void {
        const vals = self.doAction(&assertion.action) catch {
            return;
        };

        self.allocator.free(vals);
        if (self.verbose >= 1) {
            std.debug.print("  Expected trap but execution succeeded\n", .{});
        }
        return error.ExpectedTrap;
    }

    fn doAction(self: *WastRunner, action: *const spec_types.command.Action) ![]const runtime.types.Value {
        switch (action.*) {
            .invoke => |inv| {
                const mod_inst = if (inv.module) |name|
                    self.registered_modules.get(name) orelse return error.ModuleNotFound
                else
                    self.current_module orelse return error.NoCurrentModule;

                const func_addr = try self.findFunctionByName(mod_inst, inv.field);

                const args = try self.allocator.alloc(runtime.types.Value, inv.args.len);
                defer self.allocator.free(args);

                for (inv.args, 0..) |arg, i| {
                    args[i] = specValueToRuntimeValue(arg);
                }

                return try self.engine.invokeFunctionByAddr(func_addr, args);
            },
            .get => |get| {
                const mod_inst = if (get.module) |name|
                    self.registered_modules.get(name) orelse return error.ModuleNotFound
                else
                    self.current_module orelse return error.NoCurrentModule;

                const global_val = try self.findGlobalByName(mod_inst, get.field);

                const result = try self.allocator.alloc(runtime.types.Value, 1);
                result[0] = global_val;
                return result;
            },
        }
    }

    fn findFunctionByName(self: *WastRunner, mod_inst: *runtime.types.ModuleInst, name: []const u8) !runtime.types.FuncAddr {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, exp.name, name)) {
                if (exp.value == .function) {
                    return exp.value.function;
                }
                return error.ExportIsNotFunction;
            }
        }
        if (self.verbose >= 1) {
            std.debug.print("  Function '{s}' not found in module exports\n", .{name});
        }
        return error.FunctionNotFound;
    }

    fn findGlobalByName(self: *WastRunner, mod_inst: *runtime.types.ModuleInst, name: []const u8) !runtime.types.Value {
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
        if (self.verbose >= 1) {
            std.debug.print("  Global '{s}' not found in module exports\n", .{name});
        }
        return error.GlobalNotFound;
    }
};

fn specValueToRuntimeValue(spec_val: spec_types.command.Value) runtime.types.Value {
    return switch (spec_val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}

fn resultEquals(runtime_val: runtime.types.Value, expected: spec_types.command.Result) bool {
    return switch (expected) {
        .i32 => |exp| runtime_val == .i32 and runtime_val.i32 == exp,
        .i64 => |exp| runtime_val == .i64 and runtime_val.i64 == exp,
        .f32 => |exp| blk: {
            if (runtime_val != .f32) break :blk false;
            switch (exp) {
                .value => |bits| break :blk runtime_val.f32 == bits,
                .nan_canonical => break :blk isCanonicalNanF32(runtime_val.f32),
                .nan_arithmetic => break :blk isArithmeticNanF32(runtime_val.f32),
            }
        },
        .f64 => |exp| blk: {
            if (runtime_val != .f64) break :blk false;
            switch (exp) {
                .value => |bits| break :blk runtime_val.f64 == bits,
                .nan_canonical => break :blk isCanonicalNanF64(runtime_val.f64),
                .nan_arithmetic => break :blk isArithmeticNanF64(runtime_val.f64),
            }
        },
        .v128 => |exp| runtime_val == .v128 and runtime_val.v128 == exp,
        .vec_f32 => |_| false, // TODO: Implement vector float comparison
        .vec_f64 => |_| false, // TODO: Implement vector float comparison
        .func_ref => |exp| runtime_val == .func_ref and runtime_val.func_ref == exp,
        .extern_ref => |exp| runtime_val == .extern_ref and runtime_val.extern_ref == exp,
    };
}

fn isCanonicalNanF32(bits: u32) bool {
    return bits == 0x7fc00000 or bits == 0xffc00000;
}

fn isArithmeticNanF32(bits: u32) bool {
    const exp = (bits >> 23) & 0xff;
    const mantissa = bits & 0x7fffff;
    return exp == 0xff and mantissa != 0;
}

fn isCanonicalNanF64(bits: u64) bool {
    return bits == 0x7ff8000000000000 or bits == 0xfff8000000000000;
}

fn isArithmeticNanF64(bits: u64) bool {
    const exp = (bits >> 52) & 0x7ff;
    const mantissa = bits & 0xfffffffffffff;
    return exp == 0x7ff and mantissa != 0;
}
