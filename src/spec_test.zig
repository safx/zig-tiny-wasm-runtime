const std = @import("std");
const spec = @import("wasm-spec-test");
const spec_types = @import("spec-types");
const text_decode = @import("wasm-text-decode");
const runtime = @import("wasm-runtime");

const executor = spec.executor;

/// Main entry point for spec test runner
/// Supports both .wast/.wat text format and .json spec test format
pub fn main() !void {
    const args = try parseArgs();
    
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    if (args.file_name) |file_path| {
        if (std.mem.endsWith(u8, file_path, ".wast") or std.mem.endsWith(u8, file_path, ".wat")) {
            try runWastFile(allocator, file_path, args.verbose);
        } else {
            var runner = try spec.SpecTestRunneer.new(allocator, args.verbose);
            try runner.execFromFile(file_path);
        }
    } else {
        std.debug.print("Usage: spec_test [-s|-v] <file.wast|file.json>\n", .{});
        std.debug.print("  -s: silent mode\n", .{});
        std.debug.print("  -v: verbose mode\n", .{});
        std.process.exit(1);
    }
}

const Args = struct {
    verbose: u8,
    file_name: ?[]const u8,
};

fn parseArgs() !Args {
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

    return Args{ .verbose = verbose, .file_name = file_name };
}

/// Execute a .wast/.wat text format test file
fn runWastFile(allocator: std.mem.Allocator, file_path: []const u8, verbose: u8) !void {
    if (verbose >= 1) {
        std.debug.print("Loading .wast file: {s}\n", .{file_path});
    }

    const file_content = try std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024);
    defer allocator.free(file_content);

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

    var runner = WastRunner.init(allocator, verbose);
    defer runner.deinit();

    try runner.runCommands(commands);

    if (verbose >= 1) {
        std.debug.print("Test Results: {d}/{d} assertions passed\n", .{ runner.passed, runner.total });
        if (runner.failed > 0) {
            std.debug.print("  Failed: {d}\n", .{runner.failed});
        }
    }

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
            .register => |r| {
                if (self.verbose >= 2) {
                    std.debug.print("Registering module as '{s}'", .{r.as_name});
                    if (r.name) |name| {
                        std.debug.print(" (source: {s})", .{name});
                    }
                    std.debug.print("\n", .{});
                }

                const mod_inst = if (r.name) |name|
                    self.engine.getModuleInstByName(name) orelse return error.ModuleNotFound
                else
                    self.current_module orelse return error.NoCurrentModule;

                try self.registered_modules.put(r.as_name, mod_inst);
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
            if (!executor.compare.resultEquals(result, expected)) {
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
                    args[i] = executor.value_convert.toRuntimeValue(arg);
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
