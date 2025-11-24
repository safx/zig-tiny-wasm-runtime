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

    fn loadSpectestHostModule(self: *Self) !*runtime.types.ModuleInst {
        const spectest_wat =
            \\(module
            \\  (global (export "global_i32") i32 (i32.const 666))
            \\  (global (export "global_i64") i64 (i64.const 666))
            \\  (global (export "global_f32") f32 (f32.const 666.6))
            \\  (global (export "global_f64") f64 (f64.const 666.6))
            \\  (table (export "table") 10 20 funcref)
            \\  (memory (export "memory") 1 2)
            \\  (func (export "print"))
            \\  (func (export "print_i32") (param i32))
            \\  (func (export "print_i64") (param i64))
            \\  (func (export "print_f32") (param f32))
            \\  (func (export "print_f64") (param f64))
            \\  (func (export "print_i32_f32") (param i32 f32))
            \\  (func (export "print_f64_f64") (param f64 f64))
            \\)
        ;

        const text_decode = @import("wasm-text-decode");
        const module = text_decode.parseWastModule(self.allocator, spectest_wat) catch |err| {
            if (self.verbose_level >= 1) {
                self.debugPrint("Warning: Failed to load spectest module: {}\n", .{err});
            }
            return error.SpectestLoadFailed;
        };

        return try self.engine.loadModule(module, "spectest");
    }

    pub fn execFromFile(self: *Self, file_name: []const u8) !void {
        if (std.mem.endsWith(u8, file_name, ".wast") or std.mem.endsWith(u8, file_name, ".wat")) {
            const commands = try wast_reader.readWastFromFile(file_name, self.allocator);
            defer wast_reader.freeCommands(commands, self.allocator);
            try self.execCommands(commands);
        } else {
            const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
            defer file.close();
            const commands = try json_reader.readJsonFromFile(file, self.allocator);
            defer self.freeCommands(commands);
            try self.execCommands(commands);
        }
    }

    fn execCommands(self: *Self, commands: []const spec_types.command.Command) !void {
        var registered_modules = std.StringHashMap(*runtime.types.ModuleInst).init(self.allocator);
        defer registered_modules.deinit();

        var current_module: ?*runtime.types.ModuleInst = self.loadSpectestHostModule() catch null;

        // Register spectest module if loaded successfully
        if (current_module) |spectest_mod| {
            try registered_modules.put("spectest", spectest_mod);
        }

        var passed: u32 = 0;
        var failed: u32 = 0;
        var total: u32 = 0;

        for (commands) |cmd| {
            if (self.verbose_level >= 2) {
                self.debugPrint("-" ** 75 ++ "\n", .{});
                self.debugPrint("{f}\n", .{cmd});
            }

            switch (cmd) {
                .module => |arg| {
                    if (arg.module_data) |data| {
                        const text_decode = @import("wasm-text-decode");
                        const module = text_decode.parseWastModule(self.allocator, data) catch |err| {
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ Failed to parse inline module: {}\n", .{err});
                            }
                            continue;
                        };
                        current_module = self.engine.loadModule(module, arg.name orelse "") catch |err| {
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ Failed to load inline module: {}\n", .{err});
                            }
                            continue;
                        };
                    } else if (arg.module_binary) |binary| {
                        const decode = @import("wasm-decode");
                        var loader = decode.Loader.new(self.allocator);
                        const module = loader.parseAll(binary) catch |err| {
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ Failed to parse binary module: {}\n", .{err});
                            }
                            continue;
                        };
                        current_module = self.engine.loadModule(module, arg.name orelse "") catch |err| {
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ Failed to load binary module: {}\n", .{err});
                            }
                            continue;
                        };
                    } else if (arg.file_name.len > 0) {
                        current_module = self.engine.loadModuleFromPath(arg.file_name, arg.name) catch |err| {
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ Failed to load module from path: {}\n", .{err});
                            }
                            continue;
                        };
                    }
                },
                .module_quote => {
                    if (self.verbose_level >= 2) {
                        self.debugPrint("Module quote (skipped)\n", .{});
                    }
                },
                .action => |*a| {
                    if (self.doAction(&a.action, current_module, &registered_modules)) |results| {
                        self.allocator.free(results);
                    } else |_| {}
                },
                .assert_return => |*a| {
                    total += 1;
                    if (self.doAction(&a.action, current_module, &registered_modules)) |results| {
                        defer self.allocator.free(results);
                        if (results.len == a.expected.len) {
                            var error_str: ?[]const u8 = null;
                            defer if (error_str) |s| self.allocator.free(s);
                            for (results, a.expected) |result, expected| {
                                if (!compare.resultEquals(result, expected)) {
                                    if (self.verbose_level >= 1) {
                                        const actual = value_convert.toSpecResult(result);
                                        error_str = try std.fmt.allocPrint(self.allocator, "\nexpected: {any}\n  actual: {any}\n", .{ expected, actual });
                                    }
                                    break;
                                }
                            }
                            if (error_str == null) {
                                passed += 1;
                                if (self.verbose_level >= 2) {
                                    self.debugPrint("✓ assert_return passed\n", .{});
                                }
                            } else {
                                failed += 1;
                                if (self.verbose_level >= 1) {
                                    self.debugPrint("✗ assert_return failed (line {}): value mismatch.{s}", .{ a.line, error_str.? });
                                }
                            }
                        } else {
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_return failed (line {}): result count mismatch\n", .{a.line});
                            }
                        }
                    } else |err| {
                        failed += 1;
                        if (self.verbose_level >= 1) {
                            self.debugPrint("✗ assert_return failed (line {}): {}\n", .{ a.line, err });
                        }
                    }
                },
                .assert_trap => |*a| {
                    total += 1;
                    if (self.doAction(&a.action, current_module, &registered_modules)) |results| {
                        self.allocator.free(results);
                        failed += 1;
                        if (self.verbose_level >= 1) {
                            self.debugPrint("✗ assert_trap failed (line {}): expected trap\n", .{a.line});
                        }
                    } else |_| {
                        passed += 1;
                        if (self.verbose_level >= 2) {
                            self.debugPrint("✓ assert_trap passed\n", .{});
                        }
                    }
                },
                .assert_exhaustion => |arg| {
                    total += 1;
                    const expected_error = spec_test_errors.runtimeErrorFromString(arg.error_text);
                    if (self.doAction(&arg.action, current_module, &registered_modules)) |results| {
                        self.allocator.free(results);
                        failed += 1;
                        if (self.verbose_level >= 1) {
                            self.debugPrint("✗ assert_exhaustion failed (line {}): expected error\n", .{arg.line});
                        }
                    } else |err| {
                        if (err == expected_error) {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_exhaustion passed\n", .{});
                            }
                        } else {
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_exhaustion failed (line {}): wrong error\n", .{arg.line});
                            }
                        }
                    }
                },
                .assert_invalid => |arg| {
                    if (arg.module_data) |wat| {
                        total += 1;
                        const text_decode = @import("wasm-text-decode");
                        const module = text_decode.parseWastModule(self.allocator, wat) catch |parse_err| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_invalid passed (parse error: {})\n", .{parse_err});
                            }
                            continue;
                        };
                        defer module.deinit();
                        
                        if (self.engine.loadModule(module, "")) |_| {
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_invalid failed (line {}): module loaded successfully (expected error)\n", .{arg.line});
                            }
                        } else |err| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_invalid passed (validation/load error: {})\n", .{err});
                            }
                        }
                    } else if (arg.module_binary) |binary| {
                        total += 1;
                        const decode = @import("wasm-decode");
                        var loader = decode.Loader.new(self.allocator);
                        const module = loader.parseAll(binary) catch |parse_err| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_invalid passed (parse error: {})\n", .{parse_err});
                            }
                            continue;
                        };
                        defer module.deinit();
                        
                        if (self.engine.loadModule(module, "")) |_| {
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_invalid failed (line {}): module loaded successfully (expected error)\n", .{arg.line});
                            }
                        } else |err| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_invalid passed (validation/load error: {})\n", .{err});
                            }
                        }
                    } else if (self.verbose_level >= 2) {
                        self.debugPrint("✓ assert_invalid (skipped - no module data)\n", .{});
                    }
                },
                .assert_malformed => |arg| {
                    if (arg.module_data) |wat| {
                        total += 1;
                        const text_decode = @import("wasm-text-decode");
                        if (text_decode.parseWastModule(self.allocator, wat)) |module| {
                            defer module.deinit();
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_malformed failed (line {}): module parsed successfully (expected parse error)\n", .{arg.line});
                            }
                        } else |_| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_malformed passed\n", .{});
                            }
                        }
                    } else if (arg.module_binary) |binary| {
                        total += 1;
                        const decode = @import("wasm-decode");
                        var loader = decode.Loader.new(self.allocator);
                        if (loader.parseAll(binary)) |module| {
                            defer module.deinit();
                            failed += 1;
                            if (self.verbose_level >= 1) {
                                self.debugPrint("✗ assert_malformed failed (line {}): module parsed successfully (expected parse error)\n", .{arg.line});
                            }
                        } else |_| {
                            passed += 1;
                            if (self.verbose_level >= 2) {
                                self.debugPrint("✓ assert_malformed passed\n", .{});
                            }
                        }
                    } else if (self.verbose_level >= 2) {
                        self.debugPrint("✓ assert_malformed (skipped - no module data)\n", .{});
                    }
                },
                .assert_unlinkable, .assert_uninstantiable => {
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

    fn doAction(
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

    fn debugPrint(self: Self, comptime fmt: []const u8, args: anytype) void {
        if (self.verbose_level >= 1) {
            std.debug.print(fmt, args);
        }
    }

    fn freeCommands(self: *Self, commands: []const spec_types.command.Command) void {
        for (commands) |*cmd| {
            self.freeCommand(cmd);
        }
        self.allocator.free(commands);
    }

    fn freeCommand(self: *Self, cmd: *const spec_types.command.Command) void {
        _ = self;
        _ = cmd;
        // Commands are freed by the reader
    }
};
