const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const core = ModuleInfo.init(b, "wasm-core", "src/core/mod.zig", &.{});
    const decode = ModuleInfo.init(b, "wasm-decode", "src/decode/mod.zig", &.{core});
    const validate = ModuleInfo.init(b, "wasm-validate", "src/validate/mod.zig", &.{core});
    const runtime = ModuleInfo.init(b, "wasm-runtime", "src/runtime/mod.zig", &.{ core, decode, validate });
    
    // Pure type definitions (no runtime dependency)
    const spec_types = ModuleInfo.init(b, "spec-types", "src/spec-types/mod.zig", &.{});
    
    // Error string mapping (depends on decode, validate, runtime)
    const spec_test_errors = ModuleInfo.init(b, "spec-test-errors", "src/spec-test-errors/mod.zig", &.{ decode, validate, runtime });
    
    const text_decode = ModuleInfo.init(b, "wasm-text-decode", "src/text_decode/mod.zig", &.{ core, spec_types });
    const spec = ModuleInfo.init(b, "wasm-spec-test", "src/spec_test/mod.zig", &.{ core, decode, validate, runtime, spec_types, spec_test_errors });
    const all_modules = .{ core, decode, text_decode, validate, runtime, spec_types, spec_test_errors, spec };

    {
        const modules = .{ core, decode, runtime };

        const exe = b.addExecutable(.{
            .name = "zig-wasm-interp",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        inline for (modules) |info| {
            exe.root_module.addImport(info.name, info.module);
        }
        exe.linkLibC();
        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);
    }

    {
        const spectest_modules = all_modules;
        const exe = b.addExecutable(.{
            .name = "spec_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/spec_test.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        inline for (spectest_modules) |info| {
            exe.root_module.addImport(info.name, info.module);
        }
        exe.linkLibC();
        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("spec_test", "Run the wasm spec tests");
        run_step.dependOn(&run_cmd.step);
    }

    {
        const unit_tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        const run_unit_tests = b.addRunArtifact(unit_tests);

        const test_step = b.step("test", "Run unit tests");
        inline for (all_modules) |info| {
            const unit_test = b.addTest(.{
                .root_module = b.createModule(.{
                    .root_source_file = b.path(info.path),
                    .target = target,
                    .optimize = optimize,
                }),
            });

            inline for (all_modules) |i| {
                unit_test.root_module.addImport(i.name, i.module);
            }
            unit_test.linkLibC();

            const run_unit_test = b.addRunArtifact(unit_test);
            test_step.dependOn(&run_unit_test.step);
        }

        test_step.dependOn(&run_unit_tests.step);
    }
}

const ModuleInfo = struct {
    module: *std.Build.Module,
    name: []const u8,
    path: []const u8,

    pub fn init(b: *std.Build, name: []const u8, path: []const u8, dependencies: []const ModuleInfo) ModuleInfo {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allcator = gpa.allocator();
        defer _ = gpa.deinit();

        const imports = allcator.alloc(std.Build.Module.Import, dependencies.len) catch {
            std.debug.panic("allocation failed", .{});
        };
        defer _ = allcator.free(imports);
        for (dependencies, 0..) |d, i| {
            imports[i] = .{
                .name = d.name,
                .module = d.module,
            };
        }
        const module = b.addModule(name, .{
            .root_source_file = b.path(path),
            .imports = imports,
        });
        return .{ .module = module, .name = name, .path = path };
    }
};
