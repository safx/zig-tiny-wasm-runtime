const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const core = ModuleInfo.init(b, "wasm-core", "src/core/mod.zig", &.{});
    const decode = ModuleInfo.init(b, "wasm-decode", "src/decode/mod.zig", &.{core});
    const runtime = ModuleInfo.init(b, "wasm-runtime", "src/runtime/mod.zig", &.{ core, decode });
    const spec = ModuleInfo.init(b, "wasm-spec-test", "src/spec_test/mod.zig", &.{ core, decode, runtime });

    const moduleInfos = .{ core, decode, runtime, spec };

    const exe = b.addExecutable(.{
        .name = "zwasmi",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    inline for (moduleInfos) |info| {
        exe.addModule(info.name, info.module);
    }
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");
    inline for (moduleInfos) |info| {
        const unit_test = b.addTest(.{
            .root_source_file = .{ .path = info.path },
            .target = target,
            .optimize = optimize,
        });

        inline for (moduleInfos) |i| {
            unit_test.addModule(i.name, i.module);
        }
        //const test_step = b.step(info.name, "Run unit tests");
        //b.installArtifact(unit_test);
        const run_unit_test = b.addRunArtifact(unit_test);
        test_step.dependOn(&run_unit_test.step);
    }

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_unit_tests.step);
}

const ModuleInfo = struct {
    module: *std.Build.Module,
    name: []const u8,
    path: []const u8,

    pub fn init(b: *std.Build, name: []const u8, path: []const u8, dependencies: []const ModuleInfo) ModuleInfo {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allcator = gpa.allocator();
        defer _ = gpa.deinit();

        const deps = allcator.alloc(std.Build.ModuleDependency, dependencies.len) catch {
            std.debug.panic("allocation failed", .{});
        };
        defer _ = allcator.free(deps);
        for (dependencies, 0..) |d, i| {
            deps[i] = .{
                .name = d.name,
                .module = d.module,
            };
        }
        const module = b.addModule(name, .{
            .source_file = .{ .path = path },
            .dependencies = deps,
        });
        return .{ .module = module, .name = name, .path = path };
    }
};
