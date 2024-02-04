const std = @import("std");
const wa = @import("wasm-core");
pub const Instance = @import("./instance.zig").Instance;
pub const Error = @import("./errors.zig").Error;
pub const types = @import("./types.zig");

/// A type of WebAssembly runtime engine, which wraps `Instance`.
pub const Engine = struct {
    const Self = @This();
    const ModuleInstMap = std.StringHashMap(*types.ModuleInst);

    allocator: std.mem.Allocator,
    instance: Instance,
    mod_insts: ModuleInstMap,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .instance = Instance.new(allocator),
            .mod_insts = ModuleInstMap.init(allocator),
        };
    }

    pub fn loadModuleFromPath(self: *Self, file_name: []const u8) !*types.ModuleInst {
        const decode = @import("wasm-decode");
        var loader = decode.Loader.new(self.allocator);

        const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
        defer file.close();
        const data = try file.readToEndAlloc(self.allocator, 10_000_000);
        const module = try loader.parseAll(data);
        defer module.deinit();
        return try self.loadModule(module, getBasename(file_name));
    }

    fn loadModule(self: *Self, module: wa.Module, module_name: []const u8) (Error || error{OutOfMemory})!*types.ModuleInst {
        const extern_vals = try self.resolveImports(module, self.allocator);
        const mod_inst = try self.instance.instantiate(module, extern_vals);
        try self.mod_insts.put(module_name, mod_inst);
        return mod_inst;
    }

    pub fn invokeFunctionByAddr(self: *Self, func_addr: types.FuncAddr, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        return try self.instance.invokeFunctionByAddr(func_addr, args);
    }

    fn resolveImports(self: *Self, module: wa.Module, allocator: std.mem.Allocator) error{OutOfMemory}![]const types.ExternalValue {
        var external_imports = try allocator.alloc(types.ExternalValue, module.imports.len);
        for (module.imports, 0..) |imp, i| {
            if (self.mod_insts.get(imp.module_name)) |mod_inst| {
                for (mod_inst.exports) |exp| {
                    if (std.mem.eql(u8, imp.name, exp.name)) {
                        external_imports[i] = exp.value;
                    }
                }
            }
        }
        return external_imports;
    }
};

fn getBasename(filename: []const u8) []const u8 {
    // FIXME: assume that filename doesn't include path separator
    var parts = std.mem.split(u8, filename, ".");
    var prev_elem: []const u8 = "";
    var elem: []const u8 = "";
    while (parts.next()) |p| {
        prev_elem = elem;
        elem = p;
    }
    return prev_elem;
}
