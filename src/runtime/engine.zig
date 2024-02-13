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

    pub fn getModuleInstByName(self: *Self, module_name: []const u8) ?*types.ModuleInst {
        return self.mod_insts.get(module_name);
    }

    pub fn getValueFromGlobal(self: *Self, mod_inst: *types.ModuleInst, field_name: []const u8) ?types.Value {
        const externval = findExternValue(mod_inst, field_name);
        if (externval != null and externval.? == .global) {
            const glb = self.instance.store.globals.items[@intCast(externval.?.global)];
            return glb.value;
        } else {
            return null;
        }
    }

    fn findExternValue(mod_inst: *types.ModuleInst, name: []const u8) ?types.ExternalValue {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, exp.name, name)) {
                return exp.value;
            }
        }
        return null;
    }

    pub fn loadModuleFromPath(self: *Self, file_name: []const u8, module_name: ?[]const u8) !*types.ModuleInst {
        const decode = @import("wasm-decode");
        var loader = decode.Loader.new(self.allocator);

        const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
        defer file.close();
        const data = try file.readToEndAlloc(self.allocator, 10_000_000);
        const module = try loader.parseAll(data);
        defer module.deinit();

        return try self.loadModule(module, if (module_name) |n| n else getFilename(file_name));
    }

    fn loadModule(self: *Self, module: wa.Module, module_name: []const u8) (Error || error{OutOfMemory})!*types.ModuleInst {
        const extern_vals = try self.resolveImports(module, self.allocator);
        const mod_inst = try self.instance.instantiate(module, extern_vals);
        try self.registerModule(mod_inst, module_name);
        return mod_inst;
    }

    pub fn registerModule(self: *Self, mod_inst: *types.ModuleInst, module_name: []const u8) error{OutOfMemory}!void {
        try self.mod_insts.put(module_name, mod_inst);
    }

    pub fn invokeFunctionByAddr(self: *Self, func_addr: types.FuncAddr, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        try self.instance.stack.array.resize(0); // TODO: IS IT OK?
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

fn getFilename(path: []const u8) []const u8 {
    var parts = std.mem.split(u8, path, "/");
    var elem: []const u8 = "";
    while (parts.next()) |p| {
        elem = p;
    }
    return elem;
}
