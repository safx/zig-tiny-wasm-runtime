const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("./types.zig");
};
pub const Instance = @import("./instance.zig").Instance;
pub const Error = @import("./errors.zig").Error;

/// A type of WebAssembly runtime engine, which wraps `Instance`.
pub const Engine = struct {
    const Self = @This();
    const ModuleInstMap = std.StringHashMap(*types.ModuleInst);

    allocator: std.mem.Allocator,
    instance: Instance,
    mod_insts: ModuleInstMap,

    pub fn new(allocator: std.mem.Allocator, verbose: bool) Self {
        return .{
            .allocator = allocator,
            .instance = Instance.new(allocator, verbose),
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
        defer self.allocator.free(data);
        const module = try loader.parseAll(data);
        defer module.deinit();

        return try self.loadModule(module, if (module_name) |n| n else getFilename(file_name));
    }

    fn loadModule(self: *Self, module: types.Module, module_name: []const u8) (Error || error{OutOfMemory})!*types.ModuleInst {
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

    pub fn invokeFunctionByName(self: *Self, func_name: []const u8, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        var it = self.mod_insts.valueIterator();
        while (it.next()) |mod_inst| {
            const exp = try findExport(mod_inst.*.*, func_name);
            return try self.invokeFunctionByAddr(exp.function, args);
        }
        std.debug.print("Unknown function name to call: {s}\n", .{func_name});
        return Error.UnknownImport;
    }

    fn resolveImports(self: *Self, module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const types.ExternalValue {
        const imports = module.imports;
        var external_imports = try allocator.alloc(types.ExternalValue, imports.len);
        for (imports, 0..) |imp, i| {
            if (self.mod_insts.get(imp.module_name)) |mod_inst| {
                const exp = try findExport(mod_inst.*, imp.name);
                if (isMatchType(self.instance.store, exp, module, imp.desc)) {
                    external_imports[i] = exp;
                } else {
                    return Error.IncompatibleImportType;
                }
            } else {
                std.debug.print("Unknown import: {s}.{s}\n", .{ imp.module_name, imp.name });
                return Error.UnknownImport;
            }
        }
        return external_imports;
    }

    fn isMatchType(store: types.Store, exp: types.ExternalValue, module: types.Module, imp: types.ImportDesc) bool {
        return switch (exp) {
            .function => |v| imp == .function and isMatchFuncType(store.funcs.items[v].type, module.types[imp.function]),
            .table => |v| imp == .table and isMatchTableType(store.tables.items[v].type, imp.table),
            .memory => |v| imp == .memory and isMatchMemType(store.mems.items[v].type, imp.memory),
            .global => |v| imp == .global and isMatchGlobalType(store.globals.items[v].type, imp.global),
        };
    }

    fn findExport(mod_inst: types.ModuleInst, import_name: []const u8) error{UnknownImport}!types.ExternalValue {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, import_name, exp.name)) {
                return exp.value;
            }
        }
        return Error.UnknownImport;
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

fn isMatchFuncType(a: types.FuncType, b: types.FuncType) bool {
    return std.mem.eql(types.ValueType, a.parameter_types, b.parameter_types) and
        std.mem.eql(types.ValueType, a.result_types, b.result_types);
}

fn isMatchTableType(a: types.TableType, b: types.TableType) bool {
    return a.ref_type == b.ref_type and isValidLimits(a.limits, b.limits);
}

fn isMatchMemType(a: types.MemoryType, b: types.MemoryType) bool {
    return isValidLimits(a.limits, b.limits);
}

fn isMatchGlobalType(a: types.GlobalType, b: types.GlobalType) bool {
    return a.mutability == b.mutability and
        a.value_type == b.value_type;
}

fn isValidLimits(a: types.Limits, b: types.Limits) bool {
    if (b.max) |b_max| {
        if (a.max) |a_max| {
            if (a_max > b_max)
                return false;
        } else {
            return false;
        }
    }
    return a.min >= b.min;
}
