const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("./types.zig");
};
const Error = @import("./errors.zig").Error;
const Engine = @import("./engine.zig").Engine;

pub fn resolveImports(store: types.Store, mod_insts: Engine.ModuleInstMap, module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const types.ExternalValue {
    const imports = module.imports;
    const external_imports = try allocator.alloc(types.ExternalValue, imports.len);
    for (imports, 0..) |imp, i| {
        const exp = try findExportFromModules(mod_insts, imp.module_name, imp.name);
        if (isMatchType(store, exp, module, imp.desc)) {
            external_imports[i] = exp;
        } else {
            return Error.IncompatibleImportType;
        }
    }
    return external_imports;
}

fn findExportFromModules(mod_insts: Engine.ModuleInstMap, module_name: []const u8, import_name: []const u8) error{UnknownImport}!types.ExternalValue {
    if (mod_insts.get(module_name)) |mod_inst| {
        return try findExport(mod_inst.*, import_name);
    } else {
        std.debug.print("Unknown import: {s}.{s}\n", .{ module_name, import_name });
        return Error.UnknownImport;
    }
}

pub fn findExport(mod_inst: types.ModuleInst, import_name: []const u8) error{UnknownImport}!types.ExternalValue {
    for (mod_inst.exports) |exp| {
        if (std.mem.eql(u8, import_name, exp.name)) {
            return exp.value;
        }
    }
    return Error.UnknownImport;
}

fn isMatchType(store: types.Store, exp: types.ExternalValue, module: types.Module, imp: types.ImportDesc) bool {
    return switch (exp) {
        .function => |v| imp == .function and isMatchFuncType(store.funcs.items[v].type, module.types[imp.function]),
        .table => |v| imp == .table and isMatchTableType(store.tables.items[v].type, imp.table),
        .memory => |v| imp == .memory and isMatchMemType(store.mems.items[v].type, imp.memory),
        .global => |v| imp == .global and isMatchGlobalType(store.globals.items[v].type, imp.global),
    };
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
