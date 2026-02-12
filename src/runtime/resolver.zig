const std = @import("std");
const core = @import("wasm-core");
const local_types = @import("./types.zig");
const Error = @import("./errors.zig").Error;
const Engine = @import("./engine.zig").Engine;

// Type aliases for convenience
const Module = core.types.Module;
const FuncType = core.types.FuncType;
const TableType = core.types.TableType;
const MemoryType = core.types.MemoryType;
const GlobalType = core.types.GlobalType;
const ImportDesc = core.types.ImportDesc;
const Limits = core.types.Limits;
const ValueType = core.types.ValueType;
const Store = local_types.Store;
const ModuleInst = local_types.ModuleInst;
const ExternalValue = local_types.ExternalValue;

/// The coller should free the returned slice
pub fn resolveImports(store: Store, mod_insts: Engine.ModuleInstMap, module: Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const ExternalValue {
    const imports = module.imports;
    const external_imports = try allocator.alloc(ExternalValue, imports.len);
    for (imports, 0..) |imp, i| {
        const exp = try findExportFromModules(mod_insts, imp.module_name, imp.name);
        if (!isMatchType(store, exp, module, imp.desc))
            return Error.IncompatibleImportType;
        external_imports[i] = exp;
    }
    return external_imports;
}

fn findExportFromModules(mod_insts: Engine.ModuleInstMap, module_name: []const u8, import_name: []const u8) error{UnknownImport}!ExternalValue {
    if (mod_insts.get(module_name)) |mod_inst| {
        return try findExport(mod_inst.*, import_name);
    } else {
        std.debug.print("Unknown import: {s}.{s}\n", .{ module_name, import_name });
        return Error.UnknownImport;
    }
}

pub fn findExport(mod_inst: ModuleInst, import_name: []const u8) error{UnknownImport}!ExternalValue {
    for (mod_inst.exports) |exp| {
        if (std.mem.eql(u8, import_name, exp.name))
            return exp.value;
    }
    return Error.UnknownImport;
}

fn isMatchType(store: Store, exp: ExternalValue, module: Module, imp: ImportDesc) bool {
    return switch (exp) {
        .function => |v| imp == .function and isMatchFuncType(store.funcs.items[v].type, module.types[imp.function]),
        .table => |v| imp == .table and isMatchTableType(store.tables.items[v].type, imp.table),
        .memory => |v| imp == .memory and isMatchMemType(store.mems.items[v].type, imp.memory),
        .global => |v| imp == .global and isMatchGlobalType(store.globals.items[v].type, imp.global),
    };
}

fn isMatchFuncType(a: FuncType, b: FuncType) bool {
    return std.mem.eql(ValueType, a.parameter_types, b.parameter_types) and
        std.mem.eql(ValueType, a.result_types, b.result_types);
}

fn isMatchTableType(a: TableType, b: TableType) bool {
    return a.ref_type == b.ref_type and a.is_64 == b.is_64 and isValidLimits(a.limits, b.limits);
}

fn isMatchMemType(a: MemoryType, b: MemoryType) bool {
    return a.is_64 == b.is_64 and isValidLimits(a.limits, b.limits);
}

fn isMatchGlobalType(a: GlobalType, b: GlobalType) bool {
    return a.mutability == b.mutability and
        a.value_type == b.value_type;
}

fn isValidLimits(a: Limits, b: Limits) bool {
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
