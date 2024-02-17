const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("./types.zig");
};
const Error = @import("./errors.zig").Error;
const page_size = @import("./instance.zig").page_size;

/// A Moudle Instance in wasm spec
pub const ModuleInst = struct {
    types: []const types.FuncType = &.{},
    func_addrs: []types.FuncAddr = &.{},
    table_addrs: []types.TableAddr = &.{},
    mem_addrs: []types.MemAddr = &.{},
    global_addrs: []types.GlobalAddr = &.{},
    elem_addrs: []types.ElemAddr = &.{},
    data_addrs: []types.DataAddr = &.{},
    exports: []types.ExportInst = &.{},

    /// `allocmodule` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#alloc-module
    pub fn allocateModule(store: *types.Store, module: types.Module, extern_vals: []const types.ExternalValue, values: []types.Value, refs: [][]types.RefValue, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!*types.ModuleInst {
        // 1: resolve imports
        const externals = try ExternalValueGroup.new(extern_vals, allocator);
        defer externals.deinit(allocator);

        const num_import_funcs = externals.functions.len;
        const num_import_tables = externals.tables.len;
        const num_import_mems = externals.memories.len;
        const num_import_globals = externals.globals.len;

        // 20: module instance
        var mod_inst = try allocator.create(types.ModuleInst);
        mod_inst.types = module.types;
        mod_inst.func_addrs = try allocator.alloc(types.FuncAddr, num_import_funcs + module.funcs.len);
        mod_inst.table_addrs = try allocator.alloc(types.TableAddr, num_import_tables + module.tables.len);
        mod_inst.mem_addrs = try allocator.alloc(types.MemAddr, num_import_mems + module.memories.len);
        mod_inst.global_addrs = try allocator.alloc(types.GlobalAddr, num_import_globals + module.globals.len);
        mod_inst.elem_addrs = try allocator.alloc(types.ElemAddr, module.elements.len);
        mod_inst.data_addrs = try allocator.alloc(types.DataAddr, module.datas.len);
        mod_inst.exports = try allocator.alloc(types.ExportInst, module.exports.len);

        // 14, 15, 16, 17: imports
        @memcpy(mod_inst.func_addrs[0..num_import_funcs], externals.functions);
        @memcpy(mod_inst.table_addrs[0..num_import_tables], externals.tables);
        @memcpy(mod_inst.mem_addrs[0..num_import_mems], externals.memories);
        @memcpy(mod_inst.global_addrs[0..num_import_globals], externals.globals);

        // 2, 8: function
        for (module.funcs, num_import_funcs..) |func, i| {
            mod_inst.func_addrs[i] = try allocFunc(store, func, mod_inst);
        }

        // 3, 9: table
        for (module.tables, num_import_tables..) |table, i| {
            mod_inst.table_addrs[i] = try allocTable(store, table, allocator);
        }

        // 4, 10: memory
        for (module.memories, num_import_mems..) |mem, i| {
            mod_inst.mem_addrs[i] = try allocMemory(store, mem, allocator);
        }

        // 5, 11: global
        for (module.globals, 0..) |global, i| {
            mod_inst.global_addrs[num_import_globals + i] = try allocGlobal(store, global, values[i]);
        }

        // 6, 12: element segment
        for (module.elements, 0..) |element, i| {
            mod_inst.elem_addrs[i] = try allocElement(store, element, refs[i]);
        }

        // 7, 13: data segment
        for (module.datas, 0..) |data, i| {
            mod_inst.data_addrs[i] = try allocData(store, data);
        }

        // 18, 19: export
        for (module.exports, 0..) |exp, i| {
            const exp_value: types.ExternalValue = switch (exp.desc) {
                .func => |idx| .{ .function = mod_inst.func_addrs[idx] },
                .table => |idx| .{ .table = mod_inst.table_addrs[idx] },
                .memory => |idx| .{ .memory = mod_inst.mem_addrs[idx] },
                .global => |idx| .{ .global = mod_inst.global_addrs[idx] },
            };
            const exp_inst = types.ExportInst{
                .name = exp.name,
                .value = exp_value,
            };
            mod_inst.exports[i] = exp_inst;
        }

        // 21: return
        return mod_inst;
    }

    pub fn auxiliaryInstance(store: *types.Store, module: types.Module, extern_vals: []const types.ExternalValue, allocator: std.mem.Allocator) error{OutOfMemory}!ModuleInst {
        const externals = try ExternalValueGroup.new(extern_vals, allocator);
        const num_import_funcs = externals.functions.len;
        const num_import_globals = externals.globals.len;

        var mod_inst = ModuleInst{
            .types = module.types,
            .func_addrs = try allocator.alloc(types.FuncAddr, num_import_funcs + module.funcs.len),
            .global_addrs = try allocator.alloc(types.GlobalAddr, num_import_globals),
        };

        @memcpy(mod_inst.func_addrs[0..num_import_funcs], externals.functions);
        @memcpy(mod_inst.global_addrs[0..num_import_globals], externals.globals);

        for (module.funcs, num_import_funcs..) |func, i| {
            mod_inst.func_addrs[i] = try allocFunc(store, func, &mod_inst);
        }
        return mod_inst;
    }
};

/// `allocfunc` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#functions
fn allocFunc(store: *types.Store, func: types.Func, mod_inst: *types.ModuleInst) error{OutOfMemory}!types.FuncAddr {
    const inst = types.FuncInst{
        .type = mod_inst.types[func.type],
        .module = mod_inst,
        .code = func,
    };
    return try appendElement(types.FuncInst, &store.funcs, inst);
}

/// `alloctable` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#tables
fn allocTable(store: *types.Store, table: types.TableType, allocator: std.mem.Allocator) error{OutOfMemory}!types.TableAddr {
    var elem = try allocator.alloc(types.RefValue, table.limits.min);
    @memset(elem, nullFromReftype(table.ref_type));
    const inst = types.TableInst{
        .type = table,
        .elem = elem,
    };
    return try appendElement(types.TableInst, &store.tables, inst);
}

/// `allocmemory` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#memories
fn allocMemory(store: *types.Store, mem: types.MemoryType, allocator: std.mem.Allocator) error{OutOfMemory}!types.MemAddr {
    const inst = types.MemInst{
        .type = mem,
        .data = try allocator.alloc(u8, mem.limits.min * page_size),
    };
    @memset(inst.data, 0);
    return try appendElement(types.MemInst, &store.mems, inst);
}

/// `allocglobal` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#globals
fn allocGlobal(store: *types.Store, global: types.Global, value: types.Value) error{OutOfMemory}!types.MemAddr {
    const inst = types.GlobalInst{
        .type = global.type,
        .value = value,
    };
    return try appendElement(types.GlobalInst, &store.globals, inst);
}

/// `allocelem` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#element-segments
fn allocElement(store: *types.Store, elememt: types.Element, refs: []types.RefValue) error{OutOfMemory}!types.ElemAddr {
    const inst = types.ElemInst{
        .type = elememt.type,
        .elem = refs,
    };
    return try appendElement(types.ElemInst, &store.elems, inst);
}

/// `allocdata` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#alloc-data
fn allocData(store: *types.Store, data: types.Data) error{OutOfMemory}!types.DataAddr {
    const inst = types.DataInst{
        .data = data.init,
    };
    return try appendElement(types.DataInst, &store.datas, inst);
}

fn appendElement(comptime T: type, array: *std.ArrayList(T), elem: T) error{OutOfMemory}!u32 {
    const addr: u32 = @intCast(array.items.len);
    try array.append(elem);
    return addr;
}

fn nullFromReftype(ref_type: std.wasm.RefType) types.RefValue {
    return if (ref_type == std.wasm.RefType.funcref) .{ .func_ref = null } else .{ .extern_ref = null };
}

const ExternalValueGroup = struct {
    const Self = @This();

    functions: []types.FuncAddr,
    tables: []types.TableAddr,
    memories: []types.MemAddr,
    globals: []types.GlobalAddr,

    fn new(extern_values: []const types.ExternalValue, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var funcs = std.ArrayList(types.FuncAddr).init(allocator);
        var tables = std.ArrayList(types.TableAddr).init(allocator);
        var mems = std.ArrayList(types.MemAddr).init(allocator);
        var globals = std.ArrayList(types.GlobalAddr).init(allocator);

        for (extern_values) |imp| {
            switch (imp) {
                .function => |idx| try funcs.append(idx),
                .table => |idx| try tables.append(idx),
                .memory => |idx| try mems.append(idx),
                .global => |idx| try globals.append(idx),
            }
        }

        return .{
            .functions = try funcs.toOwnedSlice(),
            .tables = try tables.toOwnedSlice(),
            .memories = try mems.toOwnedSlice(),
            .globals = try globals.toOwnedSlice(),
        };
    }

    fn deinit(self: Self, allocator: std.mem.Allocator) void {
        allocator.free(self.functions);
        allocator.free(self.tables);
        allocator.free(self.memories);
        allocator.free(self.globals);
    }
};
