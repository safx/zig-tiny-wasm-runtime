const std = @import("std");
const wa = @import("wasm-core");
const types = @import("./types.zig");
const Error = @import("./errors.zig").Error;
const page_size = @import("./instance.zig").page_size;

pub const ModuleInst = struct {
    types: []const wa.FuncType = &.{},
    func_addrs: []types.FuncAddr = &.{},
    table_addrs: []types.TableAddr = &.{},
    mem_addrs: []types.MemAddr = &.{},
    global_addrs: []types.GlobalAddr = &.{},
    elem_addrs: []types.ElemAddr = &.{},
    data_addrs: []types.DataAddr = &.{},
    exports: []types.ExportInst = &.{},

    /// `allocmodule` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#alloc-module
    pub fn allocateModule(store: *types.Store, module: wa.Module, extern_vals: []const types.ExternalValue, values: []types.Value, refs: [][]types.RefValue, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!*types.ModuleInst {
        // 1: resolve imports
        var num_import_funcs: u32 = 0;
        var num_import_tables: u32 = 0;
        var num_import_mems: u32 = 0;
        var num_import_globals: u32 = 0;
        for (extern_vals) |imp|
            switch (imp) {
                .function => num_import_funcs += 1,
                .table => num_import_tables += 1,
                .memory => num_import_mems += 1,
                .global => num_import_globals += 1,
            };

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
        var num_funcs: u32 = 0;
        var num_tables: u32 = 0;
        var num_mems: u32 = 0;
        var num_globals: u32 = 0;
        for (extern_vals) |imp| {
            std.debug.print("==== {any}\n", .{imp});
            switch (imp) {
                .function => |idx| {
                    mod_inst.func_addrs[num_funcs] = idx;
                    num_funcs += 1;
                },
                .table => |idx| {
                    mod_inst.table_addrs[num_tables] = idx;
                    num_tables += 1;
                },
                .memory => |idx| {
                    mod_inst.mem_addrs[num_mems] = idx;
                    num_mems += 1;
                },
                .global => |idx| {
                    mod_inst.global_addrs[num_globals] = idx;
                    num_globals += 1;
                },
            }
        }

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
            const ext_value: types.ExternalValue = switch (exp.desc) {
                .func => |idx| .{ .function = mod_inst.func_addrs[idx] },
                .table => |idx| .{ .table = mod_inst.table_addrs[idx] },
                .memory => |idx| .{ .memory = mod_inst.mem_addrs[idx] },
                .global => |idx| .{ .global = mod_inst.global_addrs[idx] },
            };
            const exp_inst = types.ExportInst{
                .name = exp.name,
                .value = ext_value,
            };
            mod_inst.exports[i] = exp_inst;
        }

        // 21: return
        return mod_inst;
    }

    pub fn auxiliaryInstance(store: *types.Store, module: wa.Module, extern_vals: []const types.ExternalValue, allocator: std.mem.Allocator) error{OutOfMemory}!ModuleInst {
        var num_import_funcs: u32 = 0;
        var num_import_globals: u32 = 0;
        for (extern_vals) |imp|
            switch (imp) {
                .function => num_import_funcs += 1,
                .global => num_import_globals += 1,
                else => {},
            };

        var mod_inst = ModuleInst{
            .types = module.types,
            .func_addrs = try allocator.alloc(types.FuncAddr, num_import_funcs + module.funcs.len),
            .global_addrs = try allocator.alloc(types.GlobalAddr, num_import_globals),
        };

        var num_funcs: u32 = 0;
        var num_globals: u32 = 0;
        for (extern_vals) |imp|
            switch (imp) {
                .function => |idx| {
                    mod_inst.func_addrs[num_funcs] = idx;
                    num_funcs += 1;
                },
                .global => |idx| {
                    mod_inst.global_addrs[num_globals] = idx;
                    num_globals += 1;
                },
                else => {},
            };

        for (module.funcs, num_import_funcs..) |func, i| {
            mod_inst.func_addrs[i] = try allocFunc(store, func, &mod_inst);
        }
        return mod_inst;
    }
};

/// `allocfunc` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#functions
fn allocFunc(store: *types.Store, func: wa.Func, mod_inst: *types.ModuleInst) error{OutOfMemory}!types.FuncAddr {
    const inst = types.FuncInst{
        .type = mod_inst.types[func.type],
        .module = mod_inst,
        .code = func,
    };
    return try appendElement(types.FuncInst, &store.funcs, inst);
}

/// `alloctable` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#tables
fn allocTable(store: *types.Store, table: wa.TableType, allocator: std.mem.Allocator) error{OutOfMemory}!types.TableAddr {
    var elem = try allocator.alloc(types.RefValue, table.limit.min);
    @memset(elem, nullFromReftype(table.ref_type));
    const inst = types.TableInst{
        .type = table,
        .elem = elem,
    };
    return try appendElement(types.TableInst, &store.tables, inst);
}

/// `allocmemory` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#memories
fn allocMemory(store: *types.Store, mem: wa.MemoryType, allocator: std.mem.Allocator) error{OutOfMemory}!types.MemAddr {
    const inst = types.MemInst{
        .type = mem,
        .data = try allocator.alloc(u8, mem.limits.min * page_size),
    };
    @memset(inst.data, 0);
    return try appendElement(types.MemInst, &store.mems, inst);
}

/// `allocglobal` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#globals
fn allocGlobal(store: *types.Store, global: wa.Global, value: types.Value) error{OutOfMemory}!types.MemAddr {
    const inst = types.GlobalInst{
        .type = global.type,
        .value = value,
    };
    return try appendElement(types.GlobalInst, &store.globals, inst);
}

/// `allocelem` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#element-segments
fn allocElement(store: *types.Store, elememt: wa.Element, refs: []types.RefValue) error{OutOfMemory}!types.ElemAddr {
    const inst = types.ElemInst{
        .type = elememt.type,
        .elem = refs,
    };
    return try appendElement(types.ElemInst, &store.elems, inst);
}

/// `allocdata` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#alloc-data
fn allocData(store: *types.Store, data: wa.Data) error{OutOfMemory}!types.DataAddr {
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
