const std = @import("std");
const core = @import("wasm-core");
const local_types = @import("./types.zig");
const Error = @import("./errors.zig").Error;
const page_size = @import("./instance.zig").page_size;

// Type aliases for convenience
const Module = core.types.Module;
const FuncType = core.types.FuncType;
const Func = core.types.Func;
const TableType = core.types.TableType;
const MemoryType = core.types.MemoryType;
const Global = core.types.Global;
const Element = core.types.Element;
const Data = core.types.Data;
const Export = core.types.Export;
const RefType = std.wasm.RefType;
const FuncAddr = local_types.FuncAddr;
const TableAddr = local_types.TableAddr;
const MemAddr = local_types.MemAddr;
const GlobalAddr = local_types.GlobalAddr;
const ElemAddr = local_types.ElemAddr;
const DataAddr = local_types.DataAddr;
const ExternalValue = local_types.ExternalValue;
const ExportInst = local_types.ExportInst;
const Store = local_types.Store;
const TagAddr = local_types.TagAddr;
const TagInst = local_types.TagInst;
// Note: ModuleInst is defined in this file
const FuncInst = local_types.FuncInst;
const TableInst = local_types.TableInst;
const MemInst = local_types.MemInst;
const GlobalInst = local_types.GlobalInst;
const ElemInst = local_types.ElemInst;
const DataInst = local_types.DataInst;
const Value = local_types.Value;
const RefValue = local_types.RefValue;

/// A Moudle Instance in wasm spec
pub const ModuleInst = struct {
    types: []const FuncType = &.{},
    func_addrs: []FuncAddr = &.{},
    table_addrs: []TableAddr = &.{},
    mem_addrs: []MemAddr = &.{},
    global_addrs: []GlobalAddr = &.{},
    elem_addrs: []ElemAddr = &.{},
    data_addrs: []DataAddr = &.{},
    tag_addrs: []TagAddr = &.{},
    exports: []ExportInst = &.{},

    /// `allocmodule` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#alloc-module
    pub fn allocateModule(store: *Store, module: Module, extern_vals: []const ExternalValue, values: []Value, refs: [][]RefValue, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!*ModuleInst {
        // 1: resolve imports
        const externals = try ExternalValueGroup.new(extern_vals, allocator);
        defer externals.deinit(allocator);

        const num_import_funcs = externals.functions.len;
        const num_import_tables = externals.tables.len;
        const num_import_mems = externals.memories.len;
        const num_import_globals = externals.globals.len;

        // 20: module instance
        var mod_inst = try allocator.create(ModuleInst);
        mod_inst.types = module.types;
        mod_inst.func_addrs = try allocator.alloc(FuncAddr, num_import_funcs + module.funcs.len);
        mod_inst.table_addrs = try allocator.alloc(TableAddr, num_import_tables + module.tables.len);
        mod_inst.mem_addrs = try allocator.alloc(MemAddr, num_import_mems + module.memories.len);
        mod_inst.global_addrs = try allocator.alloc(GlobalAddr, num_import_globals + module.globals.len);
        mod_inst.elem_addrs = try allocator.alloc(ElemAddr, module.elements.len);
        mod_inst.data_addrs = try allocator.alloc(DataAddr, module.datas.len);
        mod_inst.tag_addrs = try allocator.alloc(TagAddr, externals.tags.len + module.tags.len);
        mod_inst.exports = try allocator.alloc(ExportInst, module.exports.len);

        // 2, 8, 14: function
        @memcpy(mod_inst.func_addrs[0..num_import_funcs], externals.functions);
        for (module.funcs, num_import_funcs..) |func, i|
            mod_inst.func_addrs[i] = try allocFunc(store, func, mod_inst, allocator);

        // 3, 9, 15: table
        @memcpy(mod_inst.table_addrs[0..num_import_tables], externals.tables);
        for (module.tables, num_import_tables..) |table, i|
            mod_inst.table_addrs[i] = try allocTable(store, table, allocator);

        // 4, 10, 16: memory
        @memcpy(mod_inst.mem_addrs[0..num_import_mems], externals.memories);
        for (module.memories, num_import_mems..) |mem, i|
            mod_inst.mem_addrs[i] = try allocMemory(store, mem, allocator);

        // 5, 11, 17: global
        @memcpy(mod_inst.global_addrs[0..num_import_globals], externals.globals);
        for (module.globals, 0..) |global, i|
            mod_inst.global_addrs[num_import_globals + i] = try allocGlobal(store, global, values[i], allocator);

        // 6, 12: element segment
        for (module.elements, 0..) |element, i|
            mod_inst.elem_addrs[i] = try allocElement(store, element, refs[i], allocator);

        // 7, 13: data segment
        for (module.datas, 0..) |data, i|
            mod_inst.data_addrs[i] = try allocData(store, data, allocator);

        // tag instances
        const num_import_tags = externals.tags.len;
        @memcpy(mod_inst.tag_addrs[0..num_import_tags], externals.tags);
        for (module.tags, num_import_tags..) |tag, i|
            mod_inst.tag_addrs[i] = try allocTag(store, tag, mod_inst, allocator);

        // 18, 19: export
        for (module.exports, 0..) |exp, i| {
            const exp_value: ExternalValue = switch (exp.desc) {
                .function => |idx| .{ .function = mod_inst.func_addrs[idx] },
                .table => |idx| .{ .table = mod_inst.table_addrs[idx] },
                .memory => |idx| .{ .memory = mod_inst.mem_addrs[idx] },
                .global => |idx| .{ .global = mod_inst.global_addrs[idx] },
                .tag => |idx| .{ .tag = mod_inst.tag_addrs[idx] },
            };
            const exp_inst = ExportInst{
                .name = exp.name,
                .value = exp_value,
            };
            mod_inst.exports[i] = exp_inst;
        }

        // 21: return
        return mod_inst;
    }

    pub fn auxiliaryInstance(store: *Store, module: Module, extern_vals: []const ExternalValue, allocator: std.mem.Allocator) error{OutOfMemory}!ModuleInst {
        const externals = try ExternalValueGroup.new(extern_vals, allocator);
        defer externals.deinit(allocator);
        const num_import_funcs = externals.functions.len;
        const num_import_globals = externals.globals.len;

        var mod_inst = ModuleInst{
            .types = module.types,
            .func_addrs = try allocator.alloc(FuncAddr, num_import_funcs + module.funcs.len),
            .global_addrs = try allocator.alloc(GlobalAddr, num_import_globals),
        };

        @memcpy(mod_inst.func_addrs[0..num_import_funcs], externals.functions);
        @memcpy(mod_inst.global_addrs[0..num_import_globals], externals.globals);

        for (module.funcs, num_import_funcs..) |func, i|
            mod_inst.func_addrs[i] = try allocFunc(store, func, &mod_inst, allocator);

        return mod_inst;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.func_addrs);
        allocator.free(self.table_addrs);
        allocator.free(self.mem_addrs);
        allocator.free(self.global_addrs);
        allocator.free(self.elem_addrs);
        allocator.free(self.data_addrs);
        allocator.free(self.tag_addrs);
        allocator.free(self.exports);
    }
};

/// `allocfunc` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#functions
fn allocFunc(store: *Store, func: Func, mod_inst: *ModuleInst, allocator: std.mem.Allocator) error{OutOfMemory}!FuncAddr {
    const inst = FuncInst{
        .type = mod_inst.types[func.type],
        .module = mod_inst,
        .code = func,
    };
    return try appendElement(FuncInst, &store.funcs, inst, allocator);
}

/// `alloctable` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#tables
fn allocTable(store: *Store, table: TableType, allocator: std.mem.Allocator) error{OutOfMemory}!TableAddr {
    const elem = try allocator.alloc(RefValue, table.limits.min);
    @memset(elem, nullFromReftype(table.ref_type));
    const inst = TableInst{
        .type = table,
        .elem = elem,
    };
    return try appendElement(TableInst, &store.tables, inst, allocator);
}

/// `allocmemory` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#memories
fn allocMemory(store: *Store, mem: MemoryType, allocator: std.mem.Allocator) error{OutOfMemory}!MemAddr {
    const inst = MemInst{
        .type = mem,
        .data = try allocator.alloc(u8, mem.limits.min * page_size),
    };
    @memset(inst.data, 0);
    return try appendElement(MemInst, &store.mems, inst, allocator);
}

/// `allocglobal` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#globals
fn allocGlobal(store: *Store, global: Global, value: Value, allocator: std.mem.Allocator) error{OutOfMemory}!MemAddr {
    const inst = GlobalInst{
        .type = global.type,
        .value = value,
    };
    return try appendElement(GlobalInst, &store.globals, inst, allocator);
}

/// `allocelem` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#element-segments
fn allocElement(store: *Store, elememt: Element, refs: []RefValue, allocator: std.mem.Allocator) error{OutOfMemory}!ElemAddr {
    const inst = ElemInst{
        .type = elememt.type,
        .elem = refs,
    };
    return try appendElement(ElemInst, &store.elems, inst, allocator);
}

/// `alloctag` for exception handling
fn allocTag(store: *Store, tag: core.types.Tag, mod_inst: *ModuleInst, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!TagAddr {
    if (tag.type_idx >= mod_inst.types.len) return Error.TypeMismatch;
    const inst = TagInst{
        .type = mod_inst.types[tag.type_idx],
    };
    return try appendElement(TagInst, &store.tags, inst, allocator);
}

/// `allocdata` in wasm spec
/// https://webassembly.github.io/spec/core/exec/modules.html#alloc-data
fn allocData(store: *Store, data: Data, allocator: std.mem.Allocator) error{OutOfMemory}!DataAddr {
    const inst = DataInst{
        .data = data.init,
    };
    return try appendElement(DataInst, &store.datas, inst, allocator);
}

fn appendElement(comptime T: type, array: *std.ArrayList(T), elem: T, allocator: std.mem.Allocator) error{OutOfMemory}!u32 {
    const addr: u32 = @intCast(array.items.len);
    try array.append(allocator, elem);
    return addr;
}

fn nullFromReftype(ref_type: std.wasm.RefType) RefValue {
    return switch (ref_type) {
        .funcref => .{ .func_ref = null },
        .externref => .{ .extern_ref = null },
    };
}

const ExternalValueGroup = struct {
    const Self = @This();

    functions: []FuncAddr,
    tables: []TableAddr,
    memories: []MemAddr,
    globals: []GlobalAddr,
    tags: []TagAddr,

    fn new(extern_values: []const ExternalValue, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var funcs: std.ArrayList(FuncAddr) = .empty;
        var tables: std.ArrayList(TableAddr) = .empty;
        var mems: std.ArrayList(MemAddr) = .empty;
        var globals: std.ArrayList(GlobalAddr) = .empty;
        var tags_list: std.ArrayList(TagAddr) = .empty;

        for (extern_values) |imp| {
            switch (imp) {
                .function => |idx| try funcs.append(allocator, idx),
                .table => |idx| try tables.append(allocator, idx),
                .memory => |idx| try mems.append(allocator, idx),
                .global => |idx| try globals.append(allocator, idx),
                .tag => |idx| try tags_list.append(allocator, idx),
            }
        }

        return .{
            .functions = try funcs.toOwnedSlice(allocator),
            .tables = try tables.toOwnedSlice(allocator),
            .memories = try mems.toOwnedSlice(allocator),
            .globals = try globals.toOwnedSlice(allocator),
            .tags = try tags_list.toOwnedSlice(allocator),
        };
    }

    fn deinit(self: Self, allocator: std.mem.Allocator) void {
        allocator.free(self.functions);
        allocator.free(self.tables);
        allocator.free(self.memories);
        allocator.free(self.globals);
        allocator.free(self.tags);
    }
};
