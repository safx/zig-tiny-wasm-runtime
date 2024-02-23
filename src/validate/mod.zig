pub const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
};
pub const Error = @import("./errors.zig").Error;

pub fn validate(module: types.Module) void {
    _ = module;
}

const Context = struct {
    types: []const types.FuncType = &.{},
    funcs: []types.Func = &.{},
    tables: []types.TableType = &.{},
    mems: []types.MemoryType = &.{},
    globals: []types.GlobalType = &.{},
    elems: []types.RefType = &.{},
    datas: []types.Data = &.{},
    locals: []u8 = &.{}, // FIXME: type
    labels: []u8 = &.{}, // FIXME: type
    @"return": []u8 = &.{}, // FIXME: type
    refs: []types.FuncIdx,

    pub fn new(module: types.Module, allocator: std.mem.Allocator) (error{OutOfMemory})!Context {
        var num_import_funcs: u32 = 0;
        var num_import_tables: u32 = 0;
        var num_import_mems: u32 = 0;
        var num_import_globals: u32 = 0;
        for (module.imports) |imp|
            switch (imp.desc) {
                .func => num_import_funcs += 1,
                .table => num_import_tables += 1,
                .memory => num_import_mems += 1,
                .global => num_import_globals += 1,
            };

        var context = Context{};
        context.types = module.types;
        context.func_addrs = try allocator.alloc(types.Func, num_import_funcs + module.funcs.len);
        context.table_addrs = try allocator.alloc(types.TableType, num_import_tables + module.tables.len);
        context.mem_addrs = try allocator.alloc(types.MemoryType, num_import_mems + module.memories.len);
        context.global_addrs = try allocator.alloc(types.GlobalType, num_import_globals + module.globals.len);
        context.elem_addrs = try allocator.alloc(types.RefType, module.elements.len);
        context.data_addrs = try allocator.alloc(types.Data, module.datas.len);

        return context;
    }
};
