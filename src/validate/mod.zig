pub const std = @import("std");
pub const wa = @import("wasm-core");

pub fn validate(module: wa.Module) void {
    _ = module;
}

const Context = struct {
    types: []const wa.FuncType = &.{},
    funcs: []wa.Func = &.{},
    tables: []wa.TableType = &.{},
    mems: []wa.MemoryType = &.{},
    globals: []wa.GlobalType = &.{},
    elems: []wa.RefType = &.{},
    datas: []wa.Data = &.{},
    locals: []u8 = &.{}, // FIXME: type
    labels: []u8 = &.{}, // FIXME: type
    @"return": []u8 = &.{}, // FIXME: type
    refs: []wa.FuncIdx,

    pub fn new(module: wa.Module, allocator: std.mem.Allocator) (error{OutOfMemory})!Context {
        // 1: resolve imports
        var num_import_funcs: u32 = 0;
        var num_import_tables: u32 = 0;
        var num_import_mems: u32 = 0;
        var num_import_globals: u32 = 0;
        for (module.imports) |imp|
            switch (imp) {
                .function => num_import_funcs += 1,
                .table => num_import_tables += 1,
                .memory => num_import_mems += 1,
                .global => num_import_globals += 1,
            };

        // 20: module instance
        var context = Context{};
        context.types = module.types;
        context.func_addrs = try allocator.alloc(wa.Func, num_import_funcs + module.funcs.len);
        context.table_addrs = try allocator.alloc(wa.TableType, num_import_tables + module.tables.len);
        context.mem_addrs = try allocator.alloc(wa.MemoryType, num_import_mems + module.memories.len);
        context.global_addrs = try allocator.alloc(wa.GlobalType, num_import_globals + module.globals.len);
        context.elem_addrs = try allocator.alloc(wa.RefType, module.elements.len);
        context.data_addrs = try allocator.alloc(wa.Data, module.datas.len);

        return context;
    }
};
