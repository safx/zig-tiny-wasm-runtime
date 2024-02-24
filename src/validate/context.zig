pub const std = @import("std");
const types = struct {
    const core = @import("wasm-core");
    usingnamespace core;
    const ResultType = []const core.ValueType;
};
pub const Error = @import("./errors.zig").Error;

pub fn validate(module: types.Module) void {
    _ = module;
}

pub const Context = struct {
    types: []const types.FuncType = &.{},
    funcs: []types.FuncType = &.{},
    tables: []types.TableType = &.{},
    mems: []types.MemoryType = &.{},
    globals: []types.GlobalType = &.{},
    elems: []types.RefType = &.{},
    datas: []bool = &.{},
    locals: []types.ValueType = &.{},
    labels: []types.ResultType = &.{},
    @"return": ?types.ResultType = null,
    refs: []types.FuncIdx = &.{},

    pub fn new(module: types.Module, allocator: std.mem.Allocator) (error{OutOfMemory})!Context {
        var num_import_funcs: u32 = 0;
        var num_import_tables: u32 = 0;
        var num_import_mems: u32 = 0;
        var num_import_globals: u32 = 0;
        for (module.imports) |imp|
            switch (imp.desc) {
                .function => num_import_funcs += 1,
                .table => num_import_tables += 1,
                .memory => num_import_mems += 1,
                .global => num_import_globals += 1,
            };

        var context = Context{};
        context.types = module.types;
        context.funcs = try allocator.alloc(types.FuncType, num_import_funcs + module.funcs.len);
        context.tables = try allocator.alloc(types.TableType, num_import_tables + module.tables.len);
        context.mems = try allocator.alloc(types.MemoryType, num_import_mems + module.memories.len);
        context.globals = try allocator.alloc(types.GlobalType, num_import_globals + module.globals.len);
        context.elems = try allocator.alloc(types.RefType, module.elements.len);
        context.datas = try allocator.alloc(bool, module.datas.len);

        return context;
    }
};
