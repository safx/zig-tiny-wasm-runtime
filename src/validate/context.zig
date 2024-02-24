const std = @import("std");
const types = struct {
    const core = @import("wasm-core");
    usingnamespace core;
    const ResultType = []const core.ValueType;
};
const Error = @import("./errors.zig").Error;

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
        const imports = try ImportGroup.new(module, allocator);
        defer imports.deinit(allocator);

        var num_import_funcs = imports.funcs.len;
        var num_import_tables = imports.tables.len;
        var num_import_mems = imports.mems.len;
        var num_import_globals = imports.globals.len;

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

const ImportGroup = struct {
    const Self = @This();

    funcs: []types.FuncType = &.{},
    tables: []types.TableType = &.{},
    mems: []types.MemoryType = &.{},
    globals: []types.GlobalType = &.{},

    fn new(module: types.Module, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var funcs = std.ArrayList(types.FuncType).init(allocator);
        var tables = std.ArrayList(types.TableType).init(allocator);
        var mems = std.ArrayList(types.MemoryType).init(allocator);
        var globals = std.ArrayList(types.GlobalType).init(allocator);

        for (module.imports) |imp| {
            switch (imp.desc) {
                .function => |idx| try funcs.append(module.types[idx]),
                .table => |ty| try tables.append(ty),
                .memory => |ty| try mems.append(ty),
                .global => |ty| try globals.append(ty),
            }
        }

        return .{
            .funcs = try funcs.toOwnedSlice(),
            .tables = try tables.toOwnedSlice(),
            .mems = try mems.toOwnedSlice(),
            .globals = try globals.toOwnedSlice(),
        };
    }

    fn deinit(self: Self, allocator: std.mem.Allocator) void {
        allocator.free(self.funcs);
        allocator.free(self.tables);
        allocator.free(self.mems);
        allocator.free(self.globals);
    }
};
