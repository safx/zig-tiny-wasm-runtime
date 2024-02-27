const std = @import("std");
const types = struct {
    const core = @import("wasm-core");
    usingnamespace core;
    const ResultType = []const core.ValueType;
};
const Error = @import("./errors.zig").Error;

pub const Context = struct {
    const Self = @This();

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

    pub fn new(module: types.Module, allocator: std.mem.Allocator) (error{OutOfMemory})!Self {
        const imports = try ImportGroup.new(module, allocator);
        defer imports.deinit(allocator);

        var num_import_funcs = imports.functions.len;
        var num_import_tables = imports.tables.len;
        var num_import_mems = imports.memories.len;
        var num_import_globals = imports.globals.len;

        var context = Context{};
        context.types = module.types;
        context.funcs = try allocator.alloc(types.FuncType, num_import_funcs + module.funcs.len);
        context.tables = try allocator.alloc(types.TableType, num_import_tables + module.tables.len);
        context.mems = try allocator.alloc(types.MemoryType, num_import_mems + module.memories.len);
        context.globals = try allocator.alloc(types.GlobalType, num_import_globals + module.globals.len);
        context.elems = try allocator.alloc(types.RefType, module.elements.len);
        context.datas = try allocator.alloc(bool, module.datas.len);

        @memcpy(context.funcs[0..num_import_funcs], imports.functions);
        for (module.funcs, num_import_funcs..) |func, i| {
            context.funcs[i] = module.types[func.type];
        }

        @memcpy(context.tables[0..num_import_tables], imports.tables);
        @memcpy(context.tables[num_import_tables..context.tables.len], module.tables);

        @memcpy(context.mems[0..num_import_mems], imports.memories);
        @memcpy(context.mems[num_import_mems..context.mems.len], module.memories);

        @memcpy(context.globals[0..num_import_globals], imports.globals);
        for (module.globals, 0..) |global, i| {
            context.globals[num_import_globals + i] = global.type;
        }

        for (module.elements, 0..) |element, i| {
            context.elems[i] = element.type;
        }

        for (module.datas, 0..) |_, i| {
            context.datas[i] = false;
        }

        return context;
    }

    pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
        allocator.free(self.funcs);
        allocator.free(self.tables);
        allocator.free(self.mems);
        allocator.free(self.globals);
        allocator.free(self.elems);
        allocator.free(self.datas);
    }
};

const ImportGroup = struct {
    const Self = @This();

    functions: []types.FuncType = &.{},
    tables: []types.TableType = &.{},
    memories: []types.MemoryType = &.{},
    globals: []types.GlobalType = &.{},

    fn new(module: types.Module, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var functions = std.ArrayList(types.FuncType).init(allocator);
        var tables = std.ArrayList(types.TableType).init(allocator);
        var memories = std.ArrayList(types.MemoryType).init(allocator);
        var globals = std.ArrayList(types.GlobalType).init(allocator);

        for (module.imports) |imp| {
            switch (imp.desc) {
                .function => |idx| try functions.append(module.types[idx]),
                .table => |ty| try tables.append(ty),
                .memory => |ty| try memories.append(ty),
                .global => |ty| try globals.append(ty),
            }
        }

        return .{
            .functions = try functions.toOwnedSlice(),
            .tables = try tables.toOwnedSlice(),
            .memories = try memories.toOwnedSlice(),
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
