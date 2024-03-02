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

        var funcs = try allocator.alloc(types.FuncType, num_import_funcs + module.funcs.len);
        var tables = try allocator.alloc(types.TableType, num_import_tables + module.tables.len);
        var mems = try allocator.alloc(types.MemoryType, num_import_mems + module.memories.len);
        var globals = try allocator.alloc(types.GlobalType, num_import_globals + module.globals.len);
        var elems = try allocator.alloc(types.RefType, module.elements.len);
        var datas = try allocator.alloc(bool, module.datas.len);
        var refs = std.ArrayList(types.FuncIdx).init(allocator);

        for (imports.functions, 0..) |func, i| {
            funcs[i] = module.types[func];
        }
        for (module.funcs, num_import_funcs..) |func, i| {
            funcs[i] = module.types[func.type];
        }

        @memcpy(tables[0..num_import_tables], imports.tables);
        @memcpy(tables[num_import_tables..tables.len], module.tables);

        @memcpy(mems[0..num_import_mems], imports.memories);
        @memcpy(mems[num_import_mems..mems.len], module.memories);

        @memcpy(globals[0..num_import_globals], imports.globals);
        for (module.globals, 0..) |global, i| {
            globals[num_import_globals + i] = global.type;
        }

        for (module.elements, 0..) |element, i| {
            elems[i] = element.type;
        }

        for (module.datas, 0..) |_, i| {
            datas[i] = false;
        }

        // create the set of function indices occurring in the module, except in its functions or start function.
        for (module.globals) |global| {
            if (global.init == .ref_func) try refs.append(global.init.ref_func);
        }
        for (module.elements) |element| {
            for (element.init) |i|
                if (i == .ref_func) try refs.append(i.ref_func);
        }
        for (module.imports) |import| {
            if (import.desc == .function) try refs.append(import.desc.function);
        }
        for (module.exports) |exp| {
            if (exp.desc == .function) try refs.append(exp.desc.function);
        }

        return .{
            .types = module.types,
            .funcs = funcs,
            .tables = tables,
            .mems = mems,
            .globals = globals,
            .elems = elems,
            .datas = datas,
            .refs = try refs.toOwnedSlice(),
        };
    }

    pub fn cloneWithFunction(c: Context, func: types.Func, allocator: std.mem.Allocator) (error{OutOfMemory})!Self {
        const ty = c.types[func.type];

        var locals = try allocator.alloc(types.ValueType, ty.parameter_types.len + func.locals.len);
        @memcpy(locals[0..ty.parameter_types.len], ty.parameter_types);
        @memcpy(locals[ty.parameter_types.len..locals.len], func.locals);

        var labels = try allocator.alloc(types.ResultType, 1);
        labels[0] = ty.result_types;

        return .{
            .types = c.types,
            .funcs = c.funcs,
            .tables = c.tables,
            .mems = c.mems,
            .globals = c.globals,
            .elems = c.elems,
            .datas = c.datas,
            .refs = c.refs,
            .locals = locals,
            .labels = labels,
            .@"return" = ty.result_types,
        };
    }

    pub fn cloneWithPrependingLabel(c: Context, result_type: types.ResultType, allocator: std.mem.Allocator) (error{OutOfMemory})!Self {
        var labels = try allocator.alloc(types.ResultType, c.labels.len + 1);
        @memcpy(labels[0..c.labels.len], c.labels);
        labels[c.labels.len] = result_type;

        return .{
            .types = c.types,
            .funcs = c.funcs,
            .tables = c.tables,
            .mems = c.mems,
            .globals = c.globals,
            .elems = c.elems,
            .datas = c.datas,
            .refs = c.refs,
            .locals = c.locals,
            .labels = labels,
            .@"return" = c.@"return",
        };
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

    functions: []types.FuncIdx = &.{},
    tables: []types.TableType = &.{},
    memories: []types.MemoryType = &.{},
    globals: []types.GlobalType = &.{},

    fn new(module: types.Module, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var functions = std.ArrayList(types.FuncIdx).init(allocator);
        var tables = std.ArrayList(types.TableType).init(allocator);
        var memories = std.ArrayList(types.MemoryType).init(allocator);
        var globals = std.ArrayList(types.GlobalType).init(allocator);

        for (module.imports) |imp| {
            switch (imp.desc) {
                .function => |idx| try functions.append(idx),
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
