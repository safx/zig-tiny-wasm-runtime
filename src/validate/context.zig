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
    refs: []const types.FuncIdx = &.{},

    pub fn new(module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Self {
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

        for (imports.functions, 0..) |func_idx, i| {
            if (func_idx >= module.types.len)
                return Error.UnknownType;
            funcs[i] = module.types[func_idx];
        }
        for (module.funcs, num_import_funcs..) |func, i| {
            if (func.type >= module.types.len)
                return Error.UnknownType;
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

        return .{
            .types = module.types,
            .funcs = funcs,
            .tables = tables,
            .mems = mems,
            .globals = globals,
            .elems = elems,
            .datas = datas,
            .refs = try createRefs(module, allocator),
        };
    }

    pub fn newLimitedContext(module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Self {
        const imports = try ImportGroup.new(module, allocator);
        defer imports.deinit(allocator);

        var num_import_funcs = imports.functions.len;
        var num_import_globals = imports.globals.len;

        var funcs = try allocator.alloc(types.FuncType, num_import_funcs + module.funcs.len);
        var globals = try allocator.alloc(types.GlobalType, num_import_globals);

        for (imports.functions, 0..) |func_idx, i| {
            if (func_idx >= module.types.len)
                return Error.UnknownType;
            funcs[i] = module.types[func_idx];
        }
        for (module.funcs, num_import_funcs..) |func, i| {
            if (func.type >= module.types.len)
                return Error.UnknownType;
            funcs[i] = module.types[func.type];
        }

        @memcpy(globals, imports.globals);

        return .{
            .funcs = funcs,
            .globals = globals,
            .refs = try createRefs(module, allocator),
        };
    }

    fn createRefs(module: types.Module, allocator: std.mem.Allocator) error{OutOfMemory}![]const types.FuncIdx {
        var refs = std.ArrayList(types.FuncIdx).init(allocator);

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

        return try refs.toOwnedSlice();
    }

    pub fn cloneWithFunction(c: Context, func: types.Func, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
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

    pub fn getType(self: Self, idx: types.TypeIdx) Error!types.FuncType {
        return if (idx < self.types.len) self.types[idx] else Error.UnknownType;
    }

    pub fn getFunc(self: Self, idx: types.FuncIdx) Error!types.FuncType {
        return if (idx < self.funcs.len) self.funcs[idx] else Error.UnknownFunction;
    }

    pub fn getTable(self: Self, idx: types.TableIdx) Error!types.TableType {
        return if (idx < self.tables.len) self.tables[idx] else Error.UnknownTable;
    }

    pub fn checkMem(self: Self, idx: types.MemIdx) Error!void {
        if (idx >= self.mems.len) return Error.UnknownMemory;
    }

    pub fn getGlobal(self: Self, idx: types.GlobalIdx) Error!types.GlobalType {
        return if (idx < self.globals.len) self.globals[idx] else Error.UnknownGlobal;
    }

    pub fn getElem(self: Self, idx: types.ElemIdx) Error!types.RefType {
        return if (idx < self.elems.len) self.elems[idx] else Error.UnknownElementSegment;
    }

    pub fn checkData(self: Self, idx: types.DataIdx) Error!void {
        if (idx >= self.datas.len) return Error.UnknownDataSegment;
    }

    pub fn checkRef(self: Self, idx: u32) Error!void {
        if (idx >= self.refs.len) return Error.UndeclaredFunctionReference;
    }

    pub fn getLocal(self: Self, idx: types.LocalIdx) Error!types.ValueType {
        return if (idx < self.locals.len) self.locals[idx] else Error.UnknownLocal;
    }

    pub fn getLabel(self: Self, idx: types.LabelIdx) Error!types.ResultType {
        return if (idx < self.labels.len) self.labels[self.labels.len - idx - 1] else Error.UnknownLabel;
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
