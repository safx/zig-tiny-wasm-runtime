const std = @import("std");
const core = @import("wasm-core");
const Error = @import("./errors.zig").Error;

// Type aliases for convenience
const Module = core.types.Module;
const FuncType = core.types.FuncType;
const TableType = core.types.TableType;
const MemoryType = core.types.MemoryType;
const GlobalType = core.types.GlobalType;
const RefType = core.types.RefType;
const ValueType = core.types.ValueType;
const TypeIdx = core.types.TypeIdx;
const FuncIdx = core.types.FuncIdx;
const TableIdx = core.types.TableIdx;
const MemIdx = core.types.MemIdx;
const GlobalIdx = core.types.GlobalIdx;
const ElemIdx = core.types.ElemIdx;
const DataIdx = core.types.DataIdx;
const Func = core.types.Func;
const LocalIdx = core.types.LocalIdx;
const LabelIdx = core.types.LabelIdx;
const ResultType = []const ValueType;

pub const Context = struct {
    const Self = @This();

    types: []const FuncType = &.{},
    funcs: []const FuncType = &.{},
    tables: []const TableType = &.{},
    mems: []const MemoryType = &.{},
    globals: []const GlobalType = &.{},
    elems: []const RefType = &.{},
    datas: []const bool = &.{},
    locals: []const ValueType = &.{},
    labels: []const ResultType = &.{},
    @"return": ?ResultType = null,
    refs: []const FuncIdx = &.{},

    /// creates the context C in sec 3.4.10
    pub fn new(module: Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Self {
        return try newContext(module, true, allocator);
    }

    /// creates the context C' in sec 3.4.10
    pub fn newLimitedContext(module: Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Self {
        return try newContext(module, false, allocator);
    }

    fn newContext(module: Module, comptime with_global: bool, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Self {
        const imports = try ImportGroup.new(module, allocator);
        defer imports.deinit(allocator);

        const num_import_tables = imports.tables.len;
        const num_import_mems = imports.memories.len;
        const num_import_globals = imports.globals.len;

        var tables = try allocator.alloc(TableType, num_import_tables + module.tables.len);
        var mems = try allocator.alloc(MemoryType, num_import_mems + module.memories.len);
        var globals = try allocator.alloc(GlobalType, num_import_globals + if (with_global) module.globals.len else 0);
        var elems = try allocator.alloc(RefType, module.elements.len);
        var datas = try allocator.alloc(bool, module.datas.len);

        @memcpy(tables[0..num_import_tables], imports.tables);
        @memcpy(tables[num_import_tables..tables.len], module.tables);

        @memcpy(mems[0..num_import_mems], imports.memories);
        @memcpy(mems[num_import_mems..mems.len], module.memories);

        @memcpy(globals[0..num_import_globals], imports.globals);
        if (with_global) {
            for (module.globals, 0..) |global, i| {
                globals[num_import_globals + i] = global.type;
            }
        }

        for (module.elements, 0..) |element, i| {
            elems[i] = element.type;
        }

        for (module.datas, 0..) |_, i| {
            datas[i] = false;
        }

        return .{
            .types = module.types,
            .funcs = try createFuncs(module, imports, allocator),
            .tables = tables,
            .mems = mems,
            .globals = globals,
            .elems = elems,
            .datas = datas,
            .refs = try createRefs(module, allocator),
        };
    }

    fn createFuncs(module: Module, imports: ImportGroup, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const FuncType {
        const num_import_funcs = imports.functions.len;
        var funcs = try allocator.alloc(FuncType, num_import_funcs + module.funcs.len);
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
        return funcs;
    }

    /// creates the set of function indices occurring in the module, except in its functions or start function.
    fn createRefs(module: Module, allocator: std.mem.Allocator) error{OutOfMemory}![]const FuncIdx {
        var refs = std.array_list.Managed(FuncIdx).init(allocator);

        for (module.globals) |global| {
            if (global.init == .ref_func) try refs.append(global.init.ref_func);
        }
        for (module.elements) |element| {
            for (element.init) |init|
                if (init == .ref_func) try refs.append(init.ref_func);
        }
        for (module.imports) |import| {
            if (import.desc == .function) try refs.append(import.desc.function);
        }
        for (module.exports) |exp| {
            if (exp.desc == .function) try refs.append(exp.desc.function);
        }

        return try refs.toOwnedSlice();
    }

    /// creates the context C' in sec 3.4.1 Functions
    pub fn cloneWithFunction(c: Context, func: Func, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        const ty = c.types[func.type];

        var locals = try allocator.alloc(ValueType, ty.parameter_types.len + func.locals.len);
        @memcpy(locals[0..ty.parameter_types.len], ty.parameter_types);
        @memcpy(locals[ty.parameter_types.len..locals.len], func.locals);

        var labels = try allocator.alloc(ResultType, 1);
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

    /// create the context C' for block (sec 3.3.8.3, 3.3.8.4 and 3.3.8.5)
    pub fn cloneWithPrependingLabel(c: Context, result_type: ResultType, allocator: std.mem.Allocator) (error{OutOfMemory})!Self {
        var labels = try allocator.alloc(ResultType, c.labels.len + 1);
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

    pub fn getType(self: Self, idx: TypeIdx) Error!FuncType {
        return if (idx < self.types.len) self.types[idx] else Error.UnknownType;
    }

    pub fn getFunc(self: Self, idx: FuncIdx) Error!FuncType {
        return if (idx < self.funcs.len) self.funcs[idx] else Error.UnknownFunction;
    }

    pub fn getTable(self: Self, idx: TableIdx) Error!TableType {
        return if (idx < self.tables.len) self.tables[idx] else Error.UnknownTable;
    }

    pub fn checkMem(self: Self, idx: MemIdx) Error!void {
        if (idx >= self.mems.len) return Error.UnknownMemory;
    }

    pub fn getGlobal(self: Self, idx: GlobalIdx) Error!GlobalType {
        return if (idx < self.globals.len) self.globals[idx] else Error.UnknownGlobal;
    }

    pub fn getElem(self: Self, idx: ElemIdx) Error!RefType {
        return if (idx < self.elems.len) self.elems[idx] else Error.UnknownElementSegment;
    }

    pub fn checkData(self: Self, idx: DataIdx) Error!void {
        if (idx >= self.datas.len) return Error.UnknownDataSegment;
    }

    pub fn checkRef(self: Self, idx: u32) Error!void {
        if (idx >= self.refs.len) return Error.UndeclaredFunctionReference;
    }

    pub fn getLocal(self: Self, idx: LocalIdx) Error!ValueType {
        return if (idx < self.locals.len) self.locals[idx] else Error.UnknownLocal;
    }

    pub fn getLabel(self: Self, idx: LabelIdx) Error!ResultType {
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

    functions: []FuncIdx = &.{},
    tables: []TableType = &.{},
    memories: []MemoryType = &.{},
    globals: []GlobalType = &.{},

    fn new(module: Module, allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        var functions = std.array_list.Managed(FuncIdx).init(allocator);
        var tables = std.array_list.Managed(TableType).init(allocator);
        var memories = std.array_list.Managed(MemoryType).init(allocator);
        var globals = std.array_list.Managed(GlobalType).init(allocator);

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
