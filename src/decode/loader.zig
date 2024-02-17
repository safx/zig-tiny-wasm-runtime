const std = @import("std");
const types = @import("wasm-core");
const BinaryReader = @import("./binary_reader.zig").BinaryReader;
const Decoder = @import("./decoder.zig").Decoder;
const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

pub const ModuleLoader = struct {
    const Self = @This();
    const assert = std.debug.assert;

    baseAllocator: std.mem.Allocator,
    allocator: std.mem.Allocator,
    reader: BinaryReader,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .baseAllocator = allocator,
            .allocator = allocator, // assigned temporarily
            .reader = BinaryReader.new(undefined),
        };
    }

    pub fn parseAll(self: *Self, buffer: []const u8) (Error || error{OutOfMemory})!types.Module {
        var arena = std.heap.ArenaAllocator.init(self.baseAllocator);
        self.allocator = arena.allocator();

        self.reader = BinaryReader.new(buffer);
        const version = try self.readHeaderVersion();
        if (version != 1) {
            return Error.UnknownBinaryVersion;
        }

        var types_: []const types.FuncType = &.{};
        var imports: []const types.Import = &.{};
        var func_idxs: []const types.FuncIdx = &.{};
        var tables: []const types.TableType = &.{};
        var memories: []const types.MemoryType = &.{};
        var globals: []const types.Global = &.{};
        var exports: []const types.Export = &.{};
        var start: ?types.FuncIdx = null;
        var elements: []const types.Element = &.{};
        var codes: []const ModFunc = &.{};
        var datas: []const types.Data = &.{};
        var data_count: u32 = 0;

        while (self.section()) |sec| {
            {
                var out = std.ArrayList(u8).init(std.heap.c_allocator);
                defer out.deinit();
                try std.json.stringify(sec, .{}, out.writer());
                std.debug.print("*** {s}\n", .{out.items});
            }
            switch (sec) {
                .type => |d| types_ = d,
                .import => |d| imports = d,
                .function => |d| func_idxs = d,
                .table => |d| tables = d,
                .memory => |d| memories = d,
                .global => |d| globals = d,
                .@"export" => |d| exports = d,
                .start => |d| start = d,
                .element => |d| elements = d,
                .code => |d| codes = d,
                .data => |d| datas = d,
                .data_count => |d| data_count = d,
                .custom => {},
            }
        } else |err| {
            if (err != error.EOF) {
                return err;
            }
        }

        const funcs = try self.allocator.alloc(types.Func, func_idxs.len);
        for (func_idxs, codes, 0..) |func_idx, f, i| {
            funcs[i] = types.Func{
                .type = func_idx,
                .locals = f.locals,
                .body = f.body,
            };
        }

        return types.Module{
            .types = types_,
            .funcs = funcs,
            .tables = tables,
            .memories = memories,
            .globals = globals,
            .elements = elements,
            .datas = datas,
            .start = start,
            .imports = imports,
            .exports = exports,

            .arena = arena,
        };
    }

    pub fn readHeaderVersion(self: *Self) Error!u32 {
        const magic_number = try self.reader.readBytes(4);
        if (!std.mem.eql(u8, magic_number, &std.wasm.magic)) {
            return Error.MagicHeaderNotDetected;
        }
        return self.reader.readU32();
    }

    fn section(self: *Self) (Error || error{OutOfMemory})!Section {
        const section_id = try self.reader.readU8();
        const sect = utils.sectionFromNum(section_id) orelse return Error.MalformedSectionId;
        const size = try self.reader.readVarU32();
        _ = size;

        std.debug.print("* {}\n", .{sect});
        switch (sect) {
            .custom => return .custom,
            .type => return .{ .type = try self.createArray(types.FuncType, funcType) },
            .import => return .{ .import = try self.createArray(types.Import, import) },
            .function => return .{ .function = try self.createArray(types.FuncIdx, funcidx) },
            .table => return .{ .table = try self.createArray(types.TableType, table) },
            .memory => return .{ .memory = try self.createArray(types.MemoryType, memtype) },
            .global => return .{ .global = try self.createArray(types.Global, global) },
            .@"export" => return .{ .@"export" = try self.createArray(types.Export, @"export") },
            .start => return .{ .start = try self.reader.readVarU32() },
            .element => return .{ .element = try self.createArray(types.Element, elem) },
            .code => return .{ .code = try self.createArray(ModFunc, code) },
            .data => return .{ .data = try self.createArray(types.Data, data) },
            .data_count => return .{ .data_count = try self.reader.readVarU32() },
            _ => return Error.MalformedSectionId,
        }
    }

    // section
    fn funcType(self: *Self) (Error || error{OutOfMemory})!types.FuncType {
        const marker = try self.reader.readU8();
        if (marker != std.wasm.function_type) {
            return Error.MalformedFunctypeMagicNumber;
        }
        const param_types = try self.resultType();
        const result_types = try self.resultType();
        return .{
            .parameter_types = param_types,
            .result_types = result_types,
        };
    }

    fn import(self: *Self) (Error || error{OutOfMemory})!types.Import {
        const mod = try self.name();
        const nm = try self.name();
        const desc = try self.importdesc();
        return .{ .module_name = mod, .name = nm, .desc = desc };
    }

    fn funcidx(self: *Self) Error!types.FuncIdx {
        return self.reader.readVarU32();
    }

    fn table(self: *Self) Error!types.TableType {
        const rtype = try self.refType();
        const limit = try self.limits();
        return .{ .limits = limit, .ref_type = rtype };
    }

    fn memtype(self: *Self) Error!types.MemoryType {
        return .{ .limits = try self.limits() };
    }

    fn global(self: *Self) Error!types.Global {
        const gtype = try self.globalType();
        const exp = try self.initExpr();
        return .{ .type = gtype, .init = exp };
    }

    fn @"export"(self: *Self) (Error || error{OutOfMemory})!types.Export {
        const nm = try self.name();
        const d = try self.exportdesc();
        return .{ .name = nm, .desc = d };
    }

    fn elem(self: *Self) (Error || error{OutOfMemory})!types.Element {
        const kind = try self.reader.readVarU32();
        switch (kind) {
            0 => {
                const expr = try self.initExpr();
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = types.InitExpression{ .ref_func = y };
                }
                const mode = types.ElementMode{ .active = .{ .table_idx = 0, .offset = expr } };
                return .{ .type = .funcref, .init = init_array, .mode = mode };
            },
            1 => {
                const et = try self.reader.readU8();
                assert(et == 0);
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = types.InitExpression{ .ref_func = y };
                }
                return .{ .type = .funcref, .init = init_array, .mode = .passive };
            },
            2 => {
                const x = try self.reader.readVarU32();
                const expr = try self.initExpr();
                const et = try self.reader.readU8();
                assert(et == 0);
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = types.InitExpression{ .ref_func = y };
                }
                const mode = types.ElementMode{ .active = .{ .table_idx = x, .offset = expr } };
                return .{ .type = .funcref, .init = init_array, .mode = mode };
            },
            3 => {
                const et = try self.reader.readU8();
                assert(et == 0);
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = types.InitExpression{ .ref_func = y };
                }
                return .{ .type = .funcref, .init = init_array, .mode = .declarative };
            },
            4 => {
                const expr = try self.initExpr();
                const init_array = try self.initExprVec();
                const mode = types.ElementMode{ .active = .{ .table_idx = 0, .offset = expr } };
                return .{ .type = .funcref, .init = init_array, .mode = mode };
            },
            5 => {
                const et = try self.refType();
                const init_array = try self.initExprVec();
                return .{ .type = et, .init = init_array, .mode = .passive };
            },
            6 => {
                const x = try self.reader.readVarU32();
                const expr = try self.initExpr();
                const et = try self.refType();
                const init_array = try self.initExprVec();
                const mode = types.ElementMode{ .active = .{ .table_idx = x, .offset = expr } };
                return .{ .type = et, .init = init_array, .mode = mode };
            },
            7 => {
                const et = try self.refType();
                const init_array = try self.initExprVec();
                return .{ .type = et, .init = init_array, .mode = .declarative };
            },
            else => return Error.MalformedElemKind,
        }
        unreachable;
    }

    fn code(self: *Self) (Error || error{OutOfMemory})!ModFunc {
        return try self.func();
    }

    fn data(self: *Self) (Error || error{OutOfMemory})!types.Data {
        const kind = try self.reader.readVarU32();
        switch (kind) {
            0 => {
                const exp = try self.initExpr();
                const len = try self.reader.readVarU32();
                const init_array = try self.readBytesWithCopy(len);
                const mode = types.DataMode{ .active = .{ .mem_idx = 0, .offset = exp } };
                return .{ .init = init_array, .mode = mode };
            },
            1 => {
                const len = try self.reader.readVarU32();
                const init_array = try self.readBytesWithCopy(len);
                return .{ .init = init_array, .mode = .passive };
            },
            2 => {
                const x = try self.reader.readVarU32();
                const exp = try self.initExpr();
                const len = try self.reader.readVarU32();
                const init_array = try self.readBytesWithCopy(len);
                const mode = types.DataMode{ .active = .{ .mem_idx = x, .offset = exp } };
                return .{ .init = init_array, .mode = mode };
            },
            else => unreachable,
        }
        unreachable;
    }

    fn skip(self: *Self, size: u32) Error!void {
        _ = try self.reader.readBytes(size);
    }

    // types

    fn importdesc(self: *Self) Error!types.ImportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0 => .{ .func = try self.reader.readVarU32() },
            1 => .{ .table = try self.table() },
            2 => .{ .memory = try self.memtype() },
            3 => .{ .global = try self.globalType() },
            else => return Error.MalformedImportKind,
        };
    }

    fn exportdesc(self: *Self) Error!types.ExportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0 => .{ .func = try self.reader.readVarU32() },
            1 => .{ .table = try self.reader.readVarU32() },
            2 => .{ .memory = try self.reader.readVarU32() },
            3 => .{ .global = try self.reader.readVarU32() },
            else => return Error.MalformedExportKind,
        };
    }

    fn funcidxs(self: *Self) (Error || error{OutOfMemory})![]const types.FuncIdx {
        return try self.createArray(types.FuncIdx, funcidx);
    }

    fn globalType(self: *Self) Error!types.GlobalType {
        const vtype = try self.valueType();
        const m = try self.mut();
        return .{ .mutability = m, .value_type = vtype };
    }

    fn initExpr(self: *Self) Error!types.InitExpression {
        const op = try self.reader.readU8();
        const value = try self.initExprValue(op);
        const end = try self.reader.readU8();
        assert(end == 0x0b);
        return value;
    }

    fn initExprValue(self: *Self, op: u8) Error!types.InitExpression {
        const n = std.wasm.opcode;
        return switch (op) {
            n(.i32_const) => .{ .i32_const = try self.reader.readVarI32() },
            n(.i64_const) => .{ .i64_const = try self.reader.readVarI64() },
            n(.f32_const) => .{ .f32_const = try self.reader.readF32() },
            n(.f64_const) => .{ .f64_const = try self.reader.readF64() },
            n(.global_get) => .{ .global_get = try self.reader.readVarU32() },
            0xd0 => .{ .ref_null = @enumFromInt(try self.reader.readU8()) },
            0xd2 => .{ .ref_func = try self.reader.readVarU32() },
            else => unreachable,
        };
    }

    fn initExprVec(self: *Self) (Error || error{OutOfMemory})![]const types.InitExpression {
        return try self.createArray(types.InitExpression, initExpr);
    }

    fn mut(self: *Self) Error!types.Mutability {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0 => .immutable,
            1 => .mutable,
            else => return Error.MalformedMutId,
        };
    }

    fn func(self: *Self) (Error || error{OutOfMemory})!ModFunc {
        const len = try self.reader.readVarU32();
        const start_pos = self.reader.position;
        const localses = try self.createArray(Locals, locals);
        const pos = self.reader.position;
        const actual_body_size = len - (pos - start_pos);

        var decoder = Decoder.new();
        const code_buf = try self.reader.readBytes(actual_body_size);
        var array = std.ArrayList(types.Instruction).init(self.allocator);
        try decoder.parseAll(code_buf, &array, self.allocator);
        return .{ .locals = try self.createLocals(localses), .body = try array.toOwnedSlice() };
    }

    fn createLocals(self: *Self, vec: []const Locals) (Error || error{OutOfMemory})![]const types.ValueType {
        var total: u32 = 0;
        for (vec) |v| total += v.size;
        if (total == 0) {
            return &.{};
        }

        const ret = try self.allocator.alloc(types.ValueType, total);
        var p: u32 = 0;
        for (vec) |v| {
            @memset(ret[p .. p + v.size], v.type);
            p += v.size;
        }
        return ret;
    }

    fn name(self: *Self) (Error || error{OutOfMemory})![]const u8 {
        return self.nameWithLength(try self.reader.readVarU32());
    }

    fn nameWithLength(self: *Self, len: u32) (Error || error{OutOfMemory})![]const u8 {
        return try self.readBytesWithCopy(len);
    }

    fn resultType(self: *Self) (Error || error{OutOfMemory})![]const types.ValueType {
        return try self.createArray(types.ValueType, valueType);
    }

    fn locals(self: *Self) Error!Locals {
        const n = try self.reader.readVarU32();
        const t = try self.valueType();
        return Locals{ .size = n, .type = t };
    }

    fn valueType(self: *Self) Error!types.ValueType {
        const byte = try self.reader.readU8();
        return utils.valueTypeFromNum(byte) orelse {
            std.debug.dumpCurrentStackTrace(null);
            std.debug.print("MalformedValueType=[{}]\n", .{byte});
            return Error.MalformedValueType;
        };
    }

    fn refType(self: *Self) Error!types.RefType {
        const byte = try self.reader.readU8();
        return utils.refTypeFromNum(byte) orelse return Error.MalformedRefType;
    }

    fn limits(self: *Self) Error!types.Limits {
        const kind = try self.reader.readU8();
        const min = try self.reader.readVarU32();
        const max = switch (kind) {
            0 => null,
            1 => try self.reader.readVarU32(),
            else => return Error.MalformedLimitId,
        };
        return .{ .min = min, .max = max };
    }

    fn readBytesWithCopy(self: *Self, size: usize) (error{EOF} || error{OutOfMemory})![]const u8 {
        const copied_array = try self.allocator.alloc(u8, size);
        const array = try self.reader.readBytes(size);
        @memcpy(copied_array, array);
        return copied_array;
    }

    fn createArray(self: *Self, comptime T: type, filler: fn (*Self) (Error || error{OutOfMemory})!T) (Error || error{OutOfMemory})![]const T {
        const size = try self.reader.readVarU32();
        const array = try self.allocator.alloc(T, size);

        for (0..size) |i| {
            array[i] = try filler(self);
        }
        return array;
    }
};

const Section = union(enum) {
    custom,
    type: []const types.FuncType,
    import: []const types.Import,
    function: []const types.TypeIdx,
    table: []const types.TableType,
    memory: []const types.MemoryType,
    global: []const types.Global,
    @"export": []const types.Export,
    start: types.FuncIdx,
    element: []const types.Element,
    code: []const ModFunc,
    data: []const types.Data,
    data_count: u32,
};

const ModFunc = struct {
    locals: []const types.ValueType,
    body: []const types.Instruction,
};

const Locals = struct {
    size: u32,
    type: types.ValueType,
};
