const std = @import("std");
const wa = @import("wasm-core");
const BinaryReader = @import("./binary_reader.zig").BinaryReader;
const Decoder = @import("./decoder.zig").Decoder;
const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

pub const ModuleLoader = struct {
    const Self = @This();

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

    pub fn parseAll(self: *Self, buffer: []const u8) (Error || error{OutOfMemory})!wa.Module {
        var arena = std.heap.ArenaAllocator.init(self.baseAllocator);
        self.allocator = arena.allocator();

        self.reader = BinaryReader.new(buffer);
        const version = try self.readHeaderVersion();
        if (version != 1) {
            return Error.UnknownBinaryVersion;
        }

        var types_: []const wa.FuncType = &.{};
        var imports: []const wa.Import = &.{};
        var func_idxs: []const wa.FuncIdx = &.{};
        var tables: []const wa.TableType = &.{};
        var memories: []const wa.MemoryType = &.{};
        var globals: []const wa.Global = &.{};
        var exports: []const wa.Export = &.{};
        var start: ?wa.FuncIdx = null;
        var elements: []const wa.Element = &.{};
        var codes: []const ModFunc = &.{};
        var datas: []const wa.Data = &.{};
        var data_count: u32 = 0;

        while (self.section()) |sec| {
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

        const funcs = try self.allocator.alloc(wa.Func, func_idxs.len);
        for (func_idxs, codes, 0..) |func_idx, f, i| {
            funcs[i] = wa.Func{
                .type = func_idx,
                .locals = f.locals,
                .body = f.body,
            };
        }

        return wa.Module{
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
            .type => return .{ .type = try self.createArray(wa.FuncType, funcType) },
            .import => return .{ .import = try self.createArray(wa.Import, import) },
            .function => return .{ .function = try self.createArray(wa.FuncIdx, funcidx) },
            .table => return .{ .table = try self.createArray(wa.TableType, table) },
            .memory => return .{ .memory = try self.createArray(wa.MemoryType, memtype) },
            .global => return .{ .global = try self.createArray(wa.Global, global) },
            .@"export" => return .{ .@"export" = try self.createArray(wa.Export, @"export") },
            .start => return .{ .start = try self.reader.readVarU32() },
            .element => return .{ .element = try self.createArray(wa.Element, elem) },
            .code => return .{ .code = try self.createArray(ModFunc, code) },
            .data => return .{ .data = try self.createArray(wa.Data, data) },
            .data_count => return .{ .data_count = try self.reader.readVarU32() },
            _ => return Error.MalformedSectionId,
        }
    }

    // section
    fn funcType(self: *Self) (Error || error{OutOfMemory})!wa.FuncType {
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

    fn import(self: *Self) (Error || error{OutOfMemory})!wa.Import {
        const mod = try self.name();
        const nm = try self.name();
        const desc = try self.importdesc();
        return .{ .module_name = mod, .name = nm, .desc = desc };
    }

    fn funcidx(self: *Self) Error!wa.FuncIdx {
        return self.reader.readVarU32();
    }

    fn table(self: *Self) Error!wa.TableType {
        const rtype = try self.refType();
        const limit = try self.limits();
        return .{ .limit = limit, .ref_type = rtype };
    }

    fn memtype(self: *Self) Error!wa.MemoryType {
        return .{ .limits = try self.limits() };
    }

    fn global(self: *Self) Error!wa.Global {
        const gtype = try self.globalType();
        const exp = try self.initExpr();
        return .{ .type = gtype, .init = exp };
    }

    fn @"export"(self: *Self) Error!wa.Export {
        const nm = try self.name();
        const d = try self.exportdesc();
        return .{ .name = nm, .desc = d };
    }

    fn elem(self: *Self) (Error || error{OutOfMemory})!wa.Element {
        const kind = try self.reader.readVarU32();
        switch (kind) {
            0 => {
                const expr = try self.initExpr();
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(wa.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = wa.InitExpression{ .ref_func = y };
                }
                const mode = wa.ElementMode{ .active = .{ .table_idx = 0, .offset = expr } };
                return .{ .type = .funcref, .init = init_array, .mode = mode };
            },
            1 => {
                const et = try self.reader.readU8();
                _ = et; // == 0x00
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(wa.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = wa.InitExpression{ .ref_func = y };
                }
                return .{ .type = .funcref, .init = init_array, .mode = .passive };
            },
            2 => {
                const x = try self.reader.readVarU32();
                const expr = try self.initExpr();
                const et = try self.reader.readU8();
                _ = et; // == 0x00
                const ys = try self.funcidxs();
                const init_array = try self.allocator.alloc(wa.InitExpression, ys.len);
                for (ys, 0..) |y, i| {
                    init_array[i] = wa.InitExpression{ .ref_func = y };
                }
                const mode = wa.ElementMode{ .active = .{ .table_idx = x, .offset = expr } };
                return .{ .type = .funcref, .init = init_array, .mode = mode };
            },
            3...7 => std.debug.print("** {}\n", .{kind}), // TODO
            else => unreachable,
        }
        unreachable;
    }

    fn code(self: *Self) (Error || error{OutOfMemory})!ModFunc {
        return try self.func();
    }

    fn data(self: *Self) (Error || error{OutOfMemory})!wa.Data {
        const kind = try self.reader.readVarU32();
        switch (kind) {
            0 => {
                const exp = try self.initExpr();
                const len = try self.reader.readVarU32();
                const init_array = try self.reader.readBytes(len);
                const mode = wa.DataMode{ .active = .{ .mem_idx = 0, .offset = exp } };
                return .{ .init = init_array, .mode = mode };
            },
            1...7 => unreachable, // TODO
            else => unreachable,
        }
        unreachable;
    }

    fn skip(self: *Self, size: u32) Error!void {
        _ = try self.reader.readBytes(size);
    }

    // types

    fn importdesc(self: *Self) Error!wa.ImportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0x00 => .{ .func = try self.reader.readVarU32() },
            0x01 => .{ .table = try self.table() },
            0x02 => .{ .memory = try self.memtype() },
            0x03 => .{ .global = try self.globalType() },
            else => return Error.MalformedImportKind,
        };
    }

    fn exportdesc(self: *Self) Error!wa.ExportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0x00 => .{ .func = try self.reader.readVarU32() },
            0x01 => .{ .table = try self.reader.readVarU32() },
            0x02 => .{ .memory = try self.reader.readVarU32() },
            0x03 => .{ .global = try self.reader.readVarU32() },
            else => return Error.MalformedExportKind,
        };
    }

    fn funcidxs(self: *Self) (Error || error{OutOfMemory})![]const wa.FuncIdx {
        return try self.createArray(wa.FuncIdx, funcidx);
    }

    fn globalType(self: *Self) Error!wa.GlobalType {
        const vtype = try self.valueType();
        const m = try self.mut();
        return .{ .mutability = m, .value_type = vtype };
    }

    fn initExpr(self: *Self) Error!wa.InitExpression {
        const op = try self.reader.readU8();
        const value = try self.initExprValue(op);
        const end = try self.reader.readU8();
        _ = end; // TODO check
        return value;
    }

    fn initExprValue(self: *Self, op: u8) Error!wa.InitExpression {
        const n = std.wasm.opcode;
        return switch (op) {
            n(.i32_const) => .{ .i32_const = try self.reader.readVarI32() },
            n(.i64_const) => .{ .i64_const = try self.reader.readVarI64() },
            n(.f32_const) => .{ .f32_const = try self.reader.readF32() },
            n(.f64_const) => .{ .f64_const = try self.reader.readF64() },
            else => unreachable,
        };
    }

    fn mut(self: *Self) Error!wa.Mutability {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0x00 => .immutable,
            0x01 => .mutable,
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
        var array = std.ArrayList(wa.Instruction).init(self.allocator);
        try decoder.parseAll(code_buf, &array, self.allocator);
        return .{ .locals = try self.createLocals(localses), .body = try array.toOwnedSlice() };
    }

    fn createLocals(self: *Self, vec: []const Locals) (Error || error{OutOfMemory})![]const wa.ValueType {
        var total: u32 = 0;
        for (vec) |v| total += v.size;
        if (total == 0) {
            return &.{};
        }

        const ret = try self.allocator.alloc(wa.ValueType, total);
        var p: u32 = 0;
        for (vec) |v| {
            @memset(ret[p .. p + v.size], v.type);
            p += v.size;
        }
        return ret;
    }

    fn name(self: *Self) Error![]const u8 {
        return self.nameWithLength(try self.reader.readVarU32());
    }

    fn nameWithLength(self: *Self, len: u32) Error![]const u8 {
        return try self.reader.readBytes(len);
    }

    fn resultType(self: *Self) (Error || error{OutOfMemory})![]const wa.ValueType {
        return try self.createArray(wa.ValueType, valueType);
    }

    fn locals(self: *Self) Error!Locals {
        const n = try self.reader.readVarU32();
        const t = try self.valueType();
        return Locals{ .size = n, .type = t };
    }

    fn valueType(self: *Self) Error!wa.ValueType {
        const byte = try self.reader.readU8();
        return utils.valueTypeFromNum(byte) orelse {
            std.debug.dumpCurrentStackTrace(null);
            std.debug.print("MalformedValueType=[{}]\n", .{byte});
            return Error.MalformedValueType;
        };
    }

    fn refType(self: *Self) Error!wa.RefType {
        const byte = try self.reader.readU8();
        return utils.refTypeFromNum(byte) orelse return Error.MalformedRefType;
    }

    fn limits(self: *Self) Error!wa.Limits {
        const kind = try self.reader.readU8();
        const min = try self.reader.readVarU32();
        const max = switch (kind) {
            0 => null,
            1 => try self.reader.readVarU32(),
            else => return Error.MalformedLimitId,
        };
        return .{ .min = min, .max = max };
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
    type: []const wa.FuncType,
    import: []const wa.Import,
    function: []const wa.TypeIdx,
    table: []const wa.TableType,
    memory: []const wa.MemoryType,
    global: []const wa.Global,
    @"export": []const wa.Export,
    start: wa.FuncIdx,
    element: []const wa.Element,
    code: []const ModFunc,
    data: []const wa.Data,
    data_count: u32,
};

const ModFunc = struct {
    locals: []const wa.ValueType,
    body: []const wa.Instruction,
};

const Locals = struct {
    size: u32,
    type: wa.ValueType,
};
