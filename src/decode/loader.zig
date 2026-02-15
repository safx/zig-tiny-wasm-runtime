const std = @import("std");
const types = @import("wasm-core").types;
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
        if (version != 1)
            return Error.UnknownBinaryVersion;

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
        var data_count: ?u32 = null;

        var tags: []const types.Tag = &.{};

        var sections: [14]bool = .{false} ** 14;
        var last_section_id: u8 = 0;
        while (!self.reader.eof()) {
            const maybe_sec = try self.section(&tags);
            const sec = maybe_sec orelse continue; // tag section handled inline
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

            if (sec != .custom) {
                const current_section: u8 = @intFromEnum(sec);
                if (sections[current_section] or current_section < last_section_id)
                    return Error.UnexpectedContentAfterLastSection;

                sections[current_section] = true;
                last_section_id = current_section;
            }
        }

        if (data_count) |count| {
            if (count != datas.len)
                return Error.DataCountAndDataSectionHaveInconsistentLengths;
        } else {
            // no dataidx operation found in code
            for (codes) |c| {
                for (c.body) |op| {
                    if (op == .memory_init or op == .data_drop)
                        return Error.DataCountSectionRequired;
                }
            }
        }

        if (func_idxs.len != codes.len)
            return Error.FunctionAndCodeSectionHaveInconsistentLengths;

        const funcs = try self.allocator.alloc(types.Func, func_idxs.len); // freed by arena allocator in Module
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
            .tags = tags,
            .start = start,
            .imports = imports,
            .exports = exports,

            .arena = arena,
        };
    }

    pub fn readHeaderVersion(self: *Self) Error!u32 {
        const magic_number = try self.reader.readBytes(4);
        if (!std.mem.eql(u8, magic_number, &std.wasm.magic))
            return Error.MagicHeaderNotDetected;

        return self.reader.readU32();
    }

    fn section(self: *Self, tags_out: *[]const types.Tag) (Error || error{OutOfMemory})!?Section {
        const section_id = try self.reader.readU8();
        const size = try self.reader.readVarU32();
        const start_pos = self.reader.position;

        // Handle tag section (ID 13) specially — not in std.wasm.Section enum
        if (section_id == utils.TAG_SECTION_ID) {
            tags_out.* = try self.createArray(types.Tag, tag);
            const pos = self.reader.position;
            if (pos - start_pos != size)
                return Error.SectionSizeMismatch;
            return null; // handled separately
        }

        const sect = utils.sectionFromNum(section_id) orelse return Error.MalformedSectionId;
        const sec = try self.sectionInternal(sect, size);
        const pos = self.reader.position;
        if (pos - start_pos != size)
            return Error.SectionSizeMismatch;

        return sec;
    }

    fn sectionInternal(self: *Self, sect: std.wasm.Section, size: u32) (Error || error{OutOfMemory})!Section {
        return switch (sect) {
            .custom => .{ .custom = try self.custom(size) },
            .type => .{ .type = try self.createArray(types.FuncType, funcType) },
            .import => .{ .import = try self.createArray(types.Import, import) },
            .function => .{ .function = try self.createArray(types.FuncIdx, funcidx) },
            .table => .{ .table = try self.createArray(types.TableType, table) },
            .memory => .{ .memory = try self.createArray(types.MemoryType, memtype) },
            .global => .{ .global = try self.createArray(types.Global, global) },
            .@"export" => .{ .@"export" = try self.createArray(types.Export, @"export") },
            .start => .{ .start = try self.reader.readVarU32() },
            .element => .{ .element = try self.createArray(types.Element, elem) },
            .code => .{ .code = try self.createArray(ModFunc, code) },
            .data => .{ .data = try self.createArray(types.Data, data) },
            .data_count => .{ .data_count = try self.reader.readVarU32() },
            else => Error.MalformedSectionId,
        };
    }

    // sections

    fn funcType(self: *Self) (Error || error{OutOfMemory})!types.FuncType {
        const marker = try self.reader.readU8();
        if (marker != std.wasm.function_type)
            return Error.MalformedFunctypeMagicNumber;

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

        if (!std.unicode.utf8ValidateSlice(mod) or !std.unicode.utf8ValidateSlice(nm))
            return Error.MalformedUtf8Encoding;

        const desc = try self.importdesc();
        return .{ .module_name = mod, .name = nm, .desc = desc };
    }

    fn funcidx(self: *Self) Error!types.FuncIdx {
        return self.reader.readVarU32();
    }

    fn table(self: *Self) Error!types.TableType {
        // Peek byte to check for ref type encoding (0x63/0x64)
        const peek_pos = self.reader.position;
        const peek_byte = try self.reader.readU8();
        self.reader.position = peek_pos;

        var table_ref_ext: ?types.RefTypeExt = null;
        const rtype = if (peek_byte == 0x63 or peek_byte == 0x64) blk: {
            const result = try self.readRefTypeExt();
            table_ref_ext = result.ext;
            break :blk result.ref_type;
        } else try self.refType();
        // Peek at the flags byte to detect table64 (bit 2 = 0x04)
        const flags_pos = self.reader.position;
        const flags = try self.reader.readU8();
        const is_64 = (flags & 0x04) != 0;
        self.reader.position = flags_pos;
        const limit = try self.limits();
        return .{ .limits = limit, .ref_type = rtype, .is_64 = is_64, .ref_type_ext = table_ref_ext };
    }

    fn memtype(self: *Self) Error!types.MemoryType {
        // Peek at the flags byte to detect memory64 (bit 2 = 0x04)
        const flags_pos = self.reader.position;
        const flags = try self.reader.readU8();
        const is_64 = (flags & 0x04) != 0;
        // Reset position so limits() can read the flags byte
        self.reader.position = flags_pos;
        return .{ .limits = try self.limits(), .is_64 = is_64 };
    }

    fn global(self: *Self) (Error || error{OutOfMemory})!types.Global {
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
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len); // freed by arena allocator in Module
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
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len); // freed by arena allocator in Module
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
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len); // freed by arena allocator in Module
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
                const init_array = try self.allocator.alloc(types.InitExpression, ys.len); // freed by arena allocator in Module
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
    }

    fn code(self: *Self) (Error || error{OutOfMemory})!ModFunc {
        return try self.func();
    }

    fn data(self: *Self) (Error || error{OutOfMemory})!types.Data {
        const kind = try self.reader.readVarU32();
        const dupe = std.mem.Allocator.dupe;
        switch (kind) {
            0 => {
                const exp = try self.initExpr();
                const len = try self.reader.readVarU32();
                const init_array = try dupe(self.allocator, u8, try self.reader.readBytes(len));
                const mode = types.DataMode{ .active = .{ .mem_idx = 0, .offset = exp } };
                return .{ .init = init_array, .mode = mode };
            },
            1 => {
                const len = try self.reader.readVarU32();
                const init_array = try dupe(self.allocator, u8, try self.reader.readBytes(len));
                return .{ .init = init_array, .mode = .passive };
            },
            2 => {
                const x = try self.reader.readVarU32();
                const exp = try self.initExpr();
                const len = try self.reader.readVarU32();
                const init_array = try dupe(self.allocator, u8, try self.reader.readBytes(len));
                const mode = types.DataMode{ .active = .{ .mem_idx = x, .offset = exp } };
                return .{ .init = init_array, .mode = mode };
            },
            else => unreachable,
        }
    }

    fn custom(self: *Self, size: u32) (Error || error{OutOfMemory})!CustomInfo {
        const start = self.reader.position;
        const cname = try self.name();
        const end = self.reader.position;
        const consumed = end - start;
        if (size < consumed)
            return Error.UnexpectedEndOfBuffer;

        if (!std.unicode.utf8ValidateSlice(cname))
            return Error.MalformedUtf8Encoding;

        const len = size - consumed;
        const cdata = try self.reader.readBytes(len);
        return .{ .name = cname, .data = cdata };
    }

    fn skip(self: *Self, size: u32) Error!void {
        _ = try self.reader.readBytes(size);
    }

    // types

    fn tag(self: *Self) Error!types.Tag {
        const attr = try self.reader.readU8();
        if (attr != 0) return Error.MalformedSectionId; // attribute must be 0 (exception)
        const type_idx = try self.reader.readVarU32();
        return .{ .type_idx = type_idx };
    }

    fn importdesc(self: *Self) Error!types.ImportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0 => .{ .function = try self.reader.readVarU32() },
            1 => .{ .table = try self.table() },
            2 => .{ .memory = try self.memtype() },
            3 => .{ .global = try self.globalType() },
            4 => blk: {
                const attr = try self.reader.readU8();
                _ = attr; // attribute byte (must be 0)
                break :blk .{ .tag = try self.reader.readVarU32() };
            },
            else => return Error.MalformedImportKind,
        };
    }

    fn exportdesc(self: *Self) Error!types.ExportDesc {
        const kind = try self.reader.readU8();
        return switch (kind) {
            0 => .{ .function = try self.reader.readVarU32() },
            1 => .{ .table = try self.reader.readVarU32() },
            2 => .{ .memory = try self.reader.readVarU32() },
            3 => .{ .global = try self.reader.readVarU32() },
            4 => .{ .tag = try self.reader.readVarU32() },
            else => return Error.MalformedExportKind,
        };
    }

    fn funcidxs(self: *Self) (Error || error{OutOfMemory})![]const types.FuncIdx {
        return try self.createArray(types.FuncIdx, funcidx);
    }

    fn globalType(self: *Self) Error!types.GlobalType {
        // Peek byte to check for ref type encoding (0x63/0x64)
        const peek_pos = self.reader.position;
        const peek_byte = try self.reader.readU8();
        self.reader.position = peek_pos;

        var ref_ext: ?types.RefTypeExt = null;
        const vtype = if (peek_byte == 0x63 or peek_byte == 0x64) blk: {
            const result = try self.readRefTypeExt();
            ref_ext = result.ext;
            break :blk result.value_type;
        } else try self.valueType();
        const m = try self.mut();
        return .{ .mutability = m, .value_type = vtype, .ref_type_ext = ref_ext };
    }

    fn initExpr(self: *Self) (Error || error{OutOfMemory})!types.InitExpression {
        const start_pos = self.reader.position;
        const op = try self.reader.readU8();
        
        // Check if this is a simple single-instruction constant expression
        const is_simple = switch (op) {
            opcode2int(.i32_const),
            opcode2int(.i64_const),
            opcode2int(.f32_const),
            opcode2int(.f64_const),
            opcode2int(.global_get),
            0xd0, // ref.null
            0xd2, // ref.func
            opcode2int(.simd_prefix),
            => true,
            else => false,
        };
        
        if (is_simple) {
            const value = try self.initExprValue(op);
            const end = try self.reader.readU8();
            if (end != 0x0b)
                return Error.EndOpcodeExpected;
            return value;
        }
        
        // Complex expression with multiple instructions
        self.reader.position = start_pos;
        const code_start = self.reader.position;
        
        // Find the end opcode
        var depth: u32 = 0;
        while (true) {
            const byte = try self.reader.readU8();
            if (byte == 0x0b) { // end
                if (depth == 0) break;
                depth -= 1;
            } else if (byte == 0x02 or byte == 0x03 or byte == 0x04) { // block/loop/if
                depth += 1;
            }
        }
        
        const code_end = self.reader.position - 1; // exclude end opcode
        const code_len = code_end - code_start;
        
        self.reader.position = code_start;
        const code_buf = try self.reader.readBytes(code_len);
        _ = try self.reader.readU8(); // consume end opcode
        
        var decoder = Decoder.new();
        const instrs = try decoder.parseAll(code_buf, self.allocator);
        
        return .{ .instructions = instrs };
    }

    fn opcode2int(op: std.wasm.Opcode) u32 {
        return @intFromEnum(op);
    }

    fn simdOpcode2int(op: std.wasm.SimdOpcode) u32 {
        return @intFromEnum(op);
    }

    fn initExprValue(self: *Self, op: u8) Error!types.InitExpression {
        const n = opcode2int;
        return switch (op) {
            n(.i32_const) => .{ .i32_const = try self.reader.readVarI32() },
            n(.i64_const) => .{ .i64_const = try self.reader.readVarI64() },
            n(.f32_const) => .{ .f32_const = try self.reader.readF32() },
            n(.f64_const) => .{ .f64_const = try self.reader.readF64() },
            n(.global_get) => .{ .global_get = try self.reader.readVarU32() },
            0xd0 => .{ .ref_null = @enumFromInt(try self.reader.readU8()) },
            0xd2 => .{ .ref_func = try self.reader.readVarU32() },

            n(.simd_prefix) => {
                const s = simdOpcode2int;
                const op2 = try self.reader.readVarU32();
                return switch (op2) {
                    s(.v128_const) => .{ .v128_const = try self.reader.readI128() },
                    else => Error.IllegalOpcode,
                };
            },

            else => return Error.IllegalOpcode,
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
        const body = try decoder.parseAll(code_buf, self.allocator);
        return .{ .locals = try self.createLocals(localses), .body = body };
    }

    fn createLocals(self: *Self, vec: []const Locals) (Error || error{OutOfMemory})![]const types.ValueType {
        var total: u32 = 0;
        for (vec) |v| {
            const r, const overflow = @addWithOverflow(total, v.size);
            if (overflow == 1)
                return Error.TooManyLocals;
            total = r;
        }
        if (total == 0) {
            return &.{};
        }

        const ret = try self.allocator.alloc(types.ValueType, total); // freed by arena allocator in Module
        var p: u32 = 0;
        for (vec) |v| {
            @memset(ret[p .. p + v.size], v.type);
            p += v.size;
        }
        return ret;
    }

    fn name(self: *Self) (Error || error{OutOfMemory})![]const u8 {
        const len = try self.reader.readVarU32();
        const array = try self.reader.readBytes(len);
        return std.mem.Allocator.dupe(self.allocator, u8, array);
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

    const ReadRefTypeExtResult = struct {
        ref_type: types.RefType,
        value_type: types.ValueType,
        ext: types.RefTypeExt,
    };

    fn readRefTypeExt(self: *Self) Error!ReadRefTypeExtResult {
        const byte = try self.reader.readU8();
        switch (byte) {
            0x63, 0x64 => {
                const nullable = byte == 0x64;
                // Read heap type as signed LEB128 (i33 spec, use i64)
                const ht_byte = try self.reader.readVarI64();
                const heap_type: types.HeapType = if (ht_byte < 0) switch (ht_byte) {
                    -0x10 => .func_ht,
                    -0x11 => .extern_ht,
                    else => .func_ht,
                } else .{ .type_idx = @intCast(ht_byte) };
                const base_ref: types.RefType = switch (heap_type) {
                    .extern_ht => .externref,
                    else => .funcref,
                };
                const base_vt: types.ValueType = switch (heap_type) {
                    .extern_ht => .extern_ref,
                    else => .func_ref,
                };
                return .{
                    .ref_type = base_ref,
                    .value_type = base_vt,
                    .ext = .{ .heap_type = heap_type, .nullable = nullable },
                };
            },
            else => return Error.MalformedRefType,
        }
    }

    fn limits(self: *Self) Error!types.Limits {
        const kind = try self.reader.readU8();
        const has_max = (kind & 0x01) != 0;
        // Accept flags 0x00, 0x01 (standard), 0x04, 0x05 (memory64)
        if (kind & ~@as(u8, 0x05) != 0)
            return Error.MalformedLimitId;
        const min = try self.reader.readVarU32();
        const max: ?u32 = if (has_max) try self.reader.readVarU32() else null;
        return .{ .min = min, .max = max };
    }

    fn createArray(self: *Self, comptime T: type, filler: fn (*Self) (Error || error{OutOfMemory})!T) (Error || error{OutOfMemory})![]const T {
        const size = try self.reader.readVarU32();
        const array = try self.allocator.alloc(T, size); // freed by arena allocator in Module

        for (0..size) |i| {
            array[i] = filler(self) catch |err| {
                return if (err == Error.UnexpectedEndOfBuffer) Error.UnexpectedEndOfSectionOrFunction else err;
            };
        }
        return array;
    }
};

const Section = union(std.wasm.Section) {
    custom: CustomInfo,
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

const CustomInfo = struct {
    name: []const u8,
    data: []const u8,
};

const ModFunc = struct {
    locals: []const types.ValueType,
    body: []const types.Instruction,
};

const Locals = struct {
    size: u32,
    type: types.ValueType,
};
