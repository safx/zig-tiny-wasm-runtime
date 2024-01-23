const std = @import("std");
const Instruction = @import("./instructions.zig").Instruction;
const wasm = std.wasm;

pub const TypeIdx = u32;
pub const FuncIdx = u32;
pub const TableIdx = u32;
pub const MemIdx = u32;
pub const GlobalIdx = u32;
pub const ElemIdx = u32;
pub const DataIdx = u32;
pub const LocalIdx = u32;
pub const LabelIdx = u32;

pub const InstractionAddr = u32;

pub const RefType = wasm.RefType;

pub const ValueType = enum(u8) {
    const valtype = wasm.valtype;
    const reftype = wasm.reftype;

    // numtype
    i32 = valtype(wasm.Valtype.i32),
    i64 = valtype(wasm.Valtype.i64),
    f32 = valtype(wasm.Valtype.f32),
    f64 = valtype(wasm.Valtype.f64),
    // vectype
    v128 = valtype(wasm.Valtype.v128),
    // reftype
    func_ref = reftype(wasm.RefType.funcref),
    extern_ref = reftype(wasm.RefType.externref),
};

pub const FuncType = struct {
    parameter_types: []const ValueType,
    result_types: []const ValueType,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("(");
        for (self.parameter_types, 0..) |ty, i| {
            _ = try writer.print("{}{s}", .{ ty, if (i + 1 < self.parameter_types.len) ", " else "" });
        }
        try writer.writeAll(") -> (");
        for (self.result_types, 0..) |ty, i| {
            _ = try writer.print("{}{s}", .{ ty, if (i + 1 < self.result_types.len) ", " else "" });
        }
        try writer.writeAll(")");
    }
};

pub const InitExpression = union(enum) {
    // num
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,
    // vec
    v128_const: i128,
    // ref
    ref_null: RefType,
    ref_func: FuncIdx,

    // pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    //     switch (self) {
    //         inline else => |val| try writer.print("{}", .{val}),
    //     }
    // }
};

pub const Limits = struct {
    min: u32,
    max: ?u32,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("[{},{?}]", .{ self.min, self.max });
    }
};

pub const Import = struct {
    module_name: []const u8,
    name: []const u8,
    desc: ImportDesc,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s}.{s} {}", .{ self.module_name, self.name, self.desc });
    }
};

pub const Export = struct {
    name: []const u8,
    desc: ExportDesc,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s} ({})", .{ self.name, self.desc });
    }
};

pub const ImportDesc = union(enum) {
    func: TypeIdx,
    table: TableType,
    memory: MemoryType,
    global: GlobalType,
};

pub const ExportDesc = union(enum) {
    func: FuncIdx,
    table: TableIdx,
    memory: MemIdx,
    global: GlobalIdx,
};

pub const TableType = struct {
    limit: Limits,
    ref_type: RefType,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}:{}", .{ self.limit, self.ref_type });
    }
};

pub const GlobalType = struct {
    value_type: ValueType,
    mutability: Mutability,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}:{}", .{ self.mutability, self.value_type });
    }
};

pub const MemoryType = struct {
    limits: Limits,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}", .{self.limits});
    }
};

pub const Global = struct {
    type: GlobalType,
    init: InitExpression,
};

pub const Func = struct {
    type: TypeIdx,
    locals: []const ValueType,
    body: []const Instruction,
};

pub const Element = struct {
    type: RefType,
    init: []const InitExpression,
    mode: ElementMode,
};

pub const ElementMode = union(enum) {
    passive,
    active: ElementActiveType,
    declarative,
};

pub const ElementActiveType = struct {
    table_idx: TableIdx,
    offset: InitExpression,
};

pub const Data = struct {
    init: []const u8,
    mode: DataMode,
};

pub const DataMode = union(enum) {
    passive,
    active: DataActiveType,
};

pub const DataActiveType = struct {
    mem_idx: MemIdx,
    offset: InitExpression,
};

pub const Mutability = enum(u8) {
    immutable,
    mutable,
};

pub const Module = struct {
    types: []const FuncType,
    funcs: []const Func,
    tables: []const TableType,
    memories: []const MemoryType,
    globals: []const Global,
    elements: []const Element,
    datas: []const Data,
    start: ?FuncIdx,
    imports: []const Import,
    exports: []const Export,

    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: @This()) void {
        self.arena.deinit();
    }
};
