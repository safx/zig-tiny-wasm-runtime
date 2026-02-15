const std = @import("std");
pub const Instruction = @import("./instructions.zig").Instruction;
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
pub const TagIdx = u32;

pub const InstractionAddr = u32;

pub const RefType = wasm.RefType;

pub const ValueType = enum(u8) {
    // numtype
    i32 = @intFromEnum(wasm.Valtype.i32),
    i64 = @intFromEnum(wasm.Valtype.i64),
    f32 = @intFromEnum(wasm.Valtype.f32),
    f64 = @intFromEnum(wasm.Valtype.f64),
    // vectype
    v128 = @intFromEnum(wasm.Valtype.v128),
    // reftype
    func_ref = @intFromEnum(wasm.RefType.funcref),
    extern_ref = @intFromEnum(wasm.RefType.externref),

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const HeapType = union(enum) {
    func_ht,
    extern_ht,
    type_idx: u32,

    pub fn isSubtypeOf(self: HeapType, other: HeapType) bool {
        if (std.meta.eql(self, other)) return true;
        // concrete func type is subtype of abstract func
        if (self == .type_idx and other == .func_ht) return true;
        return false;
    }
};

pub const RefTypeExt = struct {
    heap_type: HeapType,
    nullable: bool,

    /// self <: other ?
    pub fn isSubtypeOf(self: RefTypeExt, other: RefTypeExt) bool {
        // nullable cannot be subtype of non-nullable
        if (!other.nullable and self.nullable) return false;
        return self.heap_type.isSubtypeOf(other.heap_type);
    }

    pub fn eql(self: RefTypeExt, other: RefTypeExt) bool {
        return self.nullable == other.nullable and std.meta.eql(self.heap_type, other.heap_type);
    }
};

pub const FuncType = struct {
    parameter_types: []const ValueType,
    result_types: []const ValueType,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        try writer.writeAll("(");
        for (self.parameter_types, 0..) |ty, i| {
            _ = try writer.print("{f}{s}", .{ ty, if (i + 1 < self.parameter_types.len) ", " else "" });
        }
        try writer.writeAll(") -> (");
        for (self.result_types, 0..) |ty, i| {
            _ = try writer.print("{f}{s}", .{ ty, if (i + 1 < self.result_types.len) ", " else "" });
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

    global_get: GlobalIdx,

    // multiple instructions (for extended constant expressions)
    instructions: []const Instruction,

    // pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
    //     switch (self) {
    //         inline else => |val| try writer.print("{}", .{val}),
    //     }
    // }
};

pub const Limits = struct {
    min: u32,
    max: ?u32,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("[{d},{d?}]", .{ self.min, self.max });
    }
};

pub const Import = struct {
    module_name: []const u8,
    name: []const u8,
    desc: ImportDesc,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("{s}.{s} {f}", .{ self.module_name, self.name, self.desc });
    }
};

pub const Export = struct {
    name: []const u8,
    desc: ExportDesc,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("{s} ({f})", .{ self.name, self.desc });
    }
};

pub const ImportDesc = union(enum) {
    function: TypeIdx,
    table: TableType,
    memory: MemoryType,
    global: GlobalType,
    tag: TypeIdx,
};

pub const ExportDesc = union(enum) {
    function: FuncIdx,
    table: TableIdx,
    memory: MemIdx,
    global: GlobalIdx,
    tag: TagIdx,
};

pub const TableType = struct {
    limits: Limits,
    ref_type: RefType,
    is_64: bool = false,
    ref_type_ext: ?RefTypeExt = null,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("{f}:{f}", .{ self.limits, self.ref_type });
    }
};

pub const GlobalType = struct {
    value_type: ValueType,
    mutability: Mutability,
    ref_type_ext: ?RefTypeExt = null,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("{f}:{f}", .{ self.mutability, self.value_type });
    }
};

pub const MemoryType = struct {
    limits: Limits,
    is_64: bool = false,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        _ = try writer.print("{f}", .{self.limits});
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

pub const Tag = struct {
    type_idx: TypeIdx,
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
    tags: []const Tag = &.{},
    start: ?FuncIdx,
    imports: []const Import,
    exports: []const Export,

    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: @This()) void {
        self.arena.deinit();
    }
};
