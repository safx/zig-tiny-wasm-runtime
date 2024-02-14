const std = @import("std");
const wasm = std.wasm;
const types = @import("./types.zig");

pub const Instruction = union(enum) {
    pub const MemArg = struct {
        @"align": u32,
        offset: u32,
    };

    pub const CallIndirectArg = struct {
        type_idx: TypeIdx,
        table_idx: TableIdx,
    };

    pub const TableInitArg = struct {
        table_idx: TableIdx,
        elem_idx: ElemIdx,
    };

    pub const TableCopyArg = struct {
        table_idx_src: TableIdx,
        table_idx_dst: TableIdx,
    };

    pub const BlockInfo = struct {
        type: BlockType,
        end: InstractionAddr = 0,
    };

    pub const IfBlockInfo = struct {
        type: BlockType,
        @"else": ?InstractionAddr = 0,
        end: InstractionAddr = 0,
    };

    pub const BlockType = union(enum) {
        empty,
        value_type: ValueType,
        type_index: TypeIdx,
    };

    pub const BrTableType = struct {
        label_idxs: []const LabelIdx,
        default_label_idx: LabelIdx,
    };

    const RefType = types.RefType;
    const ValueType = types.ValueType;

    const LabelIdx = types.LabelIdx;
    const FuncIdx = types.FuncIdx;
    const TypeIdx = types.TypeIdx;
    const ElemIdx = types.ElemIdx;
    const DataIdx = types.DataIdx;
    const LocalIdx = types.LocalIdx;
    const TableIdx = types.TableIdx;
    const GlobalIdx = types.GlobalIdx;

    const InstractionAddr = types.InstractionAddr;

    end,
    @"else",

    // contronl instructions
    nop,
    @"unreachable",
    block: BlockInfo,
    loop: BlockInfo,
    @"if": IfBlockInfo,
    br: LabelIdx,
    br_if: LabelIdx,
    br_table: BrTableType,
    @"return",
    call: FuncIdx,
    call_indirect: CallIndirectArg,

    // reference instructions
    ref_null: RefType,
    ref_is_null,
    ref_func: FuncIdx,

    // parametric instructions
    drop,
    select,
    selectv: []const ValueType,

    // variable instructions
    local_get: LocalIdx,
    local_set: LocalIdx,
    local_tee: LocalIdx,
    global_get: GlobalIdx,
    global_set: GlobalIdx,

    // table instructions
    table_get: TableIdx,
    table_set: TableIdx,
    table_init: TableInitArg,
    elem_drop: ElemIdx,
    table_copy: TableCopyArg,
    table_grow: TableIdx,
    table_size: TableIdx,
    table_fill: TableIdx,

    // memory instructions
    i32_load: MemArg,
    i64_load: MemArg,
    f32_load: MemArg,
    f64_load: MemArg,
    i32_load8_s: MemArg,
    i32_load8_u: MemArg,
    i32_load16_s: MemArg,
    i32_load16_u: MemArg,
    i64_load8_s: MemArg,
    i64_load8_u: MemArg,
    i64_load16_s: MemArg,
    i64_load16_u: MemArg,
    i64_load32_s: MemArg,
    i64_load32_u: MemArg,
    i32_store: MemArg,
    i64_store: MemArg,
    f32_store: MemArg,
    f64_store: MemArg,
    i32_store8: MemArg,
    i32_store16: MemArg,
    i64_store8: MemArg,
    i64_store16: MemArg,
    i64_store32: MemArg,
    memory_size,
    memory_grow,
    memory_init: DataIdx,
    data_drop: DataIdx,
    memory_copy,
    memory_fill,

    // numeric instructions (1)
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,

    // numeric instructions (2) i32
    i32_eqz,
    i32_eq,
    i32_ne,
    i32_lt_s,
    i32_lt_u,
    i32_gt_s,
    i32_gt_u,
    i32_le_s,
    i32_le_u,
    i32_ge_s,
    i32_ge_u,

    // numeric instructions (2) i64
    i64_eqz,
    i64_eq,
    i64_ne,
    i64_lt_s,
    i64_lt_u,
    i64_gt_s,
    i64_gt_u,
    i64_le_s,
    i64_le_u,
    i64_ge_s,
    i64_ge_u,

    // numeric instructions (2) f32
    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,

    // numeric instructions (2) f64
    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,

    // numeric instructions (3) i32
    i32_clz,
    i32_ctz,
    i32_popcnt,
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_div_u,
    i32_rem_s,
    i32_rem_u,
    i32_and,
    i32_or,
    i32_xor,
    i32_shl,
    i32_shr_s,
    i32_shr_u,
    i32_rotl,
    i32_rotr,

    // numeric instructions (3) i64
    i64_clz,
    i64_ctz,
    i64_popcnt,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_div_u,
    i64_rem_s,
    i64_rem_u,
    i64_and,
    i64_or,
    i64_xor,
    i64_shl,
    i64_shr_s,
    i64_shr_u,
    i64_rotl,
    i64_rotr,

    // numeric instructions (3) f32
    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copy_sign,

    // numeric instructions (3) f64
    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copy_sign,

    // numeric instructions (4)
    i32_wrap_i64,
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i64_extend_i32_s,
    i64_extend_i32_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f32_demote_f64,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f64_promote_f32,
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,

    // numeric instructions (5)
    i32_extend8_s,
    i32_extend16_s,
    i64_extend8_s,
    i64_extend16_s,
    i64_extend32_s,

    // vector instructions
    misc: wasm.MiscOpcode,
    simd: wasm.SimdOpcode,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |val| if (@TypeOf(val) == void) {
                try writer.print("{s}", .{@tagName(self)});
            } else {
                try writer.print("{s} {any}", .{ @tagName(self), val });
            },
        }
    }
};
