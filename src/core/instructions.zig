const std = @import("std");
const wasm = std.wasm;
const types = @import("./types.zig");

pub const Instruction = union(enum) {
    pub const MemArg = struct {
        @"align": u32,
        offset: u64,
        mem_idx: u32 = 0,
    };

    pub const MemArgWithLaneIdx = struct {
        @"align": u32,
        offset: u64,
        lane_idx: LaneIdx,
        mem_idx: u32 = 0,
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

    pub const MemoryInitArg = struct {
        data_idx: DataIdx,
        mem_idx: MemIdx,
    };

    pub const MemoryCopyArg = struct {
        mem_idx_src: MemIdx,
        mem_idx_dst: MemIdx,
    };

    pub const BlockInfo = struct {
        type: BlockType,
        end: InstractionAddr = 0,
    };

    pub const IfBlockInfo = struct {
        type: BlockType,
        @"else": ?InstractionAddr = null,
        end: InstractionAddr = 0,
    };

    pub const BlockType = union(enum) {
        empty,
        value_type: ValueType,
        type_index: TypeIdx,

        pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
            switch (self) {
                inline else => |val| if (@TypeOf(val) == void) {
                    try writer.print("{s}", .{@tagName(self)});
                } else {
                    try writer.print("{s}({f})", .{ @tagName(self), val });
                },
            }
        }
    };

    pub const BrTableType = struct {
        label_idxs: []const LabelIdx,
        default_label_idx: LabelIdx,
    };

    const RefType = types.RefType;
    const ValueType = types.ValueType;

    pub const LaneIdx = u8;
    pub const LaneIdx16 = [16]u8;
    const LabelIdx = types.LabelIdx;
    const FuncIdx = types.FuncIdx;
    const TypeIdx = types.TypeIdx;
    const ElemIdx = types.ElemIdx;
    const DataIdx = types.DataIdx;
    const LocalIdx = types.LocalIdx;
    const TableIdx = types.TableIdx;
    const MemIdx = types.MemIdx;
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
    return_call: FuncIdx,
    return_call_indirect: CallIndirectArg,

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
    memory_size: MemIdx,
    memory_grow: MemIdx,
    memory_init: MemoryInitArg,
    data_drop: DataIdx,
    memory_copy: MemoryCopyArg,
    memory_fill: MemIdx,

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

    // saturating truncation instructions
    i32_trunc_sat_f32_s,
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f64_s,
    i32_trunc_sat_f64_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f64_s,
    i64_trunc_sat_f64_u,

    // SIMD instructions
    v128_load: MemArg,
    v128_load8x8_s: MemArg,
    v128_load8x8_u: MemArg,
    v128_load16x4_s: MemArg,
    v128_load16x4_u: MemArg,
    v128_load32x2_s: MemArg,
    v128_load32x2_u: MemArg,
    v128_load8_splat: MemArg,
    v128_load16_splat: MemArg,
    v128_load32_splat: MemArg,
    v128_load64_splat: MemArg,
    v128_store: MemArg,
    v128_const: i128,
    i8x16_shuffle: LaneIdx16,
    i8x16_swizzle,
    i8x16_splat,
    i16x8_splat,
    i32x4_splat,
    i64x2_splat,
    f32x4_splat,
    f64x2_splat,
    i8x16_extract_lane_s: LaneIdx,
    i8x16_extract_lane_u: LaneIdx,
    i8x16_replace_lane: LaneIdx,
    i16x8_extract_lane_s: LaneIdx,
    i16x8_extract_lane_u: LaneIdx,
    i16x8_replace_lane: LaneIdx,
    i32x4_extract_lane: LaneIdx,
    i32x4_replace_lane: LaneIdx,
    i64x2_extract_lane: LaneIdx,
    i64x2_replace_lane: LaneIdx,
    f32x4_extract_lane: LaneIdx,
    f32x4_replace_lane: LaneIdx,
    f64x2_extract_lane: LaneIdx,
    f64x2_replace_lane: LaneIdx,
    i8x16_eq,
    i16x8_eq,
    i32x4_eq,
    i8x16_ne,
    i16x8_ne,
    i32x4_ne,
    i8x16_lt_s,
    i16x8_lt_s,
    i32x4_lt_s,
    i8x16_lt_u,
    i16x8_lt_u,
    i32x4_lt_u,
    i8x16_gt_s,
    i16x8_gt_s,
    i32x4_gt_s,
    i8x16_gt_u,
    i16x8_gt_u,
    i32x4_gt_u,
    i8x16_le_s,
    i16x8_le_s,
    i32x4_le_s,
    i8x16_le_u,
    i16x8_le_u,
    i32x4_le_u,
    i8x16_ge_s,
    i16x8_ge_s,
    i32x4_ge_s,
    i8x16_ge_u,
    i16x8_ge_u,
    i32x4_ge_u,
    f32x4_eq,
    f64x2_eq,
    f32x4_ne,
    f64x2_ne,
    f32x4_lt,
    f64x2_lt,
    f32x4_gt,
    f64x2_gt,
    f32x4_le,
    f64x2_le,
    f32x4_ge,
    f64x2_ge,
    v128_not,
    v128_and,
    v128_andnot,
    v128_or,
    v128_xor,
    v128_bitselect,
    v128_any_true,
    v128_load8_lane: MemArgWithLaneIdx,
    v128_load16_lane: MemArgWithLaneIdx,
    v128_load32_lane: MemArgWithLaneIdx,
    v128_load64_lane: MemArgWithLaneIdx,
    v128_store8_lane: MemArgWithLaneIdx,
    v128_store16_lane: MemArgWithLaneIdx,
    v128_store32_lane: MemArgWithLaneIdx,
    v128_store64_lane: MemArgWithLaneIdx,
    v128_load32_zero: MemArg,
    v128_load64_zero: MemArg,
    f32x4_demote_f64x2_zero,
    f64x2_promote_low_f32x4,
    i8x16_abs,
    i16x8_abs,
    i32x4_abs,
    i64x2_abs,
    i8x16_neg,
    i16x8_neg,
    i32x4_neg,
    i64x2_neg,
    i8x16_popcnt,
    i16x8_q15mulr_sat_s,
    i8x16_all_true,
    i16x8_all_true,
    i32x4_all_true,
    i64x2_all_true,
    i8x16_bitmask,
    i16x8_bitmask,
    i32x4_bitmask,
    i64x2_bitmask,
    i8x16_narrow_i16x8_s,
    i16x8_narrow_i32x4_s,
    i8x16_narrow_i16x8_u,
    i16x8_narrow_i32x4_u,
    f32x4_ceil,
    i16x8_extend_low_i8x16_s,
    i32x4_extend_low_i16x8_s,
    i64x2_extend_low_i32x4_s,
    f32x4_floor,
    i16x8_extend_high_i8x16_s,
    i32x4_extend_high_i16x8_s,
    i64x2_extend_high_i32x4_s,
    f32x4_trunc,
    i16x8_extend_low_i8x16_u,
    i32x4_extend_low_i16x8_u,
    i64x2_extend_low_i32x4_u,
    f32x4_nearest,
    i16x8_extend_high_i8x16_u,
    i32x4_extend_high_i16x8_u,
    i64x2_extend_high_i32x4_u,
    i8x16_shl,
    i16x8_shl,
    i32x4_shl,
    i64x2_shl,
    i8x16_shr_s,
    i16x8_shr_s,
    i32x4_shr_s,
    i64x2_shr_s,
    i8x16_shr_u,
    i16x8_shr_u,
    i32x4_shr_u,
    i64x2_shr_u,
    i8x16_add,
    i16x8_add,
    i32x4_add,
    i64x2_add,
    i8x16_add_sat_s,
    i16x8_add_sat_s,
    i8x16_add_sat_u,
    i16x8_add_sat_u,
    i8x16_sub,
    i16x8_sub,
    i32x4_sub,
    i64x2_sub,
    i8x16_sub_sat_s,
    i16x8_sub_sat_s,
    i8x16_sub_sat_u,
    i16x8_sub_sat_u,
    f64x2_ceil,
    f64x2_nearest,
    f64x2_floor,
    i16x8_mul,
    i32x4_mul,
    i64x2_mul,
    i8x16_min_s,
    i16x8_min_s,
    i32x4_min_s,
    i64x2_eq,
    i8x16_min_u,
    i16x8_min_u,
    i32x4_min_u,
    i64x2_ne,
    i8x16_max_s,
    i16x8_max_s,
    i32x4_max_s,
    i64x2_lt_s,
    i8x16_max_u,
    i16x8_max_u,
    i32x4_max_u,
    i64x2_gt_s,
    f64x2_trunc,
    i32x4_dot_i16x8_s,
    i64x2_le_s,
    i8x16_avgr_u,
    i16x8_avgr_u,
    i64x2_ge_s,
    i16x8_extadd_pairwise_i8x16_s,
    i16x8_extmul_low_i8x16_s,
    i32x4_extmul_low_i16x8_s,
    i64x2_extmul_low_i32x4_s,
    i16x8_extadd_pairwise_i8x16_u,
    i16x8_extmul_high_i8x16_s,
    i32x4_extmul_high_i16x8_s,
    i64x2_extmul_high_i32x4_s,
    i32x4_extadd_pairwise_i16x8_s,
    i16x8_extmul_low_i8x16_u,
    i32x4_extmul_low_i16x8_u,
    i64x2_extmul_low_i32x4_u,
    i32x4_extadd_pairwise_i16x8_u,
    i16x8_extmul_high_i8x16_u,
    i32x4_extmul_high_i16x8_u,
    i64x2_extmul_high_i32x4_u,
    f32x4_abs,
    f64x2_abs,
    f32x4_neg,
    f64x2_neg,
    f32x4_sqrt,
    f64x2_sqrt,
    f32x4_add,
    f64x2_add,
    f32x4_sub,
    f64x2_sub,
    f32x4_mul,
    f64x2_mul,
    f32x4_div,
    f64x2_div,
    f32x4_min,
    f64x2_min,
    f32x4_max,
    f64x2_max,
    f32x4_pmin,
    f64x2_pmin,
    f32x4_pmax,
    f64x2_pmax,
    i32x4_trunc_sat_f32x4_s,
    i32x4_trunc_sat_f32x4_u,
    f32x4_convert_i32x4_s,
    f32x4_convert_i32x4_u,
    i32x4_trunc_sat_f64x2_s_zero,
    i32x4_trunc_sat_f64x2_u_zero,
    f64x2_convert_low_i32x4_s,
    f64x2_convert_low_i32x4_u,

    // Relaxed SIMD instructions
    i8x16_relaxed_swizzle,
    i32x4_relaxed_trunc_f32x4_s,
    i32x4_relaxed_trunc_f32x4_u,
    i32x4_relaxed_trunc_f64x2_s_zero,
    i32x4_relaxed_trunc_f64x2_u_zero,
    f32x4_relaxed_madd,
    f32x4_relaxed_nmadd,
    f64x2_relaxed_madd,
    f64x2_relaxed_nmadd,
    i8x16_relaxed_laneselect,
    i16x8_relaxed_laneselect,
    i32x4_relaxed_laneselect,
    i64x2_relaxed_laneselect,
    f32x4_relaxed_min,
    f32x4_relaxed_max,
    f64x2_relaxed_min,
    f64x2_relaxed_max,
    i16x8_relaxed_q15mulr_s,
    i16x8_relaxed_dot_i8x16_i7x16_s,
    i32x4_relaxed_dot_i8x16_i7x16_add_s,
    f32x4_relaxed_dot_bf16x8_add_f32x4,

    pub fn format(self: @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
        switch (self) {
            inline else => |val| if (@TypeOf(val) == void) {
                try writer.print("{s}", .{@tagName(self)});
            } else if (@typeInfo(@TypeOf(val)) == .@"struct") {
                try writer.print("{s} (", .{@tagName(self)});
                const fields = @typeInfo(@TypeOf(val)).@"struct".fields;
                inline for (fields, 0..) |f, i| {
                    try writer.print("{s} = {s}", .{ f.name, @field(val, f.name) });
                    if (i + 1 < fields.len)
                        try writer.writeAll(", ");
                }
                try writer.writeAll(")");
            } else {
                try writer.print("{s} {s}", .{ @tagName(self), val });
            },
        }
    }
};
