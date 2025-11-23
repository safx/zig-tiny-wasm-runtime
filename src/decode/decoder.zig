const std = @import("std");
const core = @import("wasm-core");
const Instruction = core.Instruction;
const BinaryReader = @import("./binary_reader.zig").BinaryReader;
const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

// Type aliases
const RefType = core.types.RefType;
const ValueType = core.types.ValueType;
const LabelIdx = core.types.LabelIdx;

pub const Decoder = struct {
    const InstArray = std.ArrayList(Instruction);

    pub fn new() Decoder {
        return .{};
    }

    pub fn parseAll(_: Decoder, buffer: []const u8, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const core.types.Instruction {
        var reader = BinaryReader.new(buffer);
        var instArray: std.ArrayList(core.types.Instruction) = .empty;
        var nested_blocks: std.ArrayList(core.types.InstractionAddr) = .empty;

        while (!reader.eof()) {
            const inst = try parse(&reader, allocator);
            const pos: u32 = @intCast(instArray.items.len);
            try instArray.append(allocator, inst);

            if (inst == .block or inst == .loop or inst == .@"if") {
                try nested_blocks.append(allocator, pos);
            } else if (inst == .@"else") {
                const idx = nested_blocks.getLast();
                try fillElse(&instArray.items[idx], pos);
            } else if (inst == .end) {
                if (nested_blocks.items.len == 0)
                    return instArray.toOwnedSlice(allocator);

                const idx = nested_blocks.pop().?;
                try fillEnd(&instArray.items[idx], pos);
            }
        }

        return Error.UnexpectedEndOfSectionOrFunction;
    }

    fn fillEnd(inst: *Instruction, pos: u32) Error!void {
        switch (inst.*) {
            .block => inst.block.end = pos,
            .loop => inst.loop.end = pos,
            .@"if" => inst.@"if".end = pos,
            else => return Error.OtherError,
        }
    }

    fn fillElse(inst: *Instruction, pos: u32) Error!void {
        switch (inst.*) {
            .@"if" => inst.@"if".@"else" = pos,
            else => return Error.OtherError,
        }
    }

    fn opcode2int(op: std.wasm.Opcode) u32 {
        return @intFromEnum(op);
    }

    fn parse(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction {
        const n = opcode2int;
        const op_code = try reader.readU8();
        const inst: Instruction = switch (op_code) {
            n(.end) => .end,
            n(.@"else") => .@"else",

            // contronl instructions
            n(.@"unreachable") => .@"unreachable",
            n(.nop) => .nop,
            n(.block) => .{ .block = try block(reader) },
            n(.loop) => .{ .loop = try block(reader) },
            n(.@"if") => .{ .@"if" = try ifBlock(reader) },
            n(.br) => .{ .br = try reader.readVarU32() },
            n(.br_if) => .{ .br_if = try reader.readVarU32() },
            n(.br_table) => .{ .br_table = try brTable(reader, allocator) },
            n(.@"return") => .@"return",
            n(.call) => .{ .call = try reader.readVarU32() },
            n(.call_indirect) => .{ .call_indirect = try callIndirect(reader) },

            // reference instructions
            0xD0 => .{ .ref_null = try refType(reader) },
            0xD1 => .ref_is_null,
            0xD2 => .{ .ref_func = try reader.readVarU32() },

            // parametric instructions
            n(.drop) => .drop,
            n(.select) => .select,
            0x1C => .{ .selectv = try selectv(reader, allocator) },

            // variable instructions
            n(.local_get) => .{ .local_get = try reader.readVarU32() },
            n(.local_set) => .{ .local_set = try reader.readVarU32() },
            n(.local_tee) => .{ .local_tee = try reader.readVarU32() },
            n(.global_get) => .{ .global_get = try reader.readVarU32() },
            n(.global_set) => .{ .global_set = try reader.readVarU32() },

            // table instructions
            0x25 => .{ .table_get = try reader.readVarU32() },
            0x26 => .{ .table_set = try reader.readVarU32() },

            // memory instructions
            n(.i32_load) => .{ .i32_load = try memArg(reader) },
            n(.i64_load) => .{ .i64_load = try memArg(reader) },
            n(.f32_load) => .{ .f32_load = try memArg(reader) },
            n(.f64_load) => .{ .f64_load = try memArg(reader) },
            n(.i32_load8_s) => .{ .i32_load8_s = try memArg(reader) },
            n(.i32_load8_u) => .{ .i32_load8_u = try memArg(reader) },
            n(.i32_load16_s) => .{ .i32_load16_s = try memArg(reader) },
            n(.i32_load16_u) => .{ .i32_load16_u = try memArg(reader) },
            n(.i64_load8_s) => .{ .i64_load8_s = try memArg(reader) },
            n(.i64_load8_u) => .{ .i64_load8_u = try memArg(reader) },
            n(.i64_load16_s) => .{ .i64_load16_s = try memArg(reader) },
            n(.i64_load16_u) => .{ .i64_load16_u = try memArg(reader) },
            n(.i64_load32_s) => .{ .i64_load32_s = try memArg(reader) },
            n(.i64_load32_u) => .{ .i64_load32_u = try memArg(reader) },
            n(.i32_store) => .{ .i32_store = try memArg(reader) },
            n(.i64_store) => .{ .i64_store = try memArg(reader) },
            n(.f32_store) => .{ .f32_store = try memArg(reader) },
            n(.f64_store) => .{ .f64_store = try memArg(reader) },
            n(.i32_store8) => .{ .i32_store8 = try memArg(reader) },
            n(.i32_store16) => .{ .i32_store16 = try memArg(reader) },
            n(.i64_store8) => .{ .i64_store8 = try memArg(reader) },
            n(.i64_store16) => .{ .i64_store16 = try memArg(reader) },
            n(.i64_store32) => .{ .i64_store32 = try memArg(reader) },
            n(.memory_size) => .{ .memory_size = try reader.readVarU32() },
            n(.memory_grow) => .{ .memory_grow = try reader.readVarU32() },

            // Numeric const instructions
            n(.i32_const) => .{ .i32_const = try reader.readVarI32() },
            n(.i64_const) => .{ .i64_const = try reader.readVarI64() },
            n(.f32_const) => .{ .f32_const = try reader.readF32() },
            n(.f64_const) => .{ .f64_const = try reader.readF64() },

            // i32 comparison
            n(.i32_eqz) => .i32_eqz,
            n(.i32_eq) => .i32_eq,
            n(.i32_ne) => .i32_ne,
            n(.i32_lt_s) => .i32_lt_s,
            n(.i32_lt_u) => .i32_lt_u,
            n(.i32_gt_s) => .i32_gt_s,
            n(.i32_gt_u) => .i32_gt_u,
            n(.i32_le_s) => .i32_le_s,
            n(.i32_le_u) => .i32_le_u,
            n(.i32_ge_s) => .i32_ge_s,
            n(.i32_ge_u) => .i32_ge_u,

            // i64 comparison
            n(.i64_eqz) => .i64_eqz,
            n(.i64_eq) => .i64_eq,
            n(.i64_ne) => .i64_ne,
            n(.i64_lt_s) => .i64_lt_s,
            n(.i64_lt_u) => .i64_lt_u,
            n(.i64_gt_s) => .i64_gt_s,
            n(.i64_gt_u) => .i64_gt_u,
            n(.i64_le_s) => .i64_le_s,
            n(.i64_le_u) => .i64_le_u,
            n(.i64_ge_s) => .i64_ge_s,
            n(.i64_ge_u) => .i64_ge_u,

            // f32 comparison
            n(.f32_eq) => .f32_eq,
            n(.f32_ne) => .f32_ne,
            n(.f32_lt) => .f32_lt,
            n(.f32_gt) => .f32_gt,
            n(.f32_le) => .f32_le,
            n(.f32_ge) => .f32_ge,

            // f64 comparison
            n(.f64_eq) => .f64_eq,
            n(.f64_ne) => .f64_ne,
            n(.f64_lt) => .f64_lt,
            n(.f64_gt) => .f64_gt,
            n(.f64_le) => .f64_le,
            n(.f64_ge) => .f64_ge,

            // i32 arithmetic
            n(.i32_clz) => .i32_clz,
            n(.i32_ctz) => .i32_ctz,
            n(.i32_popcnt) => .i32_popcnt,
            n(.i32_add) => .i32_add,
            n(.i32_sub) => .i32_sub,
            n(.i32_mul) => .i32_mul,
            n(.i32_div_s) => .i32_div_s,
            n(.i32_div_u) => .i32_div_u,
            n(.i32_rem_s) => .i32_rem_s,
            n(.i32_rem_u) => .i32_rem_u,
            n(.i32_and) => .i32_and,
            n(.i32_or) => .i32_or,
            n(.i32_xor) => .i32_xor,
            n(.i32_shl) => .i32_shl,
            n(.i32_shr_s) => .i32_shr_s,
            n(.i32_shr_u) => .i32_shr_u,
            n(.i32_rotl) => .i32_rotl,
            n(.i32_rotr) => .i32_rotr,

            // i64 arithmetic
            n(.i64_clz) => .i64_clz,
            n(.i64_ctz) => .i64_ctz,
            n(.i64_popcnt) => .i64_popcnt,
            n(.i64_add) => .i64_add,
            n(.i64_sub) => .i64_sub,
            n(.i64_mul) => .i64_mul,
            n(.i64_div_s) => .i64_div_s,
            n(.i64_div_u) => .i64_div_u,
            n(.i64_rem_s) => .i64_rem_s,
            n(.i64_rem_u) => .i64_rem_u,
            n(.i64_and) => .i64_and,
            n(.i64_or) => .i64_or,
            n(.i64_xor) => .i64_xor,
            n(.i64_shl) => .i64_shl,
            n(.i64_shr_s) => .i64_shr_s,
            n(.i64_shr_u) => .i64_shr_u,
            n(.i64_rotl) => .i64_rotl,
            n(.i64_rotr) => .i64_rotr,

            // f32 arithmetic
            n(.f32_abs) => .f32_abs,
            n(.f32_neg) => .f32_neg,
            n(.f32_ceil) => .f32_ceil,
            n(.f32_floor) => .f32_floor,
            n(.f32_trunc) => .f32_trunc,
            n(.f32_nearest) => .f32_nearest,
            n(.f32_sqrt) => .f32_sqrt,
            n(.f32_add) => .f32_add,
            n(.f32_sub) => .f32_sub,
            n(.f32_mul) => .f32_mul,
            n(.f32_div) => .f32_div,
            n(.f32_min) => .f32_min,
            n(.f32_max) => .f32_max,
            0x98 => .f32_copy_sign,

            // f64 arithmetic
            n(.f64_abs) => .f64_abs,
            n(.f64_neg) => .f64_neg,
            n(.f64_ceil) => .f64_ceil,
            n(.f64_floor) => .f64_floor,
            n(.f64_trunc) => .f64_trunc,
            n(.f64_nearest) => .f64_nearest,
            n(.f64_sqrt) => .f64_sqrt,
            n(.f64_add) => .f64_add,
            n(.f64_sub) => .f64_sub,
            n(.f64_mul) => .f64_mul,
            n(.f64_div) => .f64_div,
            n(.f64_min) => .f64_min,
            n(.f64_max) => .f64_max,
            0xA6 => .f64_copy_sign,

            // numeric conversion
            n(.i32_wrap_i64) => .i32_wrap_i64,
            n(.i32_trunc_f32_s) => .i32_trunc_f32_s,
            n(.i32_trunc_f32_u) => .i32_trunc_f32_u,
            n(.i32_trunc_f64_s) => .i32_trunc_f64_s,
            n(.i32_trunc_f64_u) => .i32_trunc_f64_u,
            n(.i64_extend_i32_s) => .i64_extend_i32_s,
            n(.i64_extend_i32_u) => .i64_extend_i32_u,
            n(.i64_trunc_f32_s) => .i64_trunc_f32_s,
            n(.i64_trunc_f32_u) => .i64_trunc_f32_u,
            n(.i64_trunc_f64_s) => .i64_trunc_f64_s,
            n(.i64_trunc_f64_u) => .i64_trunc_f64_u,
            n(.f32_convert_i32_s) => .f32_convert_i32_s,
            n(.f32_convert_i32_u) => .f32_convert_i32_u,
            n(.f32_convert_i64_s) => .f32_convert_i64_s,
            n(.f32_convert_i64_u) => .f32_convert_i64_u,
            n(.f32_demote_f64) => .f32_demote_f64,
            n(.f64_convert_i32_s) => .f64_convert_i32_s,
            n(.f64_convert_i32_u) => .f64_convert_i32_u,
            n(.f64_convert_i64_s) => .f64_convert_i64_s,
            n(.f64_convert_i64_u) => .f64_convert_i64_u,
            n(.f64_promote_f32) => .f64_promote_f32,
            n(.i32_reinterpret_f32) => .i32_reinterpret_f32,
            n(.i64_reinterpret_f64) => .i64_reinterpret_f64,
            n(.f32_reinterpret_i32) => .f32_reinterpret_i32,
            n(.f64_reinterpret_i64) => .f64_reinterpret_i64,

            // numeric extension
            n(.i32_extend8_s) => .i32_extend8_s,
            n(.i32_extend16_s) => .i32_extend16_s,
            n(.i64_extend8_s) => .i64_extend8_s,
            n(.i64_extend16_s) => .i64_extend16_s,
            n(.i64_extend32_s) => .i64_extend32_s,

            n(.misc_prefix) => try miscOpcode(reader),
            n(.simd_prefix) => try simdOpcode(reader, allocator),

            else => {
                std.debug.print("?? Unknown opcode: 0x{x}\n", .{op_code});
                unreachable;
            },
        };

        return inst;
    }

    fn miscOpcode2int(op: std.wasm.MiscOpcode) u32 {
        return @intFromEnum(op);
    }

    fn miscOpcode(reader: *BinaryReader) (Error || error{OutOfMemory})!Instruction {
        const n = miscOpcode2int;
        const op_code = try reader.readVarU32();
        return switch (op_code) {
            // table instructions
            n(.table_init) => .{ .table_init = try tableInitArg(reader) },
            n(.elem_drop) => .{ .elem_drop = try reader.readVarU32() },
            n(.table_copy) => .{ .table_copy = try tableCopyArg(reader) },
            n(.table_grow) => .{ .table_grow = try reader.readVarU32() },
            n(.table_size) => .{ .table_size = try reader.readVarU32() },
            n(.table_fill) => .{ .table_fill = try reader.readVarU32() },

            // memory instructions
            n(.memory_init) => try memoryInit(reader),
            n(.data_drop) => .{ .data_drop = try reader.readVarU32() },
            n(.memory_copy) => .{ .memory_copy = .{ .mem_idx_dst = try reader.readVarU32(), .mem_idx_src = try reader.readVarU32() } },
            n(.memory_fill) => .{ .memory_fill = try reader.readVarU32() },

            // saturating truncation instructions
            n(.i32_trunc_sat_f32_s) => .i32_trunc_sat_f32_s,
            n(.i32_trunc_sat_f32_u) => .i32_trunc_sat_f32_u,
            n(.i32_trunc_sat_f64_s) => .i32_trunc_sat_f64_s,
            n(.i32_trunc_sat_f64_u) => .i32_trunc_sat_f64_u,
            n(.i64_trunc_sat_f32_s) => .i64_trunc_sat_f32_s,
            n(.i64_trunc_sat_f32_u) => .i64_trunc_sat_f32_u,
            n(.i64_trunc_sat_f64_s) => .i64_trunc_sat_f64_s,
            n(.i64_trunc_sat_f64_u) => .i64_trunc_sat_f64_u,

            else => {
                std.debug.print("?? Unknown opcode: 0xFC {x}\n", .{op_code});
                unreachable;
            },
        };
    }

    fn simdOpcode2int(op: std.wasm.SimdOpcode) u32 {
        return @intFromEnum(op);
    }

    fn simdOpcode(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction {
        const n = simdOpcode2int;
        const op_code = try reader.readVarU32();
        return switch (op_code) {
            // SIMD instructions
            n(.v128_load) => .{ .v128_load = try memArg(reader) },
            n(.v128_load8x8_s) => .{ .v128_load8x8_s = try memArg(reader) },
            n(.v128_load8x8_u) => .{ .v128_load8x8_u = try memArg(reader) },
            n(.v128_load16x4_s) => .{ .v128_load16x4_s = try memArg(reader) },
            n(.v128_load16x4_u) => .{ .v128_load16x4_u = try memArg(reader) },
            n(.v128_load32x2_s) => .{ .v128_load32x2_s = try memArg(reader) },
            n(.v128_load32x2_u) => .{ .v128_load32x2_u = try memArg(reader) },
            n(.v128_load8_splat) => .{ .v128_load8_splat = try memArg(reader) },
            n(.v128_load16_splat) => .{ .v128_load16_splat = try memArg(reader) },
            n(.v128_load32_splat) => .{ .v128_load32_splat = try memArg(reader) },
            n(.v128_load64_splat) => .{ .v128_load64_splat = try memArg(reader) },
            n(.v128_store) => .{ .v128_store = try memArg(reader) },
            n(.v128_const) => .{ .v128_const = try reader.readI128() },
            n(.i8x16_shuffle) => .{ .i8x16_shuffle = try laneIdxs(reader, allocator) },
            n(.i8x16_swizzle) => .i8x16_swizzle,
            n(.i8x16_splat) => .i8x16_splat,
            n(.i16x8_splat) => .i16x8_splat,
            n(.i32x4_splat) => .i32x4_splat,
            n(.i64x2_splat) => .i64x2_splat,
            n(.f32x4_splat) => .f32x4_splat,
            n(.f64x2_splat) => .f64x2_splat,
            n(.i8x16_extract_lane_s) => .{ .i8x16_extract_lane_s = try laneIdx(reader) },
            n(.i8x16_extract_lane_u) => .{ .i8x16_extract_lane_u = try laneIdx(reader) },
            n(.i8x16_replace_lane) => .{ .i8x16_replace_lane = try laneIdx(reader) },
            n(.i16x8_extract_lane_s) => .{ .i16x8_extract_lane_s = try laneIdx(reader) },
            n(.i16x8_extract_lane_u) => .{ .i16x8_extract_lane_u = try laneIdx(reader) },
            n(.i16x8_replace_lane) => .{ .i16x8_replace_lane = try laneIdx(reader) },
            n(.i32x4_extract_lane) => .{ .i32x4_extract_lane = try laneIdx(reader) },
            n(.i32x4_replace_lane) => .{ .i32x4_replace_lane = try laneIdx(reader) },
            n(.i64x2_extract_lane) => .{ .i64x2_extract_lane = try laneIdx(reader) },
            n(.i64x2_replace_lane) => .{ .i64x2_replace_lane = try laneIdx(reader) },
            n(.f32x4_extract_lane) => .{ .f32x4_extract_lane = try laneIdx(reader) },
            n(.f32x4_replace_lane) => .{ .f32x4_replace_lane = try laneIdx(reader) },
            n(.f64x2_extract_lane) => .{ .f64x2_extract_lane = try laneIdx(reader) },
            n(.f64x2_replace_lane) => .{ .f64x2_replace_lane = try laneIdx(reader) },
            n(.i8x16_eq) => .i8x16_eq,
            n(.i16x8_eq) => .i16x8_eq,
            n(.i32x4_eq) => .i32x4_eq,
            n(.i8x16_ne) => .i8x16_ne,
            n(.i16x8_ne) => .i16x8_ne,
            n(.i32x4_ne) => .i32x4_ne,
            n(.i8x16_lt_s) => .i8x16_lt_s,
            n(.i16x8_lt_s) => .i16x8_lt_s,
            n(.i32x4_lt_s) => .i32x4_lt_s,
            n(.i8x16_lt_u) => .i8x16_lt_u,
            n(.i16x8_lt_u) => .i16x8_lt_u,
            n(.i32x4_lt_u) => .i32x4_lt_u,
            n(.i8x16_gt_s) => .i8x16_gt_s,
            n(.i16x8_gt_s) => .i16x8_gt_s,
            n(.i32x4_gt_s) => .i32x4_gt_s,
            n(.i8x16_gt_u) => .i8x16_gt_u,
            n(.i16x8_gt_u) => .i16x8_gt_u,
            n(.i32x4_gt_u) => .i32x4_gt_u,
            n(.i8x16_le_s) => .i8x16_le_s,
            n(.i16x8_le_s) => .i16x8_le_s,
            n(.i32x4_le_s) => .i32x4_le_s,
            n(.i8x16_le_u) => .i8x16_le_u,
            n(.i16x8_le_u) => .i16x8_le_u,
            n(.i32x4_le_u) => .i32x4_le_u,
            n(.i8x16_ge_s) => .i8x16_ge_s,
            n(.i16x8_ge_s) => .i16x8_ge_s,
            n(.i32x4_ge_s) => .i32x4_ge_s,
            n(.i8x16_ge_u) => .i8x16_ge_u,
            n(.i16x8_ge_u) => .i16x8_ge_u,
            n(.i32x4_ge_u) => .i32x4_ge_u,
            n(.f32x4_eq) => .f32x4_eq,
            n(.f64x2_eq) => .f64x2_eq,
            n(.f32x4_ne) => .f32x4_ne,
            n(.f64x2_ne) => .f64x2_ne,
            n(.f32x4_lt) => .f32x4_lt,
            n(.f64x2_lt) => .f64x2_lt,
            n(.f32x4_gt) => .f32x4_gt,
            n(.f64x2_gt) => .f64x2_gt,
            n(.f32x4_le) => .f32x4_le,
            n(.f64x2_le) => .f64x2_le,
            n(.f32x4_ge) => .f32x4_ge,
            n(.f64x2_ge) => .f64x2_ge,
            n(.v128_not) => .v128_not,
            n(.v128_and) => .v128_and,
            n(.v128_andnot) => .v128_andnot,
            n(.v128_or) => .v128_or,
            n(.v128_xor) => .v128_xor,
            n(.v128_bitselect) => .v128_bitselect,
            n(.v128_any_true) => .v128_any_true,
            n(.v128_load8_lane) => .{ .v128_load8_lane = try memArgWithLaneIdx(reader) },
            n(.v128_load16_lane) => .{ .v128_load16_lane = try memArgWithLaneIdx(reader) },
            n(.v128_load32_lane) => .{ .v128_load32_lane = try memArgWithLaneIdx(reader) },
            n(.v128_load64_lane) => .{ .v128_load64_lane = try memArgWithLaneIdx(reader) },
            n(.v128_store8_lane) => .{ .v128_store8_lane = try memArgWithLaneIdx(reader) },
            n(.v128_store16_lane) => .{ .v128_store16_lane = try memArgWithLaneIdx(reader) },
            n(.v128_store32_lane) => .{ .v128_store32_lane = try memArgWithLaneIdx(reader) },
            n(.v128_store64_lane) => .{ .v128_store64_lane = try memArgWithLaneIdx(reader) },
            n(.v128_load32_zero) => .{ .v128_load32_zero = try memArg(reader) },
            n(.v128_load64_zero) => .{ .v128_load64_zero = try memArg(reader) },
            n(.f32x4_demote_f64x2_zero) => .f32x4_demote_f64x2_zero,
            n(.f64x2_promote_low_f32x4) => .f64x2_promote_low_f32x4,
            n(.i8x16_abs) => .i8x16_abs,
            n(.i16x8_abs) => .i16x8_abs,
            n(.i32x4_abs) => .i32x4_abs,
            n(.i64x2_abs) => .i64x2_abs,
            n(.i8x16_neg) => .i8x16_neg,
            n(.i16x8_neg) => .i16x8_neg,
            n(.i32x4_neg) => .i32x4_neg,
            n(.i64x2_neg) => .i64x2_neg,
            n(.i8x16_popcnt) => .i8x16_popcnt,
            n(.i16x8_q15mulr_sat_s) => .i16x8_q15mulr_sat_s,
            n(.i8x16_all_true) => .i8x16_all_true,
            n(.i16x8_all_true) => .i16x8_all_true,
            n(.i32x4_all_true) => .i32x4_all_true,
            n(.i64x2_all_true) => .i64x2_all_true,
            n(.i8x16_bitmask) => .i8x16_bitmask,
            n(.i16x8_bitmask) => .i16x8_bitmask,
            n(.i32x4_bitmask) => .i32x4_bitmask,
            n(.i64x2_bitmask) => .i64x2_bitmask,
            n(.i8x16_narrow_i16x8_s) => .i8x16_narrow_i16x8_s,
            n(.i16x8_narrow_i32x4_s) => .i16x8_narrow_i32x4_s,
            n(.i8x16_narrow_i16x8_u) => .i8x16_narrow_i16x8_u,
            n(.i16x8_narrow_i32x4_u) => .i16x8_narrow_i32x4_u,
            n(.f32x4_ceil) => .f32x4_ceil,
            n(.i16x8_extend_low_i8x16_s) => .i16x8_extend_low_i8x16_s,
            n(.i32x4_extend_low_i16x8_s) => .i32x4_extend_low_i16x8_s,
            n(.i64x2_extend_low_i32x4_s) => .i64x2_extend_low_i32x4_s,
            n(.f32x4_floor) => .f32x4_floor,
            n(.i16x8_extend_high_i8x16_s) => .i16x8_extend_high_i8x16_s,
            n(.i32x4_extend_high_i16x8_s) => .i32x4_extend_high_i16x8_s,
            n(.i64x2_extend_high_i32x4_s) => .i64x2_extend_high_i32x4_s,
            n(.f32x4_trunc) => .f32x4_trunc,
            n(.i16x8_extend_low_i8x16_u) => .i16x8_extend_low_i8x16_u,
            n(.i32x4_extend_low_i16x8_u) => .i32x4_extend_low_i16x8_u,
            n(.i64x2_extend_low_i32x4_u) => .i64x2_extend_low_i32x4_u,
            n(.f32x4_nearest) => .f32x4_nearest,
            n(.i16x8_extend_high_i8x16_u) => .i16x8_extend_high_i8x16_u,
            n(.i32x4_extend_high_i16x8_u) => .i32x4_extend_high_i16x8_u,
            n(.i64x2_extend_high_i32x4_u) => .i64x2_extend_high_i32x4_u,
            n(.i8x16_shl) => .i8x16_shl,
            n(.i16x8_shl) => .i16x8_shl,
            n(.i32x4_shl) => .i32x4_shl,
            n(.i64x2_shl) => .i64x2_shl,
            n(.i8x16_shr_s) => .i8x16_shr_s,
            n(.i16x8_shr_s) => .i16x8_shr_s,
            n(.i32x4_shr_s) => .i32x4_shr_s,
            n(.i64x2_shr_s) => .i64x2_shr_s,
            n(.i8x16_shr_u) => .i8x16_shr_u,
            n(.i16x8_shr_u) => .i16x8_shr_u,
            n(.i32x4_shr_u) => .i32x4_shr_u,
            n(.i64x2_shr_u) => .i64x2_shr_u,
            n(.i8x16_add) => .i8x16_add,
            n(.i16x8_add) => .i16x8_add,
            n(.i32x4_add) => .i32x4_add,
            n(.i64x2_add) => .i64x2_add,
            n(.i8x16_add_sat_s) => .i8x16_add_sat_s,
            n(.i16x8_add_sat_s) => .i16x8_add_sat_s,
            n(.i8x16_add_sat_u) => .i8x16_add_sat_u,
            n(.i16x8_add_sat_u) => .i16x8_add_sat_u,
            n(.i8x16_sub) => .i8x16_sub,
            n(.i16x8_sub) => .i16x8_sub,
            n(.i32x4_sub) => .i32x4_sub,
            n(.i64x2_sub) => .i64x2_sub,
            n(.i8x16_sub_sat_s) => .i8x16_sub_sat_s,
            n(.i16x8_sub_sat_s) => .i16x8_sub_sat_s,
            n(.i8x16_sub_sat_u) => .i8x16_sub_sat_u,
            n(.i16x8_sub_sat_u) => .i16x8_sub_sat_u,
            n(.f64x2_ceil) => .f64x2_ceil,
            n(.f64x2_nearest) => .f64x2_nearest,
            n(.f64x2_floor) => .f64x2_floor,
            n(.i16x8_mul) => .i16x8_mul,
            n(.i32x4_mul) => .i32x4_mul,
            n(.i64x2_mul) => .i64x2_mul,
            n(.i8x16_min_s) => .i8x16_min_s,
            n(.i16x8_min_s) => .i16x8_min_s,
            n(.i32x4_min_s) => .i32x4_min_s,
            n(.i64x2_eq) => .i64x2_eq,
            n(.i8x16_min_u) => .i8x16_min_u,
            n(.i16x8_min_u) => .i16x8_min_u,
            n(.i32x4_min_u) => .i32x4_min_u,
            n(.i64x2_ne) => .i64x2_ne,
            n(.i8x16_max_s) => .i8x16_max_s,
            n(.i16x8_max_s) => .i16x8_max_s,
            n(.i32x4_max_s) => .i32x4_max_s,
            n(.i64x2_lt_s) => .i64x2_lt_s,
            n(.i8x16_max_u) => .i8x16_max_u,
            n(.i16x8_max_u) => .i16x8_max_u,
            n(.i32x4_max_u) => .i32x4_max_u,
            n(.i64x2_gt_s) => .i64x2_gt_s,
            n(.f64x2_trunc) => .f64x2_trunc,
            n(.i32x4_dot_i16x8_s) => .i32x4_dot_i16x8_s,
            n(.i64x2_le_s) => .i64x2_le_s,
            n(.i8x16_avgr_u) => .i8x16_avgr_u,
            n(.i16x8_avgr_u) => .i16x8_avgr_u,
            n(.i64x2_ge_s) => .i64x2_ge_s,
            n(.i16x8_extadd_pairwise_i8x16_s) => .i16x8_extadd_pairwise_i8x16_s,
            n(.i16x8_extmul_low_i8x16_s) => .i16x8_extmul_low_i8x16_s,
            n(.i32x4_extmul_low_i16x8_s) => .i32x4_extmul_low_i16x8_s,
            n(.i64x2_extmul_low_i32x4_s) => .i64x2_extmul_low_i32x4_s,
            n(.i16x8_extadd_pairwise_i8x16_u) => .i16x8_extadd_pairwise_i8x16_u,
            n(.i16x8_extmul_high_i8x16_s) => .i16x8_extmul_high_i8x16_s,
            n(.i32x4_extmul_high_i16x8_s) => .i32x4_extmul_high_i16x8_s,
            n(.i64x2_extmul_high_i32x4_s) => .i64x2_extmul_high_i32x4_s,
            n(.i32x4_extadd_pairwise_i16x8_s) => .i32x4_extadd_pairwise_i16x8_s,
            n(.i16x8_extmul_low_i8x16_u) => .i16x8_extmul_low_i8x16_u,
            n(.i32x4_extmul_low_i16x8_u) => .i32x4_extmul_low_i16x8_u,
            n(.i64x2_extmul_low_i32x4_u) => .i64x2_extmul_low_i32x4_u,
            n(.i32x4_extadd_pairwise_i16x8_u) => .i32x4_extadd_pairwise_i16x8_u,
            n(.i16x8_extmul_high_i8x16_u) => .i16x8_extmul_high_i8x16_u,
            n(.i32x4_extmul_high_i16x8_u) => .i32x4_extmul_high_i16x8_u,
            n(.i64x2_extmul_high_i32x4_u) => .i64x2_extmul_high_i32x4_u,
            n(.f32x4_abs) => .f32x4_abs,
            n(.f64x2_abs) => .f64x2_abs,
            n(.f32x4_neg) => .f32x4_neg,
            n(.f64x2_neg) => .f64x2_neg,
            n(.f32x4_sqrt) => .f32x4_sqrt,
            n(.f64x2_sqrt) => .f64x2_sqrt,
            n(.f32x4_add) => .f32x4_add,
            n(.f64x2_add) => .f64x2_add,
            n(.f32x4_sub) => .f32x4_sub,
            n(.f64x2_sub) => .f64x2_sub,
            n(.f32x4_mul) => .f32x4_mul,
            n(.f64x2_mul) => .f64x2_mul,
            n(.f32x4_div) => .f32x4_div,
            n(.f64x2_div) => .f64x2_div,
            n(.f32x4_min) => .f32x4_min,
            n(.f64x2_min) => .f64x2_min,
            n(.f32x4_max) => .f32x4_max,
            n(.f64x2_max) => .f64x2_max,
            n(.f32x4_pmin) => .f32x4_pmin,
            n(.f64x2_pmin) => .f64x2_pmin,
            n(.f32x4_pmax) => .f32x4_pmax,
            n(.f64x2_pmax) => .f64x2_pmax,
            n(.i32x4_trunc_sat_f32x4_s) => .i32x4_trunc_sat_f32x4_s,
            n(.i32x4_trunc_sat_f32x4_u) => .i32x4_trunc_sat_f32x4_u,
            n(.f32x4_convert_i32x4_s) => .f32x4_convert_i32x4_s,
            n(.f32x4_convert_i32x4_u) => .f32x4_convert_i32x4_u,
            n(.i32x4_trunc_sat_f64x2_s_zero) => .i32x4_trunc_sat_f64x2_s_zero,
            n(.i32x4_trunc_sat_f64x2_u_zero) => .i32x4_trunc_sat_f64x2_u_zero,
            n(.f64x2_convert_low_i32x4_s) => .f64x2_convert_low_i32x4_s,
            n(.f64x2_convert_low_i32x4_u) => .f64x2_convert_low_i32x4_u,

            // Relaxed SIMD instructions
            n(.i8x16_relaxed_swizzle) => .i8x16_relaxed_swizzle,
            n(.i32x4_relaxed_trunc_f32x4_s) => .i32x4_relaxed_trunc_f32x4_s,
            n(.i32x4_relaxed_trunc_f32x4_u) => .i32x4_relaxed_trunc_f32x4_u,
            n(.i32x4_relaxed_trunc_f64x2_s_zero) => .i32x4_relaxed_trunc_f64x2_s_zero,
            n(.i32x4_relaxed_trunc_f64x2_u_zero) => .i32x4_relaxed_trunc_f64x2_u_zero,
            n(.f32x4_relaxed_madd) => .f32x4_relaxed_madd,
            n(.f32x4_relaxed_nmadd) => .f32x4_relaxed_nmadd,
            n(.f64x2_relaxed_madd) => .f64x2_relaxed_madd,
            n(.f64x2_relaxed_nmadd) => .f64x2_relaxed_nmadd,
            n(.i8x16_relaxed_laneselect) => .i8x16_relaxed_laneselect,
            n(.i16x8_relaxed_laneselect) => .i16x8_relaxed_laneselect,
            n(.i32x4_relaxed_laneselect) => .i32x4_relaxed_laneselect,
            n(.i64x2_relaxed_laneselect) => .i64x2_relaxed_laneselect,
            n(.f32x4_relaxed_min) => .f32x4_relaxed_min,
            n(.f32x4_relaxed_max) => .f32x4_relaxed_max,
            n(.f64x2_relaxed_min) => .f64x2_relaxed_min,
            n(.f64x2_relaxed_max) => .f64x2_relaxed_max,
            n(.i16x8_relaxed_q15mulr_s) => .i16x8_relaxed_q15mulr_s,
            n(.i16x8_relaxed_dot_i8x16_i7x16_s) => .i16x8_relaxed_dot_i8x16_i7x16_s,
            n(.i32x4_relaxed_dot_i8x16_i7x16_add_s) => .i32x4_relaxed_dot_i8x16_i7x16_add_s,
            n(.f32x4_relaxed_dot_bf16x8_add_f32x4) => .f32x4_relaxed_dot_bf16x8_add_f32x4,

            else => {
                std.debug.print("?? Unknown opcode: 0xFC {x}\n", .{op_code});
                unreachable;
            },
        };
    }
};

fn laneIdxs(reader: *BinaryReader, _: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction.LaneIdx16 {
    const buf = try reader.readBytes(16);
    var array: [16]u8 = undefined;
    @memcpy(&array, buf[0..16]);
    return array;
}

fn laneIdx(reader: *BinaryReader) (Error || error{OutOfMemory})!Instruction.LaneIdx {
    return try reader.readU8();
}

fn memoryInit(reader: *BinaryReader) Error!Instruction {
    const data_idx = try reader.readVarU32();
    const mem_idx = try reader.readVarU32();
    return .{ .memory_init = .{ .data_idx = data_idx, .mem_idx = mem_idx } };
}

fn memArg(reader: *BinaryReader) Error!Instruction.MemArg {
    const a = try reader.readVarU32();
    const o = try reader.readVarU32();
    return .{ .@"align" = a, .offset = o };
}

fn memArgWithLaneIdx(reader: *BinaryReader) Error!Instruction.MemArgWithLaneIdx {
    const a = try reader.readVarU32();
    const o = try reader.readVarU32();
    const l = try reader.readU8();
    return .{ .@"align" = a, .offset = o, .lane_idx = l };
}

fn tableInitArg(reader: *BinaryReader) Error!Instruction.TableInitArg {
    const elem = try reader.readVarU32();
    const table = try reader.readVarU32();
    return .{ .table_idx = table, .elem_idx = elem };
}

fn tableCopyArg(reader: *BinaryReader) Error!Instruction.TableCopyArg {
    const dst = try reader.readVarU32();
    const src = try reader.readVarU32();
    return .{ .table_idx_dst = dst, .table_idx_src = src };
}

fn refType(reader: *BinaryReader) Error!RefType {
    const byte = try reader.readU8();
    return utils.refTypeFromNum(byte) orelse return Error.MalformedRefType;
}

fn valueType(reader: *BinaryReader) Error!ValueType {
    const byte = try reader.readU8();
    return utils.valueTypeFromNum(byte) orelse return Error.MalformedValueType;
}

fn block(reader: *BinaryReader) Error!Instruction.BlockInfo {
    return .{ .type = try blockType(reader) }; // `end` is set at the end of block
}

fn selectv(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]ValueType {
    const len = try reader.readVarU32();
    const array = try allocator.alloc(ValueType, len); // freed by arena allocator in Module
    for (0..len) |i|
        array[i] = try valueType(reader);

    return array;
}

fn ifBlock(reader: *BinaryReader) Error!Instruction.IfBlockInfo {
    return .{ .type = try blockType(reader) }; // `else` and `end` is set at the end of block
}

fn blockType(reader: *BinaryReader) Error!Instruction.BlockType {
    const byte = try reader.peek();
    switch (byte) {
        0x40 => {
            _ = try reader.readU8();
            return .empty;
        },
        0x6f...0x70, 0x7b...0x7f => {
            _ = try reader.readU8();
            return .{ .value_type = utils.valueTypeFromNum(byte).? };
        },
        else => return .{ .type_index = try reader.readVarU32() },
    }
}

fn brTable(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction.BrTableType {
    const size = try reader.readVarU32();
    const label_idxs = try allocator.alloc(LabelIdx, size); // freed by arena allocator in Module

    for (0..size) |i|
        label_idxs[i] = try reader.readVarU32();

    const default = try reader.readVarU32();
    return .{ .label_idxs = label_idxs, .default_label_idx = default };
}

fn callIndirect(reader: *BinaryReader) Error!Instruction.CallIndirectArg {
    const y = try reader.readVarU32();
    const x = try reader.readVarU32();
    return .{ .type_idx = y, .table_idx = x };
}
