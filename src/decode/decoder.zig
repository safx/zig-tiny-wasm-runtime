const std = @import("std");
const types = @import("wasm-core");
const Instruction = types.Instruction;
const BinaryReader = @import("./binary_reader.zig").BinaryReader;
const utils = @import("./utils.zig");
const Error = @import("./errors.zig").Error;

pub const Decoder = struct {
    const InstArray = std.ArrayList(Instruction);

    pub fn new() Decoder {
        return .{};
    }

    pub fn parseAll(self: Decoder, buffer: []const u8, outputArray: *InstArray, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
        var reader = BinaryReader.new(buffer);
        while (!reader.eof()) {
            const inst = try parse(&reader, allocator);
            try outputArray.append(inst);

            if (inst == .block or inst == .loop or inst == .@"if") {
                try self.processControlBlock(&reader, outputArray, allocator, inst);
            }
        }
    }

    fn processControlBlock(self: Decoder, reader: *BinaryReader, outputArray: *InstArray, allocator: std.mem.Allocator, inst: Instruction) (Error || error{OutOfMemory})!void {
        const block_pos = outputArray.items.len - 1;
        switch (inst) {
            .block => {
                const pos = try self.parseForBlockOrLoop(reader, outputArray, allocator);
                outputArray.items[block_pos].block.end = pos;
            },
            .loop => {
                const pos = try self.parseForBlockOrLoop(reader, outputArray, allocator);
                outputArray.items[block_pos].loop.end = pos;
            },
            .@"if" => {
                const val = try self.parseForIf(reader, outputArray, allocator);
                outputArray.items[block_pos].@"if".@"else" = val[0];
                outputArray.items[block_pos].@"if".end = val[1].?;
            },
            else => return,
        }
    }

    fn parseForBlockOrLoop(self: Decoder, reader: *BinaryReader, outputArray: *InstArray, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!types.InstractionAddr {
        while (!reader.eof()) {
            const inst = try parse(reader, allocator);
            try outputArray.append(inst);

            switch (inst) {
                .end => {
                    return @intCast(outputArray.items.len - 1);
                },
                else => try self.processControlBlock(reader, outputArray, allocator, inst),
            }
        }
        unreachable;
    }

    fn parseForIf(self: Decoder, reader: *BinaryReader, outputArray: *InstArray, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]const ?types.InstractionAddr {
        var pos1: ?types.InstractionAddr = null;
        while (!reader.eof()) {
            const inst = try parse(reader, allocator);
            try outputArray.append(inst);

            switch (inst) {
                .@"else" => {
                    std.debug.assert(pos1 == null);
                    pos1 = @intCast(outputArray.items.len);
                },
                .end => {
                    const pos2: types.InstractionAddr = @intCast(outputArray.items.len - 1);
                    return &.{ pos1, pos2 };
                },
                else => try self.processControlBlock(reader, outputArray, allocator, inst),
            }
        }
        unreachable;
    }

    fn parse(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction {
        const n = std.wasm.opcode;
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
            n(.memory_size) => if (try reader.readU8() == 0) .memory_size else unreachable,
            n(.memory_grow) => if (try reader.readU8() == 0) .memory_grow else unreachable,

            // numeric instructions (1)
            n(.i32_const) => .{ .i32_const = try reader.readVarI32() },
            n(.i64_const) => .{ .i64_const = try reader.readVarI64() },
            n(.f32_const) => .{ .f32_const = try reader.readF32() },
            n(.f64_const) => .{ .f64_const = try reader.readF64() },

            // numeric instructions (2) i32
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

            // numeric instructions (2) i64
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

            // numeric instructions (2) f32
            n(.f32_eq) => .f32_eq,
            n(.f32_ne) => .f32_ne,
            n(.f32_lt) => .f32_lt,
            n(.f32_gt) => .f32_gt,
            n(.f32_le) => .f32_le,
            n(.f32_ge) => .f32_ge,

            // numeric instructions (2) f64
            n(.f64_eq) => .f64_eq,
            n(.f64_ne) => .f64_ne,
            n(.f64_lt) => .f64_lt,
            n(.f64_gt) => .f64_gt,
            n(.f64_le) => .f64_le,
            n(.f64_ge) => .f64_ge,

            // numeric instructions (3) i32
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

            // numeric instructions (3) i64
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

            // numeric instructions (3) f32
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

            // numeric instructions (3) f64
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

            // numeric instructions (4)
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

            // numeric instructions (5)
            n(.i32_extend8_s) => .i32_extend8_s,
            n(.i32_extend16_s) => .i32_extend16_s,
            n(.i64_extend8_s) => .i64_extend8_s,
            n(.i64_extend16_s) => .i64_extend16_s,
            n(.i64_extend32_s) => .i64_extend32_s,

            0xFC => try miscOpcode(reader),

            else => {
                std.debug.print("?? Unknown opcode: 0x{x}\n", .{op_code});
                unreachable;
            },
        };

        return inst;
    }

    fn miscOpcode(reader: *BinaryReader) (Error || error{OutOfMemory})!Instruction {
        const n = std.wasm.miscOpcode;
        const op_code = try reader.readU8();
        const inst: Instruction = switch (op_code) {
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
            n(.memory_copy) => if (try reader.readU8() == 0 and try reader.readU8() == 0) .memory_copy else unreachable,
            n(.memory_fill) => if (try reader.readU8() == 0) .memory_fill else unreachable,

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
        return inst;
    }
};

fn memoryInit(reader: *BinaryReader) Error!Instruction {
    const data_idx = try reader.readVarU32();
    const zero = try reader.readVarU32();
    std.debug.assert(zero == 0);
    return .{ .memory_init = data_idx };
}

fn memArg(reader: *BinaryReader) Error!Instruction.MemArg {
    const a = try reader.readVarU32();
    const o = try reader.readVarU32();
    return .{ .@"align" = a, .offset = o };
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

fn refType(reader: *BinaryReader) Error!types.RefType {
    const byte = try reader.readU8();
    return utils.refTypeFromNum(byte) orelse return Error.MalformedRefType;
}

fn valueType(reader: *BinaryReader) Error!types.ValueType {
    const byte = try reader.readU8();
    return utils.valueTypeFromNum(byte) orelse return Error.MalformedValueType;
}

fn block(reader: *BinaryReader) Error!Instruction.BlockInfo {
    return .{ .type = try blockType(reader) }; // `end` is set at the end of block
}

fn selectv(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})![]types.ValueType {
    const len = try reader.readVarU32();
    var array = try allocator.alloc(types.ValueType, len);
    for (0..len) |i| {
        array[i] = try valueType(reader);
    }
    return array;
}

fn ifBlock(reader: *BinaryReader) Error!Instruction.IfBlockInfo {
    return .{ .type = try blockType(reader) }; // `else` and `end` is set at the end of block
}

fn blockType(reader: *BinaryReader) Error!Instruction.BlockType {
    const byte = try reader.readU8();
    return switch (byte) {
        0x40 => .empty,
        0x6f...0x70, 0x7b...0x7f => .{ .value_type = utils.valueTypeFromNum(byte).? },
        else => .{ .type_index = byte }, // TODO: handle s33
    };
}

fn brTable(reader: *BinaryReader, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!Instruction.BrTableType {
    const size = try reader.readVarU32();
    const label_idxs = try allocator.alloc(types.LabelIdx, size);

    for (0..size) |i| {
        label_idxs[i] = try reader.readVarU32();
    }

    const default = try reader.readVarU32();
    return .{ .label_idxs = label_idxs, .default_label_idx = default };
}

fn callIndirect(reader: *BinaryReader) Error!Instruction.CallIndirectArg {
    const y = try reader.readVarU32();
    const x = try reader.readVarU32();
    return .{ .type_idx = y, .table_idx = x };
}
