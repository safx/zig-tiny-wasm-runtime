const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
};
const Context = @import("./context.zig").Context;
pub const Error = @import("./errors.zig").Error;

pub fn validateModule(module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    try validateModuleInner(module, allocator);
}

fn validateModuleInner(module: types.Module, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    const context = try Context.new(module, allocator);
    defer context.deinit(allocator);

    for (module.funcs) |func| {
        try validateFunction(context, func, allocator);
    }
}

fn validateFunction(c: Context, func: types.Func, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    if (func.type >= c.types.len)
        return Error.UnknownType;

    const cp = try Context.cloneContextForValidatingFunction(c, func, allocator);
    try validateFunctionBody(cp, func.body, c.types[func.type].result_types, allocator);
}

fn validateFunctionBody(c: Context, instrs: []const types.Instruction, expect_types: []const types.ValueType, allocator: std.mem.Allocator) (Error || error{OutOfMemory})!void {
    var type_stack = try TypeStack.new(allocator);

    for (instrs) |instr| {
        try validateInstruction(c, instr, &type_stack);
    }

    // TODO: pop from stack and check with expect_types
    _ = expect_types;
}

fn validateBlocktype(c: Context, block_type: types.Instruction.BlockType, allocator: std.mem.Allocator) Error!types.FuncType {
    switch (block_type) {
        .empty => return .{ .parameter_types = &.{}, .result_types = &.{} },
        .type_index => |idx| {
            if (idx < c.types)
                return Error.UnknownFunction;

            return c.types[idx];
        },
        .value_type => {
            const parameter_types = allocator.alloc(types.ValueType, 1); // TODO: release
            parameter_types[0] = block_type.value_type;
            return .{ .parameter_types = parameter_types, .result_types = &.{} };
        },
    }
}

fn validateInstruction(c: Context, instr: types.Instruction, type_stack: *TypeStack) (Error || error{OutOfMemory})!void {
    _ = c;
    switch (instr) {
        .end => {},
        .@"else" => {},

        // contronl instructions
        .nop => {},
        .@"unreachable" => {
            try type_stack.setPolymophic();
        },
        .block => |block_info| {
            _ = block_info;
        },
        .loop => |block_info| {
            _ = block_info;
        },
        .@"if" => |block_info| {
            _ = block_info;
        },
        .br => |label_idx| {
            _ = label_idx;
        },
        .br_if => |label_idx| {
            _ = label_idx;
        },
        .br_table => |table_info| {
            _ = table_info;
        },
        .@"return" => {},
        .call => |func_idx| {
            _ = func_idx;
        },
        .call_indirect => |arg| {
            _ = arg;
        },

        // reference instructions
        .ref_null => |ref_type| {
            _ = ref_type;
        },
        .ref_is_null => {},
        .ref_func => |func_idx| {
            _ = func_idx;
        },

        // parametric instructions
        .drop => {},
        .select => {},
        .selectv => {},

        // variable instructions
        .local_get => |local_idx| {
            _ = local_idx;
        },
        .local_set => |local_idx| {
            _ = local_idx;
        },
        .local_tee => |local_idx| {
            _ = local_idx;
        },
        .global_get => |global_idx| {
            _ = global_idx;
        },
        .global_set => |global_idx| {
            _ = global_idx;
        },

        // table instructions
        .table_get => |table_idx| {
            _ = table_idx;
        },
        .table_set => |table_idx| {
            _ = table_idx;
        },
        .table_init => |arg| {
            _ = arg;
        },
        .elem_drop => |elem_idx| {
            _ = elem_idx;
        },
        .table_copy => |arg| {
            _ = arg;
        },
        .table_grow => |table_idx| {
            _ = table_idx;
        },
        .table_size => |table_idx| {
            _ = table_idx;
        },
        .table_fill => |table_idx| {
            _ = table_idx;
        },

        // memory instructions
        .i32_load => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load => |mem_arg| {
            _ = mem_arg;
        },
        .f32_load => |mem_arg| {
            _ = mem_arg;
        },
        .f64_load => |mem_arg| {
            _ = mem_arg;
        },
        .i32_load8_s => |mem_arg| {
            _ = mem_arg;
        },
        .i32_load8_u => |mem_arg| {
            _ = mem_arg;
        },
        .i32_load16_s => |mem_arg| {
            _ = mem_arg;
        },
        .i32_load16_u => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load8_s => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load8_u => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load16_s => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load16_u => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load32_s => |mem_arg| {
            _ = mem_arg;
        },
        .i64_load32_u => |mem_arg| {
            _ = mem_arg;
        },
        .i32_store => |mem_arg| {
            _ = mem_arg;
        },
        .i64_store => |mem_arg| {
            _ = mem_arg;
        },
        .f32_store => |mem_arg| {
            _ = mem_arg;
        },
        .f64_store => |mem_arg| {
            _ = mem_arg;
        },
        .i32_store8 => |mem_arg| {
            _ = mem_arg;
        },
        .i32_store16 => |mem_arg| {
            _ = mem_arg;
        },
        .i64_store8 => |mem_arg| {
            _ = mem_arg;
        },
        .i64_store16 => |mem_arg| {
            _ = mem_arg;
        },
        .i64_store32 => |mem_arg| {
            _ = mem_arg;
        },
        .memory_size => {},
        .memory_grow => {},
        .memory_init => |data_idx| {
            _ = data_idx;
        },
        .data_drop => |data_idx| {
            _ = data_idx;
        },
        .memory_copy => {},
        .memory_fill => {},

        // numeric instructions (1)
        .i32_const => |val| {
            _ = val;
        },
        .i64_const => |val| {
            _ = val;
        },
        .f32_const => |val| {
            _ = val;
        },
        .f64_const => |val| {
            _ = val;
        },

        // numeric instructions (2) i32
        .i32_eqz => {},
        .i32_eq => {},
        .i32_ne => {},
        .i32_lt_s => {},
        .i32_lt_u => {},
        .i32_gt_s => {},
        .i32_gt_u => {},
        .i32_le_s => {},
        .i32_le_u => {},
        .i32_ge_s => {},
        .i32_ge_u => {},

        // numeric instructions (2) i64
        .i64_eqz => {},
        .i64_eq => {},
        .i64_ne => {},
        .i64_lt_s => {},
        .i64_lt_u => {},
        .i64_gt_s => {},
        .i64_gt_u => {},
        .i64_le_s => {},
        .i64_le_u => {},
        .i64_ge_s => {},
        .i64_ge_u => {},

        // numeric instructions (2) f32
        .f32_eq => {},
        .f32_ne => {},
        .f32_lt => {},
        .f32_gt => {},
        .f32_le => {},
        .f32_ge => {},

        // numeric instructions (2) f64
        .f64_eq => {},
        .f64_ne => {},
        .f64_lt => {},
        .f64_gt => {},
        .f64_le => {},
        .f64_ge => {},

        // numeric instructions (3) i32
        .i32_clz => {},
        .i32_ctz => {},
        .i32_popcnt => {},
        .i32_add => {},
        .i32_sub => {},
        .i32_mul => {},
        .i32_div_s => {},
        .i32_div_u => {},
        .i32_rem_s => {},
        .i32_rem_u => {},
        .i32_and => {},
        .i32_or => {},
        .i32_xor => {},
        .i32_shl => {},
        .i32_shr_s => {},
        .i32_shr_u => {},
        .i32_rotl => {},
        .i32_rotr => {},

        // numeric instructions (3) i64
        .i64_clz => {},
        .i64_ctz => {},
        .i64_popcnt => {},
        .i64_add => {},
        .i64_sub => {},
        .i64_mul => {},
        .i64_div_s => {},
        .i64_div_u => {},
        .i64_rem_s => {},
        .i64_rem_u => {},
        .i64_and => {},
        .i64_or => {},
        .i64_xor => {},
        .i64_shl => {},
        .i64_shr_s => {},
        .i64_shr_u => {},
        .i64_rotl => {},
        .i64_rotr => {},

        // numeric instructions (3) f32
        .f32_abs => {},
        .f32_neg => {},
        .f32_ceil => {},
        .f32_floor => {},
        .f32_trunc => {},
        .f32_nearest => {},
        .f32_sqrt => {},
        .f32_add => {},
        .f32_sub => {},
        .f32_mul => {},
        .f32_div => {},
        .f32_min => {},
        .f32_max => {},
        .f32_copy_sign => {},

        // numeric instructions (3) f64
        .f64_abs => {},
        .f64_neg => {},
        .f64_ceil => {},
        .f64_floor => {},
        .f64_trunc => {},
        .f64_nearest => {},
        .f64_sqrt => {},
        .f64_add => {},
        .f64_sub => {},
        .f64_mul => {},
        .f64_div => {},
        .f64_min => {},
        .f64_max => {},
        .f64_copy_sign => {},

        // numeric instructions (4)
        .i32_wrap_i64 => {},
        .i32_trunc_f32_s => {},
        .i32_trunc_f32_u => {},
        .i32_trunc_f64_s => {},
        .i32_trunc_f64_u => {},
        .i64_extend_i32_s => {},
        .i64_extend_i32_u => {},
        .i64_trunc_f32_s => {},
        .i64_trunc_f32_u => {},
        .i64_trunc_f64_s => {},
        .i64_trunc_f64_u => {},
        .f32_convert_i32_s => {},
        .f32_convert_i32_u => {},
        .f32_convert_i64_s => {},
        .f32_convert_i64_u => {},
        .f32_demote_f64 => {},
        .f64_convert_i32_s => {},
        .f64_convert_i32_u => {},
        .f64_convert_i64_s => {},
        .f64_convert_i64_u => {},
        .f64_promote_f32 => {},
        .i32_reinterpret_f32 => {},
        .i64_reinterpret_f64 => {},
        .f32_reinterpret_i32 => {},
        .f64_reinterpret_i64 => {},

        // numeric instructions (5)
        .i32_extend8_s => {},
        .i32_extend16_s => {},
        .i64_extend8_s => {},
        .i64_extend16_s => {},
        .i64_extend32_s => {},

        // saturating truncation instructions
        .i32_trunc_sat_f32_s => {},
        .i32_trunc_sat_f32_u => {},
        .i32_trunc_sat_f64_s => {},
        .i32_trunc_sat_f64_u => {},
        .i64_trunc_sat_f32_s => {},
        .i64_trunc_sat_f32_u => {},
        .i64_trunc_sat_f64_s => {},
        .i64_trunc_sat_f64_u => {},
    }
}

const TypeStack = struct {
    const Self = @This();

    const Stack = std.ArrayList(types.ValueType);

    array: Stack,
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        return .{
            .array = Stack.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn setPolymophic(self: *Self) error{OutOfMemory}!void {
        try self.array.resize(0);
    }
};
