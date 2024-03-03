const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
};
const Context = @import("./context.zig").Context;
pub const Error = @import("./errors.zig").Error || error{OutOfMemory};

pub const ModuleValidator = struct {
    const Self = @This();
    const assert = std.debug.assert;

    baseAllocator: std.mem.Allocator,
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .baseAllocator = allocator,
            .allocator = allocator, // assigned temporarily
        };
    }

    pub fn validate(self: *Self, module: types.Module) Error!void {
        var arena = std.heap.ArenaAllocator.init(self.baseAllocator);
        self.allocator = arena.allocator();
        defer arena.deinit();

        try self.validateModule(module);
    }

    fn validateModule(self: Self, module: types.Module) Error!void {
        const context = try Context.new(module, self.allocator);
        for (module.funcs) |func| {
            try self.validateFunction(context, func);
        }
    }

    fn validateFunction(self: Self, c: Context, func: types.Func) Error!void {
        std.debug.print("=" ** 40 ++ "\n", .{});
        if (func.type >= c.types.len)
            return Error.UnknownType;

        const cp = try Context.cloneWithFunction(c, func, self.allocator);
        try self.validateFunctionBody(cp, func.body, c.types[func.type].result_types);
    }

    fn validateFunctionBody(self: Self, c: Context, instrs: []const types.Instruction, expect_types: []const types.ValueType) Error!void {
        var type_stack = try TypeStack.new(self.allocator);
        try self.loop(c, instrs, 0, @intCast(instrs.len), &type_stack);

        std.debug.print(" E {any}\n", .{expect_types});
        std.debug.print(" S {any}\n", .{type_stack.array.items});

        try type_stack.popValuesWithChecking(expect_types);
    }

    fn validateBlock(self: Self, c: Context, instrs: []const types.Instruction, start: u32, end: u32, func_type: types.FuncType) Error!void {
        var type_stack = try TypeStack.new(self.allocator);
        for (func_type.parameter_types) |ty|
            try type_stack.push(ty);

        try self.loop(c, instrs, start, end, &type_stack);

        std.debug.print(" R {any}\n", .{func_type.result_types});
        std.debug.print(" S {any}\n", .{type_stack.array.items});

        try type_stack.popValuesWithChecking(func_type.result_types);

        if (!type_stack.isEmpty())
            return Error.TypeMismatch;
    }

    fn loop(self: Self, c: Context, instrs: []const types.Instruction, start: u32, end: u32, type_stack: *TypeStack) Error!void {
        var ip = start;
        while (ip < end) {
            ip = try self.validateInstruction(c, instrs, ip, type_stack);
        }
    }

    fn validateBlocktype(self: Self, c: Context, block_type: types.Instruction.BlockType) Error!types.FuncType {
        switch (block_type) {
            .empty => return .{ .parameter_types = &.{}, .result_types = &.{} },
            .type_index => |idx| {
                if (idx >= c.types.len)
                    return Error.UnknownFunction;
                return c.types[idx];
            },
            .value_type => {
                const result_types = try self.allocator.alloc(types.ValueType, 1);
                result_types[0] = block_type.value_type;
                return .{ .parameter_types = &.{}, .result_types = result_types };
            },
        }
    }

    fn validateInstruction(self: Self, c: Context, instrs: []const types.Instruction, ip: u32, type_stack: *TypeStack) Error!u32 {
        assert(ip < instrs.len);
        std.debug.print("{} {any} {any} {s}\n", .{ ip, instrs[ip], type_stack.array.items, if (type_stack.polymophic) "polymophic" else "" });
        switch (instrs[ip]) {
            .end => {},
            .@"else" => {},

            // contronl instructions
            .nop => {},
            .@"unreachable" => try type_stack.setPolymophic(),
            .block => |block_info| {
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.result_types, self.allocator);
                try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                try type_stack.popValuesWithChecking(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .loop => |block_info| {
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.parameter_types, self.allocator);
                try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                try type_stack.popValuesWithChecking(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .@"if" => |block_info| {
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.result_types, self.allocator);
                if (block_info.@"else") |els| {
                    try self.validateBlock(cp, instrs, ip + 1, els + 1, func_type);
                    try self.validateBlock(cp, instrs, els, block_info.end + 1, func_type);
                } else {
                    try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                }
                try type_stack.popValuesWithChecking(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .br => |label_idx| {
                if (label_idx >= c.labels.len)
                    return Error.UnknownLabel;
                try type_stack.popValuesWithChecking(c.labels[label_idx]);
                try type_stack.setPolymophic();
            },
            .br_if => |label_idx| {
                if (label_idx >= c.labels.len)
                    return Error.UnknownLabel;
                try type_stack.popWithChecking(.i32);
                try type_stack.popValuesWithChecking(c.labels[label_idx]);
                try type_stack.append(c.labels[label_idx]);
            },
            .br_table => |table_info| {
                if (table_info.default_label_idx >= c.labels.len)
                    return Error.UnknownLabel;

                const default_label = c.labels[table_info.default_label_idx];
                for (table_info.label_idxs) |label_idx| {
                    if (label_idx >= c.labels.len)
                        return Error.UnknownLabel;

                    const label = c.labels[label_idx];
                    if (label.len != default_label.len)
                        return Error.TypeMismatch;

                    try type_stack.popWithChecking(.i32);
                    try type_stack.popValuesWithChecking(label);
                    try type_stack.setPolymophic();
                }

                try type_stack.popWithChecking(.i32);
                try type_stack.popValuesWithChecking(default_label);
                try type_stack.setPolymophic();
            },
            .@"return" => {
                if (c.@"return") |ty| {
                    try type_stack.popValuesWithChecking(ty);
                    try type_stack.setPolymophic();
                } else {
                    return Error.TypeMismatch;
                }
            },
            .call => |func_idx| {
                if (func_idx >= c.funcs.len)
                    return Error.UnknownFunction;
                const ft = c.funcs[func_idx];
                try type_stack.popValuesWithChecking(ft.parameter_types);
                try type_stack.append(ft.result_types);
            },
            .call_indirect => |arg| {
                if (arg.table_idx >= c.tables.len)
                    return Error.UnknownTable;
                if (c.tables[arg.table_idx].ref_type != .funcref)
                    return Error.TypeMismatch;
                if (arg.type_idx >= c.types.len)
                    return Error.UnknownType;

                try type_stack.popWithChecking(.i32);
                const ft = c.types[arg.type_idx];
                try type_stack.popValuesWithChecking(ft.parameter_types);
                try type_stack.append(ft.result_types);
            },

            // reference instructions
            .ref_null => |ref_type| { // TODO
                _ = ref_type;
            },
            .ref_is_null => {}, // TODO
            .ref_func => |func_idx| { // TODO
                _ = func_idx;
            },

            // parametric instructions
            .drop => _ = try type_stack.pop(),
            .select => {}, // TODO
            .selectv => {}, // TODO

            // variable instructions
            .local_get => |local_idx| {
                if (local_idx >= c.locals.len)
                    return Error.UnknownLocal;
                try type_stack.push(c.locals[local_idx]);
            },
            .local_set => |local_idx| {
                if (local_idx >= c.locals.len)
                    return Error.UnknownLocal;
                try type_stack.popWithChecking(c.locals[local_idx]);
            },
            .local_tee => |local_idx| {
                if (local_idx >= c.locals.len)
                    return Error.UnknownLocal;
                try type_stack.popWithChecking(c.locals[local_idx]);
                try type_stack.push(c.locals[local_idx]);
            },
            .global_get => |global_idx| {
                if (global_idx >= c.globals.len)
                    return Error.UnknownGlobal;
                try type_stack.push(c.globals[global_idx].value_type);
            },
            .global_set => |global_idx| {
                if (global_idx >= c.globals.len)
                    return Error.UnknownGlobal;
                try type_stack.popWithChecking(c.globals[global_idx].value_type);
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
            .i32_const => try type_stack.push(types.ValueType.i32),
            .i64_const => try type_stack.push(types.ValueType.i64),
            .f32_const => try type_stack.push(types.ValueType.f32),
            .f64_const => try type_stack.push(types.ValueType.f64),

            // numeric instructions (2) i32
            .i32_eqz => try testOp(i32, type_stack),
            .i32_eq => try relOp(i32, type_stack),
            .i32_ne => try relOp(i32, type_stack),
            .i32_lt_s => try relOp(i32, type_stack),
            .i32_lt_u => try relOp(u32, type_stack),
            .i32_gt_s => try relOp(i32, type_stack),
            .i32_gt_u => try relOp(u32, type_stack),
            .i32_le_s => try relOp(i32, type_stack),
            .i32_le_u => try relOp(u32, type_stack),
            .i32_ge_s => try relOp(i32, type_stack),
            .i32_ge_u => try relOp(u32, type_stack),

            // numeric instructions (2) i64
            .i64_eqz => try testOp(i64, type_stack),
            .i64_eq => try relOp(i64, type_stack),
            .i64_ne => try relOp(i64, type_stack),
            .i64_lt_s => try relOp(i64, type_stack),
            .i64_lt_u => try relOp(u64, type_stack),
            .i64_gt_s => try relOp(i64, type_stack),
            .i64_gt_u => try relOp(u64, type_stack),
            .i64_le_s => try relOp(i64, type_stack),
            .i64_le_u => try relOp(u64, type_stack),
            .i64_ge_s => try relOp(i64, type_stack),
            .i64_ge_u => try relOp(u64, type_stack),

            // numeric instructions (2) f32
            .f32_eq => try relOp(f32, type_stack),
            .f32_ne => try relOp(f32, type_stack),
            .f32_lt => try relOp(f32, type_stack),
            .f32_gt => try relOp(f32, type_stack),
            .f32_le => try relOp(f32, type_stack),
            .f32_ge => try relOp(f32, type_stack),

            // numeric instructions (2) f64
            .f64_eq => try relOp(f64, type_stack),
            .f64_ne => try relOp(f64, type_stack),
            .f64_lt => try relOp(f64, type_stack),
            .f64_gt => try relOp(f64, type_stack),
            .f64_le => try relOp(f64, type_stack),
            .f64_ge => try relOp(f64, type_stack),

            // numeric instructions (3) i32
            .i32_clz => try unOp(i32, type_stack),
            .i32_ctz => try unOp(i32, type_stack),
            .i32_popcnt => try unOp(i32, type_stack),
            .i32_add => try binOp(i32, type_stack),
            .i32_sub => try binOp(i32, type_stack),
            .i32_mul => try binOp(i32, type_stack),
            .i32_div_s => try binOp(i32, type_stack),
            .i32_div_u => try binOp(u32, type_stack),
            .i32_rem_s => try binOp(i32, type_stack),
            .i32_rem_u => try binOp(u32, type_stack),
            .i32_and => try binOp(i32, type_stack),
            .i32_or => try binOp(i32, type_stack),
            .i32_xor => try binOp(i32, type_stack),
            .i32_shl => try binOp(i32, type_stack),
            .i32_shr_s => try binOp(i32, type_stack),
            .i32_shr_u => try binOp(u32, type_stack),
            .i32_rotl => try binOp(i32, type_stack),
            .i32_rotr => try binOp(i32, type_stack),

            // numeric instructions (3) i64
            .i64_clz => try unOp(i64, type_stack),
            .i64_ctz => try unOp(i64, type_stack),
            .i64_popcnt => try unOp(i64, type_stack),
            .i64_add => try binOp(i64, type_stack),
            .i64_sub => try binOp(i64, type_stack),
            .i64_mul => try binOp(i64, type_stack),
            .i64_div_s => try binOp(i64, type_stack),
            .i64_div_u => try binOp(u64, type_stack),
            .i64_rem_s => try binOp(i64, type_stack),
            .i64_rem_u => try binOp(u64, type_stack),
            .i64_and => try binOp(i64, type_stack),
            .i64_or => try binOp(i64, type_stack),
            .i64_xor => try binOp(i64, type_stack),
            .i64_shl => try binOp(i64, type_stack),
            .i64_shr_s => try binOp(i64, type_stack),
            .i64_shr_u => try binOp(u64, type_stack),
            .i64_rotl => try binOp(i64, type_stack),
            .i64_rotr => try binOp(i64, type_stack),

            // numeric instructions (3) f32
            .f32_abs => try unOp(f32, type_stack),
            .f32_neg => try unOp(f32, type_stack),
            .f32_ceil => try unOp(f32, type_stack),
            .f32_floor => try unOp(f32, type_stack),
            .f32_trunc => try unOp(f32, type_stack),
            .f32_nearest => try unOp(f32, type_stack),
            .f32_sqrt => try unOp(f32, type_stack),
            .f32_add => try binOp(f32, type_stack),
            .f32_sub => try binOp(f32, type_stack),
            .f32_mul => try binOp(f32, type_stack),
            .f32_div => try binOp(f32, type_stack),
            .f32_min => try binOp(f32, type_stack),
            .f32_max => try binOp(f32, type_stack),
            .f32_copy_sign => try binOp(f32, type_stack),

            // numeric instructions (3) f64
            .f64_abs => try unOp(f64, type_stack),
            .f64_neg => try unOp(f64, type_stack),
            .f64_ceil => try unOp(f64, type_stack),
            .f64_floor => try unOp(f64, type_stack),
            .f64_trunc => try unOp(f64, type_stack),
            .f64_nearest => try unOp(f64, type_stack),
            .f64_sqrt => try unOp(f64, type_stack),
            .f64_add => try binOp(f64, type_stack),
            .f64_sub => try binOp(f64, type_stack),
            .f64_mul => try binOp(f64, type_stack),
            .f64_div => try binOp(f64, type_stack),
            .f64_min => try binOp(f64, type_stack),
            .f64_max => try binOp(f64, type_stack),
            .f64_copy_sign => try binOp(f64, type_stack),

            // numeric instructions (4)
            .i32_wrap_i64 => try instrOp(u32, u64, type_stack),
            .i32_trunc_f32_s => try instrTryOp(i32, f32, type_stack),
            .i32_trunc_f32_u => try instrTryOp(u32, f32, type_stack),
            .i32_trunc_f64_s => try instrTryOp(i32, f64, type_stack),
            .i32_trunc_f64_u => try instrTryOp(u32, f64, type_stack),
            .i64_extend_i32_s => try instrExtOp(i64, i32, i32, type_stack),
            .i64_extend_i32_u => try instrExtOp(u64, u32, u32, type_stack),
            .i64_trunc_f32_s => try instrTryOp(i64, f32, type_stack),
            .i64_trunc_f32_u => try instrTryOp(u64, f32, type_stack),
            .i64_trunc_f64_s => try instrTryOp(i64, f64, type_stack),
            .i64_trunc_f64_u => try instrTryOp(u64, f64, type_stack),
            .f32_convert_i32_s => try instrOp(f32, i32, type_stack),
            .f32_convert_i32_u => try instrOp(f32, u32, type_stack),
            .f32_convert_i64_s => try instrOp(f32, i64, type_stack),
            .f32_convert_i64_u => try instrOp(f32, u64, type_stack),
            .f32_demote_f64 => try instrOp(f32, f64, type_stack),
            .f64_convert_i32_s => try instrOp(f64, i32, type_stack),
            .f64_convert_i32_u => try instrOp(f64, u32, type_stack),
            .f64_convert_i64_s => try instrOp(f64, i64, type_stack),
            .f64_convert_i64_u => try instrOp(f64, u64, type_stack),
            .f64_promote_f32 => try instrOp(f64, f32, type_stack),
            .i32_reinterpret_f32 => try instrOp(i32, f32, type_stack),
            .i64_reinterpret_f64 => try instrOp(i64, f64, type_stack),
            .f32_reinterpret_i32 => try instrOp(f32, i32, type_stack),
            .f64_reinterpret_i64 => try instrOp(f64, i64, type_stack),

            // numeric instructions (5)
            .i32_extend8_s => try instrExtOp(i32, i32, i8, type_stack),
            .i32_extend16_s => try instrExtOp(i32, i32, i16, type_stack),
            .i64_extend8_s => try instrExtOp(i64, i64, i8, type_stack),
            .i64_extend16_s => try instrExtOp(i64, i64, i16, type_stack),
            .i64_extend32_s => try instrExtOp(i64, i64, i32, type_stack),

            // saturating truncation instructions
            .i32_trunc_sat_f32_s => try cvtOp(i32, f32, type_stack),
            .i32_trunc_sat_f32_u => try cvtOp(u32, f32, type_stack),
            .i32_trunc_sat_f64_s => try cvtOp(i32, f64, type_stack),
            .i32_trunc_sat_f64_u => try cvtOp(u32, f64, type_stack),
            .i64_trunc_sat_f32_s => try cvtOp(i64, f32, type_stack),
            .i64_trunc_sat_f32_u => try cvtOp(u64, f32, type_stack),
            .i64_trunc_sat_f64_s => try cvtOp(i64, f64, type_stack),
            .i64_trunc_sat_f64_u => try cvtOp(u64, f64, type_stack),
        }
        return ip + 1;
    }
};

const TypeStack = struct {
    const Self = @This();
    const Stack = std.ArrayList(types.ValueType);

    array: Stack,
    polymophic: bool = false,
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) error{OutOfMemory}!Self {
        return .{
            .array = Stack.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn push(self: *Self, value_type: types.ValueType) error{OutOfMemory}!void {
        try self.array.append(value_type);
    }

    pub fn append(self: *Self, value_types: []const types.ValueType) error{OutOfMemory}!void {
        for (value_types) |v|
            try self.push(v);
    }

    pub fn pop(self: *Self) error{TypeMismatch}!types.ValueType {
        if (self.isEmpty())
            return Error.TypeMismatch;
        return self.array.pop();
    }

    pub fn isEmpty(self: *Self) bool {
        return self.array.items.len == 0;
    }

    pub fn setPolymophic(self: *Self) error{OutOfMemory}!void {
        try self.array.resize(0);
        self.polymophic = true;
    }

    pub fn popWithChecking(self: *Self, value_type: types.ValueType) error{TypeMismatch}!void {
        if (self.polymophic)
            return;

        const popped = try self.pop();
        if (popped != value_type)
            return Error.TypeMismatch;
    }

    pub fn popValuesWithChecking(self: *Self, value_types: []const types.ValueType) error{TypeMismatch}!void {
        var i = value_types.len;
        while (i > 0) : (i -= 1) {
            try self.popWithChecking(value_types[i - 1]);
        }
    }
};

inline fn instrOp(comptime R: type, comptime T: type, type_stack: *TypeStack) Error!void {
    const r = valueTypeFrom(R);
    const t = valueTypeFrom(T);
    try type_stack.popWithChecking(t);
    try type_stack.push(r);
}

inline fn instrExtOp(comptime R: type, comptime T: type, comptime _: type, type_stack: *TypeStack) Error!void {
    try instrOp(R, T, type_stack);
}

const instrTryOp = instrOp;

inline fn unOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeFrom(T);
    try type_stack.popWithChecking(t);
    try type_stack.push(t);
}

inline fn binOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeFrom(T);
    try type_stack.popWithChecking(t);
    try type_stack.popWithChecking(t);
    try type_stack.push(t);
}

inline fn testOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeFrom(T);
    try type_stack.popWithChecking(t);
    try type_stack.push(.i32);
}

inline fn relOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeFrom(T);
    try type_stack.popWithChecking(t);
    try type_stack.popWithChecking(t);
    try type_stack.push(.i32);
}

inline fn cvtOp(comptime T2: type, comptime T1: type, type_stack: *TypeStack) Error!void {
    const t1 = valueTypeFrom(T1);
    const t2 = valueTypeFrom(T2);
    try type_stack.popWithChecking(t1);
    try type_stack.push(t2);
}

fn valueTypeFrom(comptime ty: type) types.ValueType {
    return switch (ty) {
        i32 => .i32,
        i64 => .i64,
        f32 => .f32,
        f64 => .f64,
        else => unreachable,
    };
}