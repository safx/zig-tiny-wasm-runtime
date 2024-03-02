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

        try type_stack.popValuesWithTypeCheck(expect_types);
    }

    fn validateBlock(self: Self, c: Context, instrs: []const types.Instruction, start: u32, end: u32, func_type: types.FuncType) Error!void {
        var type_stack = try TypeStack.new(self.allocator);
        for (func_type.parameter_types) |ty|
            try type_stack.push(ty);

        try self.loop(c, instrs, ip, end, &type_stack);

        std.debug.print(" R {any}\n", .{func_type.result_types});
        std.debug.print(" S {any}\n", .{type_stack.array.items});

        try type_stack.popValuesWithTypeCheck(func_type.result_types);
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
        std.debug.print("{} {any} {any}\n", .{ ip, instrs[ip], type_stack.array.items });
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
                try type_stack.popValuesWithTypeCheck(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .loop => |block_info| {
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.parameter_types, self.allocator);
                try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                try type_stack.popValuesWithTypeCheck(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .@"if" => |block_info| { // TODO
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.result_types, self.allocator);
                if (block_info.@"else") |els| {
                    try self.validateBlock(cp, instrs, ip + 1, els + 1, func_type);
                    try self.validateBlock(cp, instrs, els, block_info.end + 1, func_type);
                } else {
                    try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                }
                try type_stack.popValuesWithTypeCheck(func_type.parameter_types);
                try type_stack.append(func_type.result_types);
                return block_info.end + 1;
            },
            .br => |label_idx| {
                if (label_idx >= c.labels.len)
                    return Error.UnknownLabel;
                try type_stack.popValuesWithTypeCheck(c.labels[label_idx]);
                try type_stack.setPolymophic();
            },
            .br_if => |label_idx| {
                if (label_idx >= c.labels.len)
                    return Error.UnknownLabel;
                try type_stack.popValueWithTypeCheck(.i32);
                try type_stack.popValuesWithTypeCheck(c.labels[label_idx]);
                try type_stack.append(c.labels[label_idx]);
            },
            .br_table => |table_info| { // TODO
                _ = table_info;
            },
            .@"return" => {}, // TODO
            .call => |func_idx| {
                if (func_idx >= c.funcs.len)
                    return Error.UnknownFunction;
                const ft = c.funcs[func_idx];
                try type_stack.popValuesWithTypeCheck(ft.parameter_types);
                try type_stack.append(ft.result_types);
            },
            .call_indirect => |arg| {
                if (arg.table_idx >= c.tables.len)
                    return Error.UnknownTable;
                if (c.tables[arg.table_idx].ref_type != .funcref)
                    return Error.TypeMismatch;
                if (arg.type_idx >= c.types.len)
                    return Error.UnknownType;

                try type_stack.popValueWithTypeCheck(.i32);
                const ft = c.types[arg.type_idx];
                try type_stack.popValuesWithTypeCheck(ft.parameter_types);
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
                try type_stack.popValueWithTypeCheck(c.locals[local_idx]);
            },
            .local_tee => |local_idx| {
                if (local_idx >= c.locals.len)
                    return Error.UnknownLocal;
                try type_stack.popValueWithTypeCheck(c.locals[local_idx]);
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
                try type_stack.popValueWithTypeCheck(c.globals[global_idx].value_type);
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
        return ip + 1;
    }
};

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

    pub fn push(self: *Self, value_type: types.ValueType) error{OutOfMemory}!void {
        try self.array.append(value_type);
    }

    pub fn append(self: *Self, value_types: []const types.ValueType) error{OutOfMemory}!void {
        for (value_types) |v|
            try self.push(v);
    }

    pub fn pop(self: *Self) error{TypeMismatch}!types.ValueType {
        if (self.array.items.len == 0)
            return Error.TypeMismatch;
        return self.array.pop();
    }

    pub fn isEmpty(self: *Self) bool {
        return self.array.items.len == 0;
    }

    pub fn setPolymophic(self: *Self) error{OutOfMemory}!void {
        try self.array.resize(0);
    }

    pub fn popValueWithTypeCheck(self: *Self, value_type: types.ValueType) error{TypeMismatch}!void {
        const popped = try self.pop();
        if (popped != value_type)
            return Error.TypeMismatch;
    }

    pub fn popValuesWithTypeCheck(self: *Self, value_types: []const types.ValueType) error{TypeMismatch}!void {
        var i = value_types.len;
        while (i > 0) : (i -= 1) {
            try self.popValueWithTypeCheck(value_types[i - 1]);
        }
    }
};
