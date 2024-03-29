const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("./types.zig");
};
const TypeStack = types.TypeStack;
const Context = @import("./context.zig").Context;
const Error = @import("./errors.zig").Error || error{OutOfMemory};

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
        { // under the context c
            const c = try Context.new(module, self.allocator);
            for (module.funcs) |func|
                try self.validateFunction(c, func);

            if (module.start) |idx|
                try validateStartFunction(c, idx);

            for (module.imports) |imp|
                try validateImport(c, imp);

            for (module.exports) |exp|
                try validateExport(c, exp);

            if (c.mems.len > 1)
                return Error.MultipleMemories;
        }

        { // under the context c'
            const cp = try Context.newLimitedContext(module, self.allocator);

            for (module.tables) |table|
                try validateTableType(table);

            for (module.memories) |memory|
                try validateMemoryType(memory);

            for (module.globals) |global|
                try validateGlobal(cp, global);

            for (module.elements) |elem|
                try validateElement(cp, elem);

            for (module.datas) |data|
                try validateData(cp, data);
        }

        // All export names must be different
        var map = std.StringHashMap(bool).init(self.allocator);
        defer map.deinit();
        for (module.exports) |exp| {
            if (try map.fetchPut(exp.name, true)) |_|
                return Error.DuplicateExportName;
        }
    }

    fn validateFunction(self: Self, c: Context, func: types.Func) Error!void {
        // std.debug.print("=" ** 40 ++ " {any} \n", .{c.types[func.type]});
        const cp = try Context.cloneWithFunction(c, func, self.allocator);
        const ty = try c.getType(func.type);
        try self.validateFunctionBody(cp, func.body, ty.result_types);
    }

    fn validateFunctionBody(self: Self, c: Context, instrs: []const types.Instruction, expect_types: []const types.ValueType) Error!void {
        var type_stack = try TypeStack.new(self.allocator);
        try self.loop(c, instrs, 0, @intCast(instrs.len), &type_stack);

        try type_stack.popValuesWithCheckingValueType(expect_types);
        if (!type_stack.isEmpty())
            return Error.TypeMismatch;
    }

    fn validateBlock(self: Self, c: Context, instrs: []const types.Instruction, start: u32, end: u32, func_type: types.FuncType) Error!void {
        var type_stack = try TypeStack.new(self.allocator);
        try type_stack.appendValueType(func_type.parameter_types);

        try self.loop(c, instrs, start, end, &type_stack);

        try type_stack.popValuesWithCheckingValueType(func_type.result_types);
        if (!type_stack.isEmpty())
            return Error.TypeMismatch;
    }

    fn loop(self: Self, c: Context, instrs: []const types.Instruction, start: u32, end: u32, type_stack: *TypeStack) Error!void {
        var ip = start;
        while (ip < end)
            ip = try self.validateInstruction(c, instrs, ip, type_stack);
    }

    fn validateBlocktype(self: Self, c: Context, block_type: types.Instruction.BlockType) Error!types.FuncType {
        return switch (block_type) {
            .empty => .{ .parameter_types = &.{}, .result_types = &.{} },
            .type_index => |idx| try c.getType(idx),
            .value_type => blk: {
                const result_types = try self.allocator.alloc(types.ValueType, 1);
                result_types[0] = block_type.value_type;
                break :blk .{ .parameter_types = &.{}, .result_types = result_types };
            },
        };
    }

    fn validateInstruction(self: Self, c: Context, instrs: []const types.Instruction, ip: u32, type_stack: *TypeStack) Error!u32 {
        assert(ip < instrs.len);
        //std.debug.print("[{}] {any}   label: {any} stack: {any} {s}\n", .{ ip, instrs[ip], c.labels, type_stack.array.items, if (type_stack.polymophic) "polymophic" else "" });
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
                try type_stack.popValuesWithCheckingValueType(func_type.parameter_types);
                try type_stack.appendValueType(func_type.result_types);
                return block_info.end + 1;
            },
            .loop => |block_info| {
                const func_type = try self.validateBlocktype(c, block_info.type);
                const cp = try Context.cloneWithPrependingLabel(c, func_type.parameter_types, self.allocator);
                try self.validateBlock(cp, instrs, ip + 1, block_info.end + 1, func_type);
                try type_stack.popValuesWithCheckingValueType(func_type.parameter_types);
                try type_stack.appendValueType(func_type.result_types);
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
                    try self.validateBlock(cp, instrs, block_info.end, block_info.end + 1, func_type); // `end` only
                }
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popValuesWithCheckingValueType(func_type.parameter_types);
                try type_stack.appendValueType(func_type.result_types);
                return block_info.end + 1;
            },
            .br => |label_idx| {
                try type_stack.popValuesWithCheckingValueType(try c.getLabel(label_idx));
                try type_stack.setPolymophic();
            },
            .br_if => |label_idx| {
                try type_stack.popWithCheckingValueType(.i32);
                const label = try c.getLabel(label_idx);
                try type_stack.popValuesWithCheckingValueType(label);
                try type_stack.appendValueType(label);
            },
            .br_table => |table_info| {
                const default_label = try c.getLabel(table_info.default_label_idx);
                for (table_info.label_idxs) |label_idx| {
                    const label = try c.getLabel(label_idx);
                    if (label.len != default_label.len)
                        return Error.TypeMismatch;

                    try type_stack.popWithCheckingValueType(.i32);
                    try type_stack.popValuesWithCheckingValueType(label);
                    try type_stack.setPolymophic();
                }

                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popValuesWithCheckingValueType(default_label);
                try type_stack.setPolymophic();
            },
            .@"return" => {
                if (c.@"return") |ty| {
                    try type_stack.popValuesWithCheckingValueType(ty);
                    try type_stack.setPolymophic();
                } else {
                    return Error.TypeMismatch;
                }
            },
            .call => |func_idx| {
                const ft = try c.getFunc(func_idx);
                try type_stack.popValuesWithCheckingValueType(ft.parameter_types);
                try type_stack.appendValueType(ft.result_types);
            },
            .call_indirect => |arg| {
                const table = try c.getTable(arg.table_idx);
                if (table.ref_type != .funcref)
                    return Error.TypeMismatch;

                try type_stack.popWithCheckingValueType(.i32);
                const ft = try c.getType(arg.type_idx);
                try type_stack.popValuesWithCheckingValueType(ft.parameter_types);
                try type_stack.appendValueType(ft.result_types);
            },

            // reference instructions
            .ref_null => |ref_type| try type_stack.pushValueType(valueTypeFromRefType(ref_type)),
            .ref_is_null => {
                _ = try type_stack.polymophicPop();
                try type_stack.pushValueType(.i32);
            },
            .ref_func => |func_idx| {
                _ = try c.getFunc(func_idx);
                try c.checkRef(func_idx);
                try type_stack.pushValueType(.func_ref);
            },

            // parametric instructions
            .drop => _ = try type_stack.polymophicPop(),
            .select => {
                try type_stack.popWithCheckingValueType(.i32);
                const t = try type_stack.polymophicPop();
                if (t == .func_ref or t == .extern_ref)
                    return Error.TypeMismatch;
                try type_stack.popWithChecking(t);
                try type_stack.push(t);
            },
            .selectv => |v| {
                if (v.len != 1)
                    return Error.InvalidResultArity;
                const t = v[0];
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(t);
                try type_stack.popWithCheckingValueType(t);
                try type_stack.pushValueType(t);
            },

            // variable instructions
            .local_get => |local_idx| {
                try type_stack.pushValueType(try c.getLocal(local_idx));
            },
            .local_set => |local_idx| {
                try type_stack.popWithCheckingValueType(try c.getLocal(local_idx));
            },
            .local_tee => |local_idx| {
                const local = try c.getLocal(local_idx);
                try type_stack.popWithCheckingValueType(local);
                try type_stack.pushValueType(local);
            },
            .global_get => |global_idx| {
                const global = try c.getGlobal(global_idx);
                try type_stack.pushValueType(global.value_type);
            },
            .global_set => |global_idx| {
                const global = try c.getGlobal(global_idx);
                if (global.mutability == .immutable)
                    return Error.ImmutableGlobal;
                try type_stack.popWithCheckingValueType(global.value_type);
            },

            // table instructions
            .table_get => |table_idx| {
                const table = try c.getTable(table_idx);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.pushValueType(valueTypeFromRefType(table.ref_type));
            },
            .table_set => |table_idx| {
                const table = try c.getTable(table_idx);
                try type_stack.popWithCheckingValueType(valueTypeFromRefType(table.ref_type));
                try type_stack.popWithCheckingValueType(.i32);
            },
            .table_init => |arg| {
                const table = try c.getTable(arg.table_idx);
                const elem = try c.getElem(arg.elem_idx);
                if (table.ref_type != elem)
                    return Error.TypeMismatch;
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
            },
            .elem_drop => |elem_idx| _ = try c.getElem(elem_idx),
            .table_copy => |arg| {
                const table_src = try c.getTable(arg.table_idx_src);
                const table_dst = try c.getTable(arg.table_idx_dst);
                if (table_src.ref_type != table_dst.ref_type)
                    return Error.TypeMismatch;
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
            },
            .table_grow => |table_idx| {
                const table = try c.getTable(table_idx);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(valueTypeFromRefType(table.ref_type));
                try type_stack.pushValueType(.i32);
            },
            .table_size => |table_idx| {
                _ = try c.getTable(table_idx);
                try type_stack.pushValueType(.i32);
            },
            .table_fill => |table_idx| {
                const table = try c.getTable(table_idx);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(valueTypeFromRefType(table.ref_type));
                try type_stack.popWithCheckingValueType(.i32);
            },

            // memory instructions
            .i32_load => |mem_arg| try opLoad(i32, i32, mem_arg, type_stack, c),
            .i64_load => |mem_arg| try opLoad(i64, i64, mem_arg, type_stack, c),
            .f32_load => |mem_arg| try opLoad(f32, f32, mem_arg, type_stack, c),
            .f64_load => |mem_arg| try opLoad(f64, f64, mem_arg, type_stack, c),
            .i32_load8_s => |mem_arg| try opLoad(i32, i8, mem_arg, type_stack, c),
            .i32_load8_u => |mem_arg| try opLoad(i32, u8, mem_arg, type_stack, c),
            .i32_load16_s => |mem_arg| try opLoad(i32, i16, mem_arg, type_stack, c),
            .i32_load16_u => |mem_arg| try opLoad(i32, u16, mem_arg, type_stack, c),
            .i64_load8_s => |mem_arg| try opLoad(i64, i8, mem_arg, type_stack, c),
            .i64_load8_u => |mem_arg| try opLoad(i64, u8, mem_arg, type_stack, c),
            .i64_load16_s => |mem_arg| try opLoad(i64, i16, mem_arg, type_stack, c),
            .i64_load16_u => |mem_arg| try opLoad(i64, u16, mem_arg, type_stack, c),
            .i64_load32_s => |mem_arg| try opLoad(i64, i32, mem_arg, type_stack, c),
            .i64_load32_u => |mem_arg| try opLoad(i64, u32, mem_arg, type_stack, c),
            .i32_store => |mem_arg| try opStore(i32, 32, mem_arg, type_stack, c),
            .i64_store => |mem_arg| try opStore(i64, 64, mem_arg, type_stack, c),
            .f32_store => |mem_arg| try opStore(f32, 32, mem_arg, type_stack, c),
            .f64_store => |mem_arg| try opStore(f64, 64, mem_arg, type_stack, c),
            .i32_store8 => |mem_arg| try opStore(i32, 8, mem_arg, type_stack, c),
            .i32_store16 => |mem_arg| try opStore(i32, 16, mem_arg, type_stack, c),
            .i64_store8 => |mem_arg| try opStore(i64, 8, mem_arg, type_stack, c),
            .i64_store16 => |mem_arg| try opStore(i64, 16, mem_arg, type_stack, c),
            .i64_store32 => |mem_arg| try opStore(i64, 32, mem_arg, type_stack, c),
            .memory_size => {
                try c.checkMem(0);
                try type_stack.pushValueType(.i32);
            },
            .memory_grow => {
                try c.checkMem(0);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.pushValueType(.i32);
            },
            .memory_init => |data_idx| {
                try c.checkMem(0);
                try c.checkData(data_idx);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
            },
            .data_drop => |data_idx| {
                try c.checkData(data_idx);
            },
            .memory_copy => {
                try c.checkMem(0);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
            },
            .memory_fill => {
                try c.checkMem(0);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
                try type_stack.popWithCheckingValueType(.i32);
            },

            // numeric instructions (1)
            .i32_const => try type_stack.pushValueType(.i32),
            .i64_const => try type_stack.pushValueType(.i64),
            .f32_const => try type_stack.pushValueType(.f32),
            .f64_const => try type_stack.pushValueType(.f64),

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

inline fn exp2(n: u32) error{NegativeNumberAlignment}!u32 {
    return if (n < 32)
        @as(u32, 1) << @intCast(n)
    else
        Error.NegativeNumberAlignment;
}

inline fn opLoad(comptime T: type, comptime N: type, mem_arg: types.Instruction.MemArg, type_stack: *TypeStack, c: Context) Error!void {
    if (try exp2(mem_arg.@"align") > @bitSizeOf(N) / 8)
        return Error.NegativeNumberAlignment;
    try c.checkMem(0);

    try type_stack.popWithCheckingValueType(.i32);
    const t = valueTypeOf(T);
    try type_stack.pushValueType(t);
}

inline fn opStore(comptime T: type, comptime bit_size: u32, mem_arg: types.Instruction.MemArg, type_stack: *TypeStack, c: Context) Error!void {
    if (try exp2(mem_arg.@"align") > bit_size / 8)
        return Error.NegativeNumberAlignment;
    try c.checkMem(0);

    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.popWithCheckingValueType(.i32);
}

inline fn instrOp(comptime R: type, comptime T: type, type_stack: *TypeStack) Error!void {
    const r = valueTypeOf(R);
    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.pushValueType(r);
}

inline fn instrExtOp(comptime R: type, comptime T: type, comptime _: type, type_stack: *TypeStack) Error!void {
    try instrOp(R, T, type_stack);
}

const instrTryOp = instrOp;

inline fn unOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.pushValueType(t);
}

inline fn binOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.pushValueType(t);
}

inline fn testOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.pushValueType(.i32);
}

inline fn relOp(comptime T: type, type_stack: *TypeStack) Error!void {
    const t = valueTypeOf(T);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.popWithCheckingValueType(t);
    try type_stack.pushValueType(.i32);
}

inline fn cvtOp(comptime T2: type, comptime T1: type, type_stack: *TypeStack) Error!void {
    const t1 = valueTypeOf(T1);
    const t2 = valueTypeOf(T2);
    try type_stack.popWithCheckingValueType(t1);
    try type_stack.pushValueType(t2);
}

fn validateImport(c: Context, imp: types.Import) Error!void {
    switch (imp.desc) {
        .function => |idx| _ = try c.getType(idx),
        .table => |ty| try validateTableType(ty),
        .memory => |ty| try validateMemoryType(ty),
        .global => {},
    }
}

fn validateExport(c: Context, exp: types.Export) Error!void {
    switch (exp.desc) {
        .function => |idx| _ = try c.getFunc(idx),
        .table => |idx| _ = try c.getTable(idx),
        .memory => |idx| try c.checkMem(idx),
        .global => |idx| _ = try c.getGlobal(idx),
    }
}

fn validateGlobal(c: Context, global: types.Global) Error!void {
    try validateInitExpression(c, global.init, global.type.value_type);
}

fn validateElement(c: Context, element: types.Element) Error!void {
    for (element.init) |init|
        try validateInitExpression(c, init, valueTypeFromRefType(element.type));

    switch (element.mode) {
        .active => |eat| {
            try validateInitExpression(c, eat.offset, .i32);

            const tt = try c.getTable(eat.table_idx);
            if (element.type != tt.ref_type)
                return Error.TypeMismatch;
        },
        else => {},
    }
}

fn validateData(c: Context, data: types.Data) Error!void {
    switch (data.mode) {
        .active => |dat| {
            try c.checkMem(dat.mem_idx);
            try validateInitExpression(c, dat.offset, .i32);
        },
        else => {},
    }
}

fn validateMemory(c: Context, global: types.Global) Error!void {
    try validateInitExpression(c, global.init, global.type.value_type);
}

fn validateTableType(table_type: types.TableType) Error!void {
    try validateLimits(std.math.maxInt(u32), table_type.limits);
}

fn validateMemoryType(memory_type: types.MemoryType) Error!void {
    try validateLimits(1 << 16, memory_type.limits);
}

fn validateLimits(comptime limit_max: u32, limits: types.Limits) Error!void {
    if (limits.min > limit_max)
        return Error.MemorySizeExceeded;

    if (limits.max) |max| {
        if (max > limit_max)
            return Error.MemorySizeExceeded;
        if (max < limits.min)
            return Error.MemorySizeExceeded;
    }
}

fn validateStartFunction(c: Context, func_idx: types.FuncIdx) Error!void {
    const ft = try c.getFunc(func_idx);
    if (ft.parameter_types.len != 0 or ft.result_types.len != 0)
        return Error.StartFunction;
}

fn validateInitExpression(c: Context, init_expr: types.InitExpression, expected_type: types.ValueType) Error!void {
    const ok = switch (init_expr) {
        .i32_const => expected_type == .i32,
        .i64_const => expected_type == .i64,
        .f32_const => expected_type == .f32,
        .f64_const => expected_type == .f64,
        .v128_const => expected_type == .v128,
        .ref_null => |t| switch (t) {
            .funcref => expected_type == .func_ref,
            .externref => expected_type == .extern_ref,
        },
        .ref_func => |idx| blk: {
            _ = try c.getFunc(idx);
            try c.checkRef(idx);
            break :blk expected_type == .func_ref;
        },
        .global_get => |idx| blk: {
            const g = try c.getGlobal(idx);
            break :blk g.value_type == expected_type;
        },
    };

    if (!ok)
        return Error.TypeMismatch;
}

fn valueTypeOf(comptime ty: type) types.ValueType {
    return switch (ty) {
        i32 => .i32,
        u32 => .i32,
        i64 => .i64,
        u64 => .i64,
        f32 => .f32,
        f64 => .f64,
        else => unreachable,
    };
}

fn valueTypeFromRefType(ref_type: types.RefType) types.ValueType {
    return @enumFromInt(@intFromEnum(ref_type));
}
