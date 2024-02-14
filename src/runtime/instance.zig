const std = @import("std");
const wa = @import("wasm-core");
const Instruction = wa.Instruction;
const decode = @import("wasm-decode");
pub const types = @import("./types.zig");
pub const Error = @import("./errors.zig").Error;

pub const page_size = 65_536;

/// A type of WebAssembly instance
pub const Instance = struct {
    const Self = @This();
    const assert = std.debug.assert;
    const ModInstList = std.SinglyLinkedList(types.ModuleInst);

    allocator: std.mem.Allocator,
    modules: ModInstList,
    store: types.Store,
    stack: types.Stack,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .modules = .{},
            .store = types.Store.new(allocator),
            .stack = types.Stack.new(allocator),
        };
    }

    /// `Invocation of function address` and `Returning from a function`
    /// https://webassembly.github.io/spec/core/exec/instructions.html#function-calls
    fn invokeFunction(self: *Self, func_addr: types.FuncAddr) (Error || error{OutOfMemory})!void {
        // 1:

        // 2, 3:
        const func_inst = self.store.funcs.items[func_addr];
        const func_type = func_inst.type;

        // 4:
        const p_len = func_type.parameter_types.len;
        const num_locals = p_len + func_inst.code.locals.len;
        const locals = try self.allocator.alloc(types.Value, num_locals);
        @memset(locals[p_len..num_locals], .{ .i64 = 0 }); // TODO: should be assign actual type?

        // 6: assert

        // 7:
        var i = p_len;
        while (i > 0) : (i -= 1) {
            locals[i - 1] = self.stack.pop().value;
        }

        // 8, 9, 10:
        const num_returns = func_type.result_types.len;
        const frame = types.ActivationFrame{
            .locals = locals,
            .arity = num_returns,
            .module = func_inst.module,
            .instructions = func_inst.code.body,
        };
        try self.stack.push(.{ .frame = frame });
        try self.stack.push(.{ .label = .{ .arity = @intCast(num_returns), .type = .root } });

        // 5, 11: is done by execLoop
        std.debug.print("---------------\n", .{});
        for (func_inst.code.body, 0..) |op, idx| {
            std.debug.print("[{}] {}\n", .{ idx, op });
        }
        std.debug.print("---------------\n", .{});
    }

    fn returnFunction(self: *Self) (Error || error{OutOfMemory})![]const types.Value {
        const num_returns = self.stack.topFrame().arity;

        // 1, 2, 3: assert

        // 4: pop frames etc
        const ret = try self.allocator.alloc(types.Value, num_returns);
        var i = num_returns;
        while (i > 0) : (i -= 1) {
            ret[i - 1] = self.stack.pop().value;
        }

        // 5:

        // 6:
        self.stack.popValuesAndLabelsUntilFrame();

        // 7: push vals
        for (ret) |v| {
            try self.stack.push(.{ .value = v });
        }

        return ret;
    }

    pub fn invokeFunctionByAddr(self: *Self, func_addr: types.FuncAddr, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        // TODO: args check

        // TODO: push args
        for (args) |arg| {
            try self.stack.push(.{ .value = arg });
        }

        try self.invokeFunction(func_addr);
        const return_values = try self.execLoop(); // TODO

        // pop value : FIXME
        for (return_values) |_| {
            _ = self.stack.pop();
        }

        return return_values;
    }

    fn execLoop(self: *Self) (Error || error{OutOfMemory})![]const types.Value {
        while (true) {
            const instrs = self.stack.topFrame().instructions;
            const ip = self.stack.topFrame().ip;
            const instr = instrs[ip];

            self.printStack();
            std.debug.print("== [{}]: {}\n", .{ ip, instr });

            const flow_ctrl = try self.execInstruction(ip, instr);

            if (flow_ctrl != .none) {
                std.debug.print("\t===> {}\n", .{flow_ctrl});
            }

            switch (flow_ctrl) {
                .none => self.stack.updateTopFrameIp(ip + 1),
                .jump => |new_ip| self.stack.updateTopFrameIp(new_ip),
                .call => |func_addr| {
                    self.stack.updateTopFrameIp(ip + 1);
                    try self.invokeFunction(func_addr);
                },
                .exit => {
                    const ret = try self.returnFunction();
                    if (!self.stack.hasFrame()) {
                        return ret;
                    }
                },
            }
        }
        unreachable;
    }

    /// `instantiate` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn instantiate(self: *Self, module: wa.Module, extern_vals: []const types.ExternalValue) (Error || error{OutOfMemory})!*types.ModuleInst {
        // 1: validate
        // 2: assert
        // 3: check length
        // 4: verify external value
        // 5: ????

        // 6, 7: push init frame to stack
        var aux_module = try types.ModuleInst.auxiliaryInstance(&self.store, module, extern_vals, self.allocator);
        const aux_frame_init = types.ActivationFrame{
            .module = &aux_module,
        };
        try self.stack.push(.{ .frame = aux_frame_init });

        // 8: Get `val*`
        var vals = try self.allocator.alloc(types.Value, module.globals.len);
        for (module.globals, 0..) |global, i| {
            try self.execOneInstruction(instractionFromInitExpr(global.init));
            vals[i] = self.stack.pop().value;
        }

        // 9: Get `ref*`
        var refs = try self.allocator.alloc([]types.RefValue, module.elements.len);
        for (module.elements, 0..) |element, i| {
            var refs_i = try self.allocator.alloc(types.RefValue, element.init.len);
            refs[i] = refs_i;
            for (element.init, 0..) |e, j| {
                try self.execOneInstruction(instractionFromInitExpr(e));
                const value = self.stack.pop().value;
                const val: types.RefValue = switch (value) {
                    .func_ref => |v| .{ .func_ref = v },
                    .extern_ref => |v| .{ .extern_ref = v },
                    else => unreachable,
                };
                refs[i][j] = val;
            }
        }

        defer {
            self.allocator.free(vals);
            for (refs) |ref|
                self.allocator.free(ref);
            self.allocator.free(refs);
        }
        try self.store.funcs.resize(self.store.funcs.items.len - module.funcs.len); // purge funcs of aux_module

        // 10: pop init frame from stack
        _ = self.stack.pop();

        // 11: alloc module
        const mod_inst = try types.ModuleInst.allocateModule(&self.store, module, extern_vals, vals, refs, self.allocator);
        var node = ModInstList.Node{ .data = mod_inst.* };
        self.modules.prepend(&node);
        //std.debug.print("\n---ModInst\n{}\n", .{std.json.fmt(mod_inst, .{})});

        // 12, 13: push aux frame
        const aux_frame = types.ActivationFrame{
            .module = mod_inst,
        };
        try self.stack.push(.{ .frame = aux_frame });

        for (module.elements, 0..) |elem, i| {
            // 14: active elem
            switch (elem.mode) {
                .active => |active_type| {
                    const n = elem.init.len;
                    std.debug.print("======> {} {any}\n", .{ i, active_type });
                    try self.execOneInstruction(instractionFromInitExpr(active_type.offset));
                    try self.execOneInstruction(.{ .i32_const = 0 });
                    try self.execOneInstruction(.{ .i32_const = @intCast(n) });
                    try self.execOneInstruction(.{
                        .table_init = .{
                            .table_idx = active_type.table_idx,
                            .elem_idx = @intCast(i),
                        },
                    });
                    try self.execOneInstruction(.{ .elem_drop = @intCast(i) });
                },
                // 15: declarative elem
                .declarative => try self.execOneInstruction(.{ .elem_drop = @intCast(i) }),
                else => continue,
            }
        }

        // 16: data segment
        for (module.datas, 0..) |data, i| {
            switch (data.mode) {
                .active => |active_type| {
                    assert(active_type.mem_idx == 0);
                    const n = data.init.len;
                    try self.execOneInstruction(instractionFromInitExpr(active_type.offset));
                    try self.execOneInstruction(.{ .i32_const = 0 });
                    try self.execOneInstruction(.{ .i32_const = @intCast(n) });
                    try self.execOneInstruction(.{ .memory_init = @intCast(i) });
                    try self.execOneInstruction(.{ .data_drop = @intCast(i) });
                },
                else => continue,
            }
        }

        // 17: start function

        // 18: assert

        // 19: pop aux frame
        _ = self.stack.pop();

        return mod_inst;
    }

    fn printStack(self: *Self) void {
        // for (self.store.globals.items, 0..) |g, i| {
        //     std.debug.print("{}={any}, ", .{ i, g.value });
        // }
        // std.debug.print("\n", .{});

        const len = self.stack.array.items.len;
        const slice = if (len > 10) self.stack.array.items[len - 10 ..] else self.stack.array.items;
        if (len > 10) {
            std.debug.print("  : ({} more items)\n  :\n", .{len - 10});
        }
        for (slice) |i| {
            switch (i) {
                .value => |v| std.debug.print("  V {}\n", .{v}),
                .label => |v| std.debug.print("  L {}\n", .{v}),
                .frame => |v| std.debug.print("  F {any}\n", .{v.locals}),
            }
        }
    }

    /// executes an instruction without control flow
    fn execOneInstruction(self: *Self, instr: Instruction) (Error || error{OutOfMemory})!void {
        std.debug.print("== [_] {}\n", .{instr});
        _ = try self.execInstruction(0, instr);
    }

    /// executes an instruction and returns control flow
    fn execInstruction(self: *Self, ip: wa.InstractionAddr, instr: Instruction) (Error || error{OutOfMemory})!FlowControl {
        switch (instr) {
            .end => return self.opEnd(),
            .@"else" => return self.opElse(),

            // contronl instructions
            .nop => {},
            // .@"unreachable",
            .block => |block_info| try self.opBlock(block_info),
            .loop => |block_info| try self.opLoop(block_info, ip),
            .@"if" => |block_info| return self.opIf(block_info),
            .br => |label_idx| return self.opBr(label_idx),
            .br_if => |label_idx| return self.opBrIf(label_idx),
            .br_table => |table_info| return self.opBrTable(table_info),
            .@"return" => return FlowControl.exit,
            .call => |func_idx| return try self.opCall(func_idx),
            .call_indirect => |arg| return try self.opCallIndirect(arg),

            // reference instructions
            .ref_null => |ref_type| try self.opRefNull(ref_type),
            .ref_is_null => try self.opIsNull(),
            .ref_func => |func_idx| try self.opRefFunc(func_idx),

            // parametric instructions
            .drop => _ = self.stack.pop(),
            .select => try self.opSelect(),
            .selectv => try self.opSelect(),

            // variable instructions
            .local_get => |local_idx| try self.opLocalGet(local_idx),
            .local_set => |local_idx| self.opLocalSet(local_idx),
            .local_tee => |local_idx| try self.opLocalTee(local_idx),
            .global_get => |global_idx| try self.opGlobalGet(global_idx),
            .global_set => |global_idx| self.opGlobalSet(global_idx),

            // table instructions
            .table_get => |table_idx| try self.opTableGet(table_idx),
            .table_set => |table_idx| try self.opTableSet(table_idx),
            .table_init => |arg| try self.opTableInit(arg),
            .elem_drop => |elem_idx| self.opElemDrop(elem_idx),
            .table_copy => |arg| try self.opTableCopy(arg),
            .table_grow => |table_idx| try self.opTableGrow(table_idx),
            .table_size => |table_idx| try self.opTableSize(table_idx),
            .table_fill => |table_idx| try self.opTableFill(table_idx),

            // memory instructions
            .i32_load => |mem_arg| try self.opLoad(i32, i32, mem_arg),
            .i64_load => |mem_arg| try self.opLoad(i64, i64, mem_arg),
            .f32_load => |mem_arg| try self.opLoad(f32, f32, mem_arg),
            .f64_load => |mem_arg| try self.opLoad(f64, f64, mem_arg),
            .i32_load8_s => |mem_arg| try self.opLoad(i32, i8, mem_arg),
            .i32_load8_u => |mem_arg| try self.opLoad(i32, u8, mem_arg),
            .i32_load16_s => |mem_arg| try self.opLoad(i32, i16, mem_arg),
            .i32_load16_u => |mem_arg| try self.opLoad(i32, u16, mem_arg),
            .i64_load8_s => |mem_arg| try self.opLoad(i64, i8, mem_arg),
            .i64_load8_u => |mem_arg| try self.opLoad(i64, u8, mem_arg),
            .i64_load16_s => |mem_arg| try self.opLoad(i64, i16, mem_arg),
            .i64_load16_u => |mem_arg| try self.opLoad(i64, u16, mem_arg),
            .i64_load32_s => |mem_arg| try self.opLoad(i64, i32, mem_arg),
            .i64_load32_u => |mem_arg| try self.opLoad(i64, u32, mem_arg),
            .i32_store => |mem_arg| try self.opStore(i32, 32, mem_arg),
            .i64_store => |mem_arg| try self.opStore(i64, 64, mem_arg),
            .f32_store => |mem_arg| try self.opStore(i32, 32, mem_arg),
            .f64_store => |mem_arg| try self.opStore(i64, 64, mem_arg),
            .i32_store8 => |mem_arg| try self.opStore(i32, 8, mem_arg),
            .i32_store16 => |mem_arg| try self.opStore(i32, 16, mem_arg),
            .i64_store8 => |mem_arg| try self.opStore(i64, 8, mem_arg),
            .i64_store16 => |mem_arg| try self.opStore(i64, 16, mem_arg),
            .i64_store32 => |mem_arg| try self.opStore(i64, 32, mem_arg),
            .memory_size => try self.opMemorySize(),
            .memory_grow => try self.opMemoryGrow(),
            .memory_init => |data_idx| try self.opMemoryInit(data_idx),
            .data_drop => |data_idx| self.opDataDrop(data_idx),
            .memory_copy => try self.opMemoryGrow(),
            .memory_fill => try self.opMemoryFill(),

            // numeric instructions (1)
            .i32_const => |val| try self.stack.pushValue(val),
            .i64_const => |val| try self.stack.pushValue(val),
            .f32_const => |val| try self.stack.pushValue(val),
            .f64_const => |val| try self.stack.pushValue(val),

            // numeric instructions (2) i32
            .i32_eqz => try self.testOp(i32, opIntEqz),
            .i32_eq => try self.relOp(i32, opIntEq),
            .i32_ne => try self.relOp(i32, opIntNe),
            .i32_lt_s => try self.relOp(i32, opIntLt),
            .i32_lt_u => try self.relOp(u32, opIntLt),
            .i32_gt_s => try self.relOp(i32, opIntGt),
            .i32_gt_u => try self.relOp(u32, opIntGt),
            .i32_le_s => try self.relOp(i32, opIntLe),
            .i32_le_u => try self.relOp(u32, opIntLe),
            .i32_ge_s => try self.relOp(i32, opIntGe),
            .i32_ge_u => try self.relOp(u32, opIntGe),

            // numeric instructions (2) i64
            .i64_eqz => try self.testOp(i64, opIntEqz),
            .i64_eq => try self.relOp(i64, opIntEq),
            .i64_ne => try self.relOp(i64, opIntNe),
            .i64_lt_s => try self.relOp(i64, opIntLt),
            .i64_lt_u => try self.relOp(u64, opIntLt),
            .i64_gt_s => try self.relOp(i64, opIntGt),
            .i64_gt_u => try self.relOp(u64, opIntGt),
            .i64_le_s => try self.relOp(i64, opIntLe),
            .i64_le_u => try self.relOp(u64, opIntLe),
            .i64_ge_s => try self.relOp(i64, opIntGe),
            .i64_ge_u => try self.relOp(u64, opIntGe),

            // numeric instructions (2) f32
            .f32_eq => try self.relOp(f32, opFloatEq),
            .f32_ne => try self.relOp(f32, opFloatNe),
            .f32_lt => try self.relOp(f32, opFloatLt),
            .f32_gt => try self.relOp(f32, opFloatGt),
            .f32_le => try self.relOp(f32, opFloatLe),
            .f32_ge => try self.relOp(f32, opFloatGe),

            // numeric instructions (2) f64
            .f64_eq => try self.relOp(f64, opFloatEq),
            .f64_ne => try self.relOp(f64, opFloatNe),
            .f64_lt => try self.relOp(f64, opFloatLt),
            .f64_gt => try self.relOp(f64, opFloatGt),
            .f64_le => try self.relOp(f64, opFloatLe),
            .f64_ge => try self.relOp(f64, opFloatGe),

            // numeric instructions (3) i32
            .i32_clz => try self.unOp(i32, opIntClz),
            .i32_ctz => try self.unOp(i32, opIntCtz),
            .i32_popcnt => try self.unOp(i32, opIntPopcnt),
            .i32_add => try self.binOp(i32, opIntAdd),
            .i32_sub => try self.binOp(i32, opIntSub),
            .i32_mul => try self.binOp(i32, opIntMul),
            .i32_div_s => try self.binOp(i32, opIntDivS),
            .i32_div_u => try self.binOp(u32, opIntDivU),
            .i32_rem_s => try self.binOp(i32, opIntRemS),
            .i32_rem_u => try self.binOp(u32, opIntRemU),
            .i32_and => try self.binOp(i32, opIntAnd),
            .i32_or => try self.binOp(i32, opIntOr),
            .i32_xor => try self.binOp(i32, opIntXor),
            .i32_shl => try self.binOp(i32, opIntShl),
            .i32_shr_s => try self.binOp(i32, opIntShrS),
            .i32_shr_u => try self.binOp(u32, opIntShrU),
            .i32_rotl => try self.binOp(i32, opIntRotl),
            .i32_rotr => try self.binOp(i32, opIntRotr),

            // numeric instructions (3) i64
            .i64_clz => try self.unOp(i64, opIntClz),
            .i64_ctz => try self.unOp(i64, opIntCtz),
            .i64_popcnt => try self.unOp(i64, opIntPopcnt),
            .i64_add => try self.binOp(i64, opIntAdd),
            .i64_sub => try self.binOp(i64, opIntSub),
            .i64_mul => try self.binOp(i64, opIntMul),
            .i64_div_s => try self.binOp(i64, opIntDivS),
            .i64_div_u => try self.binOp(u64, opIntDivU),
            .i64_rem_s => try self.binOp(i64, opIntRemS),
            .i64_rem_u => try self.binOp(u64, opIntRemU),
            .i64_and => try self.binOp(i64, opIntAnd),
            .i64_or => try self.binOp(i64, opIntOr),
            .i64_xor => try self.binOp(i64, opIntXor),
            .i64_shl => try self.binOp(i64, opIntShl),
            .i64_shr_s => try self.binOp(i64, opIntShrS),
            .i64_shr_u => try self.binOp(u64, opIntShrU),
            .i64_rotl => try self.binOp(i64, opIntRotl),
            .i64_rotr => try self.binOp(i64, opIntRotr),

            // numeric instructions (3) f32
            .f32_abs => try self.unOp(f32, opFloatAbs),
            .f32_neg => try self.unOp(f32, opFloatNeg),
            .f32_ceil => try self.unOp(f32, opFloatCeil),
            .f32_floor => try self.unOp(f32, opFloatFloor),
            .f32_trunc => try self.unOp(f32, opFloatTrunc),
            .f32_nearest => try self.unOp(f32, opFloatNearest),
            .f32_sqrt => try self.unOp(f32, opFloatSqrt),
            .f32_add => try self.binOp(f32, opFloatAdd),
            .f32_sub => try self.binOp(f32, opFloatSub),
            .f32_mul => try self.binOp(f32, opFloatMul),
            .f32_div => try self.binOp(f32, opFloatDiv),
            .f32_min => try self.binOp(f32, opFloatMin),
            .f32_max => try self.binOp(f32, opFloatMax),
            .f32_copy_sign => try self.binOp(f32, opFloatCopySign),

            // numeric instructions (3) f64
            .f64_abs => try self.unOp(f64, opFloatAbs),
            .f64_neg => try self.unOp(f64, opFloatNeg),
            .f64_ceil => try self.unOp(f64, opFloatCeil),
            .f64_floor => try self.unOp(f64, opFloatFloor),
            .f64_trunc => try self.unOp(f64, opFloatTrunc),
            .f64_nearest => try self.unOp(f64, opFloatNearest),
            .f64_sqrt => try self.unOp(f64, opFloatSqrt),
            .f64_add => try self.binOp(f64, opFloatAdd),
            .f64_sub => try self.binOp(f64, opFloatSub),
            .f64_mul => try self.binOp(f64, opFloatMul),
            .f64_div => try self.binOp(f64, opFloatDiv),
            .f64_min => try self.binOp(f64, opFloatMin),
            .f64_max => try self.binOp(f64, opFloatMax),
            .f64_copy_sign => try self.binOp(f32, opFloatCopySign),

            // numeric instructions (4)
            .i32_wrap_i64 => try self.instrOp(i32, i64, opWrap),
            .i32_trunc_f32_s => try self.instrOp(i32, f32, opTrunc),
            .i32_trunc_f32_u => try self.instrOp(u32, f32, opTrunc),
            .i32_trunc_f64_s => try self.instrOp(i32, f64, opTrunc),
            .i32_trunc_f64_u => try self.instrOp(u32, f64, opTrunc),
            .i64_extend_i32_s => try self.instrOp(i64, i32, opExtend32),
            .i64_extend_i32_u => try self.instrOp(i64, i32, opExtend32),
            .i64_trunc_f32_s => try self.instrOp(i64, f32, opTrunc),
            .i64_trunc_f32_u => try self.instrOp(i64, f32, opTrunc),
            .i64_trunc_f64_s => try self.instrOp(i64, f64, opTrunc),
            .i64_trunc_f64_u => try self.instrOp(u64, f64, opTrunc),
            .f32_convert_i32_s => try self.instrOp(f32, i32, opConvert),
            .f32_convert_i32_u => try self.instrOp(f32, u32, opConvert),
            .f32_convert_i64_s => try self.instrOp(f32, i64, opConvert),
            .f32_convert_i64_u => try self.instrOp(f32, u64, opConvert),
            .f32_demote_f64 => try self.instrOp(f32, f64, opDemote),
            .f64_convert_i32_s => try self.instrOp(f64, i32, opConvert),
            .f64_convert_i32_u => try self.instrOp(f64, u32, opConvert),
            .f64_convert_i64_s => try self.instrOp(f64, i64, opConvert),
            .f64_convert_i64_u => try self.instrOp(f64, u64, opConvert),
            .f64_promote_f32 => try self.instrOp(f64, f32, opPromote),
            .i32_reinterpret_f32 => try self.instrOp(i32, f32, opReinterpret),
            .i64_reinterpret_f64 => try self.instrOp(i64, f64, opReinterpret),
            .f32_reinterpret_i32 => try self.instrOp(f32, i32, opReinterpret),
            .f64_reinterpret_i64 => try self.instrOp(f64, i64, opReinterpret),

            // numeric instructions (5)
            .i32_extend8_s => try self.instrOp(i32, i32, opExtend8),
            .i32_extend16_s => try self.instrOp(i32, i32, opExtend16),
            .i64_extend8_s => try self.instrOp(i64, i64, opExtend8),
            .i64_extend16_s => try self.instrOp(i64, i64, opExtend16),
            .i64_extend32_s => try self.instrOp(i64, i64, opExtend32),

            else => unreachable,
        }
        return .none;
    }

    // contronl instructions
    inline fn opEnd(self: *Self) FlowControl {
        const label = self.stack.popUppermostLabel().?;
        return FlowControl.newAtOpEnd(label);
    }

    inline fn opElse(self: *Self) FlowControl {
        const label = self.stack.getNthLabelFromTop(0);
        return FlowControl.newAtOpElse(label);
    }

    inline fn opBlock(self: *Self, block_info: Instruction.BlockInfo) error{OutOfMemory}!void {
        try self.insertLabel(block_info.type, .{ .block = block_info.end });
    }

    inline fn opLoop(self: *Self, block_info: Instruction.BlockInfo, ip: wa.InstractionAddr) error{OutOfMemory}!void {
        try self.insertLabel(block_info.type, .{ .loop = ip });
    }

    inline fn opIf(self: *Self, block_info: Instruction.IfBlockInfo) error{OutOfMemory}!FlowControl {
        const value = self.stack.pop().value.i32;
        try self.insertLabel(block_info.type, .{ .@"if" = block_info.end });
        return FlowControl.newAtOpIf(block_info, value);
    }

    inline fn insertLabel(self: *Self, block_type: Instruction.BlockType, label_type: types.LabelType) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const func_type = expandToFuncType(module, block_type);
        const label = types.StackItem{ .label = .{
            .arity = @intCast(func_type.result_types.len),
            .type = label_type,
        } };
        try self.stack.insertAt(func_type.parameter_types.len, label);
    }

    inline fn opBr(self: *Self, label_idx: wa.LabelIdx) error{OutOfMemory}!FlowControl {
        const label = self.stack.getNthLabelFromTop(label_idx);
        const vals = try self.stack.popValues(label.arity);

        // use copied slice for avoiding violation error in appendSlice
        const copies = try self.allocator.alloc(types.StackItem, vals.len);
        defer self.allocator.free(copies);
        @memcpy(copies, vals);

        for (0..label_idx + 1) |_| {
            self.stack.popValuesAndUppermostLabel();
        }
        try self.stack.appendSlice(copies);
        std.debug.print("== br \n", .{});
        self.printStack();

        return FlowControl.newAtOpBr(label);
    }

    inline fn opBrIf(self: *Self, label_idx: wa.LabelIdx) error{OutOfMemory}!FlowControl {
        const value = self.stack.pop().value;
        return if (value.i32 == 0) FlowControl.none else self.opBr(label_idx);
    }

    inline fn opBrTable(self: *Self, table_info: Instruction.BrTableType) error{OutOfMemory}!FlowControl {
        const value = self.stack.pop().value.i32;
        const pos: u32 = @bitCast(value);
        const label_idx = if (pos < table_info.label_idxs.len) table_info.label_idxs[pos] else table_info.default_label_idx;
        return self.opBr(label_idx);
    }

    inline fn opCall(self: *Self, func_idx: wa.FuncIdx) (Error || error{OutOfMemory})!FlowControl {
        const module = self.stack.topFrame().module;
        return .{ .call = module.func_addrs[func_idx] };
    }

    inline fn opCallIndirect(self: *Self, arg: Instruction.CallIndirectArg) (Error || error{OutOfMemory})!FlowControl {
        std.debug.print("call_indirect: {any}\n", .{arg});
        const module = self.stack.topFrame().module;
        const ta = module.table_addrs[arg.table_idx];
        const tab = self.store.tables.items[ta];

        const ft_expect = module.types[arg.type_idx];

        const i: u32 = @bitCast(self.stack.pop().value.asI32());
        if (i >= tab.elem.len)
            return Error.UndefinedElement;
        const rx = tab.elem[i];
        if (rx.isNull())
            return Error.UninitializedElement;
        const r = rx.func_ref.?;
        std.debug.print("===============================''''''''''''''''{} {any}\n", .{ i, tab.elem });
        std.debug.print("===============================''''''''''''''''{any} {}\n", .{ rx, r });
        const f = self.store.funcs.items[@intCast(r)];
        const ft_actual = f.type;
        if (ft_expect.parameter_types.len != ft_actual.parameter_types.len or ft_expect.result_types.len != ft_actual.result_types.len)
            return Error.IndirectCallTypeMismatch;

        for (ft_expect.parameter_types, ft_actual.parameter_types) |e, a| {
            if (e != a) return Error.IndirectCallTypeMismatch;
        }
        for (ft_expect.result_types, ft_actual.result_types) |e, a| {
            if (e != a) return Error.IndirectCallTypeMismatch;
        }

        return .{ .call = r };
    }

    // reference instructions
    inline fn opRefNull(self: *Self, ref_type: wa.RefType) error{OutOfMemory}!void {
        const val: types.Value = switch (ref_type) {
            .funcref => .{ .func_ref = null },
            .externref => .{ .extern_ref = null },
        };
        try self.stack.push(.{ .value = val });
    }

    inline fn opIsNull(self: *Self) error{OutOfMemory}!void {
        const val = self.stack.pop().value;
        const v: i32 = if (val.isNull()) 1 else 0;
        try self.stack.pushValueAs(i32, v);
    }

    inline fn opRefFunc(self: *Self, func_idx: wa.FuncIdx) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const a = module.func_addrs[func_idx];
        try self.stack.push(.{ .value = .{ .func_ref = a } });
    }

    // parametric instructions
    inline fn opSelect(self: *Self) error{OutOfMemory}!void {
        const c = self.stack.pop().value.asI32();
        const val2 = self.stack.pop();
        const val1 = self.stack.pop();
        if (c != 0) {
            try self.stack.push(val1);
        } else {
            try self.stack.push(val2);
        }
    }

    // variable instructions
    inline fn opLocalGet(self: *Self, local_idx: wa.LocalIdx) error{OutOfMemory}!void {
        const frame = self.stack.topFrame();
        const val = frame.locals[local_idx];
        try self.stack.push(.{ .value = val });
    }

    inline fn opLocalSet(self: *Self, local_idx: wa.LocalIdx) void {
        const frame = self.stack.topFrame();
        const val = self.stack.pop().value;
        frame.locals[local_idx] = val;
    }

    inline fn opLocalTee(self: *Self, local_idx: wa.LocalIdx) error{OutOfMemory}!void {
        _ = local_idx;
        const value = self.stack.pop();
        try self.stack.push(value);
        try self.stack.push(value);
    }
    inline fn opGlobalGet(self: *Self, global_idx: wa.GlobalIdx) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const a = module.global_addrs[global_idx];
        const glob = self.store.globals.items[a];
        try self.stack.push(.{ .value = glob.value });
    }
    inline fn opGlobalSet(self: *Self, global_idx: wa.GlobalIdx) void {
        const module = self.stack.topFrame().module;
        const a = module.global_addrs[global_idx];
        const value = self.stack.pop().value;
        self.store.globals.items[a].value = value;
    }

    // table instructions
    inline fn opTableGet(self: *Self, table_idx: wa.TableIdx) (error{OutOfBoundsTableAccess} || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const a = module.table_addrs[table_idx];
        const tab = self.store.tables.items[a];
        const i: u32 = @bitCast(self.stack.pop().value.asI32());

        if (i >= tab.elem.len)
            return Error.OutOfBoundsTableAccess;

        const val = tab.elem[i];
        try self.stack.push(.{ .value = types.Value.fromRefValue(val) });
    }

    inline fn opTableSet(self: *Self, table_idx: wa.TableIdx) error{OutOfBoundsTableAccess}!void {
        const module = self.stack.topFrame().module;
        const a = module.table_addrs[table_idx];
        const tab = self.store.tables.items[a];
        const val = self.stack.pop().value;
        const i: u32 = @bitCast(self.stack.pop().value.asI32());

        if (i >= tab.elem.len)
            return Error.OutOfBoundsTableAccess;

        tab.elem[i] = types.RefValue.fromValue(val);
    }

    fn opTableInit(self: *Self, arg: Instruction.TableInitArg) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const ta = module.table_addrs[arg.table_idx];
        const tab = self.store.tables.items[ta];
        const ea = module.elem_addrs[arg.elem_idx];
        const elem = self.store.elems.items[ea];
        var n = self.stack.pop().value.asI32();
        var s = self.stack.pop().value.asI32();
        var d = self.stack.pop().value.asI32();

        while (n > 0) : (n -= 1) {
            std.debug.print("=== s={}, d={}, n={}    E={}, T={}\n", .{ s, d, n, elem.elem.len, tab.elem.len });
            if (s + n > elem.elem.len or d + n > tab.elem.len)
                return Error.OutOfBoundsTableAccess;

            const ref = elem.elem[@intCast(s)];
            try self.stack.pushValueAs(i32, d);
            try self.stack.push(.{ .value = types.Value.fromRefValue(ref) });
            try self.execOneInstruction(.{ .table_set = arg.table_idx });
            d += 1;
            s += 1;
        }
    }

    inline fn opElemDrop(self: *Self, elem_idx: wa.ElemIdx) void {
        const module = self.stack.topFrame().module;
        const a = module.elem_addrs[elem_idx];
        self.store.elems.items[a].elem = &.{};
    }

    inline fn opTableCopy(self: *Self, arg: Instruction.TableCopyArg) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const ta_d = module.table_addrs[arg.table_idx_dst];
        const tab_d = self.store.tables.items[ta_d];
        const ta_s = module.table_addrs[arg.table_idx_src];
        const tab_s = self.store.tables.items[ta_s];

        var n = self.stack.pop().value.asI32();
        var s = self.stack.pop().value.asI32();
        var d = self.stack.pop().value.asI32();

        while (n > 0) : (n -= 1) {
            if (s + n > tab_d.elem.len or s + n > tab_s.elem.len)
                return Error.OutOfBoundsTableAccess;

            if (d <= s) {
                try self.stack.pushValueAs(i32, d);
                try self.stack.pushValueAs(i32, s);
                try self.execOneInstruction(.{ .table_get = arg.table_idx_src });
                try self.execOneInstruction(.{ .table_set = arg.table_idx_dst });
                d += 1;
                s += 1;
            } else {
                try self.stack.pushValueAs(i32, d + n - 1);
                try self.stack.pushValueAs(i32, s + n - 1);
                try self.execOneInstruction(.{ .table_get = arg.table_idx_src });
                try self.execOneInstruction(.{ .table_set = arg.table_idx_dst });
            }
        }
    }

    inline fn opTableGrow(self: *Self, table_idx: wa.TableIdx) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const ta = module.table_addrs[table_idx];
        const tab = self.store.tables.items[ta];

        var n = self.stack.pop().value.asI32();
        const val = self.stack.pop().value;

        if (tab.type.limit.max != null and @as(usize, @intCast(n)) + tab.elem.len > tab.type.limit.max.?) {
            try self.stack.pushValueAs(i32, -1);
            return;
        }

        const sz = tab.elem.len;

        const new_elem = try growtable(tab, n, types.RefValue.fromValue(val), self.allocator);
        self.store.tables.items[ta].elem = new_elem;
        self.store.tables.items[ta].type.limit.min = @intCast(new_elem.len);

        try self.stack.pushValueAs(i32, @as(i32, @intCast(sz)));
    }

    /// https://webassembly.github.io/spec/core/exec/modules.html#growing-tables
    inline fn growtable(table_inst: types.TableInst, n: i32, val: types.RefValue, allocator: std.mem.Allocator) error{OutOfMemory}![]types.RefValue {
        const data_len: i32 = @intCast(table_inst.elem.len);
        const len: i32 = data_len + n;
        if (len + n > 65536) {
            return std.mem.Allocator.Error.OutOfMemory; // TODO: use another error
        }
        const old_elem = table_inst.elem;
        const new_elem = try allocator.alloc(types.RefValue, @intCast(len));
        @memcpy(new_elem[0..old_elem.len], old_elem);
        @memset(new_elem[old_elem.len..], val);
        // `limits.min` should be updated outside this function
        return new_elem;
    }

    inline fn opTableSize(self: *Self, table_idx: wa.TableIdx) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const ta = module.table_addrs[table_idx];
        const tab = self.store.tables.items[ta];
        const val: i32 = @intCast(tab.elem.len);
        try self.stack.pushValueAs(i32, val);
    }

    inline fn opTableFill(self: *Self, table_idx: wa.TableIdx) error{OutOfBoundsTableAccess}!void {
        const module = self.stack.topFrame().module;
        const ta = module.table_addrs[table_idx];
        const tab = self.store.tables.items[ta];

        var n = self.stack.pop().value.asI32();
        const val = self.stack.pop().value;
        var i = self.stack.pop().value.asI32();

        while (n > 0) {
            if (i + n > tab.elem.len)
                return Error.OutOfBoundsTableAccess;

            tab.elem[@intCast(i)] = types.RefValue.fromValue(val);
            i += 1;
            n -= 1;
        }
    }

    // memory instructions
    inline fn opLoad(self: *Self, comptime T: type, comptime N: type, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const a = module.mem_addrs[0];
        const mem = &self.store.mems.items[a];

        const ea: u32 = @bitCast(self.stack.pop().value.asI32());

        const ea_start_with_overflow = @addWithOverflow(ea, mem_arg.offset);
        if (ea_start_with_overflow[1] == 1) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const ea_start = ea_start_with_overflow[0];
        if (ea_start > mem.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const size = @sizeOf(N);
        const ea_end_with_overflow = @addWithOverflow(ea_start, size);
        if (ea_end_with_overflow[1] == 1) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const ea_end = ea_end_with_overflow[0];
        if (ea_end > mem.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        std.debug.print("=== {any} ==", .{mem.data[ea_start..ea_end]});
        const val = decode.safeNumCast(N, mem.data[ea_start..ea_end]);

        std.debug.print("===> {}\n", .{val});
        try self.stack.pushValueAs(T, val);
    }

    inline fn opStore(self: *Self, comptime T: type, comptime bit_size: u32, mem_arg: Instruction.MemArg) error{OutOfBoundsMemoryAccess}!void {
        const module = self.stack.topFrame().module;
        const a = module.mem_addrs[0];
        const mem = &self.store.mems.items[a];

        const c = self.stack.pop().value.as(T);
        const i = self.stack.pop().value.asI32();

        var ea: u32 = @intCast(i);
        ea += mem_arg.offset;
        const byte_size = bit_size / 8;

        if (ea + byte_size > mem.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        inline for (0..byte_size) |idx| {
            mem.data[ea + idx] = @intCast((c >> idx * 8) & 0xff);
        }
    }

    inline fn opDataDrop(self: *Self, data_idx: wa.DataIdx) void {
        const module = self.stack.topFrame().module;
        const a = module.data_addrs[data_idx];
        const data = &self.store.datas.items[a];
        data.data = &.{};
    }

    inline fn opMemorySize(self: *Self) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const mem_addr = module.mem_addrs[0];
        const mem_inst = self.store.mems.items[mem_addr];

        const sz: i32 = @intCast(mem_inst.data.len / page_size);
        try self.stack.pushValueAs(i32, sz);
    }

    /// `memory.grow` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-grow
    inline fn opMemoryGrow(self: *Self) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const mem_addr = module.mem_addrs[0];
        const mem_inst = self.store.mems.items[mem_addr];

        const sz: i32 = @intCast(mem_inst.data.len / page_size);
        const n = self.stack.pop().value.asI32();

        if (mem_inst.type.limits.max) |max| {
            if (n + sz > max) {
                try self.stack.pushValueAs(i32, -1);
                return;
            }
        }

        const new_data = try growmem(mem_inst, n, self.allocator);
        self.store.mems.items[mem_addr].data = new_data;
        self.store.mems.items[mem_addr].type.limits.min = @intCast(new_data.len);
        try self.stack.pushValueAs(i32, sz);
    }

    /// https://webassembly.github.io/spec/core/exec/modules.html#growing-memories
    fn growmem(mem_inst: types.MemInst, n: i32, allocator: std.mem.Allocator) error{OutOfMemory}![]u8 {
        const data_len: i32 = @intCast(mem_inst.data.len / page_size);
        const len: i32 = data_len + n;
        if (len + n > 65536) {
            return std.mem.Allocator.Error.OutOfMemory; // TODO: use another error
        }
        const old_data = mem_inst.data;
        const new_data = try allocator.alloc(u8, @intCast(len * page_size));
        @memcpy(new_data[0..old_data.len], old_data);
        @memset(new_data[old_data.len..], 0);
        // `limits.min` should be updated outside this function
        return new_data;
    }

    /// `memory.init x` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-memory-mathsf-memory-init-x
    inline fn opMemoryInit(self: *Self, data_idx: wa.DataIdx) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const mem_addr = module.mem_addrs[0];
        const mem = self.store.mems.items[mem_addr];
        _ = mem;
        const data_addr = module.data_addrs[data_idx];
        const data = self.store.datas.items[data_addr];

        var n = self.stack.pop().value.asI32();
        var s = self.stack.pop().value.asI32();
        var d = self.stack.pop().value.asI32();

        //const ea1: u64 = @intCast(s);
        //const ea2: u64 = @intCast(d);

        while (n > 0) : (n -= 1) {
            const b = data.data[@intCast(s)];
            try self.stack.pushValueAs(i32, d);
            try self.stack.pushValueAs(i32, b);
            try self.execOneInstruction(.{ .i32_store8 = .{ .@"align" = 0, .offset = 0 } });
            s += 1;
            d += 1;
        }
    }

    inline fn opMemoryCopy(self: *Self) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const mem_addr = module.mem_addrs[0];
        const mem_inst = self.store.mems.items[mem_addr];

        var n = self.stack.pop().value.asI32();
        const s = self.stack.pop().value.asI32();
        var d = self.stack.pop().value.asI32();

        if (d + n > mem_inst.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        while (n > 0) : (n -= 1) {
            if (d <= s) {
                try self.stack.pushValueAs(i32, d);
                try self.stack.pushValueAs(i32, s);
                try self.execOneInstruction(.{ .i32_load8_u = .{ .@"align" = 0, .offset = 0 } });
                try self.execOneInstruction(.{ .i32_store8 = .{ .@"align" = 0, .offset = 0 } });
                d += 1;
                s += 1;
            } else {
                try self.stack.pushValueAs(i32, d + n - 1);
                try self.stack.pushValueAs(i32, s + n - 1);
                try self.execOneInstruction(.{ .i32_load8_u = .{ .@"align" = 0, .offset = 0 } });
                try self.execOneInstruction(.{ .i32_store8 = .{ .@"align" = 0, .offset = 0 } });
            }
        }
    }

    inline fn opMemoryFill(self: *Self) (Error || error{OutOfMemory})!void {
        const module = self.stack.topFrame().module;
        const mem_addr = module.mem_addrs[0];
        const mem_inst = self.store.mems.items[mem_addr];

        var n = self.stack.pop().value.asI32();
        const val = self.stack.pop();
        var d = self.stack.pop().value.asI32();

        if (d + n > mem_inst.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        while (n > 0) : (n -= 1) {
            try self.stack.pushValueAs(i32, d);
            try self.stack.push(val);
            try self.execOneInstruction(.{ .i32_store8 = .{ .@"align" = 0, .offset = 0 } });
            d += 1;
        }
    }

    inline fn basetype(comptime T: type) type {
        return if (T == u32) i32 else if (T == u64) i64 else T;
    }

    inline fn instrOp(self: *Self, comptime R: type, comptime T: type, comptime f: fn (type, type, T) Error!R) (Error || error{OutOfMemory})!void {
        const value: T = @bitCast(self.stack.pop().value.as(basetype(T)));
        const result: R = f(R, T, value);
        const B = basetype(R);
        const converted_result: B = @bitCast(result);
        try self.stack.pushValueAs(B, converted_result);
    }

    inline fn unOp(self: *Self, comptime T: type, comptime f: fn (type, T) T) error{OutOfMemory}!void {
        const value = self.stack.pop().value.as(T);
        const result = f(T, value);
        try self.stack.pushValueAs(T, result);
    }

    inline fn binOp(self: *Self, comptime T: type, comptime f: fn (type, T, T) Error!T) (Error || error{OutOfMemory})!void {
        const B = basetype(T);
        const rhs: T = @bitCast(self.stack.pop().value.as(B));
        const lhs: T = @bitCast(self.stack.pop().value.as(B));
        const result = try f(T, lhs, rhs);
        try self.stack.pushValueAs(T, result);
    }

    inline fn testOp(self: *Self, comptime T: type, comptime f: fn (type, T) i32) error{OutOfMemory}!void {
        const value = self.stack.pop().value.as(T);
        const result = f(T, value);
        try self.stack.pushValueAs(i32, result);
    }

    inline fn relOp(self: *Self, comptime T: type, comptime f: fn (type, T, T) i32) error{OutOfMemory}!void {
        const B = basetype(T);
        const rhs: T = @bitCast(self.stack.pop().value.as(B));
        const lhs: T = @bitCast(self.stack.pop().value.as(B));
        const result = f(T, lhs, rhs);
        try self.stack.pushValueAs(i32, result);
    }

    inline fn cvtOp(self: *Self, comptime T2: type, comptime T1: type, comptime f: fn (type, T1) Error!T2) (Error || error{OutOfMemory})!void {
        const value = self.stack.pop().value.as(T1);
        const result: T2 = try f(T1, value);
        try self.stack.pushValueAs(T2, result);
    }

    /// `expand_F` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/runtime.html#exec-expand
    inline fn expandToFuncType(module: *types.ModuleInst, block_type: Instruction.BlockType) wa.FuncType {
        return switch (block_type) {
            .empty => .{ .parameter_types = &.{}, .result_types = &.{} },
            .value_type => |vt| .{ .parameter_types = &.{}, .result_types = &.{vt} },
            .type_index => |idx| module.types[idx],
        };
    }
};

pub fn instractionFromInitExpr(init_expr: wa.InitExpression) Instruction {
    return switch (init_expr) {
        .i32_const => |val| .{ .i32_const = val },
        .i64_const => |val| .{ .i64_const = val },
        .f32_const => |val| .{ .f32_const = val },
        .f64_const => |val| .{ .f64_const = val },
        .v128_const => unreachable, // FIXME
        .ref_null => |ref_type| .{ .ref_null = ref_type },
        .ref_func => |func_idx| .{ .ref_func = func_idx },
        .global_get => |global_idx| .{ .global_get = global_idx },
    };
}

const FlowControl = union(enum) {
    none,
    jump: wa.InstractionAddr,
    exit,
    call: types.FuncAddr,

    pub fn newAtOpEnd(label: types.Label) FlowControl {
        return switch (label.type) {
            .root => .exit,
            else => .none,
        };
    }

    pub fn newAtOpElse(label: types.Label) FlowControl {
        return switch (label.type) {
            .@"if" => |idx| .{ .jump = idx },
            else => .none,
        };
    }

    pub fn newAtOpBr(label: types.Label) FlowControl {
        return switch (label.type) {
            .root => .exit,
            .loop => |idx| .{ .jump = idx }, // jump to `loop`
            inline else => |idx| .{ .jump = idx + 1 }, // jump next to `end`
        };
    }

    pub fn newAtOpIf(info: Instruction.IfBlockInfo, cond: i32) FlowControl {
        if (cond != 0) {
            return .none;
        } else if (info.@"else" != null) {
            return .{ .jump = info.@"else".? };
        } else {
            return .{ .jump = info.end };
        }
    }
};

inline fn unsignedTypeOf(comptime T: type) type {
    std.debug.assert(T == i32 or T == i64);
    return if (T == i32) u32 else u64;
}

// arithmetic ops are defined outside the struct

fn opIntClz(comptime T: type, value: T) T {
    return @clz(value);
}

fn opIntCtz(comptime T: type, value: T) T {
    return @ctz(value);
}

fn opIntPopcnt(comptime T: type, value: T) T {
    return @popCount(value);
}

fn opTrunc(comptime R: type, comptime T: type, value: T) R {
    const result: R = @intFromFloat(value);
    return result;
}

fn opConvert(comptime R: type, comptime T: type, value: T) R {
    const result: R = @floatFromInt(value);
    return result;
}

fn opReinterpret(comptime R: type, comptime T: type, value: T) R {
    const result: R = @bitCast(value);
    return result;
}

fn opPromote(comptime R: type, comptime T: type, value: T) R {
    return value;
}

fn opWrap(comptime R: type, comptime T: type, value: T) R {
    const result: i32 = @intCast(value & 0xffffffff);
    return result;
}

fn opDemote(comptime R: type, comptime T: type, value: T) R {
    const result: f32 = @floatCast(value);
    return result;
}

fn opExtend8(comptime R: type, comptime T: type, value: T) R {
    const result: i8 = @truncate(value);
    return result;
}

fn opExtend16(comptime R: type, comptime T: type, value: T) R {
    const result: i16 = @truncate(value);
    return result;
}

fn opExtend32(comptime R: type, comptime T: type, value: T) R {
    const result: i32 = @truncate(value);
    return result;
}

fn opExtend64(comptime R: type, comptime T: type, value: T) R {
    const result: i64 = @truncate(value);
    return result;
}

fn opIntEqz(comptime T: type, value: T) i32 {
    return if (value == 0) 1 else 0;
}

fn opIntEq(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs == rhs) 1 else 0;
}

fn opIntNe(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs != rhs) 1 else 0;
}

fn opIntLt(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs < rhs) 1 else 0;
}

fn opIntGt(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs > rhs) 1 else 0;
}

fn opIntLe(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs <= rhs) 1 else 0;
}

fn opIntGe(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs >= rhs) 1 else 0;
}

fn opIntAdd(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs +% rhs;
}
fn opIntSub(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs -% rhs;
}
fn opIntMul(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs *% rhs;
}
fn opIntDivS(comptime T: type, lhs: T, rhs: T) Error!T {
    if (T == i32 and lhs == -2147483648 and rhs == -1) return Error.IntegerOverflow;
    if (T == i64 and lhs == -9223372036854775808 and rhs == -1) return Error.IntegerOverflow;
    if (rhs == 0) return Error.IntegerDivideByZero;
    return @divTrunc(lhs, rhs);
}
fn opIntDivU(comptime T: type, lhs: T, rhs: T) Error!T {
    if (rhs == 0) return Error.IntegerDivideByZero;
    if (T == i32 and lhs == 2147483648 and rhs == 4294967295) return 0;
    if (T == i64 and lhs == 9223372036854775808 and rhs == 18446744073709551615) return 0;
    const res = @divTrunc(lhs, rhs);
    return @bitCast(res);
}
fn opIntRemS(comptime T: type, lhs: T, rhs: T) Error!T {
    if (rhs == 0) return Error.IntegerDivideByZero;
    if (T == i32) {
        if (lhs == -2147483648 and rhs == -1) return 0;
        if (lhs >= 0 and rhs > 0) {
            return @mod(lhs, rhs);
        } else {
            // note: @mod of negative number returns unintended result.
            //  -  @mod(-2147483647,  1000) == 353 (not -647)
            //  -  @mod(-2147483647, -1000) -> panic
            const num: i33 = @intCast(lhs);
            const den: i33 = @intCast(rhs);
            const res_tmp = @mod(if (num > 0) num else -num, if (den > 0) den else -den);
            const res: i32 = @intCast(if (lhs > 0) res_tmp else -res_tmp);
            return res;
        }
    } else {
        if (lhs == -9223372036854775808 and rhs == -1) return 0;
        if (lhs >= 0 and rhs > 0) {
            return @mod(lhs, rhs);
        } else {
            // note: @mod of negative number returns unintended result.
            //  -  @mod(-2147483647,  1000) == 353 (not -647)
            //  -  @mod(-2147483647, -1000) -> panic
            const num: i65 = @intCast(lhs);
            const den: i65 = @intCast(rhs);
            const res_tmp = @mod(if (num > 0) num else -num, if (den > 0) den else -den);
            const res: i64 = @intCast(if (lhs > 0) res_tmp else -res_tmp);
            return res;
        }
    }
}

fn opIntRemU(comptime T: type, lhs: T, rhs: T) Error!T {
    if (rhs == 0) return Error.IntegerDivideByZero;
    const res = @mod(lhs, rhs);
    return @bitCast(res);
}

fn opIntAnd(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs & rhs;
}

fn opIntOr(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs | rhs;
}

fn opIntXor(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs ^ rhs;
}

fn opIntShl(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs << @intCast(@mod(rhs, @bitSizeOf(T)));
}

fn opIntShrS(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs >> @intCast(@mod(rhs, @bitSizeOf(T)));
}

fn opIntShrU(comptime T: type, lhs: T, rhs: T) Error!T {
    const res = lhs >> @intCast(@mod(rhs, @bitSizeOf(T)));
    return @bitCast(res);
}

fn opIntRotl(comptime T: type, lhs: T, rhs: T) Error!T {
    if (T == i32) {
        var num: u32 = @bitCast(lhs);
        const b = @bitSizeOf(T);
        const r = @mod(rhs, b);
        const r1: u5 = @intCast(r);
        const r2: u5 = @intCast(@mod(b - r, b));
        const res = (num << r1) | (num >> r2);
        const res2: i32 = @bitCast(res);
        return res2;
    } else {
        var num: u64 = @bitCast(lhs);
        const b = @bitSizeOf(T);
        const r = @mod(rhs, b);
        const r1: u6 = @intCast(r);
        const r2: u6 = @intCast(@mod(b - r, b));
        const res = (num << r1) | (num >> r2);
        const res2: i64 = @bitCast(res);
        return res2;
    }
}

fn opIntRotr(comptime T: type, lhs: T, rhs: T) Error!T {
    if (T == i32) {
        var num: u32 = @bitCast(lhs);
        const b = @bitSizeOf(T);
        const r = @mod(rhs, b);
        const r1: u5 = @intCast(r);
        const r2: u5 = @intCast(@mod(b - r, b));
        const res = (num >> r1) | (num << r2);
        const res2: i32 = @bitCast(res);
        return res2;
    } else {
        var num: u64 = @bitCast(lhs);
        const b = @bitSizeOf(T);
        const r = @mod(rhs, b);
        const r1: u6 = @intCast(r);
        const r2: u6 = @intCast(@mod(b - r, b));
        const res = (num >> r1) | (num << r2);
        const res2: i64 = @bitCast(res);
        return res2;
    }
}

fn opFloatAbs(comptime T: type, value: T) T {
    return @fabs(value);
}

fn opFloatNeg(comptime T: type, value: T) T {
    return -value;
}

fn opFloatSqrt(comptime T: type, value: T) T {
    return @sqrt(value);
}

fn opFloatCeil(comptime T: type, value: T) T {
    return @ceil(value);
}

fn opFloatFloor(comptime T: type, value: T) T {
    return @floor(value);
}

fn opFloatTrunc(comptime T: type, value: T) T {
    return @trunc(value);
}

fn opFloatNearest(comptime T: type, value: T) T {
    return @trunc(value); // FIXME
}

fn opFloatEq(comptime T: type, lhs: T, rhs: T) i32 {
    if (std.math.isNan(lhs) or std.math.isNan(rhs))
        return 0;
    if (lhs == 0 and rhs == 0)
        return 1;

    return if (lhs == rhs) 1 else 0;
}

fn opFloatNe(comptime T: type, lhs: T, rhs: T) i32 {
    if (std.math.isNan(lhs) or std.math.isNan(rhs))
        return 1;
    if (lhs == 0 and rhs == 0)
        return 0;

    return if (lhs == rhs) 0 else 1;
}

fn opFloatLt(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs < rhs) 1 else 0;
}

fn opFloatGt(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs > rhs) 1 else 0;
}

fn opFloatLe(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs <= rhs) 1 else 0;
}

fn opFloatGe(comptime T: type, lhs: T, rhs: T) i32 {
    return if (lhs >= rhs) 1 else 0;
}

fn opFloatAdd(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs + rhs;
}

fn opFloatSub(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs - rhs;
}

fn opFloatMul(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs * rhs;
}

fn opFloatDiv(comptime T: type, lhs: T, rhs: T) Error!T {
    return lhs / rhs;
}

fn opFloatMin(comptime T: type, lhs: T, rhs: T) Error!T {
    if (std.math.isNan(lhs) or std.math.isNan(rhs)) {
        return canonNan(T);
    }
    return @min(lhs, rhs);
}

fn opFloatMax(comptime T: type, lhs: T, rhs: T) Error!T {
    if (std.math.isNan(lhs) or std.math.isNan(rhs)) {
        return canonNan(T);
    }
    return @max(lhs, rhs);
}

fn opFloatCopySign(comptime T: type, lhs: T, rhs: T) Error!T {
    return std.math.copysign(lhs, rhs);
}

fn canonNan(comptime T: type) T {
    // std.math.nan() is not canonical NaN
    std.debug.assert(T == f64 or T == f32);
    const v = if (T == f64) @as(u64, 0x7ff8_0000_0000_0000) else @as(u32, 0x7fc0_0000);
    return @bitCast(v);
}

test opFloatEq {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(@as(i32, 0), opFloatEq(f32, std.math.nan_f32, 1));
    try expectEqual(@as(i32, 0), opFloatEq(f32, 100, std.math.nan_f32));
    try expectEqual(@as(i32, 1), opFloatEq(f32, -0.0, -0.0));
    try expectEqual(@as(i32, 1), opFloatEq(f32, 1.0, 1.0));
    try expectEqual(@as(i32, 0), opFloatEq(f32, 1.0, 0.0));
}

test opFloatNe {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(@as(i32, 1), opFloatNe(f32, std.math.nan_f32, 1));
    try expectEqual(@as(i32, 1), opFloatNe(f32, 100, std.math.nan_f32));
    try expectEqual(@as(i32, 0), opFloatNe(f32, -0.0, -0.0));
    try expectEqual(@as(i32, 0), opFloatNe(f32, 1.0, 1.0));
    try expectEqual(@as(i32, 1), opFloatNe(f32, 1.0, 0.0));
}
