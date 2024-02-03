const std = @import("std");
const wa = @import("wasm-core");
const Instruction = wa.Instruction;
const decode = @import("wasm-decode");
pub const types = @import("./types.zig");
pub const Error = @import("./errors.zig").Error;

pub const Engine = struct {
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

    pub fn load(self: *Self, module: wa.Module) (Error || error{OutOfMemory})!*types.ModuleInst {
        const extern_vals = try resolveImports(self.store, module, self.allocator);
        return try self.instantiate(module, extern_vals);
    }

    pub fn invokeFunctionByAddr(self: *Self, func_addr: types.FuncAddr, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        // TODO: args check

        // TODO: push args
        for (args) |arg| {
            try self.stack.push(.{ .value = arg });
        }

        const ret = try self.invokeFunction(func_addr);

        // TODO: handle return
        return ret;
    }

    /// `invoke a` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/instructions.html#exec-invoke
    fn invokeFunction(self: *Self, func_addr: types.FuncAddr) (Error || error{OutOfMemory})![]const types.Value {
        self.printStack();

        const func_inst = self.store.funcs.items[func_addr];
        const func_type = func_inst.type;

        const p_len = func_type.parameter_types.len;
        const num_locals = p_len + func_inst.code.locals.len;
        const locals = try self.allocator.alloc(types.Value, num_locals);
        @memset(locals[p_len..num_locals], .{ .i64 = 0 }); // TODO: should be assign actual type?

        var i = p_len;
        while (i > 0) : (i -= 1) {
            locals[i - 1] = self.stack.pop().value;
        }

        const num_returns = func_type.result_types.len;
        const frame = types.ActivationFrame{
            .locals = locals,
            .arity = num_returns,
            .module = func_inst.module,
        };
        try self.stack.push(.{ .frame = frame });
        try self.stack.push(.{ .label = .{ .arity = @intCast(num_returns), .type = .root } });

        try self.execExpr(func_inst.code.body);

        // pop frames etc
        const ret = try self.allocator.alloc(types.Value, num_returns);
        i = num_returns;
        while (i > 0) : (i -= 1) {
            ret[i - 1] = self.stack.pop().value;
        }

        self.stack.popValuesAndLabelsUntilFrame();

        for (ret) |v| {
            try self.stack.push(.{ .value = v });
        }

        return ret;
    }

    fn execExpr(self: *Self, instrs: []const Instruction) (Error || error{OutOfMemory})!void {
        std.debug.print("---------------\n", .{});
        for (instrs, 0..) |i, idx| {
            std.debug.print("[{}] {}\n", .{ idx, i });
        }
        std.debug.print("---------------\n", .{});

        var ip: wa.InstractionAddr = 0;
        while (ip < instrs.len) {
            const instr = instrs[ip];
            const flow_ctrl = try self.execInstruction(ip, instr);

            if (flow_ctrl != .none) {
                std.debug.print("\t===> {}\n", .{flow_ctrl});
            }

            switch (flow_ctrl) {
                .none => ip += 1,
                .jump => |new_ip| ip = new_ip,
                .exit => return,
            }
        }
    }

    /// `instantiate` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    fn instantiate(self: *Self, module: wa.Module, extern_vals: []const types.ExternalValue) (Error || error{OutOfMemory})!*types.ModuleInst {
        // 1: validate
        // 2: assert
        // 3: check length
        // 4: verify external value
        // 5: ????

        // Step 6 - 10 are done in `allocateModule`. Just skip in this function.
        // 6, 7: push init frame to stack
        // 8: Get `val*`
        // 9: Get `ref*`
        // 10: pop init frame from stack

        // 11: alloc module
        const mod_inst = try self.allocateModule(module, extern_vals);
        //std.debug.print("\n---ModInst\n{}\n", .{std.json.fmt(mod_inst, .{})});
        std.debug.print("\n---ModInst\n{any}\n", .{mod_inst});

        // 12, 13: push aux frame
        const aux_frame = types.ActivationFrame{
            .module = mod_inst,
        };
        try self.stack.push(.{ .frame = aux_frame });

        // 14, 15: element segment
        for (module.elements, 0..) |elem, i| {
            switch (elem.mode) {
                .active => |active_type| {
                    const n = elem.init.len;
                    try self.execOneInstruction(instractionFromInitExpr(active_type.offset));
                    try self.execOneInstruction(.{ .i32_const = 0 });
                    try self.execOneInstruction(.{ .i32_const = @intCast(n) });
                    try self.execOneInstruction(.{
                        .table_init = .{
                            .elem_idx = @intCast(i),
                            .table_idx = 0, // FIXME: elem.mode,
                        },
                    });
                    try self.execOneInstruction(.{ .elem_drop = @intCast(i) });
                },
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

    /// `allocmodule` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#alloc-module
    fn allocateModule(self: *Self, module: wa.Module, extern_vals: []const types.ExternalValue) (Error || error{OutOfMemory})!*types.ModuleInst {
        // 1: resolve imports
        var num_import_funcs: u32 = 0;
        var num_import_tables: u32 = 0;
        var num_import_mems: u32 = 0;
        var num_import_globals: u32 = 0;
        for (extern_vals) |imp|
            switch (imp) {
                .function => num_import_funcs += 1,
                .table => num_import_tables += 1,
                .memory => num_import_mems += 1,
                .global => num_import_globals += 1,
            };

        // 20: module instance
        var mod_inst = try self.allocator.create(types.ModuleInst);
        mod_inst.types = module.types;
        mod_inst.func_addrs = try self.allocator.alloc(types.FuncAddr, num_import_funcs + module.funcs.len);
        mod_inst.table_addrs = try self.allocator.alloc(types.TableAddr, num_import_tables + module.tables.len);
        mod_inst.mem_addrs = try self.allocator.alloc(types.MemAddr, num_import_mems + module.memories.len);
        mod_inst.global_addrs = try self.allocator.alloc(types.GlobalAddr, num_import_globals + module.globals.len);
        mod_inst.elem_addrs = try self.allocator.alloc(types.ElemAddr, module.elements.len);
        mod_inst.data_addrs = try self.allocator.alloc(types.DataAddr, module.datas.len);
        mod_inst.exports = try self.allocator.alloc(types.ExportInst, module.exports.len);

        var node = ModInstList.Node{ .data = mod_inst.* };
        self.modules.prepend(&node);

        // 14, 15, 16, 17: imports
        var num_funcs: u32 = 0;
        var num_tables: u32 = 0;
        var num_mems: u32 = 0;
        var num_globals: u32 = 0;
        for (extern_vals) |imp|
            switch (imp) {
                .function => |idx| {
                    mod_inst.func_addrs[num_funcs] = idx;
                    num_funcs += 1;
                },
                .table => |idx| {
                    mod_inst.table_addrs[num_tables] = idx;
                    num_tables += 1;
                },
                .memory => |idx| {
                    mod_inst.mem_addrs[num_mems] = idx;
                    num_mems += 1;
                },
                .global => |idx| {
                    mod_inst.global_addrs[num_globals] = idx;
                    num_globals += 1;
                },
            };

        // 2, 8, 14: function
        for (module.funcs, num_import_funcs..) |func, i| {
            mod_inst.func_addrs[i] = try allocFunc(&self.store, func, mod_inst);
        }

        // 3, 9, 15: table
        for (module.tables, num_import_tables..) |table, i| {
            mod_inst.table_addrs[i] = try allocTable(&self.store, table, self.allocator);
        }

        // 4, 10, 16: memory
        for (module.memories, num_import_mems..) |mem, i| {
            mod_inst.mem_addrs[i] = try allocMemory(&self.store, mem, self.allocator);
        }

        // push the auxiliary frame (Step 6 and 7 in Instantiation)
        const aux_frame = types.ActivationFrame{
            .module = mod_inst,
        };
        try self.stack.push(.{ .frame = aux_frame });

        // 5, 11, 17: global
        for (module.globals, num_import_globals..) |global, i| {
            try self.execOneInstruction(instractionFromInitExpr(global.init));
            const value = self.stack.pop().value;
            mod_inst.global_addrs[i] = try allocGlobal(&self.store, global, value);
        }

        // 6, 12: element segment
        for (module.elements, 0..) |element, i| {
            var refs = try self.allocator.alloc(types.RefValue, element.init.len);
            for (element.init, 0..) |e, j| {
                try self.execOneInstruction(instractionFromInitExpr(e));
                const value = self.stack.pop().value;

                const val: types.RefValue = switch (value) {
                    .func_ref => |v| .{ .func_ref = v.? },
                    .extern_ref => |v| .{ .extern_ref = v.? },
                    else => unreachable,
                };
                refs[j] = val;
            }
            mod_inst.elem_addrs[i] = try allocElement(&self.store, element, refs);
        }

        // pop the auxiliary frame (Step 10 in Instantiation)
        _ = self.stack.pop();

        // 7, 13: data segment
        for (module.datas, 0..) |data, i| {
            mod_inst.data_addrs[i] = try allocData(&self.store, data);
        }

        // 18, 19: export
        for (module.exports, 0..) |exp, i| {
            const ext_value: types.ExternalValue = switch (exp.desc) {
                .func => |idx| .{ .function = mod_inst.func_addrs[idx] },
                .table => |idx| .{ .table = mod_inst.table_addrs[idx] },
                .memory => |idx| .{ .memory = mod_inst.mem_addrs[idx] },
                .global => |idx| .{ .global = mod_inst.global_addrs[idx] },
            };
            const exp_inst = types.ExportInst{
                .name = exp.name,
                .value = ext_value,
            };
            mod_inst.exports[i] = exp_inst;
        }

        // 21: return
        return mod_inst;
    }

    fn printStack(self: *Self) void {
        for (self.stack.array.items) |i| {
            switch (i) {
                .value => |v| std.debug.print("  V {}\n", .{v}),
                .label => |v| std.debug.print("  L {}\n", .{v}),
                .frame => |v| std.debug.print("  F {any}\n", .{v.locals}),
            }
        }
    }

    /// executes an instruction without control flow
    inline fn execOneInstruction(self: *Self, instr: Instruction) (Error || error{OutOfMemory})!void {
        _ = try self.execInstruction(0, instr);
    }

    /// executes an instruction and returns control flow
    fn execInstruction(self: *Self, ip: wa.InstractionAddr, instr: Instruction) (Error || error{OutOfMemory})!FlowControl {
        self.printStack();
        std.debug.print("== [{}]: {}\n", .{ ip, instr });
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
            .call => |func_idx| {
                const module = self.stack.topFrame().module;
                _ = try self.invokeFunction(module.func_addrs[func_idx]);
            },
            .call_indirect => |arg| {
                std.debug.print("call_indirect: {any}\n", .{arg});
                const module = self.stack.topFrame().module;
                const ta = module.table_addrs[arg.table_idx];
                const tab = self.store.tables.items[ta];

                const ft_expect = module.types[arg.type_idx];

                const i = self.stack.pop().value.asI32();
                if (i >= tab.elem.len)
                    return Error.UndefinedElement;
                const rx = tab.elem[@intCast(i)];
                if (rx == null)
                    return Error.UninitializedElement;
                const r = rx.?.func_ref;

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

                _ = try self.invokeFunction(r);
            },

            // reference instructions
            .ref_null => |ref_type| {
                const val: types.Value = switch (ref_type) {
                    .funcref => .{ .func_ref = null },
                    .externref => .{ .extern_ref = null },
                };
                try self.stack.push(.{ .value = val });
            },
            // .ref_is_null,
            .ref_func => |func_idx| {
                const module = self.stack.topFrame().module;
                const a = module.func_addrs[func_idx];
                try self.stack.push(.{ .value = .{ .func_ref = a } });
            },

            // parametric instructions
            .drop => _ = self.stack.pop(),
            .select => try self.opSelect(),
            .selectv => try self.opSelect(),

            // variable instructions
            .local_get => |local_idx| try self.opLocalGet(local_idx),
            .local_set => |local_idx| try self.opLocalSet(local_idx),
            .local_tee => |local_idx| {
                _ = local_idx;
                const value = self.stack.pop();
                try self.stack.push(value);
                try self.stack.push(value);
            },
            .global_get => |global_idx| {
                const module = self.stack.topFrame().module;
                const a = module.global_addrs[global_idx];
                const glob = self.store.globals.items[a];
                try self.stack.push(.{ .value = glob.value });
            },
            .global_set => |global_idx| {
                const module = self.stack.topFrame().module;
                const a = module.global_addrs[global_idx];
                var glob = self.store.globals.items[a];
                const value = self.stack.pop().value;
                glob.value = value;
            },

            // table instructions
            // .table_get: types.TableIdx,
            .table_set => |table_idx| {
                const module = self.stack.topFrame().module;
                const a = module.table_addrs[table_idx];
                const tab = self.store.tables.items[a];
                const val = self.stack.pop().value;
                const i = self.stack.pop().value.asI32();
                // TODO check
                tab.elem[@intCast(i)] = .{ .func_ref = val.func_ref.? }; // FIXME

                {
                    for (self.store.tables.items, 0..) |table, ix| {
                        std.debug.print("**** table [{}]\n", .{ix});
                        var out = std.ArrayList(u8).init(std.heap.c_allocator);
                        defer out.deinit();
                        try std.json.stringify(table.elem, .{}, out.writer());
                        std.debug.print("*** {s}\n", .{out.items});
                    }
                }
            },
            .table_init => |arg| {
                const module = self.stack.topFrame().module;
                while (true) {
                    const ta = module.table_addrs[arg.elem_idx];
                    const tab = self.store.tables.items[ta];
                    const ea = module.elem_addrs[arg.table_idx];
                    const elem = self.store.elems.items[ea];
                    const n = self.stack.pop().value.asI32();
                    const s = self.stack.pop().value.asI32();
                    const d = self.stack.pop().value.asI32();

                    // TODO: check
                    _ = tab;

                    if (n == 0)
                        break;

                    const ref = elem.elem[@intCast(s)];
                    try self.stack.pushValueAs(i32, d);
                    try self.stack.push(.{ .value = .{ .func_ref = ref.func_ref } }); // FIXME
                    const inst = Instruction{ .table_set = arg.elem_idx };
                    try self.execOneInstruction(inst);
                    try self.stack.pushValueAs(i32, d + 1);
                    try self.stack.pushValueAs(i32, s + 1);
                    try self.stack.pushValueAs(i32, n - 1);
                }
            },
            .elem_drop => |elem_idx| {
                const module = self.stack.topFrame().module;
                const a = module.elem_addrs[elem_idx];
                const elem = self.store.elems.items[a];
                _ = elem; // TODO: delete elem
            },
            // .table_copy: TblArg,
            // .table_grow: types.TableIdx,
            // .table_size: types.TableIdx,
            // .table_fill: types.TableIdx,

            // memory instructions
            .i32_load => |mem_arg| try self.opI32Load(i32, mem_arg),
            .i64_load => |mem_arg| try self.opI64Load(i64, mem_arg),
            .f32_load => |mem_arg| try self.opF32Load(mem_arg),
            .f64_load => |mem_arg| try self.opF64Load(mem_arg),
            .i32_load8_s => |mem_arg| try self.opI32Load(i8, mem_arg),
            .i32_load8_u => |mem_arg| try self.opI32Load(u8, mem_arg),
            .i32_load16_s => |mem_arg| try self.opI32Load(i16, mem_arg),
            .i32_load16_u => |mem_arg| try self.opI32Load(u16, mem_arg),
            .i64_load8_s => |mem_arg| try self.opI64Load(i8, mem_arg),
            .i64_load8_u => |mem_arg| try self.opI64Load(u8, mem_arg),
            .i64_load16_s => |mem_arg| try self.opI64Load(i16, mem_arg),
            .i64_load16_u => |mem_arg| try self.opI64Load(u16, mem_arg),
            .i64_load32_s => |mem_arg| try self.opI64Load(i32, mem_arg),
            .i64_load32_u => |mem_arg| try self.opI64Load(u32, mem_arg),
            // .i32_store: MemArg,
            // .i64_store: MemArg,
            // .f32_store: MemArg,
            // .f64_store: MemArg,
            .i32_store8 => |mem_arg| try self.opI32Store8(mem_arg),
            // .i32_store16
            // .i64_store8: MemArg,
            // .i64_store16: MemArg,
            // .i64_store32: MemArg,
            // .memory_size,
            // .memory_grow,
            .memory_init => |data_idx| try self.opMemoryInit(data_idx),
            .data_drop => |data_idx| try self.opDataDrop(data_idx),
            // .memory_copy,
            // .memory_fill,

            // numeric instructions (1)
            .i32_const => |val| try self.stack.pushValue(val),
            .i64_const => |val| try self.stack.pushValue(val),
            .f32_const => |val| try self.stack.pushValue(val),
            .f64_const => |val| try self.stack.pushValue(val),

            // numeric instructions (2) i32
            .i32_eqz => try self.unOp(i32, opIntEqz),
            .i32_eq => try self.binOp(i32, opIntEq),
            .i32_ne => try self.binOp(i32, opIntNe),
            .i32_lt_s => try self.binOp(i32, opIntLtS),
            .i32_lt_u => try self.binOp(i32, opIntLtU),
            .i32_gt_s => try self.binOp(i32, opIntGtS),
            .i32_gt_u => try self.binOp(i32, opIntGtU),
            .i32_le_s => try self.binOp(i32, opIntLeS),
            .i32_le_u => try self.binOp(i32, opIntLeU),
            .i32_ge_s => try self.binOp(i32, opIntGeS),
            .i32_ge_u => try self.binOp(i32, opIntGeU),

            // numeric instructions (2) i64
            .i64_eqz => try self.unOp(i64, opIntEqz),
            .i64_eq => try self.binOp(i64, opIntEq),
            .i64_ne => try self.binOp(i64, opIntNe),
            .i64_lt_s => try self.binOp(i64, opIntLtS),
            .i64_lt_u => try self.binOp(i64, opIntLtU),
            .i64_gt_s => try self.binOp(i64, opIntGtS),
            .i64_gt_u => try self.binOp(i64, opIntGtU),
            .i64_le_s => try self.binOp(i64, opIntLeS),
            .i64_le_u => try self.binOp(i64, opIntLeU),
            .i64_ge_s => try self.binOp(i64, opIntGeS),
            .i64_ge_u => try self.binOp(i64, opIntGeU),

            // numeric instructions (2) f32
            // .f32_eq,
            // .f32_ne,
            // .f32_lt,
            // .f32_gt,
            // .f32_le,
            // .f32_ge,

            // numeric instructions (2) f64
            // .f64_eq,
            // .f64_ne,
            // .f64_lt,
            // .f64_gt,
            // .f64_le,
            // .f64_ge,

            // numeric instructions (3) i32
            .i32_clz => try self.unOp(i32, opIntClz),
            .i32_ctz => try self.unOp(i32, opIntCtz),
            .i32_popcnt => try self.unOp(i32, opIntPopcnt),
            .i32_add => try self.binOp(i32, opIntAdd),
            .i32_sub => try self.binOp(i32, opIntSub),
            .i32_mul => try self.binOp(i32, opIntMul),
            .i32_div_s => try self.binOp(i32, opIntDivS),
            .i32_div_u => try self.binOp(i32, opIntDivU),
            .i32_rem_s => try self.binOp(i32, opIntRemS),
            .i32_rem_u => try self.binOp(i32, opIntRemU),
            .i32_and => try self.binOp(i32, opIntAnd),
            .i32_or => try self.binOp(i32, opIntOr),
            .i32_xor => try self.binOp(i32, opIntXor),
            .i32_shl => try self.binOp(i32, opIntShl),
            .i32_shr_s => try self.binOp(i32, opIntShrS),
            .i32_shr_u => try self.binOp(i32, opIntShrU),
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
            .i64_div_u => try self.binOp(i64, opIntDivU),
            .i64_rem_s => try self.binOp(i64, opIntRemS),
            .i64_rem_u => try self.binOp(i64, opIntRemU),
            .i64_and => try self.binOp(i64, opIntAnd),
            .i64_or => try self.binOp(i64, opIntOr),
            .i64_xor => try self.binOp(i64, opIntXor),
            .i64_shl => try self.binOp(i64, opIntShl),
            .i64_shr_s => try self.binOp(i64, opIntShrS),
            .i64_shr_u => try self.binOp(i64, opIntShrU),
            .i64_rotl => try self.binOp(i64, opIntRotl),
            .i64_rotr => try self.binOp(i64, opIntRotr),

            // numeric instructions (3) f32
            // .f32_abs,
            .f32_neg => try self.unOp(f32, opFloatNeg),
            // .f32_ceil,
            // .f32_floor,
            // .f32_trunc,
            // .f32_nearest,
            // .f32_sqrt,
            // .f32_add,
            // .f32_sub,
            // .f32_mul,
            // .f32_div,
            // .f32_min,
            // .f32_max,
            // .f32_copy_sign,

            // numeric instructions (3) f64
            // .f64_abs,
            .f64_neg => try self.unOp(f64, opFloatNeg),
            // .f64_ceil,
            // .f64_floor,
            // .f64_trunc,
            // .f64_nearest,
            // .f64_sqrt,
            .f64_add => try self.binOp(f64, opFloatAdd),
            // .f64_sub,
            // .f64_mul,
            // .f64_div,
            // .f64_min,
            // .f64_max,
            // .f64_copy_sign,

            // numeric instructions (4)
            // .i32_wrap_i64,
            // .i32_trunc_f32_s,
            // .i32_trunc_f32_u,
            // .i32_trunc_f64_s,
            // .i32_trunc_f64_u,
            // .i64_extend_i32_s,
            // .i64_extend_i32_u,
            // .i64_trunc_f32_s,
            // .i64_trunc_f32_u,
            .i64_trunc_f64_s => {
                const value = self.stack.pop().value.asF64();
                const v: i64 = @intFromFloat(value);
                try self.stack.pushValue(v);
            },
            // .i64_trunc_f64_u,
            // .f32_convert_i32_s,
            // .f32_convert_i32_u,
            // .f32_convert_i64_s,
            // .f32_convert_i64_u,
            // .f32_demote_f64,
            .f64_convert_i32_s => {
                const value = self.stack.pop().value.asI32();
                const v: f64 = @floatFromInt(value);
                try self.stack.pushValue(v);
            },
            .f64_convert_i32_u => {
                const value = self.stack.pop().value.asI32();
                const v: f64 = @floatFromInt(value);
                try self.stack.pushValue(v);
            },
            // .f64_convert_i64_s,
            .f64_convert_i64_u => {
                const value = self.stack.pop().value.asI64();
                const v: f64 = @floatFromInt(value);
                try self.stack.pushValue(v);
            },
            .f64_promote_f32 => {
                const value = self.stack.pop().value.asF32();
                const v: f64 = value;
                try self.stack.pushValue(v);
            },
            // .i32_reinterpret_f32,
            // .i64_reinterpret_f64,
            // .f32_reinterpret_i32,
            // .f64_reinterpret_i64,

            // numeric instructions (5)
            .i32_extend8_s => try self.unOp(i32, opExtend8),
            .i32_extend16_s => try self.unOp(i32, opExtend16),
            .i64_extend8_s => try self.unOp(i64, opExtend8),
            .i64_extend16_s => try self.unOp(i64, opExtend16),
            .i64_extend32_s => try self.unOp(i64, opExtend32),

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

    inline fn opLocalGet(self: *Self, local_idx: wa.LocalIdx) (Error || error{OutOfMemory})!void {
        const frame = self.stack.topFrame();
        const val = frame.locals[local_idx];
        try self.stack.push(.{ .value = val });
    }

    inline fn opLocalSet(self: *Self, local_idx: wa.LocalIdx) (Error || error{OutOfMemory})!void {
        const frame = self.stack.topFrame();
        const val = self.stack.pop().value;
        frame.locals[local_idx] = val;
    }

    inline fn opLoad(self: *Self, comptime T: type, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!T {
        const module = self.stack.topFrame().module;
        const a = module.mem_addrs[0];
        const mem = &self.store.mems.items[a];

        const ea = self.stack.pop().value.i32;
        const ea_cast: u32 = @bitCast(ea);

        const ea_start_with_overflow = @addWithOverflow(ea_cast, mem_arg.offset);
        if (ea_start_with_overflow[1] == 1) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const ea_start = ea_start_with_overflow[0];
        if (ea_start > mem.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const size = @sizeOf(T);
        const ea_end_with_overflow = @addWithOverflow(ea_start, size);
        if (ea_end_with_overflow[1] == 1) {
            return Error.OutOfBoundsMemoryAccess;
        }

        const ea_end = ea_end_with_overflow[0];
        if (ea_end > mem.data.len) {
            return Error.OutOfBoundsMemoryAccess;
        }

        std.debug.print("=== {any} ==", .{mem.data[ea_start..ea_end]});
        const val = decode.safeNumCast(T, mem.data[ea_start..ea_end]);

        std.debug.print("===> {}\n", .{val});
        return val;
    }

    inline fn opI64Load(self: *Self, comptime T: type, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!void {
        const val = try self.opLoad(T, mem_arg);
        try self.stack.push(.{ .value = .{ .i64 = val } });
    }

    inline fn opI32Load(self: *Self, comptime T: type, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!void {
        const val = try self.opLoad(T, mem_arg);
        try self.stack.push(.{ .value = .{ .i32 = val } });
    }

    inline fn opF32Load(self: *Self, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!void {
        const val = try self.opLoad(f32, mem_arg);
        try self.stack.pushValue(val);
    }

    inline fn opF64Load(self: *Self, mem_arg: Instruction.MemArg) (Error || error{OutOfMemory})!void {
        const val = try self.opLoad(f64, mem_arg);
        try self.stack.pushValue(val);
    }

    inline fn opI32Store8(self: *Self, mem_arg: Instruction.MemArg) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const a = module.mem_addrs[0];
        const mem = &self.store.mems.items[a];

        const c = self.stack.pop().value;
        const i = self.stack.pop().value.i32;

        var ea: u32 = @intCast(i);
        ea += mem_arg.offset;
        mem.data[ea] = @intCast(c.i32);
        std.debug.print("mem.data=[{s}]\n", .{mem.data[0..30]});
    }

    inline fn opDataDrop(self: *Self, data_idx: wa.DataIdx) error{OutOfMemory}!void {
        const module = self.stack.topFrame().module;
        const a = module.data_addrs[data_idx];
        const data = &self.store.datas.items[a];
        data.data = &.{};
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

        var n = self.stack.pop().value.i32;
        var s = self.stack.pop().value.i32;
        var d = self.stack.pop().value.i32;

        //const ea1: u64 = @intCast(s);
        //const ea2: u64 = @intCast(d);

        while (n > 0) : (n -= 1) {
            const b = data.data[@intCast(s)];
            try self.stack.push(.{ .value = .{ .i32 = d } });
            try self.stack.push(.{ .value = .{ .i32 = @intCast(b) } });

            const ins: Instruction = .{ .i32_store8 = .{ .@"align" = 0, .offset = 0 } };
            try self.execOneInstruction(ins);
            s += 1;
            d += 1;
        }
    }

    inline fn unOp(self: *Self, comptime T: type, comptime f: fn (type, T) Error!T) (Error || error{OutOfMemory})!void {
        const value = self.stack.pop().value.as(T);
        const result = try f(T, value);
        try self.stack.pushValue(result);
    }

    inline fn binOp(self: *Self, comptime T: type, comptime f: fn (type, T, T) Error!T) (Error || error{OutOfMemory})!void {
        const rhs = self.stack.pop().value.as(T);
        const lhs = self.stack.pop().value.as(T);
        const result = try f(T, lhs, rhs);
        try self.stack.pushValue(result);
    }

    fn opIntClz(comptime T: type, value: T) Error!T {
        return @clz(value);
    }

    fn opIntCtz(comptime T: type, value: T) Error!T {
        return @ctz(value);
    }

    fn opIntPopcnt(comptime T: type, value: T) Error!T {
        return @popCount(value);
    }

    fn opIntEqz(comptime T: type, value: T) Error!T {
        return if (value == 0) 1 else 0;
    }

    fn opExtend8(comptime T: type, value: T) Error!T {
        const result: i8 = @truncate(value);
        return result;
    }

    fn opExtend16(comptime T: type, value: T) Error!T {
        const result: i16 = @truncate(value);
        return result;
    }

    fn opExtend32(comptime T: type, value: T) Error!T {
        const result: i32 = @truncate(value);
        return result;
    }

    fn opIntEq(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs == rhs) 1 else 0;
    }

    fn opIntNe(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs != rhs) 1 else 0;
    }

    fn opIntLtS(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs < rhs) 1 else 0;
    }

    fn opIntLtU(comptime T: type, lhs: T, rhs: T) Error!T {
        if (T == i32) {
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            return if (l < r) 1 else 0;
        } else {
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            return if (l < r) 1 else 0;
        }
    }

    fn opIntGtS(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs > rhs) 1 else 0;
    }

    fn opIntGtU(comptime T: type, lhs: T, rhs: T) Error!T {
        if (T == i32) {
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            return if (l > r) 1 else 0;
        } else {
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            return if (l > r) 1 else 0;
        }
    }

    fn opIntLeS(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs <= rhs) 1 else 0;
    }

    fn opIntLeU(comptime T: type, lhs: T, rhs: T) Error!T {
        if (T == i32) {
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            return if (l <= r) 1 else 0;
        } else {
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            return if (l <= r) 1 else 0;
        }
    }

    fn opIntGeS(comptime T: type, lhs: T, rhs: T) Error!T {
        return if (lhs >= rhs) 1 else 0;
    }

    fn opIntGeU(comptime T: type, lhs: T, rhs: T) Error!T {
        if (T == i32) {
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            return if (l >= r) 1 else 0;
        } else {
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            return if (l >= r) 1 else 0;
        }
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
        if (T == i32) {
            if (lhs == -2147483648 and rhs == -1) return 0;
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            const res = @divTrunc(l, r);
            return @bitCast(res);
        } else {
            if (lhs == -9223372036854775808 and rhs == -1) return 0;
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            const res = @divTrunc(l, r);
            return @bitCast(res);
        }
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
        if (T == i32) {
            const num: u32 = @bitCast(lhs);
            const den: u32 = @bitCast(rhs);
            const res = @mod(num, den);
            return @bitCast(res);
        } else {
            const num: u64 = @bitCast(lhs);
            const den: u64 = @bitCast(rhs);
            const res = @mod(num, den);
            return @bitCast(res);
        }
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
        if (T == i32) {
            const l: u32 = @bitCast(lhs);
            const r: u32 = @bitCast(rhs);
            const res = l >> @intCast(@mod(r, @bitSizeOf(T)));
            return @bitCast(res);
        } else {
            const l: u64 = @bitCast(lhs);
            const r: u64 = @bitCast(rhs);
            const res = l >> @intCast(@mod(r, @bitSizeOf(T)));
            return @bitCast(res);
        }
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

    fn opFloatNeg(comptime T: type, value: T) Error!T {
        return -value;
    }

    fn opFloatAdd(comptime T: type, lhs: T, rhs: T) Error!T {
        return lhs + rhs;
    }

    fn resolveImports(store: types.Store, module: wa.Module, allocator: std.mem.Allocator) error{OutOfMemory}![]const types.ExternalValue {
        _ = allocator;
        _ = module;
        _ = store;
        // var external_imports = try allocator.alloc(types.ExternalValue, module.imports.len);

        // for (module.imports, 0..) |imp, i| {
        //     external_imports[i] = switch (imp.desc) {
        //         .func => |idx| .{ .function = idx },
        //         .table => |idx| .{ .table = idx },
        //         .memory => |idx| .{ .memory = idx },
        //         .global => |idx| .{ .global = idx },
        //     };
        // }
        return &.{};
    }

    /// `allocfunc` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#functions
    fn allocFunc(store: *types.Store, func: wa.Func, mod_inst: *types.ModuleInst) error{OutOfMemory}!types.FuncAddr {
        const inst = types.FuncInst{
            .type = mod_inst.types[func.type],
            .module = mod_inst,
            .code = func,
        };
        return try appendElement(types.FuncInst, &store.funcs, inst);
    }

    /// `alloctable` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#tables
    fn allocTable(store: *types.Store, table: wa.TableType, allocator: std.mem.Allocator) error{OutOfMemory}!types.TableAddr {
        var elem = try allocator.alloc(?types.RefValue, table.limit.min);
        @memset(elem, null);
        const inst = types.TableInst{
            .type = table,
            .elem = elem,
        };
        return try appendElement(types.TableInst, &store.tables, inst);
    }

    /// `allocmemory` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#memories
    fn allocMemory(store: *types.Store, mem: wa.MemoryType, allocator: std.mem.Allocator) error{OutOfMemory}!types.MemAddr {
        const inst = types.MemInst{
            .type = mem,
            .data = try allocator.alloc(u8, mem.limits.min * 65_536),
        };
        @memset(inst.data, 0);
        return try appendElement(types.MemInst, &store.mems, inst);
    }

    /// `allocglobal` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#globals
    fn allocGlobal(store: *types.Store, global: wa.Global, value: types.Value) error{OutOfMemory}!types.MemAddr {
        const inst = types.GlobalInst{
            .type = global.type,
            .value = value,
        };
        return try appendElement(types.GlobalInst, &store.globals, inst);
    }

    /// `allocelem` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#element-segments
    fn allocElement(store: *types.Store, elememt: wa.Element, refs: []types.RefValue) error{OutOfMemory}!types.ElemAddr {
        const inst = types.ElemInst{
            .type = elememt.type,
            .elem = refs,
        };
        return try appendElement(types.ElemInst, &store.elems, inst);
    }

    /// `allocdata` in wasm spec
    /// https://webassembly.github.io/spec/core/exec/modules.html#alloc-data
    fn allocData(store: *types.Store, data: wa.Data) error{OutOfMemory}!types.DataAddr {
        const inst = types.DataInst{
            .data = data.init,
        };
        return try appendElement(types.DataInst, &store.datas, inst);
    }

    fn appendElement(comptime T: type, array: *std.ArrayList(T), elem: T) error{OutOfMemory}!u32 {
        const addr: u32 = @intCast(array.items.len);
        try array.append(elem);
        return addr;
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
            inline else => |idx| .{ .jump = idx + 1 }, // jump next to `end`
        };
    }

    pub fn newAtOpIf(info: Instruction.IfBlockInfo, cond: i32) FlowControl {
        if (cond != 0) {
            return .none;
        } else if (info.@"else" != null) {
            return .{ .jump = info.@"else".? };
        } else {
            return .exit; // FIXME: should be unreachable?
        }
    }
};
