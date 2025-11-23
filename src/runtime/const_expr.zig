const std = @import("std");
const core = @import("wasm-core");
const Instruction = core.Instruction;
const types = @import("./types.zig");
const Value = types.Value;
const GlobalInst = types.GlobalInst;

pub const Error = error{
    InvalidConstExpr,
    StackUnderflow,
    TypeMismatch,
};

pub fn evaluateConstExpr(instrs: []const Instruction, globals: []const GlobalInst) (Error || error{OutOfMemory})!Value {
    var stack: [8]Value = undefined;
    var sp: usize = 0;

    for (instrs) |instr| {
        switch (instr) {
            .i32_const => |v| {
                stack[sp] = .{ .i32 = v };
                sp += 1;
            },
            .i64_const => |v| {
                stack[sp] = .{ .i64 = v };
                sp += 1;
            },
            .f32_const => |v| {
                stack[sp] = .{ .f32 = @bitCast(v) };
                sp += 1;
            },
            .f64_const => |v| {
                stack[sp] = .{ .f64 = @bitCast(v) };
                sp += 1;
            },
            .global_get => |idx| {
                stack[sp] = globals[idx].value;
                sp += 1;
            },
            .i32_add => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i32 +%= stack[sp].i32;
            },
            .i32_sub => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i32 -%= stack[sp].i32;
            },
            .i32_mul => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i32 *%= stack[sp].i32;
            },
            .i64_add => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i64 +%= stack[sp].i64;
            },
            .i64_sub => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i64 -%= stack[sp].i64;
            },
            .i64_mul => {
                if (sp < 2) return Error.StackUnderflow;
                sp -= 1;
                stack[sp - 1].i64 *%= stack[sp].i64;
            },
            .end => break,
            else => return Error.InvalidConstExpr,
        }
    }

    if (sp != 1) return Error.StackUnderflow;
    return stack[0];
}
