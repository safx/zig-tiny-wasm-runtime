const std = @import("std");
const types = @import("./types.zig");
const Action = types.Action;
const Command = types.Command;
const Result = types.Result;
const Const = types.Const;
const Failure = types.Failure;

pub fn readJsonFromFile(file: std.fs.File, allocator: std.mem.Allocator) ![]const types.Command {
    const buffer = try file.reader().readAllAlloc(allocator, 10_000_000);
    const value = try std.json.parseFromSliceLeaky(std.json.Value, allocator, buffer, .{});

    return try commandArrayFromJson(value, allocator);
}

fn commandArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Command {
    var array = std.ArrayList(Command).init(allocator);
    for (json.object.get("commands").?.array.items) |cmd_json| {
        const cmd = try commandFromJson(cmd_json, allocator);
        try array.append(cmd);
    }
    return array.toOwnedSlice();
}

fn commandFromJson(json: std.json.Value, allocator: std.mem.Allocator) !Command {
    const cmd_type = json.object.get("type").?.string;
    const line: u32 = @intCast(json.object.get("line").?.integer);
    if (strcmp(cmd_type, "module")) {
        const file_name = json.object.get("filename").?.string;
        return Command{ .module = .{ .line = line, .file_name = file_name } };
    } else if (strcmp(cmd_type, "module_quote")) {
        return Command.module_quote;
    } else if (strcmp(cmd_type, "register")) {
        return Command.register;
    } else if (strcmp(cmd_type, "assert_return")) {
        const action = try actionFromJson(json.object.get("action").?, allocator);
        const expected = try resultArrayFromJson(json.object.get("expected").?, allocator);
        return Command{ .assert_return = .{ .line = line, .action = action, .expected = expected } };
    } else if (strcmp(cmd_type, "assert_trap")) {
        const action = try actionFromJson(json.object.get("action").?, allocator);
        const text = json.object.get("text").?.string;
        return Command{ .assert_trap = .{ .line = line, .action = action, .trap = errorFromString(text) } };
    } else if (strcmp(cmd_type, "assert_exhaustion")) {
        return Command.assert_exhaustion;
    } else if (strcmp(cmd_type, "assert_malformed")) {
        return Command.assert_malformed;
    } else if (strcmp(cmd_type, "assert_invalid")) {
        return Command.assert_invalid;
    } else if (strcmp(cmd_type, "assert_unlinkable")) {
        return Command.assert_unlinkable;
    } else {
        unreachable;
    }
}

fn argArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Const {
    var array = std.ArrayList(Const).init(allocator);
    for (json.array.items) |arg_json| {
        const arg = try argFromJson(arg_json);
        try array.append(arg);
    }
    return try array.toOwnedSlice();
}

fn argFromJson(json: std.json.Value) !Const {
    const type_ = json.object.get("type").?.string;
    const value = json.object.get("value").?.string;
    if (strcmp(type_, "i32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Const{ .i32_const = @bitCast(num) };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Const{ .i64_const = @bitCast(num) };
    } else if (strcmp(type_, "f32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Const{ .f32_const = num };
    } else if (strcmp(type_, "f64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Const{ .f64_const = num };
    } else {
        unreachable;
    }
}

fn resultArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Result {
    var array = std.ArrayList(Result).init(allocator);
    for (json.array.items) |arg_json| {
        const arg = try resultFromJson(arg_json);
        try array.append(arg);
    }
    return try array.toOwnedSlice();
}

fn resultFromJson(json: std.json.Value) !Result {
    const type_ = json.object.get("type").?.string;
    const value = json.object.get("value").?.string;
    if (strcmp(type_, "i32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return .{ .@"const" = .{ .i32_const = @bitCast(num) } };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return .{ .@"const" = .{ .i64_const = @bitCast(num) } };
    } else if (strcmp(type_, "f32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return .{ .@"const" = .{ .f32_const = num } };
    } else if (strcmp(type_, "f64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return .{ .@"const" = .{ .f64_const = num } };
    } else {
        unreachable;
    }
}

fn actionFromJson(json: std.json.Value, allocator: std.mem.Allocator) !Action {
    const cmd_type: []const u8 = json.object.get("type").?.string;
    if (strcmp(cmd_type, "invoke")) {
        const field = json.object.get("field").?.string;
        const args: []const Const = try argArrayFromJson(json.object.get("args").?, allocator);
        return .{ .invoke = .{ .field = field, .args = args } };
    } else if (strcmp(cmd_type, "get")) {
        return Action.get;
    } else {
        std.debug.print("unknown action type: {s}", .{cmd_type});
        unreachable;
    }
}

fn errorFromString(str: []const u8) types.Error {
    const E = types.Error;
    if (strcmp(str, "integer divide by zero")) {
        return E.IntegerDivideByZero;
    } else if (strcmp(str, "integer overflow")) {
        return E.IntegerOverflow;
    } else if (strcmp(str, "out of bounds memory access")) {
        return E.OutOfBoundsMemoryAccess;
    } else {
        std.debug.print("??? {s}\n", .{str});
        unreachable;
    }
}

fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
