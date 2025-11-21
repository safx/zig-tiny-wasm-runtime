const std = @import("std");
const spec_types = @import("spec-types");
const spec_test_errors = @import("spec-test-errors");

// Type aliases for convenience
const Action = spec_types.command.Action;
const Command = spec_types.command.Command;
const Result = spec_types.command.Result;
const Value = spec_types.command.Value;
const ExternAddr = spec_types.command.ExternAddr;
const FuncAddr = spec_types.command.FuncAddr;
const FloatType = spec_types.command.FloatType;

pub fn readJsonFromFile(file: std.fs.File, allocator: std.mem.Allocator) ![]const Command {
    const buffer = try file.readToEndAlloc(allocator, 10_000_000);
    defer allocator.free(buffer);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, buffer, .{});
    defer parsed.deinit();

    return try commandArrayFromJson(parsed.value, allocator);
}

fn commandArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Command {
    var array: std.ArrayList(Command) = .empty;
    for (json.object.get("commands").?.array.items) |cmd_json| {
        const cmd = try commandFromJson(cmd_json, allocator);
        try array.append(allocator, cmd);
    }
    return array.toOwnedSlice(allocator);
}

fn commandFromJson(json: std.json.Value, allocator: std.mem.Allocator) !Command {
    const obj = json.object;

    const cmd_type = obj.get("type").?.string;
    const line: u32 = @intCast(obj.get("line").?.integer);
    if (strcmp(cmd_type, "action")) {
        const action = try actionFromJson(obj.get("action").?, allocator);
        return .{ .action = .{ .line = line, .action = action } };
    } else if (strcmp(cmd_type, "module")) {
        const file_name = try allocator.dupe(u8, obj.get("filename").?.string);
        const name = if (getStringOrNull(json.object, "name")) |n| try allocator.dupe(u8, n) else null;
        return .{ .module = .{ .line = line, .file_name = file_name, .name = name, .module_data = null } };
    } else if (strcmp(cmd_type, "module_quote")) {
        return .module_quote;
    } else if (strcmp(cmd_type, "register")) {
        const name = if (getStringOrNull(json.object, "name")) |n| try allocator.dupe(u8, n) else null;
        const as_name = try allocator.dupe(u8, obj.get("as").?.string);
        return .{ .register = .{ .as_name = as_name, .name = name } };
    } else if (strcmp(cmd_type, "assert_return")) {
        const action = try actionFromJson(obj.get("action").?, allocator);
        const expected = try resultArrayFromJson(obj.get("expected").?, allocator);
        return .{ .assert_return = .{ .line = line, .action = action, .expected = expected } };
    } else if (strcmp(cmd_type, "assert_trap")) {
        const action = try actionFromJson(obj.get("action").?, allocator);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_trap = .{ .line = line, .action = action, .error_text = text } };
    } else if (strcmp(cmd_type, "assert_exhaustion")) {
        const action = try actionFromJson(obj.get("action").?, allocator);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_exhaustion = .{ .line = line, .action = action, .error_text = text } };
    } else if (strcmp(cmd_type, "assert_malformed")) {
        const file_name = try allocator.dupe(u8, obj.get("filename").?.string);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_malformed = .{ .line = line, .file_name = file_name, .error_text = text } };
    } else if (strcmp(cmd_type, "assert_invalid")) {
        const file_name = try allocator.dupe(u8, obj.get("filename").?.string);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_invalid = .{ .line = line, .file_name = file_name, .error_text = text } };
    } else if (strcmp(cmd_type, "assert_unlinkable")) {
        const file_name = try allocator.dupe(u8, obj.get("filename").?.string);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_unlinkable = .{ .line = line, .file_name = file_name, .error_text = text } };
    } else if (strcmp(cmd_type, "assert_uninstantiable")) {
        const file_name = try allocator.dupe(u8, obj.get("filename").?.string);
        const text = try allocator.dupe(u8, obj.get("text").?.string);
        return .{ .assert_uninstantiable = .{ .line = line, .file_name = file_name, .error_text = text } };
    } else {
        std.debug.print("? Unknown command {s}\n", .{cmd_type});
        return error.UnknownCommand;
    }
}

fn argArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Value {
    var array: std.ArrayList(Value) = .empty;
    for (json.array.items) |arg_json| {
        const arg = try argFromJson(arg_json);
        try array.append(allocator, arg);
    }
    return try array.toOwnedSlice(allocator);
}

fn argFromJson(json: std.json.Value) !Value {
    const type_ = json.object.get("type").?.string;
    if (strcmp(type_, "v128"))
        return try vectorArgFromJson(json);

    const value = json.object.get("value").?.string;
    if (strcmp(type_, "i32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Value{ .i32 = @bitCast(num) };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Value{ .i64 = @bitCast(num) };
    } else if (strcmp(type_, "f32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Value{ .f32 = num }; // Store as bit pattern
    } else if (strcmp(type_, "f64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Value{ .f64 = num }; // Store as bit pattern
    } else if (strcmp(type_, "externref")) {
        var num: ?ExternAddr = null;
        if (!strcmp(value, "null"))
            num = try std.fmt.parseInt(ExternAddr, value, 10);
        return Value{ .extern_ref = num };
    } else if (strcmp(type_, "funcref")) {
        var num: ?ExternAddr = null;
        if (!strcmp(value, "null"))
            num = try std.fmt.parseInt(ExternAddr, value, 10);
        return Value{ .func_ref = num };
    } else {
        std.debug.print("? Unknown arg {s}\n", .{type_});
        return error.UnknownArgType;
    }
}

fn vectorArgFromJson(json: std.json.Value) !Value {
    const lane_type = json.object.get("lane_type").?.string;
    const arr = json.object.get("value").?.array;

    const val = if (strcmp(lane_type, "i8"))
        vectorArgFromJsonArray(u8, arr)
    else if (strcmp(lane_type, "i16"))
        vectorArgFromJsonArray(u16, arr)
    else if (strcmp(lane_type, "i32"))
        vectorArgFromJsonArray(u32, arr)
    else if (strcmp(lane_type, "i64"))
        vectorArgFromJsonArray(u64, arr)
    else if (strcmp(lane_type, "f32"))
        vectorArgFromJsonArray(u32, arr)
    else if (strcmp(lane_type, "f64"))
        vectorArgFromJsonArray(u64, arr)
    else
        unreachable;

    return .{ .v128 = try val };
}

fn vectorArgFromJsonArray(comptime T: type, arr: std.json.Array) !i128 {
    var val: u128 = 0;

    for (arr.items, 0..) |v, i| {
        const p = try std.fmt.parseInt(T, v.string, 10);
        val |= @as(u128, p) << (@as(u7, @intCast(i)) * @bitSizeOf(T));
    }
    return @bitCast(val);
}

fn resultArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Result {
    var array: std.ArrayList(Result) = .empty;
    for (json.array.items) |arg_json| {
        const arg = try resultFromJson(arg_json);
        try array.append(allocator, arg);
    }
    return try array.toOwnedSlice(allocator);
}

fn resultFromJson(json: std.json.Value) !Result {
    const type_ = json.object.get("type").?.string;
    if (strcmp(type_, "v128"))
        return try vectorFromJson(json);

    const value = json.object.get("value").?.string;
    if (strcmp(type_, "i32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return .{ .i32 = @bitCast(num) };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return .{ .i64 = @bitCast(num) };
    } else if (strcmp(type_, "f32")) {
        return .{ .f32 = try floatFromJson(u32, value) };
    } else if (strcmp(type_, "f64")) {
        return .{ .f64 = try floatFromJson(u64, value) };
    } else if (strcmp(type_, "externref")) {
        const num: ?ExternAddr = if (strcmp(value, "null")) null else try std.fmt.parseInt(ExternAddr, value, 10);
        return .{ .extern_ref = num };
    } else if (strcmp(type_, "funcref")) {
        const num: ?FuncAddr = if (strcmp(value, "null")) null else try std.fmt.parseInt(FuncAddr, value, 10);
        return .{ .func_ref = num };
    } else {
        std.debug.print("? Unknown result {s}\n", .{type_});
        return error.UnknownResultType;
    }
}

fn vectorFromJson(json: std.json.Value) !Result {
    const lane_type = json.object.get("lane_type").?.string;
    const arr = json.object.get("value").?.array;

    if (strcmp(lane_type, "i8"))
        return .{ .v128 = try vectorArgFromJsonArray(u8, arr) };
    if (strcmp(lane_type, "i16"))
        return .{ .v128 = try vectorArgFromJsonArray(u16, arr) };
    if (strcmp(lane_type, "i32"))
        return .{ .v128 = try vectorArgFromJsonArray(u32, arr) };
    if (strcmp(lane_type, "i64"))
        return .{ .v128 = try vectorArgFromJsonArray(u64, arr) };
    if (strcmp(lane_type, "f32"))
        return try vectorFloatFromJsonArray(u32, arr);
    if (strcmp(lane_type, "f64"))
        return try vectorFloatFromJsonArray(u64, arr);

    unreachable;
}

fn vectorFloatFromJsonArray(comptime T: type, arr: std.json.Array) !Result {
    const len = 16 / @sizeOf(T);
    var val: [len]FloatType(T) = undefined;
    std.debug.assert(arr.items.len == len);

    for (arr.items, 0..) |v, i| {
        val[i] = try floatFromJson(T, v.string);
    }

    var include_nan = false;
    for (val) |v| {
        include_nan = include_nan or v == .nan_arithmetic or v == .nan_canonical;
    }
    if (!include_nan) {
        return .{ .v128 = try vectorArgFromJsonArray(T, arr) };
    }

    return switch (T) {
        u32 => .{ .vec_f32 = val },
        u64 => .{ .vec_f64 = val },
        else => unreachable,
    };
}

fn floatFromJson(comptime T: type, value: []const u8) !FloatType(T) {
    if (std.mem.eql(u8, "nan:canonical", value))
        return .nan_canonical;
    if (std.mem.eql(u8, "nan:arithmetic", value))
        return .nan_arithmetic;
    return .{ .value = try std.fmt.parseInt(T, value, 10) };
}

fn actionFromJson(json: std.json.Value, allocator: std.mem.Allocator) !Action {
    const obj = json.object;

    const cmd_type: []const u8 = obj.get("type").?.string;
    if (strcmp(cmd_type, "invoke")) {
        const module = if (getStringOrNull(json.object, "module")) |m| try allocator.dupe(u8, m) else null;
        const field = try allocator.dupe(u8, obj.get("field").?.string);
        const args: []const Value = try argArrayFromJson(obj.get("args").?, allocator);
        return .{ .invoke = .{ .field = field, .args = args, .module = module } };
    } else if (strcmp(cmd_type, "get")) {
        const module = if (getStringOrNull(json.object, "module")) |m| try allocator.dupe(u8, m) else null;
        const field = try allocator.dupe(u8, obj.get("field").?.string);
        return .{ .get = .{ .field = field, .module = module } };
    } else {
        std.debug.print("? Unknown action type: {s}\n", .{cmd_type});
        return error.UnknownActionType;
    }
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

inline fn getStringOrNull(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    return if (obj.get(key)) |v| v.string else null;
}
