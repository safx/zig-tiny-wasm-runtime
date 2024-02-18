const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-runtime");
    usingnamespace @import("./types.zig");
};
const errors = @import("./errors.zig");
const Action = types.Action;
const Command = types.Command;
const Result = types.Result;
const Value = types.Value;

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
    if (strcmp(cmd_type, "action")) {
        const action = try actionFromJson(json.object.get("action").?, allocator);
        return .{ .action = .{ .line = line, .action = action } };
    } else if (strcmp(cmd_type, "module")) {
        const file_name = json.object.get("filename").?.string;
        const name = getStringOrNull(json.object, "name");
        return .{ .module = .{ .line = line, .file_name = file_name, .name = name } };
    } else if (strcmp(cmd_type, "module_quote")) {
        return .module_quote;
    } else if (strcmp(cmd_type, "register")) {
        const name = getStringOrNull(json.object, "name");
        const as_name = json.object.get("as").?.string;
        return .{ .register = .{ .as_name = as_name, .name = name } };
    } else if (strcmp(cmd_type, "assert_return")) {
        const action = try actionFromJson(json.object.get("action").?, allocator);
        const expected = try resultArrayFromJson(json.object.get("expected").?, allocator);
        return .{ .assert_return = .{ .line = line, .action = action, .expected = expected } };
    } else if (strcmp(cmd_type, "assert_trap")) {
        const action = try actionFromJson(json.object.get("action").?, allocator);
        const text = json.object.get("text").?.string;
        return .{ .assert_trap = .{ .line = line, .action = action, .trap = errors.errorFromString(text) } };
    } else if (strcmp(cmd_type, "assert_exhaustion")) {
        return .assert_exhaustion;
    } else if (strcmp(cmd_type, "assert_malformed")) {
        return .assert_malformed;
    } else if (strcmp(cmd_type, "assert_invalid")) {
        return .assert_invalid;
    } else if (strcmp(cmd_type, "assert_unlinkable")) {
        return .assert_unlinkable;
    } else if (strcmp(cmd_type, "assert_uninstantiable")) {
        return .assert_uninstantiable;
    } else {
        std.debug.print("? Unknown command {s}\n", .{cmd_type});
        unreachable;
    }
}

fn argArrayFromJson(json: std.json.Value, allocator: std.mem.Allocator) ![]const Value {
    var array = std.ArrayList(Value).init(allocator);
    for (json.array.items) |arg_json| {
        const arg = try argFromJson(arg_json);
        try array.append(arg);
    }
    return try array.toOwnedSlice();
}

fn argFromJson(json: std.json.Value) !Value {
    const type_ = json.object.get("type").?.string;
    const value = json.object.get("value").?.string;
    if (strcmp(type_, "i32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Value{ .i32 = @bitCast(num) };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Value{ .i64 = @bitCast(num) };
    } else if (strcmp(type_, "f32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return Value{ .f32 = num };
    } else if (strcmp(type_, "f64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return Value{ .f64 = num };
    } else if (strcmp(type_, "externref")) {
        var num: ?types.ExternAddr = null;
        if (!strcmp(value, "null")) {
            num = try std.fmt.parseInt(types.ExternAddr, value, 10);
        }
        return Value{ .extern_ref = num };
    } else if (strcmp(type_, "funcref")) {
        var num: ?types.ExternAddr = null;
        if (!strcmp(value, "null")) {
            num = try std.fmt.parseInt(types.ExternAddr, value, 10);
        }
        return Value{ .func_ref = num };
    } else {
        std.debug.print("? Unknown arg {s}\n", .{type_});
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
        return .{ .@"const" = .{ .i32 = @bitCast(num) } };
    } else if (strcmp(type_, "i64")) {
        const num = try std.fmt.parseInt(u64, value, 10);
        return .{ .@"const" = .{ .i64 = @bitCast(num) } };
    } else if (strcmp(type_, "f32")) {
        if (std.mem.eql(u8, "nan:canonical", value)) {
            return .f32_nan_canonical;
        }
        if (std.mem.eql(u8, "nan:arithmetic", value)) {
            return .f32_nan_arithmetic;
        }
        const num = try std.fmt.parseInt(u32, value, 10);
        return .{ .@"const" = .{ .f32 = num } };
    } else if (strcmp(type_, "f64")) {
        if (std.mem.eql(u8, "nan:canonical", value)) {
            return .f64_nan_canonical;
        }
        if (std.mem.eql(u8, "nan:arithmetic", value)) {
            return .f64_nan_arithmetic;
        }
        const num = try std.fmt.parseInt(u64, value, 10);
        return .{ .@"const" = .{ .f64 = num } };
    } else if (strcmp(type_, "externref")) {
        var num: ?types.ExternAddr = null;
        if (!strcmp(value, "null")) {
            num = try std.fmt.parseInt(types.ExternAddr, value, 10);
        }
        return .{ .@"const" = .{ .extern_ref = num } };
    } else if (strcmp(type_, "funcref")) {
        var num: ?types.ExternAddr = null;
        if (!strcmp(value, "null")) {
            num = try std.fmt.parseInt(types.ExternAddr, value, 10);
        }
        return .{ .@"const" = .{ .func_ref = num } };
    } else {
        std.debug.print("? Unknown result {s}\n", .{type_});
        unreachable;
    }
}

fn actionFromJson(json: std.json.Value, allocator: std.mem.Allocator) !Action {
    const cmd_type: []const u8 = json.object.get("type").?.string;
    if (strcmp(cmd_type, "invoke")) {
        const module = getStringOrNull(json.object, "module");
        const field = json.object.get("field").?.string;
        const args: []const Value = try argArrayFromJson(json.object.get("args").?, allocator);
        return .{ .invoke = .{ .field = field, .args = args, .module = module } };
    } else if (strcmp(cmd_type, "get")) {
        const module = getStringOrNull(json.object, "module");
        const field = json.object.get("field").?.string;
        return .{ .get = .{ .field = field, .module = module } };
    } else {
        std.debug.print("? Unknown action type: {s}", .{cmd_type});
        unreachable;
    }
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

inline fn getStringOrNull(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    return if (obj.get(key)) |v| v.string else null;
}
