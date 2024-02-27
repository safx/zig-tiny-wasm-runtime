const std = @import("std");
const core = @import("wasm-core");
const CallStackExhausted = @import("./errors.zig").Error.CallStackExhausted;
const Error = error{CallStackExhausted};
pub const ModuleInst = @import("./module_instance.zig").ModuleInst;

pub const Store = struct {
    funcs: std.ArrayList(FuncInst),
    tables: std.ArrayList(TableInst),
    mems: std.ArrayList(MemInst),
    globals: std.ArrayList(GlobalInst),
    elems: std.ArrayList(ElemInst),
    datas: std.ArrayList(DataInst),

    pub fn new(allocator: std.mem.Allocator) Store {
        return .{
            .funcs = std.ArrayList(FuncInst).init(allocator),
            .tables = std.ArrayList(TableInst).init(allocator),
            .mems = std.ArrayList(MemInst).init(allocator),
            .globals = std.ArrayList(GlobalInst).init(allocator),
            .elems = std.ArrayList(ElemInst).init(allocator),
            .datas = std.ArrayList(DataInst).init(allocator),
        };
    }
};

pub const Stack = struct {
    const Self = @This();
    const max_stack_size = 4096;

    array: std.ArrayList(StackItem),

    pub fn new(allocator: std.mem.Allocator) Stack {
        return .{
            .array = std.ArrayList(StackItem).init(allocator),
        };
    }

    inline fn checkStackExhausted(self: Self) Error!void {
        if (self.array.items.len > max_stack_size) {
            return CallStackExhausted;
        }
    }

    pub fn pushValueAs(self: *Self, comptime T: type, value: T) Error!void {
        const val = Value.from(value);
        self.push(.{ .value = val }) catch return CallStackExhausted;
        try self.checkStackExhausted();
    }

    pub fn push(self: *Self, value: StackItem) Error!void {
        self.array.append(value) catch return CallStackExhausted;
        try self.checkStackExhausted();
    }

    pub fn appendSlice(self: *Self, values: []const StackItem) Error!void {
        self.array.appendSlice(values) catch return CallStackExhausted;
        try self.checkStackExhausted();
    }

    pub fn insertAt(self: *Self, index_from_top: usize, value: StackItem) Error!void {
        const pos = self.array.items.len - index_from_top;
        self.array.insert(pos, value) catch return CallStackExhausted;
        try self.checkStackExhausted();
    }

    pub fn pop(self: *Self) StackItem {
        return self.array.pop();
    }

    pub fn getNthLabelFromTop(self: *Self, num_labels: usize) Label {
        var n = num_labels + 1;
        var i = self.array.items.len;
        while (i > 0 and n > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == .label) {
                n -= 1;
                if (n == 0)
                    return item.label;
            }
        }
        unreachable;
    }

    /// pops values until `popped_values` is full
    pub fn popValues(self: *Self, popped_values: *[]StackItem) Error![]const StackItem {
        const len = self.array.items.len;
        const num_items = popped_values.len;
        const new_len = len - num_items;

        @memcpy(popped_values.*, self.array.items[new_len..len]);
        self.array.resize(new_len) catch return CallStackExhausted;

        return popped_values.*;
    }

    /// returns the current activation frame
    pub fn topFrame(self: Self) ActivationFrame {
        var i = self.array.items.len;
        while (i > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == .frame)
                return item.frame;
        }
        unreachable;
    }

    /// updates instraction pointer in the current frame
    pub fn updateTopFrameIp(self: *Self, ip: u32) void {
        var i = self.array.items.len;
        while (i > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == .frame) {
                self.array.items[i - 1].frame.ip = ip;
                return;
            }
        }
        unreachable;
    }

    /// finds the uppermost label and remove it.
    pub fn popUppermostLabel(self: *Self) ?Label {
        var len = self.array.items.len;
        while (len > 0) : (len -= 1) {
            const pos = len - 1;
            const item = self.array.items[pos];
            if (item == .label) {
                const ret = self.array.orderedRemove(pos);
                return ret.label;
            }
        }
        return null;
    }

    /// drops all values until the uppermost label and drop the label as well.
    pub fn popValuesAndUppermostLabel(self: *Self) void {
        // FIXME: find and resize
        while (true) {
            const item = self.pop();
            switch (item) {
                .value => continue,
                .label => return,
                .frame => std.debug.assert(false),
            }
        }
    }

    /// drops all values and labels until the uppermost frame.
    pub fn popValuesAndLabelsUntilFrame(self: *Self) void {
        // FIXME: find and resize
        while (true) {
            const item = self.pop();
            if (item == .frame)
                return;
        }
    }

    pub fn hasFrame(self: *Self) bool {
        for (self.array.items) |e| {
            if (e == .frame)
                return true;
        }
        return false;
    }
};

pub const StackItem = union(enum) {
    value: Value,
    label: Label,
    frame: ActivationFrame,
};

pub const FuncAddr = u32;
pub const TableAddr = u32;
pub const MemAddr = u32;
pub const GlobalAddr = u32;
pub const ElemAddr = u32;
pub const DataAddr = u32;
pub const ExternAddr = u32;

pub const FuncInst = struct {
    type: core.FuncType,
    module: *ModuleInst,
    code: core.Func,
};

pub const TableInst = struct {
    type: core.TableType,
    elem: []RefValue,
};

pub const MemInst = struct {
    type: core.MemoryType,
    data: []u8,
};

pub const GlobalInst = struct {
    type: core.GlobalType,
    value: Value,
};

pub const ElemInst = struct {
    type: core.RefType,
    elem: []RefValue,
};

pub const DataInst = struct {
    data: []const u8,
};

pub const ExportInst = struct {
    name: []const u8,
    value: ExternalValue,
};

pub const Value = union(core.ValueType) {
    const Self = @This();

    // num
    i32: i32,
    i64: i64,
    f32: u32,
    f64: u64,

    // vec
    v128: i128,

    // ref
    // `.func_ref = null` and `.extern_ref = null` are the null external reference, created by ref.null extern in Wasm.
    func_ref: ?FuncAddr,
    extern_ref: ?ExternAddr,

    pub fn from(v: anytype) Self {
        return switch (@TypeOf(v)) {
            i32 => .{ .i32 = v },
            u32 => .{ .i32 = @bitCast(v) },
            i64 => .{ .i64 = v },
            u64 => .{ .i64 = @bitCast(v) },
            f32 => .{ .f32 = @bitCast(v) },
            f64 => .{ .f64 = @bitCast(v) },
            else => @panic("unknown type: " ++ @typeName(@TypeOf(v))),
        };
    }

    pub fn fromRefValue(val: RefValue) Self {
        return switch (val) {
            .func_ref => |v| .{ .func_ref = v },
            .extern_ref => |v| .{ .extern_ref = v },
        };
    }

    pub fn defaultValueFrom(ty: core.ValueType) Self {
        return switch (ty) {
            .i32 => .{ .i32 = 0 },
            .i64 => .{ .i64 = 0 },
            .f32 => .{ .f32 = 0 },
            .f64 => .{ .f64 = 0 },
            .v128 => .{ .v128 = 0 },
            .func_ref => .{ .func_ref = null },
            .extern_ref => .{ .extern_ref = null },
        };
    }

    pub fn isNull(self: Self) bool {
        return switch (self) {
            .func_ref, .extern_ref => |v| v == null,
            else => false,
        };
    }

    pub fn as(self: Self, comptime T: type) T {
        return switch (T) {
            i32 => self.i32,
            u32 => @bitCast(self.i32),
            i64 => self.i64,
            u64 => @bitCast(self.i64),
            f32 => @bitCast(self.f32),
            f64 => @bitCast(self.f64),
            else => @panic("unknown type: " ++ @typeName(T)),
        };
    }

    pub inline fn asU32(self: Self) u32 {
        return @bitCast(self.i32);
    }

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .i32 => |val| try writer.print("{}_i32", .{val}),
            .i64 => |val| try writer.print("{}_i64", .{val}),
            .f32 => try writer.print("{d:.2}_f32", .{self.as(f32)}),
            .f64 => try writer.print("{d:.2}_f64", .{self.as(f64)}),
            .v128 => |val| try writer.print("{}_i128", .{val}),
            .func_ref => |val| if (val) |v| try writer.print("{}_ref", .{v}) else try writer.print("null_ref", .{}),
            .extern_ref => |val| if (val) |v| try writer.print("{}_extref", .{v}) else try writer.print("null_extref", .{}),
        }
    }
};

pub const RefValue = union(enum) {
    const Self = @This();

    func_ref: ?FuncAddr,
    extern_ref: ?ExternAddr,

    pub fn fromValue(val: Value) Self {
        return switch (val) {
            .func_ref => |v| .{ .func_ref = v },
            .extern_ref => |v| .{ .extern_ref = v },
            else => unreachable,
        };
    }

    pub fn isNull(self: Self) bool {
        return switch (self) {
            inline else => |val| val == null,
        };
    }

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .func_ref => |val| if (val) |v| try writer.print("{}_ref", .{v}) else try writer.print("null_ref", .{}),
            .extern_ref => |val| if (val) |v| try writer.print("{}_extref", .{v}) else try writer.print("null_extref", .{}),
        }
    }
};

pub const ExternalValue = union(std.wasm.ExternalKind) {
    function: FuncAddr,
    table: TableAddr,
    memory: MemAddr,
    global: GlobalAddr,
};

pub const Label = struct {
    arity: u32,
    type: LabelType,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{any} (arity = {})", .{ self.type, self.arity });
    }
};

pub const LabelType = union(enum) {
    root,
    func: core.InstractionAddr,
    block: core.InstractionAddr,
    @"if": core.InstractionAddr,
    loop: core.InstractionAddr,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |addr| if (@TypeOf(addr) == void) {
                try writer.print("{s}", .{@tagName(self)});
            } else {
                try writer.print("{s} ({})", .{ @tagName(self), addr });
            },
        }
    }
};

pub const ActivationFrame = struct {
    locals: []Value = &.{},
    arity: usize = 0,
    module: *ModuleInst = undefined,
    instructions: []const core.Instruction = &.{},
    ip: u32 = 0,
};
