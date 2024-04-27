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
        return .{ .array = std.ArrayList(StackItem).init(allocator) };
    }

    inline fn checkStackExhausted(self: Self) Error!void {
        if (self.array.items.len > max_stack_size)
            return CallStackExhausted;
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
    pub fn popValues(self: *Self, popped_values: *[]StackItem) Error!void {
        const len = self.array.items.len;
        const num_items = popped_values.len;
        const new_len = len - num_items;

        @memcpy(popped_values.*, self.array.items[new_len..len]);
        for (popped_values.*) |v| std.debug.assert(v == .value);

        self.array.resize(new_len) catch return CallStackExhausted;
    }

    fn find(self: Self, comptime item_type: StackItemType) usize {
        var i = self.array.items.len;
        while (i > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == item_type)
                return i - 1;
        }
        unreachable;
    }

    /// returns the current activation frame
    pub fn topFrame(self: Self) ActivationFrame {
        const idx = self.find(.frame);
        return self.array.items[idx].frame;
    }

    /// updates instraction pointer in the current frame
    pub fn updateTopFrameIp(self: *Self, ip: u32) void {
        const idx = self.find(.frame);
        self.array.items[idx].frame.ip = ip;
    }

    /// finds the uppermost label and remove it.
    pub fn popUppermostLabel(self: *Self) ?Label {
        const idx = self.find(.label);
        const ret = self.array.orderedRemove(idx);
        return ret.label;
    }

    /// drops all values until the uppermost label and drop the label as well.
    pub fn popValuesAndUppermostLabel(self: *Self) void {
        while (true) {
            const item = self.pop();
            switch (item) {
                .value => continue,
                .label => return,
                .frame => unreachable,
            }
        }
    }

    /// drops all values and labels until the uppermost frame.
    pub fn popValuesAndLabelsUntilFrame(self: *Self) void {
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

pub const StackItemType = enum {
    value,
    label,
    frame,
};

pub const StackItem = union(StackItemType) {
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
            i128 => .{ .v128 = v },
            u128 => .{ .v128 = @bitCast(v) },
            else => fromVec(v),
        };
    }

    pub fn fromVec(val: anytype) Self {
        const v: V128 = switch (std.meta.Child(@TypeOf(val))) {
            i8 => .{ .i8 = val },
            u8 => .{ .u8 = val },
            i16 => .{ .i16 = val },
            u16 => .{ .u16 = val },
            i32 => .{ .i32 = val },
            u32 => .{ .u32 = val },
            i64 => .{ .i64 = val },
            u64 => .{ .u64 = val },
            f32 => .{ .f32 = val },
            f64 => .{ .f64 = val },
            else => @panic("unknown type: " ++ @typeName(@TypeOf(val))),
        };

        return .{ .v128 = v.i128 };
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
            i128 => self.v128,
            u128 => @bitCast(self.v128),
            else => @panic("unknown type: " ++ @typeName(T)),
        };
    }

    pub fn asVec(self: Self, comptime T: type) T {
        const v = V128{ .i128 = self.v128 };
        return switch (std.meta.Child(T)) {
            i8 => v.i8,
            u8 => v.u8,
            i16 => v.i16,
            u16 => v.u16,
            i32 => v.i32,
            u32 => v.u32,
            i64 => v.i64,
            u64 => v.u64,
            f32 => v.f32,
            f64 => v.f64,
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

const V128 = extern union {
    u128: u128,
    i128: i128,
    u64: [2]u64,
    i64: [2]i64,
    u32: [4]u32,
    i32: [4]i32,
    u16: [8]u16,
    i16: [8]i16,
    u8: [16]u8,
    i8: [16]i8,
    f64: [2]f64,
    f32: [4]f32,
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
    arity: u32 = 0,
    module: *ModuleInst = undefined,
    instructions: []const core.Instruction = &.{},
    ip: core.InstractionAddr = 0,
};
