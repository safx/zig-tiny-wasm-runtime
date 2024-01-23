const std = @import("std");
const wa = @import("wasm-core");

pub const ModuleInst = struct {
    types: []const wa.FuncType = &.{},
    func_addrs: []FuncAddr = &.{},
    table_addrs: []TableAddr = &.{},
    mem_addrs: []MemAddr = &.{},
    global_addrs: []GlobalAddr = &.{},
    elem_addrs: []ElemAddr = &.{},
    data_addrs: []DataAddr = &.{},
    exports: []ExportInst = &.{},
};

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
    array: std.ArrayList(StackItem),

    pub fn new(allocator: std.mem.Allocator) Stack {
        return .{
            .array = std.ArrayList(StackItem).init(allocator),
        };
    }

    pub fn pushValue(self: *Self, value: anytype) error{OutOfMemory}!void {
        const val = Value.from(value);
        try self.push(.{ .value = val });
    }

    pub fn pushValueAs(self: *Self, T: type, value: T) error{OutOfMemory}!void {
        const val = Value.from(value);
        try self.push(.{ .value = val });
    }

    pub fn push(self: *Self, value: StackItem) error{OutOfMemory}!void {
        try self.array.append(value);
    }

    pub fn appendSlice(self: *Self, values: []const StackItem) error{OutOfMemory}!void {
        try self.array.appendSlice(values);
    }

    pub fn insertAt(self: *Self, index_from_top: usize, value: StackItem) error{OutOfMemory}!void {
        const pos = self.array.items.len - index_from_top;
        try self.array.insert(pos, value);
    }

    pub fn pop(self: *Self) StackItem {
        return self.array.pop();
    }

    pub fn maybePopForLabel(self: *Self) ?StackItem {
        if (self.array.items.len == 0) {
            return null;
        }
        const last = self.array.getLast();
        switch (last) {
            .label => {
                return self.array.pop();
            },
            else => return null,
        }
    }

    pub fn getNthLabelFromTop(self: *Self, num_labels: usize) Label {
        var n = num_labels + 1;
        var i = self.array.items.len;
        while (i > 0 and n > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == .label) {
                n -= 1;
                if (n == 0) {
                    return item.label;
                }
            }
        }
        unreachable;
    }

    pub fn popValues(self: *Self, num_labels: usize) error{OutOfMemory}![]const StackItem {
        const len = self.array.items.len;
        const new_len = len - num_labels;
        const ret = self.array.items[new_len..len];
        try self.array.resize(new_len);
        return ret;
    }

    pub fn topFrame(self: Self) ActivationFrame {
        var i = self.array.items.len;
        while (i > 0) : (i -= 1) {
            const item = self.array.items[i - 1];
            if (item == .frame) {
                return item.frame;
            }
        }
        unreachable;
    }

    // finds the uppermost label and remove it.
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
            if (item == .frame) {
                return;
            }
        }
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
    type: wa.FuncType,
    module: *ModuleInst,
    code: wa.Func,
};

pub const TableInst = struct {
    type: wa.TableType,
    elem: []RefValue,
};

pub const MemInst = struct {
    type: wa.MemoryType,
    data: []u8,
};

pub const GlobalInst = struct {
    type: wa.GlobalType,
    value: Value,
};

pub const ElemInst = struct {
    type: wa.RefType,
    elem: RefValue,
};

pub const DataInst = struct {
    data: []const u8,
};

pub const ExportInst = struct {
    name: []const u8,
    value: ExternalValue,
};

pub const Value = union(wa.ValueType) {
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

    pub fn as(self: Self, comptime T: type) T {
        return switch (T) {
            i32 => self.asI32(),
            i64 => self.asI64(),
            f32 => self.asF32(),
            f64 => self.asF64(),
            else => @panic("unknown type: " ++ @typeName(T)),
        };
    }

    pub fn asI32(self: Self) i32 {
        switch (self) {
            .i32 => |val| return val,
            .i64 => |val| return @intCast(val),
            .f32 => |val| return @bitCast(val),
            .f64 => |val| return @intCast(val),
            else => @panic(""),
        }
    }

    pub fn asI64(self: Self) i64 {
        switch (self) {
            .i32 => |val| return val,
            .i64 => |val| return val,
            .f32 => |val| {
                const v: i64 = @intCast(val);
                return @bitCast(v);
            },
            .f64 => |val| return @bitCast(val),
            else => @panic(""),
        }
    }

    pub fn asF32(self: Self) f32 {
        switch (self) {
            .i32 => |val| return @bitCast(val),
            .i64 => |val| {
                const v: i32 = @intCast(val);
                return @bitCast(v);
            },
            .f32 => |val| return @bitCast(val),
            .f64 => |val| {
                const v: i32 = @intCast(val);
                return @bitCast(v);
            },
            else => @panic(""),
        }
    }

    pub fn asF64(self: Self) f64 {
        switch (self) {
            .i32 => |val| {
                const x: i64 = val;
                return @bitCast(x);
            },
            .i64 => |val| return @bitCast(val),
            .f32 => |val| return {
                const v: f32 = @bitCast(val);
                return v;
            },
            .f64 => |val| return @bitCast(val),
            else => @panic(""),
        }
    }

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .i32 => |val| try writer.print("{}_i32", .{val}),
            .i64 => |val| try writer.print("{}_i64", .{val}),
            .f32 => try writer.print("{d:.2}_f32", .{self.asF32()}),
            .f64 => try writer.print("{d:.2}_f64", .{self.asF64()}),
            inline else => |val| try writer.print("{any}", .{val}),
        }
    }
};

test "Value" {
    const expectEqual = std.testing.expectEqual;

    {
        const v = Value.from(@as(i32, 0));
        try expectEqual(@as(i32, 0), v.i32);
        try expectEqual(@as(i32, 0), v.asI32());
        try expectEqual(@as(i64, 0), v.asI64());
        try expectEqual(@as(f32, 0), v.asF32());
        try expectEqual(@as(f64, 0), v.asF64());
    }
    {
        const v = Value.from(@as(i32, 5));
        try expectEqual(@as(i32, 5), v.i32);
        try expectEqual(@as(i64, 5), v.asI64());
    }
    {
        const v = Value.from(@as(i64, 1));
        try expectEqual(@as(i64, 1), v.i64);
        //try expectEqual(@as(f64, 1), v.asF64());
    }
}

pub const RefValue = union(enum) {
    ref_null: wa.RefType,
    ref: FuncAddr,
    ref_extern: ExternAddr,
};

pub const ExternalValue = union(enum) {
    func: FuncAddr,
    table: TableAddr,
    memory: MemAddr,
    global: GlobalAddr,
};

pub const Label = struct {
    arity: u32,
    type: LabelType,
};

pub const LabelType = union(enum) {
    root,
    func: wa.InstractionAddr,
    block: wa.InstractionAddr,
    @"if": wa.InstractionAddr,
    loop: wa.InstractionAddr,
};

pub const ActivationFrame = struct {
    locals: []Value = &.{},
    arity: usize = 0,
    module: *ModuleInst,
};
