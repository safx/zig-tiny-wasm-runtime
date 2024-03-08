const std = @import("std");
const types = struct {
    usingnamespace @import("wasm-core");
    usingnamespace @import("./types.zig");
};
const validate = @import("wasm-validate");
pub const Instance = @import("./instance.zig").Instance;
const resolver = @import("./resolver.zig");
pub const Error = @import("./errors.zig").Error;

/// A type of WebAssembly runtime engine, which wraps `Instance`.
pub const Engine = struct {
    const Self = @This();
    pub const ModuleInstMap = std.StringHashMap(*types.ModuleInst);

    allocator: std.mem.Allocator,
    instance: Instance,
    mod_insts: ModuleInstMap,

    pub fn new(allocator: std.mem.Allocator, verbose: bool) Self {
        return .{
            .allocator = allocator,
            .instance = Instance.new(allocator, verbose),
            .mod_insts = ModuleInstMap.init(allocator),
        };
    }

    pub fn getModuleInstByName(self: *Self, module_name: []const u8) ?*types.ModuleInst {
        return self.mod_insts.get(module_name);
    }

    pub fn getValueFromGlobal(self: *Self, mod_inst: *types.ModuleInst, field_name: []const u8) ?types.Value {
        if (findExternValue(mod_inst, field_name)) |externval| {
            return switch (externval) {
                .global => |glb| self.instance.store.globals.items[@intCast(glb)].value,
                else => null,
            };
        }
        return null;
    }

    fn findExternValue(mod_inst: *types.ModuleInst, name: []const u8) ?types.ExternalValue {
        for (mod_inst.exports) |exp| {
            if (std.mem.eql(u8, exp.name, name))
                return exp.value;
        }
        return null;
    }

    pub fn loadModuleFromPath(self: *Self, file_name: []const u8, module_name: ?[]const u8) !*types.ModuleInst {
        const decode = @import("wasm-decode");
        var loader = decode.Loader.new(self.allocator);

        const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
        defer file.close();
        const data = try file.readToEndAlloc(self.allocator, 10_000_000);
        defer self.allocator.free(data);
        const module = try loader.parseAll(data);
        defer module.deinit();

        try validate.validateModule(module, self.allocator);

        return try self.loadModule(module, if (module_name) |n| n else getFilename(file_name));
    }

    fn loadModule(self: *Self, module: types.Module, module_name: []const u8) (Error || error{OutOfMemory})!*types.ModuleInst {
        const extern_vals = try resolver.resolveImports(self.instance.store, self.mod_insts, module, self.allocator);
        defer self.allocator.free(extern_vals);
        const mod_inst = try self.instance.instantiate(module, extern_vals);
        try self.registerModule(mod_inst, module_name);
        return mod_inst;
    }

    pub fn registerModule(self: *Self, mod_inst: *types.ModuleInst, module_name: []const u8) error{OutOfMemory}!void {
        try self.mod_insts.put(module_name, mod_inst);
    }

    pub fn invokeFunctionByAddr(self: *Self, func_addr: types.FuncAddr, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        std.debug.assert(self.instance.stack.array.items.len == 0);
        return try self.instance.invokeFunctionByAddr(func_addr, args);
    }

    pub fn invokeFunctionByName(self: *Self, func_name: []const u8, args: []const types.Value) (Error || error{OutOfMemory})![]const types.Value {
        var it = self.mod_insts.valueIterator();
        while (it.next()) |mod_inst| {
            const exp = try resolver.findExport(mod_inst.*.*, func_name);
            return try self.invokeFunctionByAddr(exp.function, args);
        }
        std.debug.print("Unknown function name to call: {s}\n", .{func_name});
        return Error.UnknownImport;
    }
};

fn getFilename(path: []const u8) []const u8 {
    var parts = std.mem.split(u8, path, "/");
    var elem: []const u8 = "";
    while (parts.next()) |p|
        elem = p;

    return elem;
}
