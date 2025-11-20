# Purity and Ownership Fixed

## Date
2025-11-21 00:06-00:13

## Issues Fixed

### 1. types.zig Purity (Critical)

**Problem**: types.zig imported errors.zig, which depends on runtime/validate, breaking purity.

**Solution**: Changed trap fields from error types to strings.

**Changes**:
```zig
// BEFORE
const errors = @import("./errors.zig");

pub const AssertTrapCommandArg = struct {
    trap: errors.RuntimeError,
    // ...
};

// AFTER
// No errors import

pub const AssertTrapCommandArg = struct {
    error_text: []const u8,  // Error string from test file
    // ...
};
```

**Impact**:
- types.zig no longer depends on errors.zig
- types.zig is now pure (only depends on std and decode)
- Error conversion happens at execution time in test_runner.zig

---

### 2. Memory Ownership (Critical)

**Problem**: 
- `parseFromSliceLeaky` kept JSON tree alive
- Strings in Commands pointed to JSON tree
- Buffer was freed but JSON strings became dangling pointers

**Solution**: 
- Use normal `parseFromSlice` with `defer parsed.deinit()`
- Duplicate all strings with `allocator.dupe()`
- Free commands and strings in `test_runner.execFromFile`

**Changes in reader.zig**:
```zig
// BEFORE
const value = try std.json.parseFromSliceLeaky(std.json.Value, allocator, buffer, .{});
const file_name = obj.get("filename").?.string;  // Direct reference

// AFTER
const parsed = try std.json.parseFromSlice(std.json.Value, allocator, buffer, .{});
defer parsed.deinit();
const file_name = try allocator.dupe(u8, obj.get("filename").?.string);  // Copy
```

**Changes in test_runner.zig**:
```zig
// BEFORE
const commands = try reader.readJsonFromFile(file, self.allocator);
try self.execSpecTests(commands);
// No cleanup

// AFTER
const commands = try reader.readJsonFromFile(file, self.allocator);
defer self.freeCommands(commands);
try self.execSpecTests(commands);
```

**New functions**:
- `freeCommands()`: Frees all strings and arrays in commands
- `freeAction()`: Frees strings in actions

---

### 3. Register Command Bug (Medium)

**Problem**: `Command.register.name` was ignored, always registered `current_module`.

**Solution**: Select module by name if provided.

**Changes**:
```zig
// BEFORE
.register => |arg| {
    try self.engine.registerModule(current_module, arg.as_name);
},

// AFTER
.register => |arg| {
    const mod = if (arg.name) |name|
        self.engine.getModuleInstByName(name) orelse current_module
    else
        current_module;
    try self.engine.registerModule(mod, arg.as_name);
},
```

---

## Verification

```bash
zig build        # ✅ Success
zig build test   # ✅ Success
```

## Dependency Graph

**Before**:
```
types.zig → errors.zig → runtime/validate
```

**After**:
```
types.zig (pure, only std + decode)
test_runner.zig → errors.zig → runtime/validate
```

## Files Modified

1. `src/spec_test/types.zig` - Removed errors import, changed trap to error_text
2. `src/spec_test/reader.zig` - Dupe all strings, use parseFromSlice
3. `src/spec_test/test_runner.zig` - Convert error_text to errors, free commands, fix register
4. `REFACTORING_PLAN_X.md` - Added checkReturnValue panic issue

## Memory Safety

**Before**: Dangling pointers after JSON tree freed
**After**: All strings properly owned and freed

## Next Steps

Ready for Phase 1: Create spec-types as independent module in `src/spec-types/`
