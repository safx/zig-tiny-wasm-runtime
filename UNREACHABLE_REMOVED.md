# Unreachable Removal - errors.zig

## Date
2025-11-20 23:52-23:55

## Issue
The error mapping functions in `errors.zig` used `unreachable` for unknown error strings, causing panics on unexpected input from JSON/WAST files.

## Fix

### Step 1: Add OtherError to error types

Added `OtherError` variant to error types that didn't have it:

**src/validate/errors.zig**
```zig
pub const Error = error{
    // ... existing errors ...
    OtherError,  // Added
};
```

**src/runtime/errors.zig**
```zig
pub const Error = error{
    // ... existing errors ...
    OtherError,  // Added
};
```

**Note**: `DecodeError` already had `OtherError`.

### Step 2: Replace unreachable with OtherError

All error mapping functions now return `E.OtherError` for unknown strings:

#### decodeErrorFromString
```zig
// BEFORE
std.debug.print("? Unknown decode error \"{s}\"\n", .{str});
unreachable;

// AFTER
std.debug.print("? Unknown decode error \"{s}\"\n", .{str});
return E.OtherError;
```

#### validationErrorFromString
```zig
// BEFORE
std.debug.print("? Unknown validation error \"{s}\"\n", .{str});
unreachable;

// AFTER
std.debug.print("? Unknown validation error \"{s}\"\n", .{str});
return E.OtherError;
```

#### linkErrorFromString
```zig
// BEFORE
std.debug.print("? Unknown decode error \"{s}\"\n", .{str});  // Wrong message
unreachable;

// AFTER
std.debug.print("? Unknown link error \"{s}\"\n", .{str});  // Fixed message
return E.OtherError;
```
**Note**: Also fixed incorrect error message.

#### runtimeErrorFromString
```zig
// BEFORE
std.debug.print("? Unknown runtime error \"{s}\"\n", .{str});
unreachable;

// AFTER
std.debug.print("? Unknown runtime error \"{s}\"\n", .{str});
return E.OtherError;
```

## Fallback Strategy

| Function | Fallback Error | Reason |
|----------|---------------|--------|
| decodeErrorFromString | `OtherError` | Explicit unknown error variant |
| validationErrorFromString | `OtherError` | Added to ValidationError |
| linkErrorFromString | `OtherError` | Added to RuntimeError |
| runtimeErrorFromString | `OtherError` | Added to RuntimeError |

## Verification

```bash
zig build        # ✅ Success
zig build test   # ✅ Success
```

## Impact

**Before**: Unknown error strings caused panic (program crash)
**After**: Unknown error strings return `OtherError` (clear indication of unmapped error)

Benefits:
- No confusion with existing error types
- Clear signal that error string was not recognized
- Debug print still logs unknown strings for investigation
- Program continues execution gracefully

## Related

- REFACTORING_PLAN_X.md Section 2: "Unknown error strings causing panic"
- This enables safe error handling for external data (JSON/WAST files)

