# Runtime Dependency Analysis - types.zig

## 実施日時
2025-11-20 23:37

## 調査結果

### 現状の runtime 依存箇所

#### 1. import 文
```zig
const runtime = @import("wasm-runtime");
```

#### 2. Result 型のアドレス型
```zig
pub const Result = union(enum) {
    func_ref: ?runtime.types.FuncAddr,    // line 57
    extern_ref: ?runtime.types.ExternAddr, // line 58
    // ...
};
```

**実体**: `FuncAddr = u32`, `ExternAddr = u32`

#### 3. InvokeCommandArg の args フィールド
```zig
pub const InvokeCommandArg = struct {
    field: []const u8,
    args: []const runtime.types.Value,  // line 189
    module: ?[]const u8,
};
```

**実体**: `runtime.types.Value` は以下の union:
```zig
pub const Value = union(core.types.ValueType) {
    i32: i32,
    i64: i64,
    f32: u32,  // ビットパターン
    f64: u64,  // ビットパターン
    v128: i128,
    func_ref: ?FuncAddr,
    extern_ref: ?ExternAddr,
};
```

#### 4. エラー型 (errors.zig 経由)
```zig
trap: errors.RuntimeError  // lines 130, 140, 170, 180
```

**実体**: `errors.RuntimeError = @import("wasm-runtime").Error`

---

## 依存関係の分類

### 直接依存 (types.zig → runtime)
1. ✅ **FuncAddr, ExternAddr**: 単純な型エイリアス (`u32`)
   - **対応**: ローカルで定義可能

2. ✅ **runtime.types.Value**: 構造が同一
   - **対応**: ローカルで同じ構造を定義

### 間接依存 (errors.zig → runtime)
3. ⚠️ **errors.RuntimeError**: errors.zig が runtime に依存
   - **対応**: errors.zig は別モジュール (spec-test-errors) に分離予定

---

## 修正方針

### Phase 1: types.zig の runtime 依存を排除

#### 変更内容

1. **ローカル型定義を追加**
```zig
// runtime 非依存のアドレス型定義
pub const FuncAddr = u32;
pub const ExternAddr = u32;

// runtime 非依存の Value 型
pub const Value = union(enum) {
    i32: i32,
    i64: i64,
    f32: u32, // ビットパターン
    f64: u64, // ビットパターン
    v128: i128,
    func_ref: ?FuncAddr,
    extern_ref: ?ExternAddr,
};
```

2. **Result 型の修正**
```zig
pub const Result = union(enum) {
    // ...
    func_ref: ?FuncAddr,      // runtime.types.FuncAddr → FuncAddr
    extern_ref: ?ExternAddr,  // runtime.types.ExternAddr → ExternAddr
    // ...
};
```

3. **InvokeCommandArg の修正**
```zig
pub const InvokeCommandArg = struct {
    field: []const u8,
    args: []const Value,  // runtime.types.Value → Value
    module: ?[]const u8,
};
```

4. **import 文の削除**
```zig
// 削除
// const runtime = @import("wasm-runtime");
```

---

## 影響範囲の調査

### types.zig を使用しているファイル

#### 1. reader.zig
```zig
const local_types = @import("./types.zig");
```

**影響**: 
- `argFromJson()` が `runtime.types.Value` を返している
- **修正必要**: `local_types.Value` を返すように変更

#### 2. test_runner.zig
```zig
const spec_types = @import("./types.zig");
```

**影響**:
- `doAction()` が `runtime.types.Value` を返している
- `invokeFunctionByAddr()` に `runtime.types.Value` を渡している
- **修正必要**: 変換層が必要

---

## 変換層の設計

### executor/value_convert.zig (新規作成)

```zig
const std = @import("std");
const spec_types = @import("../types.zig");
const runtime = @import("wasm-runtime");

/// spec_types.Value → runtime.types.Value
pub fn toRuntimeValue(val: spec_types.Value) runtime.types.Value {
    return switch (val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },  // ビットパターンそのまま
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}

/// runtime.types.Value → spec_types.Value
pub fn fromRuntimeValue(val: runtime.types.Value) spec_types.Value {
    return switch (val) {
        .i32 => |v| .{ .i32 = v },
        .i64 => |v| .{ .i64 = v },
        .f32 => |v| .{ .f32 = v },
        .f64 => |v| .{ .f64 = v },
        .v128 => |v| .{ .v128 = v },
        .func_ref => |v| .{ .func_ref = v },
        .extern_ref => |v| .{ .extern_ref = v },
    };
}
```

---

## 修正手順

### Step 1: types_new.zig の作成 ✅
- runtime 依存を排除した新しい types.zig を作成
- **完了**: `types_new.zig` として作成済み

### Step 2: reader.zig の修正
```zig
fn argFromJson(json: std.json.Value) !local_types.Value {
    const type_ = json.object.get("type").?.string;
    const value = json.object.get("value").?.string;
    
    if (strcmp(type_, "f32")) {
        const num = try std.fmt.parseInt(u32, value, 10);
        return .{ .f32 = num };  // ビットパターンとして保存
    }
    // ...
}
```

### Step 3: test_runner.zig の修正
```zig
const executor = @import("./executor/value_convert.zig");

fn doAction(self: *Self, action: spec_types.Action, current_module: *runtime.types.ModuleInst) ![]const runtime.types.Value {
    switch (action) {
        .invoke => |arg| {
            // spec_types.Value → runtime.types.Value 変換
            const runtime_args = try self.allocator.alloc(runtime.types.Value, arg.args.len);
            defer self.allocator.free(runtime_args);
            
            for (arg.args, 0..) |spec_arg, i| {
                runtime_args[i] = executor.toRuntimeValue(spec_arg);
            }
            
            return try self.engine.invokeFunctionByAddr(func_addr, runtime_args);
        },
        // ...
    }
}
```

### Step 4: types.zig の置き換え
```bash
mv src/spec_test/types.zig src/spec_test/types_old.zig
mv src/spec_test/types_new.zig src/spec_test/types.zig
```

### Step 5: ビルドとテスト
```bash
zig build
zig build test
```

---

## 検証項目

- [ ] types.zig が runtime を import していない
- [ ] reader.zig が spec_types.Value を返す
- [ ] test_runner.zig が変換層を使用
- [ ] ビルドが成功する
- [ ] 既存のテストが通る

---

## 次のステップ

1. ✅ **types_new.zig 作成完了**
2. ⏳ reader.zig の修正
3. ⏳ executor/value_convert.zig の作成
4. ⏳ test_runner.zig の修正
5. ⏳ types.zig の置き換え
6. ⏳ ビルドとテスト

---

## 備考

- errors.zig の runtime 依存は Phase 2 (spec-test-errors モジュール分離) で対応
- この修正により、types.zig は純粋な型定義モジュールになる
- 変換層 (executor) が spec_types と runtime の橋渡しを担当
