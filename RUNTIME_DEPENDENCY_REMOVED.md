# Runtime Dependency Removal - Complete

## 実施日時
2025-11-20 23:41-23:43

## 実施内容

### ✅ Step 1: types_new.zig の作成
- runtime 非依存の types.zig を作成
- `FuncAddr`, `ExternAddr` をローカル定義 (u32)
- `Value` 型をローカル定義
- `InvokeCommandArg.args` を `[]const Value` に変更

### ✅ Step 2: reader.zig の修正
**変更箇所:**
```zig
// Before
const Value = runtime.types.Value;
const ExternAddr = runtime.types.ExternAddr;
const FuncAddr = runtime.types.FuncAddr;

// After
const Value = local_types.Value;
const ExternAddr = local_types.ExternAddr;
const FuncAddr = local_types.FuncAddr;
```

**f32/f64 の処理:**
```zig
// Before
return Value{ .f32 = @bitCast(num) };

// After
return Value{ .f32 = num };  // ビットパターンとして保存
```

### ✅ Step 3: executor/value_convert.zig の作成
**機能:**
- `toRuntimeValue()`: spec_types.Value → runtime.types.Value
- `fromRuntimeValue()`: runtime.types.Value → spec_types.Value

### ✅ Step 4: test_runner.zig の修正
**変更箇所:**
```zig
// import 追加
const value_convert = @import("./executor/value_convert.zig");

// doAction 内で変換
const runtime_args = try self.allocator.alloc(runtime.types.Value, arg.args.len);
defer self.allocator.free(runtime_args);
for (arg.args, 0..) |spec_arg, i| {
    runtime_args[i] = value_convert.toRuntimeValue(spec_arg);
}
```

### ✅ Step 5: types.zig の置き換え
```bash
mv src/spec_test/types.zig src/spec_test/types_old.zig
mv src/spec_test/types_new.zig src/spec_test/types.zig
```

### ✅ Step 6: ビルドとテスト
```bash
zig build        # ✅ 成功
zig build test   # ✅ 成功
```

### ✅ Step 7: 確認と削除
```bash
grep "runtime" src/spec_test/types.zig  # ✅ import なし
rm src/spec_test/types_old.zig          # ✅ 削除完了
```

---

## 結果

### types.zig の依存関係
**Before:**
```
types.zig → runtime (wasm-runtime)
```

**After:**
```
types.zig (独立、runtime 依存なし)
```

### 変換層の導入
```
spec_test/types.zig (spec_types.Value)
         ↓
executor/value_convert.zig (変換層)
         ↓
runtime/types.zig (runtime.types.Value)
```

---

## 検証結果

- ✅ types.zig が runtime を import していない
- ✅ reader.zig が spec_types.Value を返す
- ✅ test_runner.zig が変換層を使用
- ✅ ビルドが成功する
- ✅ 既存のテストが通る
- ✅ types_old.zig が削除されている

---

## 影響を受けたファイル

1. `src/spec_test/types.zig` - runtime 依存を排除
2. `src/spec_test/reader.zig` - local_types.Value を使用
3. `src/spec_test/test_runner.zig` - 変換層を使用
4. `src/spec_test/executor/value_convert.zig` - 新規作成

---

## 次のステップ

この修正により、REFACTORING_PLAN_X.md の Phase 1 の準備が整いました。

次は:
1. ⏳ checkReturnValue の OR バグ修正
2. ⏳ errors.zig の unreachable 削除
3. ⏳ メモリ所有権の明確化

---

## 備考

- types.zig は純粋な型定義モジュールになった
- runtime への依存は executor/value_convert.zig に集約
- spec_types と runtime の橋渡しが明確になった
