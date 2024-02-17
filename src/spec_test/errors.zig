const decode = @import("wasm-decode");
const runtime = @import("wasm-runtime");
pub const Error = decode.Error || runtime.Error;
