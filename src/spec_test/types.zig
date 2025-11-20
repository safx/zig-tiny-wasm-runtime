// Re-export from spec-types module
const spec_types = @import("spec-types");

pub const Command = spec_types.Command;
pub const Action = spec_types.Action;
pub const Value = spec_types.Value;
pub const Result = spec_types.Result;
pub const FuncAddr = spec_types.FuncAddr;
pub const ExternAddr = spec_types.ExternAddr;
pub const FloatType = spec_types.FloatType;

// Re-export all command arg types
pub const ActionCommandArg = spec_types.command.ActionCommandArg;
pub const ModuleCommandArg = spec_types.command.ModuleCommandArg;
pub const RegisterCommandArg = spec_types.command.RegisterCommandArg;
pub const AssertReturnCommandArg = spec_types.command.AssertReturnCommandArg;
pub const AssertTrapCommandArg = spec_types.command.AssertTrapCommandArg;
pub const AssertExhaustionCommandArg = spec_types.command.AssertExhaustionCommandArg;
pub const AssertMalformedCommandArg = spec_types.command.AssertMalformedCommandArg;
pub const AssertInvalidCommandArg = spec_types.command.AssertInvalidCommandArg;
pub const AssertUnlinkableCommandArg = spec_types.command.AssertUnlinkableCommandArg;
pub const AssertUninstantiableCommandArg = spec_types.command.AssertUninstantiableCommandArg;
pub const InvokeCommandArg = spec_types.command.InvokeCommandArg;
pub const GetCommandArg = spec_types.command.GetCommandArg;
