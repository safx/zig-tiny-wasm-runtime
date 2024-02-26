pub const Error = error{
    ExportItemNotFound,
    OutOfBoundsMemoryAccess,
    OutOfBoundsTableAccess,
    IntegerDivideByZero,
    IntegerOverflow,
    UndefinedElement,
    UninitializedElement,
    IndirectCallTypeMismatch,
    InvocationParameterMismatch,
    InvalidConversionToInteger,
    InstantiationFailed,
    Unreachable,
    IncompatibleImportType,
    UnknownImport,
    CallStackExhausted,
};
