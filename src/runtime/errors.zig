pub const Error = error{
    ExportItemNotFound,
    OutOfBoundsMemoryAccess,
    OutOfBoundsTableAccess,
    IntegerDivideByZero,
    IntegerOverflow,
    UndefinedElement,
    UninitializedElement,
    IndirectCallTypeMismatch,
    InvalidConversionToInteger,
    Unreachable,
};
