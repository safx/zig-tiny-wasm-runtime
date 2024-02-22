pub const Error = error{
    UnexpectedEndOfBuffer,
    MalformedImportKind,
    MalformedRefType,
    MalformedSectionId,
    UnknownBinaryVersion,
    MagicHeaderNotDetected,
    EndOpcodeExpected,
    UnexpectedEndOfSectionFunction,
    SectionSizeMismatch,
    ZeroByteExpected,
    IntegerTooLarge,
    TooManyLocals,
    FunctionAndCodeSectionHaveInconsistentLengths,
    DataCountAndDataSectionHaveInconsistentLengths,
    DataCountSectionRequired,
    IllegalOpcode,
    //
    MalformedFunctypeMagicNumber,
    MalformedElemKind,
    MalformedExportKind,
    MalformedValueType,
    MalformedLimitId,
    MalformedMutId,
    MalformedBlockType,
    TypeMismatch,
    OtherError,
};
