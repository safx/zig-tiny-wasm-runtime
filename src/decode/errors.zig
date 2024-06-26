pub const Error = error{
    UnexpectedEndOfBuffer,
    MalformedImportKind,
    MalformedRefType,
    MalformedSectionId,
    UnknownBinaryVersion,
    MagicHeaderNotDetected,
    EndOpcodeExpected,
    UnexpectedEndOfSectionOrFunction,
    SectionSizeMismatch,
    ZeroByteExpected,
    IntegerTooLarge,
    TooManyLocals,
    FunctionAndCodeSectionHaveInconsistentLengths,
    DataCountAndDataSectionHaveInconsistentLengths,
    DataCountSectionRequired,
    IllegalOpcode,
    UnexpectedContentAfterLastSection,
    IntegerRepresentationTooLong,
    MalformedUtf8Encoding,
    AlignmentMustBePowerOfTwo,
    //
    MalformedFunctypeMagicNumber,
    MalformedElemKind,
    MalformedExportKind,
    MalformedValueType,
    MalformedLimitId,
    MalformedMutId,
    MalformedBlockType,
    MalformedLaneIndex,
    InvalidLaneLength,
    TypeMismatch,
    OtherError,
};
