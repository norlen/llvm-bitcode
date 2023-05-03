#![allow(unused)]

use num_enum::TryFromPrimitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u64)]
pub enum ModuleBlockId {
    /// `StandardBlockIDs::BLOCKINFO_BLOCK_ID`.
    Id = 0,
    // These below are checked for in `Error BitcodeReader::parseModule(uint64_t ResumeBit,
    //                           bool ShouldLazyLoadMetadata,
    //                           DataLayoutCallbackTy DataLayoutCallback)`
    // BLOCKINFO_BLOCK_ID
    // PARAMATTR_BLOCK_ID = 9,
    // PARAMATTR_GROUP_BLOCK_ID = 10,
    // CONSTANTS_BLOCK_ID = 11,
    // FUNCTION_BLOCK_ID = 12,
    // VALUE_SYMTAB_BLOCK_ID = 14,
    // METADATA_BLOCK_ID = 15,
    // TYPE_BLOCK_ID_NEW = 17,
    // USELIST_BLOCK_ID = 18,
    // OPERAND_BUNDLE_TAGS_BLOCK_ID = 21,
    // METADATA_KIND_BLOCK_ID = 22,
    // SYNC_SCOPE_NAMES_BLOCK_ID = 26,

    // These are checked in `Error ModuleSummaryIndexBitcodeReader::parseModule()`
    // BLOCKINFO_BLOCK_ID
    // VALUE_SYMTAB_BLOCK_ID
    // GLOBALVAL_SUMMARY_BLOCK_ID = 20
    // FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID = 24
    // MODULE_STRTAB_BLOCK_ID = 19
    ParameterAttributes = 9,
    ParameterAttributeGroups = 10,
    Constants = 11,
    Function = 12,
    ValueSymtab = 14,
    Metadata = 15,
    Types = 17,
    Uselist = 18,
    ModuleStrtab = 19,
    GlobalValueSummary = 20,
    OperandBundleTags = 21,
    MetadataKind = 22,
    FullLtoGlobalValueSummary = 24,
    SyncScopeNames = 26,
}

// Not sure hwo this relates to the one above.
// LLVM seems to parse these from records and converts to those above.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u64)]
pub enum AttributeId {
    /// `align(<n>)`
    Alignment = 1,

    /// `alwaysinline`
    AlwaysInline = 2,

    /// `byval`
    ByVal = 3,

    /// `inlinehint`
    InlineHint = 4,

    /// `inreg`
    InReg = 5,

    /// `minsize`
    MinSize = 6,

    /// `naked`
    Naked = 7,

    /// `nest`
    Nest = 8,

    /// `noalias`
    NoAlias = 9,

    /// `nobuiltin`
    NoBuiltin = 10,

    /// `nocapture`
    NoCapture = 11,

    /// `nodeduplicate`
    NoDuplicate = 12,

    /// `noimplicitfloat`
    NoImplicitFloat = 13,

    /// `noinline`
    NoInline = 14,

    /// `nonlazybind`
    NonLazyBind = 15,

    /// `noredzone`
    NoRedZone = 16,

    /// `noreturn`
    NoReturn = 17,

    /// `nounwind`
    NoUnwind = 18,

    /// `optsize`
    OptimizeForSize = 19,

    /// `readnone`
    ReadNone = 20,

    /// `readonly`
    ReadOnly = 21,

    /// `returned`
    Returned = 22,

    /// `returns_twice`
    ReturnsTwice = 23,

    /// `signext`
    SExt = 24,

    /// `alignstack(<n>)`
    StackAlignment = 25,

    /// `ssp`
    StackProtect = 26,

    /// `sspreq`
    StackProtectReq = 27,

    /// `sspstrong`
    StackProtectStrong = 28,

    /// `sret`
    StructRet = 29,

    /// `sanitize_address`
    SanitizeAddress = 30,

    /// `sanitize_thread`
    SanitizeThread = 31,

    /// `sanitize_memory`
    SanitizeMemory = 32,

    /// `uwtable`
    UWTable = 33,

    /// `zeroext`
    ZExt = 34,

    /// `builtin`
    Builtin = 35,

    /// `cold`
    Cold = 36,

    /// `optnone`
    OptimizeNone = 37,

    /// `inalloca`
    InAlloca = 38,

    /// `nonnull`
    NonNull = 39,

    /// `jumptable`
    JumpTable = 40,

    /// `dereferenceable(<n>)`
    Dereferenceable = 41,

    /// `dereferenceable_or_null(<n>)`
    DereferenceableOrNull = 42,

    /// `convergent`
    Convergent = 43,

    /// `safestack`
    SafeStack = 44,

    /// `argmemonly`
    ArgMemOnly = 45,

    /// `swiftself`
    SwiftSelf = 46,

    /// `swifterror`
    SwiftError = 47,

    /// `norecurse`
    NoRecurse = 48,

    /// `inaccessiblememonly`
    InaccessibleMemOnly = 49,

    /// `inaccessiblememonly_or_argmemonly`
    InaccessibleMemOrArgMemOnly = 50,

    /// `allocsize(<EltSizeParam>[, <NumEltsParam>])`
    AllocSize = 51,

    /// `writeonly`
    WriteOnly = 52,

    /// `speculatable`
    Speculatable = 53,

    /// `strictfp`
    StrictFp = 54,

    /// `sanitize_hwaddress`
    SanitizeHwAddress = 55,

    /// `nocf_check`
    NoCfCheck = 56,

    /// `optforfuzzing`
    OptForFuzzing = 57,

    /// `shadowcallstack`
    ShadowCallStack = 58,

    /// `speculative_load_hardening`
    SpeculativeLoadHardening = 59,

    /// `immarg`
    ImmArg = 60,

    /// `willreturn`
    WillReturn = 61,

    /// `nofree`
    NoFree = 62,

    /// `nosync`
    NoSync = 63,

    /// `sanitize_memtag`
    SanitizeMemtag = 64,

    /// `preallocated`
    Preallocated = 65,

    /// `no_merge`
    NoMerge = 66,

    /// `null_pointer_is_valid`
    NullPointerIsValid = 67,

    /// `noundef`
    NoUndef = 68,

    /// `byref`
    ByRef = 69,

    /// `mustprogress`
    MustProgress = 70,

    /// Not present in LLVM IR docs.
    NoCallback = 71,

    /// Not present in LLVM IR docs.
    Hot = 72,

    /// Not present in LLVM IR docs.
    NoProfile = 73,

    /// `vscale_range(<Min>[, <Max>])`
    VScaleRange = 74,

    /// `swiftasync`
    SwiftAsync = 75,

    /// `nosanitize_coverage`
    NoSanitizeCoverage = 76,

    /// `elementtype`
    ElementType = 77,

    /// `disable_sanitizer_instrumentation`
    DisableSanitizerInstrumentation = 78,

    /// `nosanitize_bounds`
    NoSantitizeBounds = 79,

    /// Not present in LLVM IR docs.
    AllocAlign = 80,

    /// Not present in LLVM IR docs.
    AllocatedPointer = 81,

    /// Not present in LLVM IR docs.
    AllocKind = 82,

    /// Not present in LLVM IR docs.
    PresplitCoroutine = 83,

    /// Not present in LLVM IR docs.
    FnRetThunkExtern = 84,

    /// Not present in LLVM IR docs.
    SkipProfile = 85,

    /// Not present in LLVM IR docs.
    Memory = 86,
}

/// Attribute kind.
///
/// Each attribute kind is either a well-known LLVM attribute or an arbitrary string. Both of these
/// can have either an integer respectively a string value associated with it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u64)]
pub enum RecordAttributeKind {
    /// Well-known attribute.
    ///
    /// [key#].
    Enum = 0,

    /// Well-known attribute with an integer value.
    ///
    /// [key#, value#].
    Int = 1,

    /// String attribute.
    ///
    /// [strchr x N].
    String = 3,

    /// String attribute with a string value.
    ///
    /// [strchr x N, strchr x N].
    StringWithValue = 4,

    /// Type attribute.
    ///
    ///
    Type = 5,
    TypeWithValue = 6,
}

/// Synchronization scope ids.
///
///
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u64)]
pub enum SyncScopeId {
    /// Synchronized with respect to signal handlers executing in the same thread.
    SingleThread = 0,

    /// Synchronized with respect to all concurrently executing threads.
    System = 1,
}
