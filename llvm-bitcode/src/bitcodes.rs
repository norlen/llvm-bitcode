//! LLVM Bitcodes
//!
//! Last updated to commit `5a49fee3299d18573d89813e7a3172c553d9b257` from May 1 2023.
#![allow(unused)]
use num_enum::TryFromPrimitive;

/// Subset of block ids from [`BlockId`] that only contain top-level blocks.
#[derive(Debug, Clone, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum TopLevelBlockId {
    /// `MODULE_BLOCK_ID`
    Module = 8,

    /// `IDENTIFICATION_BLOCK_ID`
    Identification = 13,

    /// `STRTAB_BLOCK_ID`
    StringTable = 23,

    /// `SYMTAB_BLOCK_ID`
    Symtab = 25,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum BlockId {
    /// `MODULE_BLOCK_ID`
    Module = 8,

    /// `PARAMATTR_BLOCK_ID`
    ParameterAttributes,

    /// `PARAMATTR_GROUP_BLOCK_ID`
    ParameterAttributeGroups,

    /// `CONSTANTS_BLOCK_ID`
    Constants,

    /// `FUNCTION_BLOCK_ID`
    Function,

    /// `IDENTIFICATION_BLOCK_ID`
    Identification,

    /// `VALUE_SYMTAB_BLOCK_ID`
    ValueSymtab,

    /// `METADATA_BLOCK_ID`
    Metadata,

    /// `METADATA_ATTACHMENT_ID`
    MetadataAttachment,

    /// `TYPE_BLOCK_ID_NEW`
    Types,

    /// `USELIST_BLOCK_ID`
    Uselist,

    /// `MODULE_STRTAB_BLOCK_ID`
    ModuleStrtab,

    /// `GLOBALVAL_SUMMARY_BLOCK_ID`
    GlobalValueSummary,

    /// `OPERAND_BUNDLE_TAGS_BLOCK_ID`
    OperandBundleTags,

    /// `METADATA_KIND_BLOCK_ID`
    MetadataKind,

    /// `STRTAB_BLOCK_ID`
    StringTable,

    /// `FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID`
    FullLtoGlobalValueSummary,

    /// `SYMTAB_BLOCK_ID`
    Symtab,

    /// `SYNC_SCOPE_NAMES_BLOCK_ID`
    SyncScopeNames,
}

impl BlockId {
    /// Get a name for the block id.
    pub fn name(&self) -> &'static str {
        match self {
            BlockId::Module => "MODULE_BLOCK",
            BlockId::ParameterAttributes => "PARAMATTR_BLOCK",
            BlockId::ParameterAttributeGroups => "PARAMATTR_GROUP_BLOCK_ID",
            BlockId::Constants => "CONSTANTS_BLOCK",
            BlockId::Function => "FUNCTION_BLOCK",
            BlockId::Identification => "IDENTIFICATION_BLOCK_ID",
            BlockId::ValueSymtab => "VALUE_SYMTAB",
            BlockId::Metadata => "METADATA_BLOCK",
            BlockId::MetadataAttachment => "METADATA_ATTACHMENT_BLOCK",
            BlockId::Types => "TYPE_BLOCK_ID",
            BlockId::Uselist => "USELIST_BLOCK_ID",
            BlockId::ModuleStrtab => "MODULE_STRTAB_BLOCK",
            BlockId::GlobalValueSummary => "GLOBALVAL_SUMMARY_BLOCK",
            BlockId::OperandBundleTags => "OPERAND_BUNDLE_TAGS_BLOCK",
            BlockId::MetadataKind => "METADATA_KIND_BLOCK",
            BlockId::StringTable => "STRTAB_BLOCK",
            BlockId::FullLtoGlobalValueSummary => "FULL_LTO_GLOBALVAL_SUMMARY_BLOCK",
            BlockId::Symtab => "SYMTAB_BLOCK",
            BlockId::SyncScopeNames => "SYNC_SCOPE_NAMES_BLOCK",
        }
    }

    pub fn from_id(id: u32) -> Option<BlockId> {
        let id = id.try_into().ok()?;
        Self::try_from_primitive(id).ok()
    }
}

/// The identification block contain a string containing producer details and an epoch that define
/// auto-upgrade capability.
///
/// `IdentificationCodes` from `LLVMBitCodes.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum IdentificationCode {
    /// `IDENTIFICATION_CODE_STRING`: `[u8 x N]`
    Producer = 1,

    /// `IDENTIFICATION_CODE_EPOCH`: `[epoch#]`
    Epoch = 2,
}

impl IdentificationCode {
    /// Current bitcode epoch, see LLVM for more details.
    pub const BITCODE_CURRENT_EPOCH: u64 = 0;

    pub fn name(&self) -> &'static str {
        match self {
            IdentificationCode::Producer => "STRING",
            IdentificationCode::Epoch => "EPOCH",
        }
    }

    pub fn from_code(code: u32) -> Option<IdentificationCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ModuleCode {
    /// `MODULE_CODE_VERSION`: `[version#]`.
    Version = 1,

    /// `MODULE_CODE_TRIPLE`: `[ch x N]`.
    Triple = 2,

    /// `MODULE_CODE_DATALAYOUT`: `[ch x N]`.
    Datalayout = 3,

    /// `MODULE_CODE_ASM`: `[ch x N]`.
    Asm = 4,

    /// `MODULE_CODE_SECTIONNAME`: `[ch x N]`.
    SectionName = 5,

    /// `MODULE_CODE_DEPLIB`: `[ch x N]`.
    ///
    /// Deprecated, only used to read old bitcode files.
    DepLib = 6,

    /// `MODULE_CODE_GLOBALVAR`
    GlobalVariable = 7,

    /// `MODULE_CODE_FUNCTION`
    Function = 8,

    /// `MODULE_CODE_ALIAS_OLD`
    AliasOld = 9,

    /// `MODULE_CODE_GCNAME` `[ch x N]`.
    GcName = 11,

    /// `MODULE_CODE_COMDAT`
    Comdat = 12,

    /// `MODULE_CODE_VSTOFFSET`: `[vst_offset#]`.
    VstOffset = 13,

    /// `MODULE_CODE_ALIAS`
    Alias = 14,

    /// `MODULE_CODE_METADATA_VALUES_UNUSED`
    MetadataValuesUnused = 15,

    /// `MODULE_CODE_SOURCE_FILENAME`: `[ch x N]`.
    Filename = 16,

    /// `MODULE_CODE_HASH`: `[i32 * 5]`.
    Hash = 17,

    /// `MODULE_CODE_IFUNC`
    IFunc = 18,
}

impl ModuleCode {
    pub fn name(&self) -> &'static str {
        match self {
            ModuleCode::Version => "VERSION",
            ModuleCode::Triple => "TRIPLE",
            ModuleCode::Datalayout => "DATALAYOUT",
            ModuleCode::Asm => "ASM",
            ModuleCode::SectionName => "SECTIONNAME",
            ModuleCode::DepLib => "DEPLIB",
            ModuleCode::GlobalVariable => "GLOBALVAR",
            ModuleCode::Function => "FUNCTION",
            ModuleCode::AliasOld => "ALIAS_OLD",
            ModuleCode::GcName => "GCNAME",
            ModuleCode::Comdat => "COMDAT",
            ModuleCode::VstOffset => "VSTOFFSET",
            ModuleCode::Alias => "ALIAS",
            ModuleCode::MetadataValuesUnused => "METADATA_VALUES_UNUSED",
            ModuleCode::Filename => "SOURCE_FILENAME",
            ModuleCode::Hash => "HASH",
            ModuleCode::IFunc => "IFUNC",
        }
    }

    pub fn from_code(code: u32) -> Option<ModuleCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum AttributeCode {
    /// `PARAMATTR_CODE_ENTRY_OLD`
    EntryOld = 1,

    /// `PARAMATTR_CODE_ENTRY`
    Entry = 2,

    /// `PARAMATTR_GRP_CODE_ENTRY`
    GroupEntry = 3,
}

impl AttributeCode {
    pub fn name(&self) -> &'static str {
        match self {
            AttributeCode::EntryOld => "ENTRY_OLD",
            AttributeCode::Entry => "ENTRY",
            AttributeCode::GroupEntry => "GROUP_ENTRY",
        }
    }

    pub fn from_code(code: u32) -> Option<AttributeCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum TypeCode {
    /// `TYPE_CODE_NUMENTRY`: `[numentries#]`.
    NumEntries = 1,

    /// `TYPE_CODE_VOID`.
    Void = 2,

    /// `TYPE_CODE_FLOAT`.
    Float = 3,

    /// `TYPE_CODE_DOUBLE`.
    Double = 4,

    /// `TYPE_CODE_LABEL`.
    Label = 5,

    /// `TYPE_CODE_OPAQUE`.
    Opaque = 6,

    /// `TYPE_CODE_INTEGER`: `[width#]`.
    Integer = 7,

    /// `TYPE_CODE_POINTER`: `[pointee_type]`.
    Pointer = 8,

    /// `TYPE_CODE_FUNCTION_OLD`: `[vararg, attrid, retty, paramty x N]`.
    FunctionOld = 9,

    /// `TYPE_CODE_HALF`.
    Half = 10,

    /// `TYPE_CODE_ARRAY`: `[numelts, eltty]`.
    Array = 11,

    /// `TYPE_CODE_VECTOR`: `[numelts, eltty]`.
    Vector = 12,

    /// `TYPE_CODE_X86_FP80`: `X86 LONG DOUBLE`.
    X86Fp80 = 13,

    /// `TYPE_CODE_FP128`: `LONG DOUBLE` (112-bit mantissa).
    Fp128 = 14,

    /// `TYPE_CODE_PPC_FP128`: `PPC LONG DOUBLE` (2 doubles).
    PpcFp128 = 15,

    /// `TYPE_CODE_METADATA`.
    Metadata = 16,

    /// `TYPE_CODE_X86_MMX`.
    X86Mmx = 17,

    /// `TYPE_CODE_STRUCT_ANON`: [ispacked, eltty x N].
    StructAnon = 18,

    /// `TYPE_CODE_STRUCT_NAME`: [strchar x N].
    StructName = 19,

    /// `TYPE_CODE_STRUCT_NAMED`: [ispacked, eltty x N].
    StructNamed = 20,

    /// `TYPE_CODE_FUNCTION`: [vararg, retty, paramty x N].
    Function = 21,

    /// `TYPE_CODE_TOKEN`.
    Token = 22,

    /// `TYPE_CODE_BFLOAT`: Brain floating point.
    BFloat = 23,

    /// `TYPE_CODE_X86_AMX`.
    X86Amx = 24,

    /// `TYPE_CODE_OPAQUE_POINTER`: [addrspace].
    OpaquePointer = 25,

    /// `TYPE_CODE_TARGET_TYPE`.
    TargetType = 26,
}

impl TypeCode {
    pub fn name(&self) -> &'static str {
        match self {
            TypeCode::NumEntries => "NUMENTRY",
            TypeCode::Void => "VOID",
            TypeCode::Float => "FLOAT",
            TypeCode::Double => "DOUBLE",
            TypeCode::Label => "LABEL",
            TypeCode::Opaque => "OPAQUE",
            TypeCode::Integer => "INTEGER",
            TypeCode::Pointer => "POINTER",
            TypeCode::FunctionOld => "FUNCTION_OLD",
            TypeCode::Half => "HALF",
            TypeCode::Array => "ARRAY",
            TypeCode::Vector => "VECTOR",
            TypeCode::X86Fp80 => "X86_FP80",
            TypeCode::Fp128 => "FP128",
            TypeCode::PpcFp128 => "PPC_FP128",
            TypeCode::Metadata => "METADATA",
            TypeCode::X86Mmx => "X86_MMX",
            TypeCode::StructAnon => "STRUCT_ANON",
            TypeCode::StructName => "STRUCT_NAME",
            TypeCode::StructNamed => "STRUCT_NAMED",
            TypeCode::Function => "FUNCTION",
            TypeCode::Token => "TOKEN",
            TypeCode::BFloat => "BFLOAT",
            TypeCode::X86Amx => "X86_AMX",
            TypeCode::OpaquePointer => "OPAQUE_POINTER",
            TypeCode::TargetType => "TARGET_TYPE",
        }
    }

    pub fn from_code(code: u32) -> Option<TypeCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum OperandBundleTagCode {
    /// `OPERAND_BUNDLE_TAG`: `[strchr x N]`
    Tag = 1,
}

impl OperandBundleTagCode {
    pub fn name(&self) -> &'static str {
        match self {
            OperandBundleTagCode::Tag => "OPERAND_BUNDLE_TAG",
        }
    }

    pub fn from_code(code: u32) -> Option<OperandBundleTagCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum SyncScopeNameCode {
    /// `SYNC_SCOPE_NAME`: `[strchr x N]`
    Name = 1,
}

impl SyncScopeNameCode {
    pub fn name(&self) -> &'static str {
        match self {
            SyncScopeNameCode::Name => "SYNC_SCOPE_NAME",
        }
    }

    pub fn from_code(code: u32) -> Option<SyncScopeNameCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ValueSymbolTableCode {
    /// `VST_CODE_ENTRY`
    Entry = 1,

    /// `VST_CODE_BBENTRY`
    BasicBlockEntry = 2,

    /// `VST_CODE_FNENTRY`
    FunctionEntry = 3,

    /// `VST_CODE_COMBINED_ENTRY`
    CombinedEntry = 5,
}

impl ValueSymbolTableCode {
    pub fn name(&self) -> &'static str {
        match self {
            ValueSymbolTableCode::Entry => "ENTRY",
            ValueSymbolTableCode::BasicBlockEntry => "BBENTRY",
            ValueSymbolTableCode::FunctionEntry => "FNENTRY",
            ValueSymbolTableCode::CombinedEntry => "COMBINED_ENTRY",
        }
    }

    pub fn from_code(code: u32) -> Option<ValueSymbolTableCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ModulePathSymbolTableCode {
    /// `MST_CODE_ENTRY`
    Entry = 1,

    /// `MST_CODE_HASH`
    Hash = 2,
}

impl ModulePathSymbolTableCode {
    pub fn name(&self) -> &'static str {
        match self {
            ModulePathSymbolTableCode::Entry => "ENTRY",
            ModulePathSymbolTableCode::Hash => "HASH",
        }
    }

    pub fn from_code(code: u32) -> Option<ModulePathSymbolTableCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum GlobalValueSummarySymbolTableCode {
    /// `FS_PERMODULE`.
    PerModule = 1,

    /// `FS_PERMODULE_PROFILE`.
    PerModuleProfile = 2,

    /// `FS_PERMODULE_GLOBALVAR_INIT_REFS`.
    PerModuleGlobalVarInitRefs = 3,

    /// `FS_COMBINED`.
    Combined = 4,

    /// `FS_COMBINED_PROFILE`.
    CombinedProfile = 5,

    /// `FS_COMBINED_GLOBALVAR_INIT_REFS`.
    CombinedGlobalVarInitRefs = 6,

    /// `FS_ALIAS`.
    Alias = 7,

    /// `FS_COMBINED_ALIAS`.
    CombinedAlias = 8,

    /// `FS_COMBINED_ORIGINAL_NAME`.
    CombinedOriginalName = 9,

    /// `FS_VERSION`.
    Version = 10,

    /// `FS_TYPE_TESTS`.
    TypeTests = 11,

    /// `FS_TYPE_TEST_ASSUME_VCALLS`.
    TypeTestAssumeVCalls = 12,

    /// `FS_TYPE_CHECKED_LOAD_VCALLS`.
    TypeCheckedLoadVCalls = 13,

    /// `FS_TYPE_TEST_ASSUME_CONST_VCALL`.
    TypeTestAssumeConstVCall = 14,

    /// `FS_TYPE_CHECKED_LOAD_CONST_VCALL`.
    TypeCheckedLoadConstVCall = 15,

    /// `FS_VALUE_GUID`.
    ValueGuid = 16,

    /// `FS_CFI_FUNCTION_DEFS`.
    CFIFunctionDefs = 17,

    /// `FS_CFI_FUNCTION_DECLS`.
    CFIFunctionDecls = 18,

    /// `FS_PERMODULE_RELBF`.
    PerModuleRelBf = 19,

    /// `FS_FLAGS`.
    Flags = 20,

    /// `FS_TYPE_ID`.
    TypeId = 21,

    /// `FS_TYPE_ID_METADATA`.
    TypeIdMetadata = 22,

    /// `FS_PERMODULE_VTABLE_GLOBALVAR_INIT_REFS`.
    PerModuleVTableGloalVarInitRefs = 23,

    /// `FS_BLOCK_COUNT`.
    BlockCount = 24,

    /// `FS_PARAM_ACCESS`.
    ParamAccess = 25,

    /// `FS_PERMODULE_CALLSITE_INFO`.
    PerModuleCallsiteInfo = 26,

    /// `FS_PERMODULE_ALLOC_INFO`.
    PerModuleAllocInfo = 27,

    /// `FS_COMBINED_CALLSITE_INFO`.
    CombinedCallsiteInfo = 28,

    /// `FS_COMBINED_ALLOC_INFO`.
    CombinedAllocInof = 29,

    /// `FS_STACK_IDS`.
    StackIds = 30,
}

impl GlobalValueSummarySymbolTableCode {
    pub fn name(&self) -> &'static str {
        match self {
            GlobalValueSummarySymbolTableCode::PerModule => "PERMODULE",
            GlobalValueSummarySymbolTableCode::PerModuleProfile => "PERMODULE_PROFILE",
            GlobalValueSummarySymbolTableCode::PerModuleGlobalVarInitRefs => {
                "PERMODULE_GLOBALVAR_INIT_REFS"
            }
            GlobalValueSummarySymbolTableCode::Combined => "COMBINED",
            GlobalValueSummarySymbolTableCode::CombinedProfile => "COMBINED_PROFILE",
            GlobalValueSummarySymbolTableCode::CombinedGlobalVarInitRefs => {
                "COMBINED_GLOBALVAR_INIT_REFS"
            }
            GlobalValueSummarySymbolTableCode::Alias => "ALIAS",
            GlobalValueSummarySymbolTableCode::CombinedAlias => "COMBINED_ALIAS",
            GlobalValueSummarySymbolTableCode::CombinedOriginalName => "COMBINED_ORIGINAL_NAME",
            GlobalValueSummarySymbolTableCode::Version => "VERSION",
            GlobalValueSummarySymbolTableCode::TypeTests => "TYPE_TESTS",
            GlobalValueSummarySymbolTableCode::TypeTestAssumeVCalls => "TYPE_TEST_ASSUME_VCALLS",
            GlobalValueSummarySymbolTableCode::TypeCheckedLoadVCalls => "TYPE_CHECKED_LOAD_VCALLS",
            GlobalValueSummarySymbolTableCode::TypeTestAssumeConstVCall => {
                "TYPE_TEST_ASSUME_CONST_VCALL"
            }
            GlobalValueSummarySymbolTableCode::TypeCheckedLoadConstVCall => {
                "TYPE_CHECKED_LOAD_CONST_VCALL"
            }
            GlobalValueSummarySymbolTableCode::ValueGuid => "VALUE_GUID",
            GlobalValueSummarySymbolTableCode::CFIFunctionDefs => "CFI_FUNCTION_DEFS",
            GlobalValueSummarySymbolTableCode::CFIFunctionDecls => "CFI_FUNCTION_DECLS",
            GlobalValueSummarySymbolTableCode::PerModuleRelBf => "PERMODULE_RELBF",
            GlobalValueSummarySymbolTableCode::Flags => "FLAGS",
            GlobalValueSummarySymbolTableCode::TypeId => "TYPE_ID",
            GlobalValueSummarySymbolTableCode::TypeIdMetadata => "TYPE_ID_METADATA",
            GlobalValueSummarySymbolTableCode::PerModuleVTableGloalVarInitRefs => {
                "PERMODULE_VTABLE_GLOBALVAR_INIT_REFS"
            }
            GlobalValueSummarySymbolTableCode::BlockCount => "BLOCK_COUNT",
            GlobalValueSummarySymbolTableCode::ParamAccess => "PARAM_ACCESS",
            GlobalValueSummarySymbolTableCode::PerModuleCallsiteInfo => "PERMODULE_CALLSITE_INFO",
            GlobalValueSummarySymbolTableCode::PerModuleAllocInfo => "PERMODULE_ALLOC_INFO",
            GlobalValueSummarySymbolTableCode::CombinedCallsiteInfo => "COMBINED_CALLSITE_INFO",
            GlobalValueSummarySymbolTableCode::CombinedAllocInof => "COMBINED_ALLOC_INFO",
            GlobalValueSummarySymbolTableCode::StackIds => "STACK_IDS",
        }
    }

    pub fn from_code(code: u32) -> Option<GlobalValueSummarySymbolTableCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum MetadataCode {
    /// `METADATA_STRING_OLD`.
    StringOld = 1,

    /// `METADATA_VALUE`.
    Value = 2,

    /// `METADATA_NODE`.
    Node = 3,

    /// `METADATA_NAME`.
    Name = 4,

    /// `METADATA_DISTINCT_NODE`.
    DistinctNode = 5,

    /// `METADATA_KIND`.
    Kind = 6,

    /// `METADATA_LOCATION`.
    Location = 7,

    /// `METADATA_OLD_NODE`.
    OldNode = 8,

    /// `METADATA_OLD_FN_NODE`.
    OldFunctionNode = 9,

    /// `METADATA_NAMED_NODE`.
    NamedNode = 10,

    /// `METADATA_ATTACHMENT`.
    Attachment = 11,

    /// `METADATA_GENERIC_DEBUG`.
    GenericDebug = 12,

    /// `METADATA_SUBRANGE`.
    Subrange = 13,

    /// `METADATA_ENUMERATOR`.
    Enumerator = 14,

    /// `METADATA_BASIC_TYPE`.
    BasicType = 15,

    /// `METADATA_FILE`.
    File = 16,

    /// `METADATA_DERIVED_TYPE`.
    DerivedType = 17,

    /// `METADATA_COMPOSITE_TYPE`.
    CompositeType = 18,

    /// `METADATA_SUBROUTINE_TYPE`.
    SubroutineType = 19,

    /// `METADATA_COMPILE_UNIT`.
    CompileUnit = 20,

    /// `METADATA_SUBPROGRAM`.
    SubProgram = 21,

    /// `METADATA_LEXICAL_BLOCK`.
    LexicalBlock = 22,

    /// `METADATA_LEXICAL_BLOCK_FILE`.
    LexicalBlockFile = 23,

    /// `METADATA_NAMESPACE`.
    Namespace = 24,

    /// `METADATA_TEMPLATE_TYPE`.
    TemplateType = 25,

    /// `METADATA_TEMPLATE_VALUE`.
    TemplateValue = 26,

    /// `METADATA_GLOBAL_VAR`.
    GlobalVariable = 27,

    /// `METADATA_LOCAL_VAR`.
    LocalVariable = 28,

    /// `METADATA_EXPRESSION`.
    Expression = 29,

    /// `METADATA_OBJC_PROPERTY`.
    ObjCProperty = 30,

    /// `METADATA_IMPORTED_ENTITY`.
    ImportedEntity = 31,

    /// `METADATA_MODULE`.
    Module = 32,

    /// `METADATA_MACRO`.
    Macro = 33,

    /// `METADATA_MACRO_FILE`.
    MacroFile = 34,

    /// `METADATA_STRINGS`.
    Strings = 35,

    /// `METADATA_GLOBAL_DECL_ATTACHMENT`.
    GlobalDeclAttachment = 36,

    /// `METADATA_GLOBAL_VAR_EXPR`.
    GlobalVarExpr = 37,

    /// `METADATA_INDEX_OFFSET`.
    IndexOffset = 38,

    /// `METADATA_INDEX`.
    Index = 39,

    /// `METADATA_LABEL`.
    Label = 40,

    /// `METADATA_STRING_TYPE`.
    StringType = 41,

    /// `METADATA_COMMON_BLOCK`.
    CommonBlock = 44,

    /// `METADATA_GENERIC_SUBRANGE`.
    GenericSubrange = 45,

    /// `METADATA_ARG_LIST`.
    ArgList = 46,

    /// `METADATA_ASSIGN_ID`.
    AssignId = 47,
}

impl MetadataCode {
    pub fn name(&self) -> &'static str {
        match self {
            MetadataCode::StringOld => "STRING_OLD",
            MetadataCode::Value => "VALUE",
            MetadataCode::Node => "NODE",
            MetadataCode::Name => "NAME",
            MetadataCode::DistinctNode => "DISTINCT_NODE",
            MetadataCode::Kind => "KIND",
            MetadataCode::Location => "LOCATION",
            MetadataCode::OldNode => "OLD_NODE",
            MetadataCode::OldFunctionNode => "OLD_FN_NODE",
            MetadataCode::NamedNode => "NAMED_NODE",
            MetadataCode::Attachment => "ATTACHMENT",
            MetadataCode::GenericDebug => "GENERIC_DEBUG",
            MetadataCode::Subrange => "SUBRANGE",
            MetadataCode::Enumerator => "ENUMERATOR",
            MetadataCode::BasicType => "BASIC_TYPE",
            MetadataCode::File => "FILE",
            MetadataCode::DerivedType => "DERIVED_TYPE",
            MetadataCode::CompositeType => "COMPOSITE_TYPE",
            MetadataCode::SubroutineType => "SUBROUTINE_TYPE",
            MetadataCode::CompileUnit => "COMPILE_UNIT",
            MetadataCode::SubProgram => "SUBPROGRAM",
            MetadataCode::LexicalBlock => "LEXICAL_BLOCK",
            MetadataCode::LexicalBlockFile => "LEXICAL_BLOCK_FILE",
            MetadataCode::Namespace => "NAMESPACE",
            MetadataCode::TemplateType => "TEMPLATE_TYPE",
            MetadataCode::TemplateValue => "TEMPLATE_VALUE",
            MetadataCode::GlobalVariable => "GLOBAL_VAR",
            MetadataCode::LocalVariable => "LOCAL_VAR",
            MetadataCode::Expression => "EXPRESSION",
            MetadataCode::ObjCProperty => "OBJC_PROPERTY",
            MetadataCode::ImportedEntity => "IMPORTED_ENTITY",
            MetadataCode::Module => "MODULE",
            MetadataCode::Macro => "MACRO",
            MetadataCode::MacroFile => "MACRO_FILE",
            MetadataCode::Strings => "STRINGS",
            MetadataCode::GlobalDeclAttachment => "GLOBAL_DECL_ATTACHMENT",
            MetadataCode::GlobalVarExpr => "GLOBAL_VAR_EXPR",
            MetadataCode::IndexOffset => "INDEX_OFFSET",
            MetadataCode::Index => "INDEX",
            MetadataCode::Label => "LABEL",
            MetadataCode::StringType => "STRING_TYPE",
            MetadataCode::CommonBlock => "COMMON_BLOCK",
            MetadataCode::GenericSubrange => "GENERIC_SUBRANGE",
            MetadataCode::ArgList => "ARG_LIST",
            MetadataCode::AssignId => "ASSIGN_ID",
        }
    }

    pub fn from_code(code: u32) -> Option<MetadataCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

/// `CONSTANTS_BLOCK_ID` record codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ConstantsCode {
    /// `CST_CODE_SETTYPE`: [typeid]
    SetType = 1,

    /// `CST_CODE_NULL` NULL
    Null = 2,

    /// `CST_CODE_UNDEF`: UNDEF
    Undef = 3,

    /// `CST_CODE_INTEGER`: [intval]
    Integer = 4,

    /// `CST_CODE_WIDE_INTEGER`: [n x intval]
    WideInteger = 5,

    /// `CST_CODE_FLOAT`: [fpval]
    Float = 6,

    /// `CST_CODE_AGGREGATE`: [n x value number]
    Aggregate = 7,

    /// `CST_CODE_STRING: [values]
    String = 8,

    /// `CST_CODE_CSTRING`: [values]
    CString = 9,

    /// `CST_CODE_CE_BINOP`: [opcode, opval, opval]
    ConstexprBinop = 10,

    /// `CST_CODE_CE_CAST`: [opcode, opty, opval]
    ConstexprCast = 11,

    /// `CST_CODE_CE_GEP`: [n x operands]
    ConstexprGep = 12,

    /// `CST_CODE_CE_SELECT`: [opval, opval, opval]
    ConstexprSelect = 13,

    /// `CST_CODE_CE_EXTRACTELT`: [opty, opval, opval]
    ConstexprExtractElement = 14,

    /// `CST_CODE_CE_INSERTELT`: [opval, opval, opval]
    ConstexprInsertElement = 15,

    /// `CST_CODE_CE_SHUFFLEVEC`: [opval, opval, opval]
    ConstexprShuffleVector = 16,

    /// `CST_CODE_CE_CMP`: [opty, opval, opval, pred]
    ConstexprCompare = 17,

    /// `CST_CODE_INLINEASM_OLD`:[sideeffect|alignstack, asmstr, conststr]
    InlineAsmOld = 18,

    /// `CST_CODE_CE_SHUFVEC_EX`: [opty, opval, opval, opval]
    ConstexprShuffleVectorEx = 19,

    /// `CST_CODE_CE_INBOUNDS_GEP`: [n x operands]
    ConstexprInboundsGep = 20,

    /// `CST_CODE_BLOCKADDRESS`: [fnty, fnval, bb#]
    BlockAddress = 21,

    /// `CST_CODE_DATA`: [n x elements]
    Data = 22,

    /// `CST_CODE_INLINEASM_OLD2`: [sideeffect|alignstack|asmdialect, asmstr, conststr]
    InlineAsmOld2 = 23,

    /// `CST_CODE_CE_GEP_WITH_INRANGE_INDEX`: [opty, flags, n x operands]
    ConstexprGepWithInrangeIndex = 24,

    /// `CST_CODE_CE_UNOP`: [opcode, opval]
    ConstexprUnaryOp = 25,

    /// `CST_CODE_POISON`: POISON
    Poison = 26,

    /// `CST_CODE_DSO_LOCAL_EQUIVALENT`: [gvty, gv]
    DsoLocalEquivalent = 27,

    /// `CST_CODE_INLINEASM_OLD3`: [sideeffect|alignstack|asmdialect|unwind, asmstr, conststr]
    InlineAsmOld3 = 28,

    /// `CST_CODE_NO_CFI_VALUE`: [fty, f]
    NoCfiValue = 29,

    /// `CST_CODE_INLINEASM`: [fnty, sideeffect|alignstack|asmdialect|unwind, asmstr,conststr]
    InlineAsm = 30,
}

impl ConstantsCode {
    pub fn name(&self) -> &'static str {
        match self {
            ConstantsCode::SetType => "SETTYPE",
            ConstantsCode::Null => "NULL",
            ConstantsCode::Undef => "UNDEF",
            ConstantsCode::Integer => "INTEGER",
            ConstantsCode::WideInteger => "WIDE_INTEGER",
            ConstantsCode::Float => "FLOAT",
            ConstantsCode::Aggregate => "AGGREGATE",
            ConstantsCode::String => "STRING",
            ConstantsCode::CString => "CSTRING",
            ConstantsCode::ConstexprBinop => "CE_BINOP",
            ConstantsCode::ConstexprCast => "CE_CAST",
            ConstantsCode::ConstexprGep => "CE_GEP",
            ConstantsCode::ConstexprSelect => "CE_SELECT",
            ConstantsCode::ConstexprExtractElement => "CE_EXTRACTELT",
            ConstantsCode::ConstexprInsertElement => "CE_INSERTELT",
            ConstantsCode::ConstexprShuffleVector => "CE_SHUFFLEVEC",
            ConstantsCode::ConstexprCompare => "CE_CMP",
            ConstantsCode::InlineAsmOld => "INLINEASM_OLD",
            ConstantsCode::ConstexprShuffleVectorEx => "CE_SHUFVEC_EX",
            ConstantsCode::ConstexprInboundsGep => "CE_INBOUNDS_GEP",
            ConstantsCode::BlockAddress => "BLOCK_ADDRESS",
            ConstantsCode::Data => "DATA",
            ConstantsCode::InlineAsmOld2 => "INLINEASM_OLD2",
            ConstantsCode::ConstexprGepWithInrangeIndex => "CE_GEP_WITH_INRANGE_INDEX",
            ConstantsCode::ConstexprUnaryOp => "CE_UNOP",
            ConstantsCode::Poison => "POISON",
            ConstantsCode::DsoLocalEquivalent => "DSO_LOCAL_EQUIVALENT",
            ConstantsCode::InlineAsmOld3 => "INLINEASM_OLD3",
            ConstantsCode::NoCfiValue => "NO_CFI_VALUE",
            ConstantsCode::InlineAsm => "INLINEASM",
        }
    }

    pub fn from_code(code: u32) -> Option<ConstantsCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

/// Cast operation codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum CastOperationCode {
    /// `CAST_TRUNC`
    Truncate = 0,

    /// `CAST_ZEXT`
    ZeroExtend = 1,

    /// `CAST_SEXT`
    SignExtend = 2,

    /// `CAST_FPTOUI`
    FloatingPointToUnsignedInt = 3,

    /// `CAST_FPTOSI`
    FloatingPointToSignedInt = 4,

    /// `CAST_UITOFP`
    UnsignedIntToFloatingPoint = 5,

    /// `CAST_SITOFP`
    SignedIntToFloatingPoint = 6,

    /// `CAST_FPTRUNC`
    FloatingPointTruncate = 7,

    /// `CAST_FPEXT`
    FloatingPointExtend = 8,

    /// `CAST_PTRTOINT`
    PointerToInt = 9,

    /// `CAST_INTTOPTR`
    IntToPointer = 10,

    /// `CAST_BITCAST`
    Bitcast = 11,

    /// `CAST_ADDRSPACECAST`
    AddressSpaceCast = 12,
}

impl CastOperationCode {
    pub fn from_code(code: u32) -> Option<CastOperationCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

/// Unary operation codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
enum UnaryOperationCode {
    /// `UNOP_FNEG`
    FloatingPointNegation = 0,
}

impl UnaryOperationCode {
    pub fn from_code(code: u32) -> Option<UnaryOperationCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

/// Binary operation codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum BinaryOperationCode {
    /// `BINOP_ADD`
    Add = 0,

    /// `BINOP_SUB`
    Sub = 1,

    /// `BINOP_MUL`
    Mul = 2,

    /// `BINOP_UDIV`
    UDiv = 3,

    /// `BINOP_SDIV`
    ///
    /// overloaded for FP
    SDiv = 4,

    /// `BINOP_UREM`
    URem = 5,

    /// `BINOP_SREM`
    ///
    /// overloaded for FP
    SRem = 6,

    /// `BINOP_SHL`
    Shl = 7,

    /// `BINOP_LSHR`
    LShr = 8,

    /// `BINOP_ASHR`
    AShr = 9,

    /// `BINOP_AND`
    And = 10,

    /// `BINOP_OR`
    Or = 11,

    /// `BINOP_XOR`
    Xor = 12,
}

impl BinaryOperationCode {
    pub fn from_code(code: u32) -> Option<BinaryOperationCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

/// Atomic RMW operation codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum AtomicRMWOperation {
    /// `RMW_XCHG`.
    Xchg = 0,

    /// `RMW_ADD`.
    Add = 1,

    /// `RMW_SUB`.
    Sub = 2,

    /// `RMW_AND`.
    And = 3,

    /// `RMW_NAND`.
    Nand = 4,

    /// `RMW_OR`.
    Or = 5,

    /// `RMW_XOR`.
    Xor = 6,

    /// `RMW_MAX`.
    Max = 7,

    /// `RMW_MIN`.
    Min = 8,

    /// `RMW_UMAX`.
    UMax = 9,

    /// `RMW_UMIN`.
    UMin = 10,

    /// `RMW_FADD`.
    FAdd = 11,

    /// `RMW_FSUB`.
    FSub = 12,

    /// `RMW_FMAX`.
    FMax = 13,

    /// `RMW_FMIN`.
    FMin = 14,

    /// `RMW_UINC_WRAP`.
    UIncWrap = 15,

    /// `RMW_UDEC_WRAP`.
    UDecWrap = 16,
}

impl AtomicRMWOperation {
    pub fn from_code(code: u32) -> Option<AtomicRMWOperation> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum OverflowingBinaryOperatorOptionalFlag {
    /// `OBO_NO_UNSIGNED_WRAP`.
    NoUnsignedWrap = 0,

    /// `OBO_NO_SIGNED_WRAP`.
    NoSignedWrap = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum FastMathFlag {
    UnsafeAlgebra = (1 << 0), // Legacy
    NoNaNs = (1 << 1),
    NoInfs = (1 << 2),
    NoSignedZeros = (1 << 3),
    AllowReciprocal = (1 << 4),
    AllowContract = (1 << 5),
    ApproxFunc = (1 << 6),
    AllowReassoc = (1 << 7),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum PossiblyExactOperatorOptionalFlag {
    /// `PEO_EXACT`.
    Exact = 0,
}

/// Encoded atomic ordering codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum AtomicOrderingCode {
    // `ORDERING_NOTATOMIC`
    NotAtomic = 0,

    // `ORDERING_UNORDERED`
    Unordered = 1,

    // `ORDERING_MONOTONIC`
    Monotonic = 2,

    // `ORDERING_ACQUIRE`
    Acquire = 3,

    // `ORDERING_RELEASE`
    Release = 4,

    // `ORDERING_ACQREL`
    AcquireRelease = 5,

    // `ORDERING_SEQCST`
    SequentiallyConsistent = 6,
}

impl AtomicOrderingCode {
    pub fn from_code(code: u32) -> Option<AtomicOrderingCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum CallMarkersFlag {
    /// `CALL_TAIL`.
    Tail = 0,

    /// `CALL_CCONV`.
    CConv = 1,

    /// `CALL_MUSTTAIL`.
    MustTail = 14,

    /// `CALL_EXPLICIT_TYPE`.
    ExplicitType = 15,

    /// `CALL_NOTAIL`.
    NoTail = 16,

    /// `CALL_FMF`: call has optional fast-math flags.
    FastMathFlags = 17,
}

/// Function body record codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum FunctionCode {
    // Note the the following codes are unused [14, 17, 18, 21, 22, 25, 32, 53, 54]
    /// `FUNC_CODE_DECLAREBLOCKS`: [n]
    DeclareBlocks = 1,

    /// `FUNC_CODE_INST_BINOP`: [opcode, ty, opval, opval]
    InstBinop = 2,

    /// `FUNC_CODE_INST_CAST`: [opcode, ty, opty, opval]
    InstCast = 3,

    // `FUNC_CODE_INST_GEP_OLD`: [n x operands]
    InstGepOld = 4,

    // `FUNC_CODE_INST_SELECT`: [ty, opval, opval, opval]
    InstSelect = 5,

    // `FUNC_CODE_INST_EXTRACTELT`: [opty, opval, opval]
    InstExtractElt = 6,

    // `FUNC_CODE_INST_INSERTELT`: [ty, opval, opval, opval]
    IntInsertElt = 7,

    // `FUNC_CODE_INST_SHUFFLEVEC`: [ty, opval, opval, opval]
    IsntShuffleVec = 8,

    // `FUNC_CODE_INST_CMP`: [opty, opval, opval, pred]
    InstCmp = 9,

    // `FUNC_CODE_INST_RET`: [opty, opval<both optional>]
    InstRet = 10,

    // `FUNC_CODE_INST_BR`: [bb#, bb#, cond] or [bb#]
    InstBr = 11,

    // `FUNC_CODE_INST_SWITCH`: [opty, op0, op1, ...]
    InstSwitch = 12,

    // `FUNC_CODE_INST_INVOKE`: [attr, fnty, op0, op1, ...]
    InstInvoke = 13,

    // `FUNC_CODE_INST_UNREACHABLE`
    InstUnreachable = 15,

    /// `FUNC_CODE_INST_PHI`: [ty, val0, bb0, ...]
    InstPhi = 16,

    /// `FUNC_CODE_INST_ALLOCA`: [instty, opty, op, align, addrspace]
    InstAlloca = 19,

    /// `FUNC_CODE_INST_LOAD`: [opty, op, align, vol]
    InstLoad = 20,

    /// `FUNC_CODE_INST_VAARG`: [valistty, valist, instty]
    InstVaArg = 23,

    /// `FUNC_CODE_INST_STORE_OLD`: [ptrty,ptr,val, align, vol]
    ///
    /// This `STORE` code encodes the pointer type and not the value type. Allowing information
    /// stored in the pointer, such as address spaces, to be retained.
    InstStoreOld = 24,

    /// `FUNC_CODE_INST_EXTRACTVAL`: [n x operands]
    InstExtractVal = 26,

    /// `FUNC_CODE_INST_INSERTVAL`:  [n x operands]
    InstInsertVal = 27,

    /// `FUNC_CODE_INST_CMP2`: [opty, opval, opval, pred]
    ///
    /// `fcmp`/`icmp` returning `Int1TY` or vector of `Int1Ty`. Same as `CMP` but exists to support
    /// legacy `vicmp`/`vfcmp` instructions.
    InstCmp2 = 28,

    /// `FUNC_CODE_INST_VSELECT`: [ty, opval, opval, predty, pred]
    ///
    /// New select on `i1` or `[n x i1]`.
    InstVSelect = 29,

    /// `FUNC_CODE_INST_INBOUNDS_GEP_OLD`: [n x operands]
    InstInboundsGepOld = 30,

    /// `FUNC_CODE_INST_INDIRECTBR`: [opty, op0, op1, ...]
    InstIndirectBr = 31,

    /// `FUNC_CODE_DEBUG_LOC_AGAIN`
    DebugLocAgain = 33,

    /// `FUNC_CODE_INST_CALL`: [attr, cc, fnty, fnid, args...]
    InstCall = 34,

    /// `FUNC_CODE_DEBUG_LOC`: [Line, Col, ScopeVal, IAVal]
    DebugLoc = 35,

    /// `FUNC_CODE_INST_FENCE`: [ordering, synchscope]
    InstFence = 36,

    /// `FUNC_CODE_INST_CMPXCHG_OLD`: [ptrty, ptr, cmp, val, vol, ordering, synchscope, failure_ordering?, weak?]
    InstCmpxchgOld = 37,

    /// `FUNC_CODE_INST_ATOMICRMW_OLD`: [ptrty, ptr, val, operation, align, vol, ordering, synchscope]
    InstAtomicRMWOld = 38,

    /// `FUNC_CODE_INST_RESUME`: [opval]
    InstResume = 39,

    /// `FUNC_CODE_INST_LANDINGPAD_OLD`: [ty, val, val, num, id0, val0...]
    InstLandingpadOld = 40,

    /// `FUNC_CODE_INST_LOADATOMIC`: [opty, op, align, vol, ordering, synchscope]
    InstLoadAtomic = 41,

    /// `FUNC_CODE_INST_STOREATOMIC_OLD`: [ptrty, ptr, val, align, vol ordering, synchscope]
    InstStoreAtomicOld = 42,

    /// `FUNC_CODE_INST_GEP`:  [inbounds, n x operands]
    InstGep = 43,

    /// `FUNC_CODE_INST_STORE`: [ptrty, ptr, valty, val, align, vol]
    InstStore = 44,

    /// `FUNC_CODE_INST_STOREATOMIC`: [ptrty, ptr, val, align, vol
    ///
    /// TODO: (Should array end there or what?)
    InstStoreAtomic = 45,

    /// `FUNC_CODE_INST_CMPXCHG`: [ptrty, ptr, cmp, val, vol, success_ordering, synchscope, failure_ordering, weak]
    InstCmpxchg = 46,

    /// `FUNC_CODE_INST_LANDINGPAD`: [ty, val, num, id0, val0...]
    InstLandingpad = 47,

    /// `FUNC_CODE_INST_CLEANUPRET`: [val] or [val, bb#]
    InstCleanupret = 48,

    /// `FUNC_CODE_INST_CATCHRET`: [val, bb#]
    InstCatchret = 49,

    /// `FUNC_CODE_INST_CATCHPAD`: [bb#, bb#, num, args...]
    InstCatchpad = 50,

    /// `FUNC_CODE_INST_CLEANUPPAD`: [num, args...]
    InstCleanuppad = 51,

    /// `FUNC_CODE_INST_CATCHSWITCH`: [num, args...] or [num, args..., bb]
    InstCatchSwitch = 52,

    /// `FUNC_CODE_OPERAND_BUNDLE`: [tag#, value...]
    OperandBundle = 55,

    /// `FUNC_CODE_INST_UNOP`: [opcode, ty, opval]
    InstUnop = 56,

    /// `FUNC_CODE_INST_CALLBR`: [attr, cc, norm, transfs, fnty, fnid, args...]
    InstCallBr = 57,

    /// `FUNC_CODE_INST_FREEZE`: [opty, opval]
    InstFreeze = 58,

    /// `FUNC_CODE_INST_ATOMICRMW`: [ptrty, ptr, valty, val, operation, align, vol,ordering, synchscope]
    InstAtomicRMW = 59,

    /// `FUNC_CODE_BLOCKADDR_USERS`: [value...]
    BlockaddrUsers = 60,
}

impl FunctionCode {
    pub fn name(&self) -> &'static str {
        match self {
            FunctionCode::DeclareBlocks => "DECLAREBLOCKS",
            FunctionCode::InstBinop => "INST_BINOP",
            FunctionCode::InstCast => "INST_CAST",
            FunctionCode::InstGepOld => "INST_GEP_OLD",
            FunctionCode::InstSelect => "INST_SELECT",
            FunctionCode::InstExtractElt => "INST_EXTRACTELT",
            FunctionCode::IntInsertElt => "INST_INSERTELT",
            FunctionCode::IsntShuffleVec => "INST_SHUFFLEVEC",
            FunctionCode::InstCmp => "INST_CMP",
            FunctionCode::InstRet => "INST_RET",
            FunctionCode::InstBr => "INST_BR",
            FunctionCode::InstSwitch => "INST_SWITCH",
            FunctionCode::InstInvoke => "INST_INVOKE",
            FunctionCode::InstUnreachable => "INST_UNREACHABLE",
            FunctionCode::InstPhi => "INST_PHI",
            FunctionCode::InstAlloca => "INST_ALLOCA",
            FunctionCode::InstLoad => "INST_LOAD",
            FunctionCode::InstVaArg => "INST_VAARG",
            FunctionCode::InstStoreOld => "INST_STORE_OLD",
            FunctionCode::InstExtractVal => "INST_EXTRACTVAL",
            FunctionCode::InstInsertVal => "INST_INSERTVAL",
            FunctionCode::InstCmp2 => "INST_CMP2",
            FunctionCode::InstVSelect => "INST_VSELECT",
            FunctionCode::InstInboundsGepOld => "INST_INBOUNDS_GEP_OLD",
            FunctionCode::InstIndirectBr => "INST_INDIRECTBR",
            FunctionCode::DebugLocAgain => "DEBUG_LOC_AGAIN",
            FunctionCode::InstCall => "INST_CALL",
            FunctionCode::DebugLoc => "DEBUG_LOC",
            FunctionCode::InstFence => "INST_FENCE",
            FunctionCode::InstCmpxchgOld => "INST_CMPXCHG_OLD",
            FunctionCode::InstAtomicRMWOld => "INST_ATOMICRMW_OLD",
            FunctionCode::InstResume => "INST_RESUME",
            FunctionCode::InstLandingpadOld => "INST_LANDINGPAD_OLD",
            FunctionCode::InstLoadAtomic => "INST_LOADATOMIC",
            FunctionCode::InstStoreAtomicOld => "INST_STOREATOMIC_OLD",
            FunctionCode::InstGep => "INST_GEP",
            FunctionCode::InstStore => "INST_STORE",
            FunctionCode::InstStoreAtomic => "INST_STOREATOMIC",
            FunctionCode::InstCmpxchg => "INST_CMPXCHG",
            FunctionCode::InstLandingpad => "INST_LANDINGPAD",
            FunctionCode::InstCleanupret => "INST_CLEANUPRET",
            FunctionCode::InstCatchret => "INST_CATCHRET",
            FunctionCode::InstCatchpad => "INST_CATCHPAD",
            FunctionCode::InstCleanuppad => "INST_CLEANUPPAD",
            FunctionCode::InstCatchSwitch => "INST_CATCHSWITCH",
            FunctionCode::OperandBundle => "OPERAND_BUNDLE",
            FunctionCode::InstUnop => "INST_UNOP",
            FunctionCode::InstCallBr => "INST_CALLBR",
            FunctionCode::InstFreeze => "INST_FREEZE",
            FunctionCode::InstAtomicRMW => "INST_ATOMICRMW",
            FunctionCode::BlockaddrUsers => "BLOCKADDR_USERS",
        }
    }

    pub fn from_code(code: u32) -> Option<FunctionCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum UseListCode {
    /// `USELIST_CODE_DEFAULT`.
    Default = 1,

    /// `USELIST_CODE_BB`.
    BasicBlock = 2,
}

impl UseListCode {
    pub fn name(&self) -> &'static str {
        match self {
            UseListCode::Default => "USELIST_CODE_DEFAULT",
            UseListCode::BasicBlock => "USELIST_CODE_BB",
        }
    }

    pub fn from_code(code: u32) -> Option<UseListCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum AttributeKindCode {
    /// `ATTR_KIND_ALIGNMENT`.
    Alignment = 1,

    /// `ATTR_KIND_ALWAYS_INLINE`.
    AlwaysInline = 2,

    /// `ATTR_KIND_BY_VAL`.
    ByVal = 3,

    /// `ATTR_KIND_INLINE_HINT`.
    InlineHint = 4,

    /// `ATTR_KIND_IN_REG`.
    InReg = 5,

    /// `ATTR_KIND_MIN_SIZE`.
    MinSize = 6,

    /// `ATTR_KIND_NAKED`.
    Naked = 7,

    /// `ATTR_KIND_NEST`.
    Nest = 8,

    /// `ATTR_KIND_NO_ALIAS`.
    NoAlias = 9,

    /// `ATTR_KIND_NO_BUILTIN`.
    NoBuiltin = 10,

    /// `ATTR_KIND_NO_CAPTURE`.
    NoCapture = 11,

    /// `ATTR_KIND_NO_DUPLICATE`.
    NoDuplicate = 12,

    /// `ATTR_KIND_NO_IMPLICIT_FLOAT`.
    NoImplicitFloat = 13,

    /// `ATTR_KIND_NO_INLINE`.
    NoInline = 14,

    /// `ATTR_KIND_NON_LAZY_BIND`.
    NonLazyBind = 15,

    /// `ATTR_KIND_NO_RED_ZONE`.
    NoRedZone = 16,

    /// `ATTR_KIND_NO_RETURN`.
    NoReturn = 17,

    /// `ATTR_KIND_NO_UNWIND`.
    NoUnwind = 18,

    /// `ATTR_KIND_OPTIMIZE_FOR_SIZE`.
    OptimizeForSize = 19,

    /// `ATTR_KIND_READ_NONE`.
    ReadNone = 20,

    /// `ATTR_KIND_READ_ONLY`.
    ReadOnly = 21,

    /// `ATTR_KIND_RETURNED`.
    Returned = 22,

    /// `ATTR_KIND_RETURNS_TWICE`.
    ReturnsTwice = 23,

    /// `ATTR_KIND_S_EXT`.
    SExt = 24,

    /// `ATTR_KIND_STACK_ALIGNMENT`.
    StackAlignment = 25,

    /// `ATTR_KIND_STACK_PROTECT`.
    StackProtect = 26,

    /// `ATTR_KIND_STACK_PROTECT_REQ`.
    StackProtectReq = 27,

    /// `ATTR_KIND_STACK_PROTECT_STRONG`.
    StackProtectStrong = 28,

    /// `ATTR_KIND_STRUCT_RET`.
    StructRet = 29,

    /// `ATTR_KIND_SANITIZE_ADDRESS`.
    SanitizeAddress = 30,

    /// `ATTR_KIND_SANITIZE_THREAD`.
    SanitizeThread = 31,

    /// `ATTR_KIND_SANITIZE_MEMORY`.
    SanitizeMemory = 32,

    /// `ATTR_KIND_UW_TABLE`.
    UWTable = 33,

    /// `ATTR_KIND_Z_EXT`.
    ZExt = 34,

    /// `ATTR_KIND_BUILTIN`.
    Builtin = 35,

    /// `ATTR_KIND_COLD`.
    Cold = 36,

    /// `ATTR_KIND_OPTIMIZE_NONE`.
    OptimizeNone = 37,

    /// `ATTR_KIND_IN_ALLOCA`.
    InAlloca = 38,

    /// `ATTR_KIND_NON_NULL`.
    NonNull = 39,

    /// `ATTR_KIND_JUMP_TABLE`.
    JumpTable = 40,

    /// `ATTR_KIND_DEREFERENCEABLE`.
    Dereferenceable = 41,

    /// `ATTR_KIND_DEREFERENCEABLE_OR_NULL`.
    DereferenceableOrNull = 42,

    /// `ATTR_KIND_CONVERGENT`.
    Convergent = 43,

    /// `ATTR_KIND_SAFESTACK`.
    SafeStack = 44,

    /// `ATTR_KIND_ARGMEMONLY`.
    ArgMemOnly = 45,

    /// `ATTR_KIND_SWIFT_SELF`.
    SwiftSelf = 46,

    /// `ATTR_KIND_SWIFT_ERROR`.
    SwiftError = 47,

    /// `ATTR_KIND_NO_RECURSE`.
    NoRecurse = 48,

    /// `ATTR_KIND_INACCESSIBLEMEM_ONLY`.
    InaccessibleMemOnly = 49,

    /// `ATTR_KIND_INACCESSIBLEMEM_OR_ARGMEMONLY`.
    InaccessibleMemOrArgMemOnly = 50,

    /// `ATTR_KIND_ALLOC_SIZE`.
    AllocSize = 51,

    /// `ATTR_KIND_WRITEONLY`.
    WriteOnly = 52,

    /// `ATTR_KIND_SPECULATABLE`.
    Speculatable = 53,

    /// `ATTR_KIND_STRICT_FP`.
    StrictFp = 54,

    /// `ATTR_KIND_SANITIZE_HWADDRESS`.
    SanitizeHwAddress = 55,

    /// `ATTR_KIND_NOCF_CHECK`.
    NoCfCheck = 56,

    /// `ATTR_KIND_OPT_FOR_FUZZING`.
    OptForFuzzing = 57,

    /// `ATTR_KIND_SHADOWCALLSTACK`.
    ShadowCallStack = 58,

    /// `ATTR_KIND_SPECULATIVE_LOAD_HARDENING`.
    SpeculativeLoadHardening = 59,

    /// `ATTR_KIND_IMMARG`.
    ImmArg = 60,

    /// `ATTR_KIND_WILLRETURN`.
    WillReturn = 61,

    /// `ATTR_KIND_NOFREE`.
    NoFree = 62,

    /// `ATTR_KIND_NOSYNC`.
    NoSync = 63,

    /// `ATTR_KIND_SANITIZE_MEMTAG`.
    SanitizeMemtag = 64,

    /// `ATTR_KIND_PREALLOCATED`.
    Preallocated = 65,

    /// `ATTR_KIND_NO_MERGE`.
    NoMerge = 66,

    /// `ATTR_KIND_NULL_POINTER_IS_VALID`.
    NullPointerIsValid = 67,

    /// `ATTR_KIND_NOUNDEF`.
    NoUndef = 68,

    /// `ATTR_KIND_BYREF`.
    ByRef = 69,

    /// `ATTR_KIND_MUSTPROGRESS`.
    MustProgress = 70,

    /// `ATTR_KIND_NO_CALLBACK`.
    NoCallback = 71,

    /// `ATTR_KIND_HOT`.
    Hot = 72,

    /// `ATTR_KIND_NO_PROFILE`.
    NoProfile = 73,

    /// `ATTR_KIND_VSCALE_RANGE`.
    VScaleRange = 74,

    /// `ATTR_KIND_SWIFT_ASYNC`.
    SwiftAsync = 75,

    /// `ATTR_KIND_NO_SANITIZE_COVERAGE`.
    NoSanitizeCoverage = 76,

    /// `ATTR_KIND_ELEMENTTYPE`.
    ElementType = 77,

    /// `ATTR_KIND_DISABLE_SANITIZER_INSTRUMENTATION`.
    DisableSanitizerInstrumentation = 78,

    /// `ATTR_KIND_NO_SANITIZE_BOUNDS`.
    NoSantitizeBounds = 79,

    /// `ATTR_KIND_ALLOC_ALIGN`.
    AllocAlign = 80,

    /// `ATTR_KIND_ALLOCATED_POINTER`.
    AllocatedPointer = 81,

    /// `ATTR_KIND_ALLOC_KIND`.
    AllocKind = 82,

    /// `ATTR_KIND_PRESPLIT_COROUTINE`.
    PresplitCoroutine = 83,

    /// `ATTR_KIND_FNRETTHUNK_EXTERN`.
    FnRetThunkExtern = 84,

    /// `ATTR_KIND_SKIP_PROFILE`.
    SkipProfile = 85,

    /// `ATTR_KIND_MEMORY`.
    Memory = 86,

    /// `ATTR_KIND_NOFPCLASS`.
    NoFpClass = 87,
}

impl AttributeKindCode {
    pub fn from_code(code: u32) -> Option<AttributeKindCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ComdatSelectionKindCode {
    /// `COMDAT_SELECTION_KIND_ANY`.
    Any = 1,

    /// `COMDAT_SELECTION_KIND_EXACT_MATCH`.
    ExactMatch = 2,

    /// `COMDAT_SELECTION_KIND_LARGEST`.
    Largest = 3,

    /// `COMDAT_SELECTION_KIND_NO_DUPLICATES`.
    NoDuplicates = 4,

    /// `COMDAT_SELECTION_KIND_SAME_SIZE`.
    SameSize = 5,
}

impl ComdatSelectionKindCode {
    pub fn from_code(code: u32) -> Option<ComdatSelectionKindCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum StringTableCode {
    /// `STRTAB_BLOB`.
    Blob = 1,
}

impl StringTableCode {
    pub fn name(&self) -> &'static str {
        match self {
            StringTableCode::Blob => "BLOB",
        }
    }

    pub fn from_code(code: u32) -> Option<StringTableCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum SymbolTableCode {
    /// `SYMTAB_BLOB`.
    Blob = 1,
}

impl SymbolTableCode {
    pub fn name(&self) -> &'static str {
        match self {
            SymbolTableCode::Blob => "BLOB",
        }
    }

    pub fn from_code(code: u32) -> Option<SymbolTableCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}

// Start of values from `BitCodeEnums.h`.

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum BlockInfoCode {
    /// `BLOCKINFO_CODE_SETBID`.
    SetBId = 1,

    /// `BLOCKINFO_CODE_BLOCKNAME`.
    BlockName = 2,

    /// `BLOCKINFO_CODE_SETRECORDNAME`.
    SetRecordName = 3,
}

impl BlockInfoCode {
    fn name(&self) -> &'static str {
        match self {
            BlockInfoCode::SetBId => "SETBID",
            BlockInfoCode::BlockName => "BLOCKNAME",
            BlockInfoCode::SetRecordName => "SETRECORDNAME",
        }
    }

    pub fn from_code(code: u32) -> Option<BlockInfoCode> {
        let code = code.try_into().ok()?;
        Self::try_from_primitive(code).ok()
    }
}
