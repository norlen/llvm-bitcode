use bitflags::bitflags;
use smallvec::SmallVec;
use std::rc::Rc;

use crate::util::types::Type;

// TODO
//
// Check if we want to instead split these off by where they are present, i.e., as parameter
// attributes, function attributes, etc. Instead of all attributes being able to represent any kind
// of attribute.
//
// Splitting them up would mean lot of duplicates, but it may be worth it for consumers as they
// only care about the attributes that are actually valid for the use case.

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeList(SmallVec<[Rc<AttributeGroup>; 8]>);

impl std::fmt::Display for AttributeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{{")?;
        for (i, attribute) in self.0.iter().enumerate() {
            if i + 1 == self.0.len() {
                write!(f, "!{}", attribute.group_id)?;
            } else {
                write!(f, "!{} ", attribute.group_id)?;
            }
        }
        write!(f, "}}")
    }
}

impl AttributeList {
    pub fn new(attribute_groups: SmallVec<[Rc<AttributeGroup>; 8]>) -> Self {
        Self(attribute_groups)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeGroup {
    pub group_id: u64,
    pub attributes: Vec<Attribute>,
}

impl AttributeGroup {
    pub fn new(group_id: u64) -> Self {
        Self {
            group_id,
            attributes: Vec::new(),
        }
    }
}

impl std::fmt::Display for AttributeGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // attributes #0 = { noinline nounwind nonlazybind "probe-stack"="__rust_probestack" "target-cpu"="x86-64" }
        write!(f, "attributes #{} {{", self.group_id)?;
        for (i, attr) in self.attributes.iter().enumerate() {
            if i + 1 == self.attributes.len() {
                write!(f, " {attr} ")?;
            } else {
                write!(f, " {attr}")?;
            }
        }
        write!(f, "}}")
    }
}

#[repr(u8)]
pub enum UnwindTableKind {
    /// No unwind table requested.
    None = 0,

    /// Synchronous unwind tables.
    Sync = 1,

    /// Asynchronous unwind tables.
    Async = 2,

    /// Default.
    Default = 3,
}

bitflags! {
    /// Flags that indicate if a memory access modifies or references memory.
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
    pub struct MemoryEffectsFlags: u8 {
        /// Access does not modify or reference the value stored in memory.
        const NoModifyOrReference = 0b00000000;

        /// Access may reference the value stored in memory.
        const Reference = 0b00000001;

        /// Access may modify the value stored in memory.
        const Modify = 0b00000010;

        /// Access may reference and may modify the value stored in memory.
        const ModifyReference = Self::Reference.bits() | Self::Modify.bits();
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
#[repr(u8)]
pub enum Location {
    ArgMem = 0,

    InaccessibleMem = 1,

    Other = 2,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct MemoryEffects {
    data: u8,
}

impl MemoryEffects {
    const BITS_PER_LOCATION: u8 = 2;
    const LOCATION_MASK: u8 = (1 << Self::BITS_PER_LOCATION) - 1;

    /// New `MemoryEffects` with `flags` set on all locations.
    pub fn with_flags(flags: MemoryEffectsFlags) -> Self {
        let mut s = MemoryEffects { data: 0 };
        s.set(Location::ArgMem, flags);
        s.set(Location::InaccessibleMem, flags);
        s.set(Location::Other, flags);
        s
    }

    /// New `MemoryEffects` that can both read and write any memory.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::unknown();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::ModifyReference);
    /// assert_eq!(mem.get(Location::InaccessibleMem), MemoryEffectsFlags::ModifyReference);
    /// assert_eq!(mem.get(Location::Other), MemoryEffectsFlags::ModifyReference);
    /// ```
    pub fn unknown() -> Self {
        Self::with_flags(MemoryEffectsFlags::ModifyReference)
    }

    /// New `MemoryEffects` that cannot read or write any memory.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::none();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::NoModifyOrReference);
    /// assert_eq!(mem.get(Location::InaccessibleMem), MemoryEffectsFlags::NoModifyOrReference);
    /// assert_eq!(mem.get(Location::Other), MemoryEffectsFlags::NoModifyOrReference);
    /// ```
    pub fn none() -> Self {
        Self::with_flags(MemoryEffectsFlags::NoModifyOrReference)
    }

    /// New `MemoryEffects` that can only read any memory.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::read_only();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::Reference);
    /// assert_eq!(mem.get(Location::InaccessibleMem), MemoryEffectsFlags::Reference);
    /// assert_eq!(mem.get(Location::Other), MemoryEffectsFlags::Reference);
    /// ```
    pub fn read_only() -> Self {
        Self::with_flags(MemoryEffectsFlags::Reference)
    }

    /// New `MemoryEffects` that can only write any memory.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::write_only();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::Modify);
    /// assert_eq!(mem.get(Location::InaccessibleMem), MemoryEffectsFlags::Modify);
    /// assert_eq!(mem.get(Location::Other), MemoryEffectsFlags::Modify);
    /// ```
    pub fn write_only() -> Self {
        Self::with_flags(MemoryEffectsFlags::Modify)
    }

    /// New `MemoryEffects` that can only access [`Location::ArgMem`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::arg_mem_only();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::ModifyReference);
    /// ```
    pub fn arg_mem_only() -> Self {
        Self::with_flags(MemoryEffectsFlags::ModifyReference)
    }

    /// New `MemoryEffects` that can only access [`Location::InaccessibleMem`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::inacessible_mem_only();
    /// assert_eq!(mem.get(Location::InaccessibleMem), MemoryEffectsFlags::ModifyReference);
    /// ```
    pub fn inacessible_mem_only() -> Self {
        Self::with_flags(MemoryEffectsFlags::ModifyReference)
    }

    /// New `MemoryEffects` that can only access [`Location::ArgMem`] or [`Location::InaccessibleMem`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::{MemoryEffects, MemoryEffectsFlags, Location};
    /// let mem = MemoryEffects::arg_or_inaccessible_mem_only();
    /// assert_eq!(mem.get(Location::ArgMem), MemoryEffectsFlags::ModifyReference);
    /// ```
    pub fn arg_or_inaccessible_mem_only() -> Self {
        Self::with_flags(MemoryEffectsFlags::ModifyReference)
    }

    /// Union with another `MemoryEffects` returning the result.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::MemoryEffects;
    /// let mem = MemoryEffects::none();
    /// let mem = mem.union(MemoryEffects::unknown());
    /// assert_eq!(mem, MemoryEffects::unknown());
    /// ```
    pub fn union(&self, other: MemoryEffects) -> Self {
        MemoryEffects {
            data: self.data | other.data,
        }
    }

    /// Intersect with another `MemoryEffects` returning the result.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::ir::MemoryEffects;
    /// let mem = MemoryEffects::none();
    /// let mem = mem.intersect(MemoryEffects::unknown());
    /// assert_eq!(mem, MemoryEffects::none());
    /// ```
    pub fn intersect(&self, other: MemoryEffects) -> Self {
        MemoryEffects {
            data: self.data & other.data,
        }
    }

    /// Get [`MemoryEffectsFlags`] for a [`Location`].
    pub fn get(&self, location: Location) -> MemoryEffectsFlags {
        let d = self.data >> Self::location_shift(location) & Self::LOCATION_MASK;

        // Unwrap safety:
        // `LOCATION_MASK` is 3 is bits, matching all possible combinations in `MemoryEffectsFlags`.
        MemoryEffectsFlags::from_bits(d).unwrap()
    }

    /// Return the underlying data.
    pub fn data(&self) -> u8 {
        self.data
    }

    fn set(&mut self, location: Location, flags: MemoryEffectsFlags) {
        self.data &= !(Self::LOCATION_MASK << Self::location_shift(location));
        self.data |= flags.bits() << Self::location_shift(location);
    }

    fn location_shift(location: Location) -> u8 {
        location as u8 * Self::BITS_PER_LOCATION
    }
}

///
///
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Attribute {
    /// Well-known attribute.
    Enum(EnumAttribute),

    /// Well-known attribute with an integer value.
    Integer(IntAttribute, u64),

    /// String attribute with an optional string value.
    String(String, Option<String>),

    /// Type attribute with an optional type parameter.
    Type(TypeAttribute, Option<Rc<Type>>),
}

impl Attribute {}

impl std::fmt::Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Enum(attr) => write!(f, "{attr}"),
            Attribute::Integer(attr, value) => write!(f, "{attr}({value})"),
            Attribute::String(key, value) => match value {
                Some(value) => write!(f, "\"{key}\"=\"{value}\""),
                None => write!(f, "\"{key}\""),
            },
            Attribute::Type(attr, ty) => match ty {
                Some(ty) => write!(f, "{attr}({ty})"),
                None => write!(f, "{attr}"),
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum EnumAttribute {
    /// Parameter of a function that tells the alignment of an allocation.
    AllocAlign,

    /// Parameter is the pointer to be manipulated by the allocator function.
    AllocatedPointer,

    /// `inline=always`.
    AlwaysInline,

    /// Callee is recognized as a builtin, despite nobuiltin attribute on its declaration.
    Builtin,

    /// Marks function as being in a cold path.
    Cold,

    /// Can only be moved to control-equivalent blocks.
    Convergent,

    /// Do not instrument function with sanitizers.
    DisableSanitizerInstrumentation,

    /// Whether to keep return instructions, or replace with a jump to an external symbol.
    FnRetThunkExtern,

    /// Marks function as being in a hot path and frequently called.
    Hot,

    /// Parameter is required to be a trivial constant.
    ImmArg,

    /// Force argument to be passed in register.
    InReg,

    /// Source said inlining was desirable.
    InlineHint,

    /// Build jump-instruction tables and replace refs.
    JumpTable,

    /// Function must be optimized for size first.
    MinSize,

    /// Function is required to make forward progress.
    MustProgress,

    /// Naked function.
    Naked,

    /// Nested function static chain.
    Nest,

    /// Considered to not alias after call.
    NoAlias,

    /// Callee isn't recognized as a builtin.
    NoBuiltin,

    /// Function cannot enter into caller's translation unit.
    NoCallback,

    /// Function creates no aliases of pointer.
    NoCapture,

    /// Disable Indirect Branch Tracking.
    NoCfCheck,

    /// Call cannot be duplicated.
    NoDuplicate,

    /// Function does not deallocate memory.
    NoFree,

    /// Disable implicit floating point insts.
    NoImplicitFloat,

    /// `inline=never`.
    NoInline,

    /// Disable merging for specified functions or call sites.
    NoMerge,

    /// Function should not be instrumented.
    NoProfile,

    /// The function does not recurse.
    NoRecurse,

    /// Disable redzone.
    NoRedZone,

    /// Mark the function as not returning.
    NoReturn,

    /// No SanitizeBounds instrumentation.
    NoSanitizeBounds,

    /// No SanitizeCoverage instrumentation.
    NoSanitizeCoverage,

    /// Function does not synchronize.
    NoSync,

    /// Parameter or return value may not contain uninitialized or poison bits.
    NoUndef,

    /// Function doesn't unwind stack.
    NoUnwind,

    /// Function is called early and/or often, so lazy binding isn't worthwhile.
    NonLazyBind,

    /// Pointer is known to be not null.
    NonNull,

    /// Null pointer in address space zero is valid.
    NullPointerIsValid,

    /// Select optimizations for best fuzzing signal.
    OptForFuzzing,

    /// opt_size.
    OptimizeForSize,

    /// Function must not be optimized.
    OptimizeNone,

    /// Function is a presplit coroutine.
    PresplitCoroutine,

    /// Return value is always equal to this argument.
    Returned,

    /// Function can return twice.
    ReturnsTwice,

    /// Sign extended before/after call.
    SExt,

    /// Safe Stack protection.
    SafeStack,

    /// AddressSanitizer is on.
    SanitizeAddress,

    /// HWAddressSanitizer is on.
    SanitizeHwAddress,

    /// MemTagSanitizer is on.
    SanitizeMemTag,

    /// MemorySanitizer is on.
    SanitizeMemory,

    /// ThreadSanitizer is on.
    SanitizeThread,

    /// Shadow Call Stack protection.
    ShadowCallStack,

    /// Function can be speculated.
    Speculatable,

    /// Speculative Load Hardening is enabled.
    SpeculativeLoadHardening,

    /// Stack protection.
    StackProtect,

    /// Stack protection required.
    StackProtectReq,

    /// Strong Stack protection.
    StackProtectStrong,

    /// Function was called in a scope requiring strict floating point semantics.
    StrictFp,

    /// Argument is swift async context.
    SwiftAsync,

    /// Argument is swift error.
    SwiftError,

    /// Argument is swift self/context.
    SwiftSelf,

    /// Function always comes back to callsite.
    WillReturn,

    /// Zero extended before/after call.
    ZExt,

    /// This function should not be instrumented but it is ok to inline profiled functions into it.
    SkipProfile,
}

impl std::fmt::Display for EnumAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnumAttribute::AllocAlign => write!(f, "allocalign"),
            EnumAttribute::AllocatedPointer => write!(f, "allocptr"),
            EnumAttribute::AlwaysInline => write!(f, "alwaysinline"),
            EnumAttribute::Builtin => write!(f, "builtin"),
            EnumAttribute::Cold => write!(f, "cold"),
            EnumAttribute::Convergent => write!(f, "convergent"),
            EnumAttribute::DisableSanitizerInstrumentation => {
                write!(f, "disable_sanitizer_instrumentation")
            }
            EnumAttribute::FnRetThunkExtern => write!(f, "fnret_thunk_extern"),
            EnumAttribute::Hot => write!(f, "hot"),
            EnumAttribute::ImmArg => write!(f, "immarg"),
            EnumAttribute::InReg => write!(f, "inreg"),
            EnumAttribute::InlineHint => write!(f, "inlinehint"),
            EnumAttribute::JumpTable => write!(f, "jumptable"),
            EnumAttribute::MinSize => write!(f, "minsize"),
            EnumAttribute::MustProgress => write!(f, "mustprogress"),
            EnumAttribute::Naked => write!(f, "naked"),
            EnumAttribute::Nest => write!(f, "nest"),
            EnumAttribute::NoAlias => write!(f, "noalias"),
            EnumAttribute::NoBuiltin => write!(f, "nobuiltin"),
            EnumAttribute::NoCallback => write!(f, "nocallback"),
            EnumAttribute::NoCapture => write!(f, "nocapture"),
            EnumAttribute::NoCfCheck => write!(f, "nocf_check"),
            EnumAttribute::NoDuplicate => write!(f, "noduplicate"),
            EnumAttribute::NoFree => write!(f, "nofree"),
            EnumAttribute::NoImplicitFloat => write!(f, "noimplicitfloat"),
            EnumAttribute::NoInline => write!(f, "noinline"),
            EnumAttribute::NoMerge => write!(f, "nomerge"),
            EnumAttribute::NoProfile => write!(f, "noprofile"),
            EnumAttribute::NoRecurse => write!(f, "norecurse"),
            EnumAttribute::NoRedZone => write!(f, "noredzone"),
            EnumAttribute::NoReturn => write!(f, "noreturn"),
            EnumAttribute::NoSanitizeBounds => write!(f, "nosanitize_bounds"),
            EnumAttribute::NoSanitizeCoverage => write!(f, "nosanitize_coverage"),
            EnumAttribute::NoSync => write!(f, "nosync"),
            EnumAttribute::NoUndef => write!(f, "noundef"),
            EnumAttribute::NoUnwind => write!(f, "nounwind"),
            EnumAttribute::NonLazyBind => write!(f, "nonlazybind"),
            EnumAttribute::NonNull => write!(f, "nonnull"),
            EnumAttribute::NullPointerIsValid => write!(f, "null_pointer_is_valid"),
            EnumAttribute::OptForFuzzing => write!(f, "optforfuzzing"),
            EnumAttribute::OptimizeForSize => write!(f, "optsize"),
            EnumAttribute::OptimizeNone => write!(f, "optnone"),
            EnumAttribute::PresplitCoroutine => write!(f, "presplit_coroutine"),
            EnumAttribute::Returned => write!(f, "returned"),
            EnumAttribute::ReturnsTwice => write!(f, "returns_twice"),
            EnumAttribute::SExt => write!(f, "signext"),
            EnumAttribute::SafeStack => write!(f, "safestack"),
            EnumAttribute::SanitizeAddress => write!(f, "sanitize_address"),
            EnumAttribute::SanitizeHwAddress => write!(f, "sanitize_hwaddress"),
            EnumAttribute::SanitizeMemTag => write!(f, "sanitize_memtag"),
            EnumAttribute::SanitizeMemory => write!(f, "sanitize_memory"),
            EnumAttribute::SanitizeThread => write!(f, "sanitize_thread"),
            EnumAttribute::ShadowCallStack => write!(f, "shadowcallstack"),
            EnumAttribute::Speculatable => write!(f, "speculatable"),
            EnumAttribute::SpeculativeLoadHardening => write!(f, "speculative_load_hardening"),
            EnumAttribute::StackProtect => write!(f, "ssp"),
            EnumAttribute::StackProtectReq => write!(f, "sspreq"),
            EnumAttribute::StackProtectStrong => write!(f, "sspstrong"),
            EnumAttribute::StrictFp => write!(f, "strictfp"),
            EnumAttribute::SwiftAsync => write!(f, "swiftasync"),
            EnumAttribute::SwiftError => write!(f, "swifterror"),
            EnumAttribute::SwiftSelf => write!(f, "swiftself"),
            EnumAttribute::WillReturn => write!(f, "willreturn"),
            EnumAttribute::ZExt => write!(f, "zeroext"),
            EnumAttribute::SkipProfile => write!(f, "skipprofile"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum IntAttribute {
    /// Alignment of parameter.
    Alignment,

    /// Describes behaviour of an allocator in terms of known properties.
    AllocKind,

    /// The result of the function is guaranteed to point to a number of bytes that we can determine
    /// if we know the value of the function's arguments.
    AllocSize,

    /// Pointer is known to be dereferenceable.
    Dereferenceable,

    /// Pointer is either null or dereferenceable.
    DereferenceableOrNull,

    /// Alignment of stack for function.
    StackAlignment,

    /// Function must be in a unwind table.
    UnwindTable,

    /// Minimum/Maximum vscale value for function.
    VScaleRange,

    /// Memory effects of the function.
    Memory,

    /// Forbidden floating-point classes.
    NoFpClass,
}

impl std::fmt::Display for IntAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntAttribute::Alignment => write!(f, "align"),
            IntAttribute::AllocKind => write!(f, "allockind"),
            IntAttribute::AllocSize => write!(f, "allocsize"),
            IntAttribute::Dereferenceable => write!(f, "dereferenceable"),
            IntAttribute::DereferenceableOrNull => write!(f, "dereferenceable_or_null"),
            IntAttribute::StackAlignment => write!(f, "alignstack"),
            IntAttribute::UnwindTable => write!(f, "uwtable"),
            IntAttribute::VScaleRange => write!(f, "vscalerange"),
            IntAttribute::Memory => write!(f, "memory"),
            IntAttribute::NoFpClass => write!(f, "nofpclass"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TypeAttribute {
    /// Mark in-memory ABI type.
    ByRef,

    /// Similar to byval but without a copy.
    ByVal,

    /// Provide pointer element type to intrinsic.
    ElementType,

    /// Pass structure in an alloca.
    InAlloca,

    /// Similar to byval but without a copy.
    Preallocated,

    /// Hidden pointer to structure to return.
    StructRet,
}

impl std::fmt::Display for TypeAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAttribute::ByRef => write!(f, "byref"),
            TypeAttribute::ByVal => write!(f, "byval"),
            TypeAttribute::ElementType => write!(f, "elementtype"),
            TypeAttribute::InAlloca => write!(f, "inalloca"),
            TypeAttribute::Preallocated => write!(f, "preallocated"),
            TypeAttribute::StructRet => write!(f, "sret"),
        }
    }
}
