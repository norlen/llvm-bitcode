use std::rc::Rc;

use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Void type does not represent any value as has no size.
    Void,

    /// Integer type with a fixed bit-width.
    Integer(IntegerType),

    /// Floating point type, can be a number of different types.
    FloatingPoint(FloatingPointType),

    /// Opaque pointer ([Pointer Type](https://llvm.org/docs/LangRef.html#pointer-type)).
    Pointer { address_space: u64 },

    /// Vector type is a simple type that represents a vecotr of elements ([Vector Type](https://llvm.org/docs/LangRef.html#vector-type))
    ///
    /// Vector types are used when multiple primitive values are operated on in parallel using SIMD
    /// instructions.
    Vector {
        num_elements: u64,
        ty: Rc<Type>,
        is_scalable: bool,
    },

    /// An array arranges elements sequentially in memory ([Array Type](https://llvm.org/docs/LangRef.html#array-type)).
    Array { num_elements: u64, ty: Rc<Type> },

    /// Structure represents a collection of data members together in memory ([Structure Type](https://llvm.org/docs/LangRef.html#structure-type)).
    Structure(Structure),

    /// Function declaration.
    Function {
        parameters: SmallVec<[Rc<Type>; 8]>,
        return_ty: Rc<Type>,
        is_var_arg: bool,
    },

    /// Code label ([Label Type](https://llvm.org/docs/LangRef.html#label-type)).
    Label,

    /// ([Token Type](https://llvm.org/docs/LangRef.html#token-type)).
    Token,

    /// Represents embedded metadata ([Metadata Type](https://llvm.org/docs/LangRef.html#metadata-type)).
    Metadata,

    /// ([Target Extension Type](https://llvm.org/docs/LangRef.html#target-extension-type)).
    TargetExtension {
        name: String,
        type_parameters: SmallVec<[Rc<Type>; 4]>,
        int_parameters: SmallVec<[u32; 8]>,
    },
}

impl Type {
    // pub fn is_pointer(&self) -> bool {
    //     matches!(self, Type::Pointer { address_space: _ })
    // }

    // pub fn is_sized(&self) -> bool {
    //     match self {
    //         // Primitive types are always sized.
    //         Type::Integer(_) | Type::FloatingPoint(_) | Type::Pointer { address_space: _ } => true,

    //         // Certain types cannot have a size such as functions or labels.
    //         Type::Void
    //         | Type::Function {
    //             parameters: _,
    //             return_ty: _,
    //             is_var_arg: _,
    //         }
    //         | Type::Label
    //         | Type::Metadata
    //         | Type::Token => false,

    //         // Other types are harder to decide.
    //         Type::Vector {
    //             num_elements: _,
    //             ty: _,
    //             is_scalable: _,
    //         } => todo!(),
    //         Type::Array {
    //             num_elements: _,
    //             ty: _,
    //         } => todo!(),
    //         Type::Structure(_) => todo!(),

    //         // TODO
    //         Type::TargetExtension {
    //             name: _,
    //             type_parameters: _,
    //             int_parameters: _,
    //         } => todo!(),
    //     }
    // }

    /// Returns `true` if the type is ["first class"](https://llvm.org/docs/LangRef.html#t-firstclass)
    /// , i.e., it is a valid type for a `Value`.
    ///
    /// A first class value is currently all types except for `void` and `function`.
    pub fn is_first_class(&self) -> bool {
        !matches!(
            self,
            Type::Void
                | Type::Function {
                    parameters: _,
                    return_ty: _,
                    is_var_arg: _
                }
        )
    }

    /// Returns `true` if the type can be part of an array.
    pub fn is_valid_element_type(&self) -> bool {
        !matches!(
            self,
            Type::Void
                | Type::Label
                | Type::Metadata
                | Type::Function {
                    parameters: _,
                    return_ty: _,
                    is_var_arg: _,
                }
                | Type::FloatingPoint(FloatingPointType::X86Amx)
                | Type::Vector {
                    num_elements: _,
                    ty: _,
                    is_scalable: true,
                }
        )
    }

    // pub fn is_fp_ty(&self) -> bool {
    //     matches!(self, Type::FloatingPoint(_)) // Probably not true with x86 AMX amd MMX
    // }

    // pub fn is_fp_or_fp_vector_ty(&self) -> bool {
    //     self.scalar_type().is_fp_ty()
    // }

    // /// For vector types return the contained type, otherwise return the type itself.
    // pub fn scalar_type(&self) -> &Type {
    //     match self {
    //         Type::Vector {
    //             num_elements: _,
    //             ty,
    //             is_scalable: _,
    //         } => ty,
    //         _ => self,
    //     }
    // }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Integer(IntegerType { bits }) => write!(f, "i{bits}"),
            Type::FloatingPoint(fp) => match fp {
                FloatingPointType::Half => write!(f, "half"),
                FloatingPointType::BFloat => write!(f, "bfloat"),
                FloatingPointType::Float => write!(f, "float"),
                FloatingPointType::Double => write!(f, "double"),
                FloatingPointType::Fp128 => write!(f, "fp128"),
                FloatingPointType::X86Fp80 => write!(f, "x86_fp80"),
                FloatingPointType::PpcFp128 => write!(f, "ppc_fp128"),
                FloatingPointType::X86Amx => write!(f, "x86_amx"),
                FloatingPointType::X86Mmx => write!(f, "x86_mmx"),
            },
            Type::Pointer { address_space } => {
                // Default address space.
                if *address_space == 0 {
                    write!(f, "ptr")
                } else {
                    write!(f, "ptr addrspace({address_space})")
                }
            }
            Type::Vector {
                num_elements,
                ty,
                is_scalable,
            } => {
                if *is_scalable {
                    write!(f, "<vscale x {num_elements} x {ty}>")
                } else {
                    write!(f, "<{num_elements} x {ty}>")
                }
            }
            Type::Array { num_elements, ty } => write!(f, "[{num_elements} x {ty}]"),
            Type::Structure(structure) => match structure {
                Structure::Literal(StructureType { fields, is_packed }) => {
                    if *is_packed {
                        write!(f, "<")?;
                    }
                    if fields.is_empty() {
                        write!(f, "{{}}")?;
                    } else {
                        write!(f, "{{ ")?;
                        for (i, field) in fields.iter().enumerate() {
                            if i + 1 == fields.len() {
                                write!(f, "{field}")?;
                            } else {
                                write!(f, "{field}, ")?;
                            }
                        }
                        write!(f, " }}")?;
                    }
                    if *is_packed {
                        write!(f, ">")?;
                    }
                    Ok(())
                }
                Structure::Identified(name, StructureType { fields, is_packed }) => {
                    write!(f, "%{name} = ")?;
                    if *is_packed {
                        write!(f, "<")?;
                    }
                    if fields.is_empty() {
                        write!(f, "{{}}")?;
                    } else {
                        write!(f, "{{ ")?;
                        for (i, field) in fields.iter().enumerate() {
                            if i + 1 == fields.len() {
                                write!(f, "{field}")?;
                            } else {
                                write!(f, "{field}, ")?;
                            }
                        }
                        write!(f, " }}")?;
                    }
                    if *is_packed {
                        write!(f, ">")?;
                    }
                    Ok(())
                }
                Structure::Opaque(name) => write!(f, "%{name} = type opaque"),
            },
            Type::Function {
                parameters,
                return_ty,
                is_var_arg,
            } => {
                write!(f, "{return_ty} (")?;
                for (i, parameter) in parameters.iter().enumerate() {
                    if i + 1 == parameters.len() && !*is_var_arg {
                        write!(f, "{parameter}")?;
                    } else {
                        write!(f, "{parameter}, ")?;
                    }
                }
                if *is_var_arg {
                    write!(f, "...")?;
                }

                write!(f, ")")
            }
            Type::Label => write!(f, "label"),
            Type::Token => write!(f, "token"),
            Type::Metadata => write!(f, "metadata"),
            Type::TargetExtension {
                name,
                type_parameters,
                int_parameters,
            } => {
                write!(f, "target(\"{name}\"")?;
                if !type_parameters.is_empty() || !int_parameters.is_empty() {
                    write!(f, ", ")?;
                }
                for (i, type_parameter) in type_parameters.iter().enumerate() {
                    if i + 1 == type_parameters.len() && int_parameters.is_empty() {
                        write!(f, "{type_parameter}")?;
                    } else {
                        write!(f, "{type_parameter}, ")?;
                    }
                }
                for (i, int_parameter) in int_parameters.iter().enumerate() {
                    if i + 1 == int_parameters.len() {
                        write!(f, "{int_parameter}")?;
                    } else {
                        write!(f, "{int_parameter}, ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

/// Integer type.
///
/// Simple type that specifies an arbitrary bid-width for the integer type. Bit widths from `1` to
/// `2^23` can be specified.
///
/// [LLVM Reference](https://llvm.org/docs/LangRef.html#integer-type)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerType {
    pub bits: u32,
}

impl IntegerType {
    /// Minimum number of bits for an integer type.
    pub const MIN_BITS: u32 = 1;

    /// Maximum number of bits for an integer type.
    pub const MAX_BITS: u32 = 8388608;
}

/// Floating-point type.
///
/// [LLVM Reference](https://llvm.org/docs/LangRef.html#floating-point-types)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FloatingPointType {
    /// 16-bit floating point type.
    Half,

    /// 16-bit floating point type (7-bit significant).
    BFloat,

    /// 32-bit floating point type.
    Float,

    /// 64-bit floating point type.
    Double,

    /// 128-bit floating point type (112-bit significand).
    Fp128,

    /// 80-bit floating point type (X87).
    X86Fp80,

    /// 128-bit floating point type (two 64-bits, PowerPC).
    PpcFp128,

    /// Value held in the AMX tile register on an x86 machine ([X86_amx Type](https://llvm.org/docs/LangRef.html#x86-amx-type)]).
    ///
    /// Operations are quite limited for this type.
    ///
    /// - The only intrinsics allowed are: stride load and store, zero and dot product.
    /// - No instruction is allowed for this type.
    /// - There are no arguments, arrays, pointers, vectors, or constants of this type.
    X86Amx,

    /// Value held in an MMX register on an x86 machine ([X86_mmx Type](https://llvm.org/docs/LangRef.html#x86-mmx-type)).
    ///
    /// Operations are quite limited for this type.
    ///
    /// - User specified MMX instructions are represented as instric or asm calls of with arguments
    ///   and/or results of this type.
    /// - Parameters and return values, load and store, and bitcast.
    /// - There are no arrays, vectors or constants of this type.
    X86Mmx,
}

impl FloatingPointType {
    // pub fn is_ieee_like(&self) -> bool {
    //     match self {
    //         FloatingPointType::Half
    //         | FloatingPointType::BFloat
    //         | FloatingPointType::Float
    //         | FloatingPointType::Double
    //         | FloatingPointType::Fp128 => true,
    //         FloatingPointType::X86Fp80
    //         | FloatingPointType::PpcFp128
    //         | FloatingPointType::X86Amx
    //         | FloatingPointType::X86Mmx => false,
    //     }
    // }
}

/// Structure type ([LLVM](https://llvm.org/docs/LangRef.html#structure-type)).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Structure {
    /// Unnamed structured with fields.
    Literal(StructureType),

    /// Named structure with fields
    Identified(String, StructureType),

    /// Named forward declard structure.
    Opaque(String),
}

impl Structure {
    pub fn fields(&self) -> Option<&[Rc<Type>]> {
        match self {
            Structure::Literal(s) | Structure::Identified(_, s) => Some(s.fields.as_ref()),
            Structure::Opaque(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructureType {
    pub fields: SmallVec<[Rc<Type>; 8]>,
    pub is_packed: bool,
}
