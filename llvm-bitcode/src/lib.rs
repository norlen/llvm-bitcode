// BEGIN - Embark standard lints v6 for Rust 1.55+
// do not change or add/remove here, but one can add exceptions after this section
// for more info see: <https://github.com/EmbarkStudios/rust-ecosystem/issues/59>
#![deny(unsafe_code)]
#![warn(
    clippy::all,
    clippy::await_holding_lock,
    clippy::char_lit_as_u8,
    clippy::checked_conversions,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::doc_markdown,
    clippy::empty_enum,
    clippy::enum_glob_use,
    clippy::exit,
    clippy::expl_impl_clone_on_copy,
    clippy::explicit_deref_methods,
    clippy::explicit_into_iter_loop,
    clippy::fallible_impl_from,
    clippy::filter_map_next,
    clippy::flat_map_option,
    clippy::float_cmp_const,
    clippy::fn_params_excessive_bools,
    clippy::from_iter_instead_of_collect,
    clippy::if_let_mutex,
    clippy::implicit_clone,
    clippy::imprecise_flops,
    clippy::inefficient_to_string,
    clippy::invalid_upcast_comparisons,
    clippy::large_digit_groups,
    clippy::large_stack_arrays,
    clippy::large_types_passed_by_value,
    clippy::let_unit_value,
    clippy::linkedlist,
    clippy::lossy_float_literal,
    clippy::macro_use_imports,
    clippy::manual_ok_or,
    clippy::map_err_ignore,
    clippy::map_flatten,
    clippy::map_unwrap_or,
    clippy::match_on_vec_items,
    clippy::match_same_arms,
    clippy::match_wild_err_arm,
    clippy::match_wildcard_for_single_variants,
    clippy::mem_forget,
    clippy::mismatched_target_os,
    clippy::missing_enforced_import_renames,
    clippy::mut_mut,
    clippy::mutex_integer,
    clippy::needless_borrow,
    clippy::needless_continue,
    clippy::needless_for_each,
    clippy::option_option,
    clippy::path_buf_push_overwrite,
    clippy::ptr_as_ptr,
    clippy::rc_mutex,
    clippy::ref_option_ref,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_functions_in_if_condition,
    clippy::semicolon_if_nothing_returned,
    clippy::single_match_else,
    clippy::string_add_assign,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_to_string,
    clippy::todo,
    clippy::trait_duplication_in_bounds,
    clippy::unimplemented,
    clippy::unnested_or_patterns,
    clippy::unused_self,
    clippy::useless_transmute,
    clippy::verbose_file_reads,
    clippy::zero_sized_map_values,
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms
)]
// END - Embark standard lints v6 for Rust 1.55+
// crate-specific exceptions:
// #![allow()]
#![allow(unused)]

pub mod bitcodes;
mod block;
mod context;
mod record;
mod util;

use block::{IdentificationError, StrtabError};
use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use num_enum::TryFromPrimitiveError;
use tracing::info;
use typed_arena::Arena;
pub use util::{Fields, FieldsIter, RecordError};

use crate::bitcodes::TopLevelBlockId;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ParserError {
    #[error("{0}")]
    IdentificationError(#[from] IdentificationError),

    #[error("{0}")]
    StrtabError(#[from] StrtabError),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),

    /// Failed to parse the code to a valid [`TopLevelBlockId`].
    #[error("No discriminant in enum `TopLevelBlockId` matches the value `{0}`")]
    InvalidTopLevelBlockId(u8),
}

impl From<TryFromPrimitiveError<TopLevelBlockId>> for ParserError {
    fn from(value: TryFromPrimitiveError<TopLevelBlockId>) -> Self {
        ParserError::InvalidTopLevelBlockId(value.number)
    }
}

#[allow(unused_variables, unused_assignments)]
pub fn parse<T: AsRef<[u8]>>(bytes: T) -> Result<(), ParserError> {
    let mut bitstream = BitstreamReader::from_bytes(bytes)?;
    parse_modules(&mut bitstream)?;
    Ok(())
}

pub fn parse_modules<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
) -> Result<(), ParserError> {
    // Parse top-level modules.
    //
    // We have to take a little more care here, our first entry should be a top-level block. But,
    // when the stream has been exhausted we'll get `None` back, since we don't have a "modules"
    // block.

    // let type_arena = Arena::new();
    // let string_arena = Arena::new();
    // let attributes_group_arena = Arena::new();
    // let value_arena = Arena::new();
    // let mut context = Context::new(
    //     &type_arena,
    //     &string_arena,
    //     &attributes_group_arena,
    //     &value_arena,
    // );

    // let mut identification = None;

    // let mut module_bitstream = None;
    while !bitstream.cursor().is_empty() {
        let block = match bitstream.advance()? {
            Some(entry) => match entry {
                Entry::SubBlock(block) => block,
                Entry::Record(_) => todo!(),
            },
            None => todo!(),
        };
        let block_id = TopLevelBlockId::try_from(block.id as u8)?;

        // We only expect top-level blocks here.
        use TopLevelBlockId::*;
        match block_id {
            Identification => {
                info!("Identification");
                bitstream.skip_block(block)?;
                // identification = Some(parse_identification(bitstream)?);
            }
            Module => {
                info!("Module");
                bitstream.skip_block(block)?;
                // module_bitstream = Some(bitstream.clone());
                // bitstream.skip().unwrap();
            }
            StringTable => {
                info!("StringTable");
                bitstream.skip_block(block)?;
                // let strtab = parse_strtab(bitstream).unwrap();
                // context.set_strtab(strtab);
            }
            Symtab => {
                info!("Symtab");
                bitstream.skip_block(block)?;
                // bitstream.skip().unwrap();
                // symtab = Some(SymtabBlock::parse(bitstream)?);
            }
        }
    }

    // let _ = parse_module(&mut module_bitstream.unwrap(), &mut context).unwrap();

    Ok(())
}
