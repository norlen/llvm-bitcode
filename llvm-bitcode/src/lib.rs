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
#![allow(non_upper_case_globals)]

pub mod bitcodes;
mod block;
mod context;
pub mod ir;
mod record;
pub(crate) mod util;

use block::{Identification, IdentificationError, ModuleError, StringTableError, SymbolTableError};
use context::Context;
use llvm_bitstream::{BitstreamReader, CursorError, Entry, ReaderError};
use tracing::{info, warn};
pub use util::fields::{Fields, FieldsIter, RecordError};

use crate::{
    bitcodes::TopLevelBlockId,
    block::{parse_module, StringTable, SymbolTable},
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ParserError {
    /// Failed to parse the code to a valid [`TopLevelBlockId`].
    #[error("No discriminant in enum `TopLevelBlockId` matches the value `{0}`")]
    InvalidTopLevelBlockId(u8),

    /// Unexpected termination of the bitstream.
    #[error("Found endblock at top-level, invalid bitstream")]
    InvalidBitstream,

    /// Failed to parse the identification block.
    #[error("{0}")]
    IdentificationError(#[from] IdentificationError),

    /// Failed to parse the string table block.
    #[error("{0}")]
    StringTableError(#[from] StringTableError),

    /// Failed to parse the symbol table block.
    #[error("{0}")]
    SymbolTableError(#[from] SymbolTableError),

    /// Failed to parse the module block.
    #[error("{0}")]
    ModuleError(#[from] ModuleError),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),

    /// Error from the underlying `BitstreamCursor`.
    #[error("{0}")]
    CursorError(#[from] CursorError),
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
    // We have to take a little more care here compared to when we parse subblocks. The top-level
    // blocks are "free standing" and are not enclosed in enter/leave blocks. After a top-level
    // block has been parsed, if the bitstream is exhausted, we are done.

    let mut ctx = Context::new();

    // Parse all blocks except for the module block. We want to have parsed the string table
    // beforehand, and it's typically after the module block in the bitstream. Thus, we parse
    // everything but that before resuming at the module block.
    //
    // Here, bitcode files can also be concatenated together, and thus we can get more than one
    // set of top-level blocks. This is currently not supported.
    let mut module_block = None;
    while !bitstream.cursor().is_empty() {
        let block = match bitstream.advance()? {
            Some(entry) => match entry {
                Entry::SubBlock(block) => block,
                Entry::Record(entry) => {
                    warn!("Record at top-level: skipping");
                    bitstream.skip_record(entry)?;
                    continue;
                }
            },
            None => return Err(ParserError::InvalidBitstream),
        };

        let Some(block_id) = TopLevelBlockId::from_id(block.id) else {
            warn!("Found unknown top-level block id: {}: skipping", block.id);
            bitstream.skip_block()?;
            continue;
        };

        // We only expect top-level blocks here.
        match block_id {
            TopLevelBlockId::Identification => {
                info!("Parse Identification block");
                bitstream.enter_block(block)?;
                let identification = Identification::parse(bitstream)?;
                ctx.set_identification(identification);
            }
            TopLevelBlockId::Module => {
                info!("Found Module block, saving for later");
                module_block = Some((block, bitstream.cursor().bit_position()));
                bitstream.skip_block()?;
            }
            TopLevelBlockId::StringTable => {
                info!("Parse StringTable block");
                bitstream.enter_block(block)?;
                let string_table = StringTable::parse(bitstream)?;
                ctx.set_string_table(string_table);
            }
            TopLevelBlockId::Symtab => {
                info!("Parse Symtab block");
                bitstream.enter_block(block)?;
                let symbol_table = SymbolTable::parse(bitstream)?;
                ctx.set_symbol_table(symbol_table);
            }
        }
    }

    let Some((block, position)) = module_block else {
        panic!("Could not find module block");
    };

    info!("Resuming parsing for module block");
    // Restart parsing to the module block and resume there.
    bitstream.mut_cursor().set_bit_position(position)?;
    bitstream.enter_block(block)?;

    let module_info = parse_module(bitstream, &mut ctx)?;
    println!("ModuleInfo: {module_info:#?}");

    Ok(())
}
