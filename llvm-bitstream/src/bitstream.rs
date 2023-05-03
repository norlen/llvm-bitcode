use std::{collections::HashMap, rc::Rc};

use smallvec::SmallVec;

use crate::{
    abbreviation::{ArrayOperand, CodeAbbreviation, OperandEncoding},
    is_bitcode_wrapper, is_llvm_bitcode, AbbreviationOperand, AbbreviationRecord, BitstreamCursor,
    Block, CursorError, Entry, Record,
};

/// Errors when performing operations using a [`BitstreamReader`].
#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ReaderError {
    /// Byte-buffer to parse did not contain LLVM magic numbers.
    #[error("Bitcode did not contain LLVM magic numbers")]
    InvalidBitcode,

    /// When entering a subblock the given new abbreviation bit-width is invalid.
    #[error("Invalid code size")]
    InvalidCodeSize,

    /// When the parsed length exeeds the number of bits in the bitstream.
    #[error("Array length is too large")]
    InvalidSize,

    /// When a leave block is not matched with a entry to a subblock.
    #[error("Too many leave block abbrevations, cannot leave top-most scope")]
    InvalidLeaveBlock,

    /// Failed to parse a define abbreviation record.
    #[error("Error while parsing abbreviation: {0}")]
    InvalidAbbreviationRecord(&'static str),

    /// Did not find a stored abbrevation with that id.
    #[error("Could not find abbrevation with given id")]
    InvalidAbbrevationId,

    /// Unknown code when parsing records in a block info block.
    #[error("Encountered an invalid code for block id")]
    InvalidBlockId,

    /// Failed to parse a block info block.
    #[error("Failed to parse blockinfo block")]
    InvalidBlockInfo,

    /// Errors from the underlying [`BitstreamCursor`].
    #[error(transparent)]
    CursorError(#[from] CursorError),
}

/// Abbreviations are a self-describing way to parse the bitstream.
///
/// Standard abbreviations can
///
/// - Enter and exit blocks.
/// - Define abbreviations.
/// - Define unabbreviated records.
///
/// Apart from these predefined abbreviations the bitstream can contain additional `DEFINE_ABBREV`
/// entries which define new abbreviations.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum AbbreviationId {
    /// `END_BLOCK` ends a block.
    EndBlock,

    /// `ENTER_SUBBLOCK` designated the start of a subblock.
    EnterSubblock,

    /// `DEFINE_ABBREV` defines an abbreviation for the current block.
    DefineAbbreviation,

    /// `UNABBREV_RECORD` does not have defined abbrevation and use `vbr6` for all fields.
    Unabbreviated,

    /// Application defined abbreviation.
    ApplicationDefined(u64),
}

impl From<u64> for AbbreviationId {
    fn from(value: u64) -> Self {
        match value {
            0 => AbbreviationId::EndBlock,
            1 => AbbreviationId::EnterSubblock,
            2 => AbbreviationId::DefineAbbreviation,
            3 => AbbreviationId::Unabbreviated,
            n => AbbreviationId::ApplicationDefined(n),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[repr(u8)]
enum BlockInfoCode {
    /// `BLOCKINFO_CODE_SETBID`: `[blockid#]`.
    SetId = 1,

    /// `BLOCKINFO_CODE_BLOCKNAME`: `[name]`
    Name,

    /// `BLOCKINFO_CODE_SETRECORDNAME`: `[id, name]`.
    SetRecordName,
}

impl TryFrom<u32> for BlockInfoCode {
    type Error = ReaderError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(BlockInfoCode::SetId),
            2 => Ok(BlockInfoCode::Name),
            3 => Ok(BlockInfoCode::SetRecordName),
            _ => Err(ReaderError::InvalidBlockId),
        }
    }
}

/// Track abbreviations read from a `BLOCKINFO` block.
#[derive(Clone, Debug, Eq, PartialEq)]
struct BlockInfo {
    id: u32,
    name: String,
    abbreviations: Vec<Rc<AbbreviationRecord>>,
    record_names: Vec<(u32, String)>,
}

impl BlockInfo {
    fn new(id: u32) -> Self {
        Self {
            id,
            name: String::new(),
            abbreviations: Vec::new(),
            record_names: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct StreamBlock {
    /// Declared size of code values used for the current block, in bits.
    code_width: u32,

    /// Abbreviations installed at this block.
    abbreviations: Vec<Rc<AbbreviationRecord>>,
}

impl StreamBlock {
    const FIRST_ABBREVIATION_ID: u32 = 4;

    fn from_code_size(code_width: u32) -> Self {
        Self {
            code_width,
            abbreviations: Vec::new(),
        }
    }

    fn get_abbreviation(&self, abbreviation_id: u32) -> Option<&AbbreviationRecord> {
        let adjusted_id = abbreviation_id - Self::FIRST_ABBREVIATION_ID;
        self.abbreviations
            .get(adjusted_id as usize)
            .map(|v| v.as_ref())
    }
}

/// Low-level entries found in the bitstream.
///
/// Commpared to [`Entry`] these are much more low-level, while also being used in conjunction with
/// more low-level functions in the bitstream.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum RawEntry {
    /// Start of a new sub-block with a specific id.
    SubBlock(Block),

    /// End of the current block.
    EndBlock,

    /// Record with an optional abbreviation id if specified.
    Record(Record),

    /// An abbrevation should be defined.
    DefineAbbreviation,
}

/// A `BitstreamReader` wraps a [`BitstreamCursor`] while providing handling for common LLVM blocks.
///
/// The `BitstreamReader` automatically handles define abbreviation records, and block info blocks.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BitstreamReader<T: AsRef<[u8]>> {
    /// Underlying bitstream cursor.
    cursor: BitstreamCursor<T>,

    /// Block info for the block that is being parsed.
    current_block: StreamBlock,

    /// Block info are handled in a stack-like fashion as subblock are entered and left. Only the
    /// outer block infos are stored here. The current block info is stored in `current_block`.
    block_scope: SmallVec<[StreamBlock; 8]>,

    block_info_records: HashMap<u32, BlockInfo>,
}

impl<T: AsRef<[u8]>> BitstreamReader<T> {
    /// VBR width of the subblock id.
    const BLOCK_ID_WIDTH: u32 = 8;

    /// VBR width of the code length.
    const CODE_LEN_WIDTH: u32 = 4;

    /// Bits the block size is encoded in.
    const BLOCK_SIZE_WIDTH: u32 = 32;

    /// Maximum size for a code len chunk.
    const MAXIMUM_CODE_LEN_CHUNK_SIZE: u32 = 32;

    /// Width of all VBR fields in an unabbreviated record.
    const UNABBREVIATED_WIDTH: u32 = 6;

    /// Default width for code values.
    const DEFAULT_CODE_WIDTH_IN_BITS: u32 = 2;

    /// `BlockInfo` defines metadata about blocks, e.g., abbreviations that should be available to
    /// all blocks with a certain ID.
    pub const BLOCKINFO_BLOCK_ID: u32 = 0;

    /// Create a new reader from an array of bytes.
    ///
    /// The passed bitstream can both be a raw bitstream or a wrapped bitstream.
    ///
    /// # Errors
    ///
    /// If the passed array of bytes is not a valid LLVM bitstream [`ReaderError::InvalidBitcode`]
    /// is returned.
    ///
    /// For wrapped bitstreams if it cannot read the additional data [`CursorError::UnexpectedEOF`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamReader;
    ///
    /// let wrapped_bitstream_bytes = [
    ///     0xde, 0xc0, 0x17, 0x0b, // Magic.
    ///     0x00, 0x00, 0x00, 0x00, // Version.
    ///     0x00, 0x00, 0x00, 0x00, // Offset.
    ///     0x00, 0x00, 0x00, 0x00, // Size.
    ///     0x00, 0x00, 0x00, 0x00, // Cpu type.
    /// ];
    /// let bitstream = BitstreamReader::from_bytes(&wrapped_bitstream_bytes).unwrap();
    /// ```
    pub fn from_bytes(inner: T) -> Result<Self, ReaderError> {
        if !is_llvm_bitcode(inner.as_ref()) {
            return Err(ReaderError::InvalidBitcode);
        }
        let is_wrapped = is_bitcode_wrapper(inner.as_ref());

        let mut bitstream_reader = Self {
            cursor: BitstreamCursor::new(inner),
            current_block: StreamBlock::from_code_size(Self::DEFAULT_CODE_WIDTH_IN_BITS),
            block_scope: SmallVec::new(),
            block_info_records: HashMap::new(),
        };

        // Skip over the magic bytes, we know these exist.
        bitstream_reader.cursor.read(32)?;
        if is_wrapped {
            // Wrapper format is:
            //   [magic (32), version (32), offset (32), size (32), cpu_type (32)]
            let _version = bitstream_reader.cursor.read(32)?;
            let _offset = bitstream_reader.cursor.read(32)?;
            let _size = bitstream_reader.cursor.read(32)?;
            let _cpu_type = bitstream_reader.cursor.read(32)?;
        }

        Ok(bitstream_reader)
    }

    /// Advance the bitstream.
    ///
    /// Low-level advancing of the bitstream. It will only consume `DEFINE_ABBREV` records.
    ///
    /// # Errors
    ///
    /// Can return [`CursorError::UnexpectedEOF`] if the bitstream ends early.
    ///
    /// If it cannot parse define abbreviation records encountered in the bitstream [`ReaderError::InvalidAbbreviationRecord`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
    /// use smallvec::SmallVec;
    ///
    /// fn parse_block<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<(), ReaderError> {
    ///     let mut record: SmallVec<[u64; 32]> = SmallVec::new();
    ///     while let Some(entry) = bitstream.advance()? {
    ///         match entry {
    ///             Entry::SubBlock(block) => {
    ///                 // Process subblock.
    ///                 bitstream.skip_block(block)?;
    ///             }
    ///             Entry::Record(entry) => {
    ///                 let _code = bitstream.read_record(entry, &mut record)?;
    ///                 // Process read record.
    ///             }
    ///         }
    ///     }
    ///     Ok(())
    /// }
    /// ```
    ///
    /// More generally as we start parsing we may want to handle "top-level" blocks. These are blocks
    /// at the highest level that appear one after another and are not contained in a block. For these
    /// cases we should just parse until [`CursorError::UnexpectedEOF`] is encountered.
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
    ///
    /// fn parse<T: AsRef<[u8]>>(bytes: T) -> Result<(), ReaderError> {
    ///     let mut bitstream = BitstreamReader::from_bytes(bytes)?;
    ///
    ///     while !bitstream.cursor().is_empty() {
    ///         let entry = bitstream.advance()?.expect("Should not encounter leave block here");
    ///
    ///         match entry {
    ///             Entry::SubBlock(block) => {
    ///                 // Process top-level block.
    ///                 bitstream.skip_block(block)?;
    ///             }
    ///             Entry::Record(_) => {
    ///                 // At the top-level we should never encounter records.
    ///                 panic!("Unexpected record");
    ///             }
    ///         }
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub fn advance(&mut self) -> Result<Option<Entry>, ReaderError> {
        loop {
            let code = self.cursor.read(self.current_block.code_width)?;
            let entry = match AbbreviationId::from(code) {
                // Parse all `DefineAbbreviation` as we go, the consumer can't do anything with these.
                AbbreviationId::DefineAbbreviation => {
                    let abbreviation = self.parse_define_abbreviation_record()?;
                    self.current_block.abbreviations.push(Rc::new(abbreviation));
                    continue;
                }
                AbbreviationId::EndBlock => {
                    self.leave_block()?;
                    None
                }
                AbbreviationId::EnterSubblock => {
                    let id = self.cursor.read_vbr(Self::BLOCK_ID_WIDTH)?;
                    let size = self.enter_block(id)?;
                    let block = Block { id, size };

                    if block.id == Self::BLOCKINFO_BLOCK_ID {
                        self.parse_block_info()?;
                        continue;
                    }
                    Some(Entry::SubBlock(block))
                }
                AbbreviationId::Unabbreviated => Some(Entry::Record(Record::new(None))),
                AbbreviationId::ApplicationDefined(n) => {
                    Some(Entry::Record(Record::new(Some(n as u32))))
                }
            };

            return Ok(entry);
        }
    }

    /// Advance the bitstream only returning record entries.
    ///
    /// Skips over `SubBlock` entries, returning `None` on `EndBlock`.
    ///
    /// # Errors
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, ReaderError};
    /// use smallvec::SmallVec;
    ///
    /// fn parse_subblock<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<(), ReaderError> {
    ///     let mut values: SmallVec<[u64; 32]> = SmallVec::new();
    ///     while let Some(record) = bitstream.records()? {
    ///         let _code = bitstream.read_record(record, &mut values)?;
    ///         // Process record.
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub fn records(&mut self) -> Result<Option<Record>, ReaderError> {
        while let Some(entry) = self.advance()? {
            match entry {
                Entry::SubBlock(block) => self.skip_block(block)?,
                Entry::Record(record) => return Ok(Some(record)),
            }
        }
        Ok(None)
    }

    /// Read record
    ///
    /// # Errors
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, ReaderError};
    /// use smallvec::SmallVec;
    ///
    /// fn parse_subblock<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<(), ReaderError> {
    ///     let mut values: SmallVec<[u64; 32]> = SmallVec::new();
    ///     while let Some(record) = bitstream.records()? {
    ///         let _code = bitstream.read_record(record, &mut values)?;
    ///         // Process record.
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub fn read_record<const N: usize>(
        &mut self,
        record: Record,
        values: &mut SmallVec<[u64; N]>,
    ) -> Result<u32, ReaderError> {
        values.clear();
        match record.abbreviation {
            Some(id) => self.read_abbreviated_record(id, values),
            None => self.read_unabbreviated_record(values),
        }
    }

    /// Skip over the current block.
    ///
    /// Requires that a subblock has *just* been entered, as it seeks to the end of the block based
    /// on its length.
    ///
    /// # Errors
    ///
    /// If the block length puts the bitstream over the end of the underlying buffer [`CursorError::InvalidPosition`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
    ///
    /// fn skip_blocks<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<(), ReaderError> {
    ///     while let Some(entry) = bitstream.advance()? {
    ///         match entry {
    ///             Entry::SubBlock(block) => bitstream.skip_block(block)?,
    ///             Entry::Record(_) => {}, // Process record.
    ///         }
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub fn skip_block(&mut self, block: Block) -> Result<(), ReaderError> {
        let seek_to = self.cursor.bit_position() + block.size * 4 * 8;
        self.cursor.set_bit_position(seek_to)?;
        Ok(())
    }

    /// Skips over the current record, returning the skipped code.
    ///
    /// # Errors
    ///
    /// If the passed abbreviation from [`Record`] is invalid, then [`ReaderError::InvalidAbbrevationId`]
    /// is returned.
    ///
    /// For unabbreviated records if the number of elements is implausible then [`ReaderError::InvalidSize`] is returned.
    ///
    /// Skipping a record requires reading fields to know what to skip leading to most [`CursorError`]
    /// being possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
    ///
    /// fn skip_record<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<(), ReaderError> {
    ///     while let Some(entry) = bitstream.advance()? {
    ///         match entry {
    ///             Entry::SubBlock(_) => {}, // Process block.
    ///             Entry::Record(record) => {
    ///                 bitstream.skip_record(record)?;
    ///             },
    ///         }
    ///     }
    ///     Ok(())
    /// }
    /// ```
    pub fn skip_record(&mut self, record: Record) -> Result<u32, ReaderError> {
        let code = if let Some(abbreviation) = record.abbreviation {
            // Abbreviated record.
            let abbrev = self
                .current_block
                .get_abbreviation(abbreviation)
                .ok_or(ReaderError::InvalidAbbrevationId)?;

            let code = match abbrev.code {
                CodeAbbreviation::Literal(value) => value,
                CodeAbbreviation::Fixed(n) => self.cursor.read(n)?,
                CodeAbbreviation::Vbr(n) => self.cursor.read_vbr64(n)?,
                CodeAbbreviation::Char6 => self.cursor.read_char6()?,
            } as u32;

            for operand in abbrev.operands.iter().skip(1).copied() {
                match operand {
                    AbbreviationOperand::Literal(_) => {}
                    AbbreviationOperand::Fixed(n) => {
                        self.cursor.read(n)?;
                    }
                    AbbreviationOperand::Vbr(n) => {
                        self.cursor.read_vbr64(n)?;
                    }
                    AbbreviationOperand::Char6 => {
                        self.cursor.read_char6()?;
                    }
                    AbbreviationOperand::Array(element_encoding) => {
                        // Read the number of elements as `vbr6`.
                        let num_elements = self.cursor.read_vbr(6)? as usize;
                        if !self.is_size_plausible(num_elements) {
                            return Err(ReaderError::InvalidSize);
                        }

                        for _ in 0..num_elements {
                            match element_encoding {
                                ArrayOperand::Zero => {}
                                ArrayOperand::Fixed(n) => {
                                    self.cursor.read(n)?;
                                }
                                ArrayOperand::Vbr(n) => {
                                    self.cursor.read_vbr64(n)?;
                                }
                                ArrayOperand::Char6 => {
                                    self.cursor.read_char6()?;
                                }
                            };
                        }
                    }
                    AbbreviationOperand::Blob => {
                        let num_bytes = self.cursor.read_vbr(6)? as u64;

                        // Find where the end of the blob is, including tail padding.
                        let new_bit_position =
                            self.cursor.bit_position() + align_to(num_bytes, 4) * 8;

                        self.cursor.align_32bits();
                        self.cursor.set_bit_position(new_bit_position)?;
                    }
                }
            }
            code
        } else {
            // Unabbreviated record.
            let code = self.cursor.read_vbr(6)?;
            let num_elements = self.cursor.read_vbr(6)? as usize;
            for _ in 0..num_elements {
                self.cursor.read_vbr(6)?;
            }
            code
        };

        Ok(code)
    }

    /// Parse `BLOCK_INFO` block.
    fn parse_block_info(&mut self) -> Result<(), ReaderError> {
        // Block info is similar to other blocks in the bitstream, the difference is that we handle
        // these in the parser as the contain information how we should parse other records.
        let mut block_id: Option<u32> = None;
        let mut record: SmallVec<[u64; 64]> = SmallVec::new();

        loop {
            // Perform raw advancing as we have to handle *all* things here, the define abbrevations
            // should not be added to our current
            match self.raw_advance()? {
                RawEntry::SubBlock(block) => {
                    // Should not encounter subblocks here, but if we do, skip over those.
                    self.skip_block(block)?;
                }
                RawEntry::EndBlock => break,
                RawEntry::DefineAbbreviation => {
                    // For abbreviations defined in a `BLOCK_INFO` block these are added to the
                    // block id set by the `SET_ID` record.
                    let abbreviation = self.parse_define_abbreviation_record()?;

                    let block_id = block_id.ok_or(ReaderError::InvalidBlockInfo)?;
                    self.block_info_records
                        .entry(block_id)
                        .or_insert_with(|| BlockInfo::new(block_id))
                        .abbreviations
                        .push(Rc::new(abbreviation));
                }
                RawEntry::Record(abbreviation) => {
                    let code = self.read_record(abbreviation, &mut record)?;
                    match BlockInfoCode::try_from(code) {
                        Ok(BlockInfoCode::SetId) => {
                            let id = *record.first().ok_or(ReaderError::InvalidBlockInfo)? as u32;
                            block_id = Some(id);
                        }
                        Ok(BlockInfoCode::Name) => {
                            let name = get_string(&record, 0);

                            let block_id = block_id.ok_or(ReaderError::InvalidBlockInfo)?;
                            self.block_info_records
                                .entry(block_id)
                                .or_insert_with(|| BlockInfo::new(block_id))
                                .name = name;
                        }
                        Ok(BlockInfoCode::SetRecordName) => {
                            let id = *record.first().ok_or(ReaderError::InvalidBlockInfo)? as u32;
                            let record_name = get_string(&record, 1);

                            let block_id = block_id.ok_or(ReaderError::InvalidBlockInfo)?;
                            self.block_info_records
                                .entry(block_id)
                                .or_insert_with(|| BlockInfo::new(block_id))
                                .record_names
                                .push((id, record_name));
                        }
                        _ => {} // Ignore unknown codes.
                    }
                }
            }
        }

        Ok(())
    }

    /// Return the underlying [`BitstreamCursor`].
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
    ///
    /// fn cursor<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) {
    ///     let cursor = bitstream.cursor();
    ///     let _position = cursor.bit_position();
    /// }
    /// ```
    pub fn cursor(&self) -> &BitstreamCursor<T> {
        &self.cursor
    }

    /// Advance at the lowest level.
    ///
    /// This function does not automatically continue over internal details, meaning that the caller
    /// must handle these themselves.
    ///
    /// This includes both
    ///
    /// - Handling the definition of abbrevations which *must* be handled by the same `BitstreamReader`
    /// for a correct parse of the bitstream.
    /// - Handling of block info blocks, which also has abbreviations for other blocks in the bitstream.
    ///
    /// This should only be used when creating additional tooling around how bitstreams actually look,
    /// and should not generally be used.
    ///
    /// Note that it does however handle both leaving and entering blocks and consuming the required
    /// bits necessary to perform those tasks.
    fn raw_advance(&mut self) -> Result<RawEntry, ReaderError> {
        let code = self.cursor.read(self.current_block.code_width)?;
        let entry = match AbbreviationId::from(code) {
            // Parse all `DefineAbbreviation` as we go, the consumer can't do anything with these.
            AbbreviationId::DefineAbbreviation => RawEntry::DefineAbbreviation,
            AbbreviationId::EndBlock => {
                self.leave_block()?;
                RawEntry::EndBlock
            }
            AbbreviationId::EnterSubblock => {
                let id = self.cursor.read_vbr(Self::BLOCK_ID_WIDTH)?;
                let size = self.enter_block(id)?;
                RawEntry::SubBlock(Block { id, size })
            }
            AbbreviationId::Unabbreviated => RawEntry::Record(Record::new(None)),
            AbbreviationId::ApplicationDefined(n) => RawEntry::Record(Record::new(Some(n as u32))),
        };

        Ok(entry)
    }

    /// Handle entering a subblock.
    ///
    /// A subblock is defined by:
    ///   `[ENTER_SUBBLOCK, block_id (vbr8), new_abbreviation_len (vbr4), <align 32-bits>, block_len (32-bits)]`
    ///
    /// and before we enter this function the abbrevation id `ENTER_SUBBLOCK` should have been consumed.
    fn enter_block(&mut self, block_id: u32) -> Result<u64, ReaderError> {
        let new_abbreviation_len = self.cursor.read_vbr(Self::CODE_LEN_WIDTH)?;
        if new_abbreviation_len == 0 || new_abbreviation_len > Self::MAXIMUM_CODE_LEN_CHUNK_SIZE {
            return Err(ReaderError::InvalidCodeSize);
        }

        self.cursor.align_32bits();
        let block_len = self.cursor.read(Self::BLOCK_SIZE_WIDTH)?;

        // Add outer block to the scope while creating a fresh block for the new subblock.
        let previous_block = std::mem::replace(
            &mut self.current_block,
            StreamBlock::from_code_size(new_abbreviation_len),
        );
        self.block_scope.push(previous_block);

        // Add abbreviations specific to this block.
        if let Some(block) = self.block_info_records.get(&block_id) {
            self.current_block
                .abbreviations
                .extend(block.abbreviations.iter().cloned());
        }

        Ok(block_len)
    }

    /// Leave the current block.
    ///
    /// Leaving a block is defined by:
    ///   `[END_BLOCK, <align 32-bits>]`
    ///
    /// Requires that `EndBlock` has *just* been parsed.
    fn leave_block(&mut self) -> Result<(), ReaderError> {
        self.cursor.align_32bits();
        self.current_block = self
            .block_scope
            .pop()
            .ok_or(ReaderError::InvalidLeaveBlock)?;
        Ok(())
    }

    /// Read an unabbreviated record.
    fn read_unabbreviated_record<const N: usize>(
        &mut self,
        values: &mut SmallVec<[u64; N]>,
    ) -> Result<u32, ReaderError> {
        let code = self.cursor.read_vbr(Self::UNABBREVIATED_WIDTH)?;

        let len = self.cursor.read_vbr(Self::UNABBREVIATED_WIDTH)? as usize;
        if !self.is_size_plausible(len) {
            return Err(ReaderError::InvalidSize);
        }
        values.reserve(values.len() + len);

        for _ in 0..len {
            let v = self.cursor.read_vbr64(Self::UNABBREVIATED_WIDTH)?;
            values.push(v);
        }
        tracing::debug!("parsed unabbreviated_record: {values:?}");

        Ok(code)
    }

    /// Read an abbreviated record.
    fn read_abbreviated_record<const N: usize>(
        &mut self,
        abbreviation: u32,
        values: &mut SmallVec<[u64; N]>,
    ) -> Result<u32, ReaderError> {
        let abbrev = self
            .current_block
            .get_abbreviation(abbreviation)
            .ok_or(ReaderError::InvalidAbbrevationId)?;

        let code = match abbrev.code {
            CodeAbbreviation::Literal(value) => value,
            CodeAbbreviation::Fixed(n) => self.cursor.read(n)?,
            CodeAbbreviation::Vbr(n) => self.cursor.read_vbr64(n)?,
            CodeAbbreviation::Char6 => self.cursor.read_char6()?,
        } as u32;

        for operand in abbrev.operands.iter().copied() {
            match operand {
                AbbreviationOperand::Literal(value) => values.push(value),
                AbbreviationOperand::Fixed(n) => values.push(self.cursor.read(n)?),
                AbbreviationOperand::Vbr(n) => values.push(self.cursor.read_vbr64(n)?),
                AbbreviationOperand::Char6 => values.push(self.cursor.read_char6()?),
                AbbreviationOperand::Array(element_encoding) => {
                    // Read the number of elements as `vbr6`.
                    let num_elements = self.cursor.read_vbr(6)? as usize;
                    if !self.is_size_plausible(num_elements) {
                        return Err(ReaderError::InvalidSize);
                    }
                    values.reserve(values.len() + num_elements);

                    for _ in 0..num_elements {
                        let value = match element_encoding {
                            ArrayOperand::Zero => 0,
                            ArrayOperand::Fixed(n) => self.cursor.read(n)?,
                            ArrayOperand::Vbr(n) => self.cursor.read_vbr64(n)?,
                            ArrayOperand::Char6 => self.cursor.read_char6()?,
                        };
                        values.push(value);
                    }
                }
                AbbreviationOperand::Blob => {
                    // TODO: LLVM has a fast-path for blobs.
                    let num_bytes = self.cursor.read_vbr(6)? as usize;

                    self.cursor.align_32bits();
                    for _ in 0..num_bytes {
                        values.push(self.cursor.read(8)?);
                    }
                    self.cursor.align_32bits();
                }
            }
        }
        tracing::debug!("parsed abbreviated_record: {values:?}");

        Ok(code)
    }

    /// Returns `true` if the given size is plausible.
    ///
    /// The passed `size` must be less than the number of bits in the bitstream.
    fn is_size_plausible(&self, size: usize) -> bool {
        (size as u64) < self.cursor.len() * 8
    }

    /// Parse a define abbreviation record.
    ///
    /// Expects the `DEFINE_ABBREV` code to be parsed already.
    fn parse_define_abbreviation_record(&mut self) -> Result<AbbreviationRecord, ReaderError> {
        // The define abbrevation record is defined as:
        //   [DEFINE_ABBREV, num_abbrevation_operands (vbr5), (operand){num_abbrevation_operands}].
        //
        // Where each of the operands are defined as:
        // 1. Literal operands: [1 (1), literal_value (vbr8)].
        // 2. Encoding info without data: [0 (1), encoding (3)].
        // 3. Encoding info with data: [0 (1), encoding (3), value (vbr5)].
        //
        // The possible encodings are according to their `code`:
        // - Fixed (code 1): Field is emitted as a fixed-width value, width in bits is given by the extra data.
        // - VBR   (code 2): Field is emitted as a VBR, width in bits is given by the extra data.
        // - Array (code 3):
        // - Char6 (code 4): Field is emitted as a `char6` encoded value, no extra data.
        // - Blob  (code 5): Field is emitted as a `vbr6`
        let num_operands = self.cursor.read_vbr(5)? as usize;
        if num_operands == 0 {
            return Err(ReaderError::InvalidAbbreviationRecord(
                "no operands for abbreviation",
            ));
        }

        let code = match self.parse_abbreviation_operand()? {
            AbbreviationOperand::Literal(value) => CodeAbbreviation::Literal(value),
            AbbreviationOperand::Fixed(bit_width) => CodeAbbreviation::Fixed(bit_width),
            AbbreviationOperand::Vbr(bit_width) => CodeAbbreviation::Vbr(bit_width),
            AbbreviationOperand::Char6 => CodeAbbreviation::Char6,
            AbbreviationOperand::Array(_) | AbbreviationOperand::Blob => {
                return Err(ReaderError::InvalidAbbreviationRecord(
                    "Unexpected operand encoding for the code",
                ))
            }
        };

        let mut abbreviation = AbbreviationRecord::from_code(code);
        abbreviation.operands.reserve(num_operands - 1);

        for i in 1..num_operands {
            let operand = self.parse_abbreviation_operand()?;

            let is_array = matches!(operand, AbbreviationOperand::Array(_));
            if is_array && i + 2 != num_operands {
                return Err(ReaderError::InvalidAbbreviationRecord(
                    "blob operand must be the last operand",
                ));
            }

            if matches!(operand, AbbreviationOperand::Blob) && i + 1 != num_operands {
                return Err(ReaderError::InvalidAbbreviationRecord(
                    "blob operand must be the last operand",
                ));
            }

            abbreviation.push(operand);

            // The array element must but the second to last element, and we parse the array element
            // eagerly putting it inside the variant. So we have already parsed the last operand at
            // this point.
            if is_array {
                break;
            }
        }
        tracing::debug!("parsed abbreviation: {abbreviation:?}");

        Ok(abbreviation)
    }

    fn parse_abbreviation_operand(&mut self) -> Result<AbbreviationOperand, ReaderError> {
        let is_literal = self.cursor.read(1)? != 0;
        if is_literal {
            let value = self.cursor.read_vbr64(8)?;
            return Ok(AbbreviationOperand::Literal(value));
        }

        let encoding = self.cursor.read(3)?;

        let operand = match OperandEncoding::try_from(encoding)? {
            OperandEncoding::Char6 => AbbreviationOperand::Char6,
            OperandEncoding::Blob => AbbreviationOperand::Blob,
            OperandEncoding::Fixed => {
                let bit_width = self.cursor.read_vbr64(5)?;
                if bit_width == 0 {
                    // Special case zero-sized value, treating these as literal zero.
                    AbbreviationOperand::Literal(0)
                } else {
                    AbbreviationOperand::Fixed(bit_width as u32)
                }
            }
            OperandEncoding::Vbr => {
                let bit_width = self.cursor.read_vbr64(5)?;
                if bit_width == 0 {
                    // Special case zero-sized value, treating these as literal zero.
                    AbbreviationOperand::Literal(0)
                } else {
                    AbbreviationOperand::Vbr(bit_width as u32)
                }
            }
            OperandEncoding::Array => {
                // Instead of having the array elements encoding as an operand after the array
                // we grab it here straight away and force it to be only the operands it's
                // allowed.
                let is_literal = self.cursor.read(1)? != 0;
                if is_literal {
                    return Err(ReaderError::InvalidAbbreviationRecord(
                        "literal encoding in array",
                    ));
                }

                let encoding = self.cursor.read(3)?;
                let element_operand = match OperandEncoding::try_from(encoding)? {
                    OperandEncoding::Fixed => {
                        let bit_width = self.cursor.read_vbr64(5)?;
                        if bit_width == 0 {
                            ArrayOperand::Zero
                        } else {
                            ArrayOperand::Fixed(bit_width as u32)
                        }
                    }
                    OperandEncoding::Vbr => {
                        let bit_width = self.cursor.read_vbr64(5)?;
                        if bit_width == 0 {
                            ArrayOperand::Zero
                        } else {
                            ArrayOperand::Vbr(bit_width as u32)
                        }
                    }
                    OperandEncoding::Char6 => ArrayOperand::Char6,
                    OperandEncoding::Array => {
                        return Err(ReaderError::InvalidAbbreviationRecord(
                            "array encoding in array",
                        ))
                    }
                    OperandEncoding::Blob => {
                        return Err(ReaderError::InvalidAbbreviationRecord(
                            "blob encoding in array",
                        ))
                    }
                };
                AbbreviationOperand::Array(element_operand)
            }
        };

        Ok(operand)
    }
}

fn align_to(value: u64, align: u64) -> u64 {
    (value + align - 1) / align * align
}

fn get_string(values: &SmallVec<[u64; 64]>, start_idx: usize) -> String {
    let mut s = String::with_capacity(values.len());

    for ch in values.iter().copied().skip(start_idx) {
        s.push(ch as u8 as char);
    }

    s
}
