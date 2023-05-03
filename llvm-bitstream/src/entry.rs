/// Entries found in a bitstream.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Entry {
    /// Start of a new sub-block with a specific id.
    SubBlock(Block),

    /// Record with an optional abbreviation id if specified.
    Record(Record),
}

/// Block in the bitstream with an `id`.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Block {
    /// Id for the block.
    pub id: u32,
}

/// Record that contain an optional abbreviation.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Record {
    /// Abbreviation id, `None` if the record is unabbreviated.
    pub abbreviation: Option<u32>,
}

impl Record {
    /// Create a new record with an optional abbreviation.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::Record;
    ///
    /// let record = Record::new(Some(1));
    /// let unabbreviated_record = Record::new(None);
    /// ```
    pub fn new(abbreviation: Option<u32>) -> Self {
        Self { abbreviation }
    }
}
