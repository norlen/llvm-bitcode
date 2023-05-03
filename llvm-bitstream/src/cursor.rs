use byteorder::{ByteOrder, LittleEndian};

/// Errors when performing operations using a [`BitstreamCursor`].
#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CursorError {
    /// End-of-file enountered, often when trying to read more bits than is available in the
    /// underlying bitstream.
    #[error("Unexpected end of file")]
    UnexpectedEOF,

    /// Invalid position in the bitstream.
    #[error("Invalid position")]
    InvalidPosition,

    /// Zero or more than 32/64-bits not supported for the given operation.
    #[error("Cannot read more than machine word number of bits")]
    InvalidNumberOfBits,

    /// Invalid `char6` encoding.
    #[error("Invalid char6 encoding")]
    InvalidChar6,

    /// The VBR did not terminate in 32/64-bits.
    #[error("Unterminated VBR")]
    UnterminatedVBR,
}

/// A `BitstreamCursor` wraps an in-memory buffer containing LLVM bitcode.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BitstreamCursor<T: AsRef<[u8]>> {
    /// Underlying buffer.
    inner: T,

    /// Next offset to bytes to read after `value` have been exhausted.
    next_pos: u64,

    /// Current value that we're reading bits from.
    value: u64,

    /// Number of bits we've read into `value`.
    value_bits: u32,
}

impl<T: AsRef<[u8]>> BitstreamCursor<T> {
    const BITS_IN_BYTE: u64 = 8;

    /// Number of bytes in an [`u64`].
    const U64_BYTES: u64 = core::mem::size_of::<u64>() as u64;

    /// Number of bits in an [`u64`].
    const U64_BITS: u64 = Self::U64_BYTES * 8;

    /// Mask for getting the byte offset to the u64 value from a bit position.
    const BIT_POSITION_BYTE_OFFSET: u64 = !(Self::U64_BYTES - 1);

    /// Mask for getting the bit offset to the u64 value from a bit position.
    const BIT_POSITION_BIT_OFFSET: u64 = (Self::U64_BYTES * Self::BITS_IN_BYTE - 1);

    // LLVM uses this mask when performing a shift later on. Check out the bit pattern to see
    // whats actually happening here.
    const SOME_MASK: u32 = if Self::U64_BYTES > 4 { 0x3f } else { 0x1f };

    /// Possible `char6` encodings.
    const CHAR6: &[u8] =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._".as_bytes();

    /// Creates a new cursor wrapping the provided underlying in-memory buffer.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let cursor = BitstreamCursor::new(Vec::new());
    /// ```
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            next_pos: 0,
            value: 0,
            value_bits: 0,
        }
    }

    /// Returns the current bit we are reading.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    ///
    /// assert_eq!(cursor.bit_position(), 0);
    ///
    /// cursor.set_bit_position(2).unwrap();
    /// assert_eq!(cursor.bit_position(), 2);
    ///
    /// cursor.set_byte_position(4);
    /// assert_eq!(cursor.bit_position(), 32);
    /// ```
    pub fn bit_position(&self) -> u64 {
        self.next_pos * Self::BITS_IN_BYTE - self.value_bits as u64
    }

    /// Sets the position of this cursor in bits.
    ///
    /// A position beyond the end of the underlying bistream is **not** allowed here.
    ///
    /// # Errors
    ///
    /// Setting a bit position beyond the end of the underlying bitstream returns [`CursorError::InvalidPosition`].
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    ///
    /// assert_eq!(cursor.byte_position(), 0);
    ///
    /// cursor.set_bit_position(2).unwrap();
    /// assert_eq!(cursor.byte_position(), 0);
    ///
    /// cursor.set_bit_position(63).unwrap();
    /// assert_eq!(cursor.byte_position(), 7);
    ///
    /// cursor.set_bit_position(65).unwrap();
    /// assert_eq!(cursor.byte_position(), 8);
    /// ```
    pub fn set_bit_position(&mut self, bit_pos: u64) -> Result<(), CursorError> {
        let pos = (bit_pos / Self::BITS_IN_BYTE) & Self::BIT_POSITION_BYTE_OFFSET;
        let bit_offset = bit_pos & Self::BIT_POSITION_BIT_OFFSET;

        if pos >= self.len() {
            return Err(CursorError::InvalidPosition);
        }

        self.set_byte_position(pos);
        if bit_offset > 0 {
            self.read(bit_offset as u32).map(|_| ())
        } else {
            Ok(())
        }
    }

    /// Returns the current position of this cursor.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    /// use std::io::SeekFrom;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);
    ///
    /// assert_eq!(cursor.byte_position(), 0);
    ///
    /// cursor.set_bit_position(17);
    /// assert_eq!(cursor.byte_position(), 2);
    /// ```
    pub fn byte_position(&self) -> u64 {
        self.bit_position() / Self::BITS_IN_BYTE
    }

    /// Sets the position of this cursor and resetting the current bit offset to `0`.
    ///
    /// A position beyond the end of the underlying bistream is allowed.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);
    ///
    /// assert_eq!(cursor.byte_position(), 0);
    ///
    /// cursor.set_byte_position(2);
    /// assert_eq!(cursor.byte_position(), 2);
    /// ```
    pub fn set_byte_position(&mut self, pos: u64) {
        self.next_pos = pos;
        self.value = 0;
        self.value_bits = 0;
    }

    /// Returns `true` if all bits in bitstream have been exhausted.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);
    ///
    /// cursor.set_byte_position(2);
    /// assert!(!cursor.is_empty());
    ///
    /// cursor.set_byte_position(5);
    /// assert!(cursor.is_empty());
    ///
    /// cursor.set_byte_position(10);
    /// assert!(cursor.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.value_bits == 0 && self.next_pos >= self.len()
    }

    /// Returns the length of the underlying in-memory buffer in bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);
    /// assert_eq!(cursor.len(), 5);
    /// ```
    pub fn len(&self) -> u64 {
        self.inner.as_ref().len() as u64
    }

    /// Read `num_bits` from the underlying bitstream.
    ///
    /// When reading the assumption is that the underlying bitstream is encoded in little-endian.
    ///
    /// # Errors
    ///
    /// If the number of bits are greater than the number of bits in an [`u64`] then [`CursorError::InvalidNumberOfBits`]
    /// is returned.
    ///
    /// If trying to read more bits than are left in the bitstream then [`CursorError::UnexpectedEOF`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);
    ///
    /// assert_eq!(cursor.read(8), Ok(1));
    /// assert_eq!(cursor.read(4), Ok(2));
    /// assert_eq!(cursor.read(4), Ok(0));
    /// assert_eq!(cursor.read(16), Ok((4 << 8) | 3));
    /// ```
    pub fn read(&mut self, num_bits: u32) -> Result<u64, CursorError> {
        if num_bits == 0 || num_bits > Self::U64_BITS as u32 {
            return Err(CursorError::InvalidNumberOfBits);
        }

        let (value, bits_used_in_value) = if self.value_bits >= num_bits {
            // If all bits are contained in the current word we can just grab those.

            let value_mask = !0 >> (Self::U64_BITS - num_bits as u64);
            let value = self.value & value_mask;

            (value, num_bits)
        } else {
            let bits_remaining = num_bits - self.value_bits;
            let value0 = if self.value_bits > 0 { self.value } else { 0 };

            self.fill_current_word()?;
            if bits_remaining > self.value_bits {
                return Err(CursorError::UnexpectedEOF);
            }

            let value_mask = !0 >> (Self::U64_BITS - bits_remaining as u64);
            let value1 = self.value & value_mask;

            let value = value0 | (value1 << (num_bits - bits_remaining));

            (value, bits_remaining)
        };

        // Use mask to prevent undefined behavior.
        self.value >>= bits_used_in_value & Self::SOME_MASK;
        self.value_bits -= bits_used_in_value;

        Ok(value)
    }

    /// Read a `VBR` (variable width integer).
    ///
    /// A `VBR` can be encoded into multiple "chunks", where each chunk has the most-significant bit
    /// (MSB) set. If we read two bits each and each read yields `0b10`, `0b10`, `0b10`, and `0b01`
    /// this gives 4 chunks with the last chunk terminating the sequence. The MSB is stripped when
    /// the value is re-created giving the final integer `0b1000 = 8`.
    ///
    /// # Errors
    ///
    /// Number if bits must be greater than `0` and less than `32` otherwise the error [`CursorError::InvalidNumberOfBits`]
    /// is returned.
    ///
    /// If it cannot read a `VBR` (all chunks included) in less than 32-bits [`CursorError::UnterminatedVBR`] is returned.
    ///
    /// If trying to read more bits than are left in the bitstream then [`CursorError::UnexpectedEOF`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![0b0010_0001]);
    ///
    /// assert_eq!(cursor.read_vbr(4).unwrap(), 1);
    /// assert_eq!(cursor.read_vbr(4).unwrap(), 2);
    /// ```
    ///
    /// The VBR encoding can also continue in multiple chunks, if the chunks has the MSB set.
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// // 4 chunks of 2 bits each with the MSB stripped gives 0b1000
    /// let mut cursor = BitstreamCursor::new(vec![0b01_10_10_10]);
    ///
    /// assert_eq!(cursor.read_vbr(2).unwrap(), 0b1000);
    /// ```
    pub fn read_vbr(&mut self, num_bits: u32) -> Result<u32, CursorError> {
        if num_bits == 0 || num_bits >= 32 {
            return Err(CursorError::InvalidNumberOfBits);
        }

        // If the most-significant bit is set, the VBR encoding continues in more chunks of
        // `num_bits` bits.
        let msb_mask = 1 << (num_bits - 1);

        let value = self.read(num_bits)? as u32;
        if value & msb_mask == 0 {
            return Ok(value);
        }

        let mut value = value;
        let mut result = 0;
        let mut next_bit = 0;
        loop {
            result |= (value & (msb_mask - 1)) << next_bit;
            if value & msb_mask == 0 {
                return Ok(result);
            }

            next_bit += num_bits - 1;
            if next_bit >= 32 {
                return Err(CursorError::UnterminatedVBR);
            }

            value = self.read(num_bits)? as u32;
        }
    }

    /// Read a `VBR` (variable width integer).
    ///
    /// The difference compared to `read_vbr` is that the value in multiple chunks be up to 64-bits.
    ///
    /// # Errors
    ///
    /// Number if bits must be greater than `0` and less than `32` otherwise the error [`CursorError::InvalidNumberOfBits`]
    /// is returned.
    ///
    /// If it cannot read a `VBR` (all chunks included) in less than 64-bits [`CursorError::UnterminatedVBR`] is returned.
    ///
    /// If trying to read more bits than are left in the bitstream then [`CursorError::UnexpectedEOF`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![0b0010_0001]);
    ///
    /// assert_eq!(cursor.read_vbr64(4).unwrap(), 1);
    /// assert_eq!(cursor.read_vbr64(4).unwrap(), 2);
    /// ```
    pub fn read_vbr64(&mut self, num_bits: u32) -> Result<u64, CursorError> {
        if num_bits == 0 || num_bits >= 32 {
            return Err(CursorError::InvalidNumberOfBits);
        }

        let value = self.read(num_bits)?;

        // VBRs are terminated if the most significant bit is set. Otherwise, the the final value
        // can span multiple VBR encodings.
        let mask = 1 << (num_bits - 1);

        if value & mask == 0 {
            return Ok(value);
        }

        let mut value = value;
        let mut result = 0;
        let mut next_bit = 0;
        loop {
            result |= (value & (mask - 1)) << next_bit;
            if value & mask == 0 {
                return Ok(result);
            }

            next_bit += num_bits - 1;
            if next_bit >= 64 {
                return Err(CursorError::UnterminatedVBR);
            }

            value = self.read(num_bits)?;
        }
    }

    /// Read a `char6` value.
    ///
    /// `char6` is an encoded value into a 6-bit field and represent the characters `[a-zA-Z0-9._]`.
    ///
    /// # Errors
    ///
    /// If the read value cannot be represented in the `char6` encoding [`CursorError::InvalidChar6`]
    /// is returned.
    ///
    /// If trying to read more bits than are left in the bitstream then [`CursorError::UnexpectedEOF`]
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// let mut cursor = BitstreamCursor::new(vec![0b0000_0010]);
    ///
    /// assert_eq!(cursor.read_char6(), Ok('c' as u64));
    /// ```
    pub fn read_char6(&mut self) -> Result<u64, CursorError> {
        let value = self.read(6)?;
        Self::CHAR6
            .get(value as usize)
            .map(|value| *value as u64)
            .ok_or(CursorError::InvalidChar6)
    }

    /// Align the position to the next 32-bit boundary.
    ///
    /// # Examples
    ///
    /// ```
    /// use llvm_bitstream::BitstreamCursor;
    ///
    /// // Cursor contain 9 * 7 = 72 bits.
    /// let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    ///
    /// cursor.align_32bits();
    /// assert_eq!(cursor.bit_position(), 0);
    ///
    /// cursor.read(1).unwrap();
    /// cursor.align_32bits(); // Move to the 32-bit boundary.
    /// assert_eq!(cursor.bit_position(), 32);
    ///
    /// cursor.set_byte_position(2);
    /// cursor.align_32bits(); // Move to the next 32-bit boundary.
    /// assert_eq!(cursor.bit_position(), 32);
    ///
    /// cursor.read(32).unwrap();
    /// cursor.align_32bits(); // Should be at bit 32 + 32 = 64, and we are aligned.
    /// assert_eq!(cursor.bit_position(), 64);
    ///
    /// cursor.read(1).unwrap();
    /// cursor.align_32bits();
    /// assert_eq!(cursor.bit_position(), 96);
    /// ```
    pub fn align_32bits(&mut self) {
        let next_byte_pos = ((self.bit_position() + 31) / 8) & !0x3;

        // If our next byte position is the same as our current, but we just have read less than
        // 32-bits currently, we just throw these away and move up the bit position.
        if next_byte_pos == self.next_pos && self.value_bits >= 32 {
            self.value >>= self.value_bits - 32;
            self.value_bits = 32;
        } else {
            self.set_byte_position(next_byte_pos);
        }
    }

    /// This is how it's done in LLVM, but this does not really align to 32-bits...
    #[allow(unused)]
    fn skip_to_32bit_boundary(&mut self) {
        if self.value_bits >= 32 {
            // If we have read less than 32-bits, we throw those away and just move up the bit position.
            self.value >>= self.value_bits - 32;
            self.value_bits = 32;
        } else {
            // If we have read more than 32-bits reset the position and it will update to the boundary
            // on the next read.
            self.value_bits = 0;
        }
    }

    /// Fill the current `value` with up to 64-bits and advance our next position.
    fn fill_current_word(&mut self) -> Result<(), CursorError> {
        if self.next_pos >= self.len() {
            return Err(CursorError::UnexpectedEOF);
        }

        let remainder = &self.inner.as_ref()[self.next_pos as usize..];
        let remains = remainder.len() as u64;

        let (value, bytes_read) = if remains > Self::U64_BYTES {
            // If we can fit an entire u64 with data from the stream.
            (LittleEndian::read_u64(remainder), Self::U64_BYTES)
        } else {
            // Otherwise, read the bytes that are left.
            let value = LittleEndian::read_uint(remainder, remains as usize);
            (value, remains)
        };

        self.next_pos += bytes_read;
        self.value = value;
        self.value_bits = bytes_read as u32 * 8;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_byte_position() {
        let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5]);

        assert_eq!(cursor.byte_position(), 0);

        cursor.set_byte_position(2);
        assert_eq!(cursor.byte_position(), 2);

        cursor.set_byte_position(4);
        assert_eq!(cursor.byte_position(), 4);
    }

    #[test]
    fn set_position_in_bits() {
        let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);

        assert_eq!(cursor.byte_position(), 0);

        cursor.set_bit_position(2).unwrap();
        assert_eq!(cursor.byte_position(), 0);

        cursor.set_bit_position(64).unwrap();
        assert_eq!(cursor.byte_position(), 8);
    }

    #[test]
    fn read() {
        let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

        assert_eq!(cursor.read(8), Ok(1));
        assert_eq!(cursor.read(4), Ok(2));
        assert_eq!(cursor.read(4), Ok(0));
        assert_eq!(cursor.read(16), Ok((4 << 8) | 3));
        assert_eq!(cursor.read(8), Ok(5));
        assert_eq!(cursor.read(8), Ok(6));
        assert_eq!(cursor.read(8), Ok(7));
        assert_eq!(cursor.read(8), Ok(8));
        assert_eq!(cursor.read(8), Ok(9));
        assert!(!cursor.is_empty());
        assert_eq!(cursor.read(8), Ok(10));
        assert!(cursor.is_empty());
    }

    #[test]
    fn read_vbr() {
        let mut cursor = BitstreamCursor::new(vec![0b0000_0001]);

        assert_eq!(cursor.read_vbr(4).unwrap(), 1);
    }

    #[test]
    fn align_32_bits() {
        let mut cursor = BitstreamCursor::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

        cursor.align_32bits();
        assert_eq!(cursor.bit_position(), 0);

        cursor.read(1).unwrap();
        cursor.align_32bits();
        assert_eq!(cursor.bit_position(), 32);

        cursor.set_byte_position(5);
        cursor.align_32bits();
        assert_eq!(cursor.bit_position(), 64);

        cursor.read(1).unwrap();
        cursor.align_32bits();
        assert_eq!(cursor.bit_position(), 96);
    }
}
