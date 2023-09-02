use core::ops::Deref;
use std::ops::DerefMut;

use smallvec::SmallVec;

#[derive(Debug, thiserror::Error, PartialEq, Eq, PartialOrd, Ord)]
pub enum RecordError {
    /// Record did not have all the required fields.
    #[error("record does not have enough fields")]
    IncompleteRecord,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IncompleteRecordError;

impl std::error::Error for IncompleteRecordError {}

impl std::fmt::Display for IncompleteRecordError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("record does not have enough fields")
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct Fields<const N: usize>(SmallVec<[u64; N]>);

impl<const N: usize> Deref for Fields<N> {
    type Target = SmallVec<[u64; N]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const N: usize> DerefMut for Fields<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const N: usize> Fields<N> {
    pub fn new() -> Self {
        Self(SmallVec::new())
    }

    /// Returns a reference to an element as an `u32`.
    ///
    /// - Returns a reference to the element at the index or `None` if out of bounds.
    /// - Returns the value as `u32` instead of `u64` or `None` if the value cannot be converted
    ///   to a `u32`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use llvm_bitcode::Fields;
    ///
    /// let mut v = Fields::<32>::new();
    /// v.push(1);
    /// assert_eq!(Some(&1), v.get(0));
    /// assert_eq!(None, v.get(1));
    /// ```
    pub fn get_u32(&self, index: usize) -> Option<u32> {
        self.get(index)
            .copied()
            .and_then(|value| value.try_into().ok())
    }

    pub fn to_string(&self, index: usize) -> Option<String> {
        if index >= self.len() {
            return None;
        }

        let len = self.len() - index;
        let mut s = String::with_capacity(len);

        for value in self.0.iter().skip(index).copied() {
            let byte = match u8::try_from(value) {
                Ok(v) => Some(v),
                Err(_) => None,
            }?;
            s.push(byte as char);
        }

        Some(s)
    }

    pub fn to_blob(&self, index: usize) -> Option<Vec<u8>> {
        if index >= self.len() {
            return None;
        }

        let len = self.len() - index;
        let mut s = Vec::with_capacity(len);
        for ch in self.0.iter().skip(index).copied() {
            let byte = match u8::try_from(ch) {
                Ok(v) => Some(v),
                Err(_) => None,
            }?;

            s.push(byte);
        }

        Some(s)
    }
}

// ---------------------------------------------

pub struct FieldsIter<'a, const N: usize> {
    record: &'a Fields<N>,
    index: usize,
}

impl<'a, const N: usize> Iterator for FieldsIter<'a, N> {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        self.record.get(self.index - 1).copied()
    }
}

impl<'a, const N: usize> From<&'a Fields<N>> for FieldsIter<'a, N> {
    fn from(value: &'a Fields<N>) -> Self {
        Self {
            record: value,
            index: 0,
        }
    }
}

impl<'a, const N: usize> FieldsIter<'a, N> {
    /// Require that the record contain `size` fields.
    ///
    /// # Errors
    ///
    /// Returns [`IncompleteRecordError`] if `size` is less than the length of the fields.
    pub fn require_min_size(&self, size: usize) -> Result<(), IncompleteRecordError> {
        if self.record.len() < size {
            Err(IncompleteRecordError)
        } else {
            Ok(())
        }
    }

    pub fn len(&self) -> usize {
        self.record.len()
    }

    pub fn is_empty(&self) -> bool {
        self.record.is_empty()
    }

    pub fn next_or_err(&mut self) -> Result<u64, IncompleteRecordError> {
        self.index += 1;
        self.record
            .get(self.index - 1)
            .copied()
            .ok_or(IncompleteRecordError)
    }

    // pub fn maybe_next(&mut self) -> Option<u64> {
    //     self.index += 1;
    //     self.record.get(self.index - 1).copied()
    // }

    // pub fn unpack<const N: usize>(&mut self) -> Result<[u64; N], ReaderError> {
    //     if self.record.fields().len() < N {
    //         Err(ReaderError::IncompleteRecord)
    //     } else {
    //         let start = self.index;
    //         let end = start + N;
    //         let Fields = self.record.fields()[start..end].try_into().unwrap();
    //         self.index += N;
    //         Ok(Fields)
    //     }
    // }

    /// Try and parse the remaining record as a [`String`].
    pub fn string(&mut self) -> Option<String> {
        if self.index >= self.len() {
            return None;
        }

        let len = self.len() - self.index;
        let mut s = String::with_capacity(len);

        for value in self.by_ref() {
            let byte = match u8::try_from(value) {
                Ok(v) => Some(v),
                Err(_) => None,
            }?;
            s.push(byte as char);
        }

        Some(s)
    }

    pub fn remaining(&mut self) -> &[u64] {
        &self.record.0[self.index..]
    }
}
