use core::ops::Deref;
use std::ops::DerefMut;

use smallvec::SmallVec;

#[derive(Debug, thiserror::Error, PartialEq, Eq, PartialOrd, Ord)]
pub enum RecordError {
    /// Record did not have all the required fields.
    #[error("record does not have enough fields")]
    IncompleteRecord,
}

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
    /// Returns [`RecordError::IncompleteRecord`] if `size` is less than the length of the fields.
    pub fn require_min_size(&self, size: usize) -> Result<(), RecordError> {
        if self.record.len() < size {
            Err(RecordError::IncompleteRecord)
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

    pub fn next(&mut self) -> Result<u64, RecordError> {
        self.index += 1;
        self.record
            .get(self.index - 1)
            .copied()
            .ok_or(RecordError::IncompleteRecord)
    }

    pub fn maybe_next(&mut self) -> Option<u64> {
        self.index += 1;
        self.record.get(self.index - 1).copied()
    }

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

        while let Some(value) = self.maybe_next() {
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
