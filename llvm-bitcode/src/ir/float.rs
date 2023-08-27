#![allow(unused)]

/// Arbitrary precision floating point value.
///
/// NOTE: Currently WIP, all values are f64 now.
#[derive(Clone, Debug, PartialEq)]
pub struct APFloat {
    precision: u32,
    value: f64,
}

impl APFloat {
    pub fn from_f64(precision: u32, value: f64) -> Self {
        Self { precision, value }
    }

    pub fn precision(&self) -> u32 {
        self.precision
    }

    pub fn value(&self) -> f64 {
        self.value
    }
}
