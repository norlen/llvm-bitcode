use num_bigint::BigUint;

/// Arbitrary precision integer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct APInt {
    bit_width: u32,
    value: BigUint,
}

impl APInt {
    pub fn from_u64(bit_width: u32, value: u64) -> Self {
        Self {
            bit_width,
            value: BigUint::from(value),
        }
    }

    pub fn from_arr(_bit_width: u32, _values: &[u64]) -> Self {
        todo!()
    }

    pub fn bit_width(&self) -> u32 {
        self.bit_width
    }

    pub fn value(&self) -> &BigUint {
        &self.value
    }
}
