pub(super) mod fields;
pub(super) mod types;
pub(super) mod value;

/// Parse an encoded alignment value.
///
/// Alignment in bitode is encoded. Where `0` represent the default alignment, otherwise it contains
/// an exponent. To get the true exponent the value has to be subtracted by `1`.
pub fn parse_alignment(align: u64) -> Option<u64> {
    const MAX_ALIGNMENT_EXPONENT: u64 = 32;

    // Since `align` uses zero as a default value, to get the real exponent we have to subtract 1.
    // For the error case we thus have to add 1 instead, since we haven't decoded it yet.
    match align {
        0 => None,
        n if n > MAX_ALIGNMENT_EXPONENT + 1 => panic!("invalid alignment value"),
        n => Some(1 << (n - 1)),
    }
}
