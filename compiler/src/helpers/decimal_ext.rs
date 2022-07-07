use rust_decimal::prelude::*;

pub trait DecimalExt
where
    Self: Sized,
{
    fn to_i64_exact(self) -> Option<i64>;
    fn to_u64_exact(self) -> Option<u64>;
}

impl DecimalExt for Decimal {
    fn to_i64_exact(self) -> Option<i64> {
        (self.trunc() == self).then(|| self.to_i64()).flatten()
    }

    fn to_u64_exact(self) -> Option<u64> {
        (self.trunc() == self).then(|| self.to_u64()).flatten()
    }
}
