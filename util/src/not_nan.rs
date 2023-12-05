use serde::{Deserialize, Serialize};

macro_rules! not_nan {
    ($vis:vis $name:ident($wrapping:ident)) => {
        #[derive(Clone, Copy, Default, Serialize, Deserialize)]
        $vis struct $name(pub $wrapping);

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                assert!(!self.0.is_nan() && !other.0.is_nan(), "NaN encountered");
                self.0 == other.0
            }
        }

        impl Eq for $name {}

        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.to_ne_bytes().hash(state);
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }

        impl std::str::FromStr for $name {
            type Err = std::num::ParseFloatError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(Self(s.parse()?))
            }
        }

    };
}

not_nan!(pub NotNanF32(f32));
not_nan!(pub NotNanF64(f64));
