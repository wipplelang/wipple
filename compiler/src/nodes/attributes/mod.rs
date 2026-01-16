mod attribute;
mod attribute_value;

pub use attribute::*;
pub use attribute_value::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_named_attribute() {
        test_parse("parse_named_attribute", parse_attribute, r#"[foo]"#);
    }

    #[test]
    fn test_parse_valued_attribute() {
        test_parse("parse_valued_attribute", parse_attribute, r#"[a : "b"]"#);
    }
}
