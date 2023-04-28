use lazy_static::lazy_static;
use regex::Regex;

pub fn math(name: &str) -> Option<(&str, &str, &str)> {
    lazy_static! {
        static ref MATH: Regex =
            Regex::new(r#"^([0-9](?:\.[0-9])?)([+\-*/])([0-9](?:\.[0-9])?)$"#).unwrap();
    }

    let captures = MATH.captures(name)?;

    Some((
        captures.get(1).unwrap().as_str(),
        captures.get(2).unwrap().as_str(),
        captures.get(3).unwrap().as_str(),
    ))
}

pub fn trailing_colon(name: &str) -> Option<&str> {
    lazy_static! {
        static ref TRAILING_COLON: Regex = Regex::new(r#"^(.*):$"#).unwrap();
    }

    let captures = TRAILING_COLON.captures(name)?;

    Some(captures.get(1).unwrap().as_str())
}

pub fn comment(name: &str) -> Option<()> {
    (name == "//" || name == "#" || name == "/*" || name == "*/").then_some(())
}
