use crate::*;

stored_closure!(pub for<'a> struct Pattern(&'a Value, &Env, &Stack) -> Result<Option<Cow<'a, Value>>>);

impl Primitive for Pattern {}

impl Pattern {
    pub fn any() -> Self {
        Pattern::new(|value, _, _| Ok(Some(Cow::Borrowed(value))))
    }

    pub fn for_trait(r#trait: Trait) -> Self {
        Pattern::new(move |value, env, stack| value.get_trait_if_present(&r#trait, env, stack))
    }

    pub fn is(pattern: Pattern) -> Self {
        Pattern::new(move |value, env, stack| {
            pattern(value, env, stack)?;
            Ok(Some(Cow::Borrowed(value)))
        })
    }
}
