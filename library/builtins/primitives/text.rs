use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Text {
    pub text: String,
    pub location: Option<SourceLocation>,
}

impl Primitive for Text {}

impl Text {
    pub fn new(text: String) -> Self {
        Text::new_located(text, None)
    }

    pub fn new_located(text: String, location: Option<SourceLocation>) -> Self {
        Text { text, location }
    }
}

#[ext(pub, name = EnvTextRelationExt)]
impl Env {
    fn add_text_relation<T: Primitive>(
        &self,
        value_name: &'static str,
        stack: &Stack,
    ) -> Result<()> {
        self.add_relation_between(stack, move |_: T| Text::new(format!("<{}>", value_name)))
    }
}

#[ext(pub, name = ValueFormatExt)]
impl Value {
    fn format_with_fallback(&self, env: &Env, stack: &Stack) -> Cow<str> {
        let mut stack = stack.clone();
        stack.evaluation_mut().disable_recording();

        match self.get_if_present::<Text>(env, &stack) {
            Ok(Some(text)) => match text {
                Cow::Owned(text) => Cow::Owned(text.text),
                Cow::Borrowed(text) => Cow::Borrowed(&text.text),
            },
            Ok(None) => "<value>".into(),
            Err(_) => "<error retrieving text>".into(),
        }
    }

    fn format(&self, env: &Env, stack: &Stack) -> Result<Cow<String>> {
        let mut stack = stack.clone();
        stack.evaluation_mut().disable_recording();

        Ok(self
            .get_if_present::<Text>(env, &stack)?
            .map(|text| match text {
                Cow::Owned(text) => Cow::Owned(text.text),
                Cow::Borrowed(text) => Cow::Borrowed(&text.text),
            })
            .unwrap_or_else(|| Cow::Owned(String::from("<value>"))))
    }
}

#[allow(clippy::unnecessary_wraps)]
pub(crate) fn setup(env: &Env, _stack: &Stack) -> Result<()> {
    // Text : trait
    env.set_variable("Text", Value::of(Trait::of::<Text>()));

    Ok(())
}
