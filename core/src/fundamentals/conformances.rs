use crate::*;
use std::rc::Rc;

pub type Conformances = Vec<Conformance>;

fundamental_env_key!(pub conformances for Conformances {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Conformances, new| {
        parent.clone().into_iter().chain(new.clone()).collect()
    })),
});

#[derive(Clone)]
pub struct Conformance {
    pub derived_trait_id: ID,

    #[allow(clippy::type_complexity)]
    pub derive_trait_value: Rc<dyn Fn(&Value, &EnvironmentRef, Stack) -> Result<Option<Value>>>,
}

impl Environment {
    pub fn add_conformance(
        &mut self,
        derived_trait_id: ID,
        derive_trait_value: impl Fn(&Value, &EnvironmentRef, Stack) -> Result<Option<Value>> + 'static,
    ) {
        self.conformances().push(Conformance {
            derived_trait_id,
            derive_trait_value: Rc::new(derive_trait_value),
        })
    }

    pub fn add_primitive_conformance<A: Primitive, B: Primitive>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        self.add_conformance(ID::of::<B>(), move |value, env, stack| {
            let a = match value.get_primitive_if_present::<A>(env, stack)? {
                Some(primitive) => primitive,
                None => return Ok(None),
            };

            let b = derive_trait_value(a);

            Ok(Some(Value::of(b)))
        });
    }

    pub fn add_text_conformance(&mut self, id: ID, value_name: &'static str) {
        self.add_conformance(ID::text(), move |value, env, stack| {
            if !value.has_trait(id, env, stack.clone())? {
                return Ok(None);
            }

            let named = value.get_primitive_if_present::<Named>(env, stack)?;

            Ok(Some(Value::of(Text::new(&match named {
                Some(named) => format!("<{} '{}'>", value_name, named.name),
                None => format!("<{}>", value_name),
            }))))
        });
    }
}
