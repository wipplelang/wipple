use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Relation {
    pub pattern: Pattern,
    pub derived_trait: Trait,
    pub derive_value: Rc<dyn Fn(Value, &Environment, &Stack) -> Result>,
}

impl Relation {
    pub fn new(
        pattern: Pattern,
        derived_trait: Trait,
        derive_value: impl Fn(Value, &Environment, &Stack) -> Result + 'static,
    ) -> Self {
        Relation {
            pattern,
            derived_trait,
            derive_value: Rc::new(derive_value),
        }
    }
}

impl std::fmt::Debug for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "(Relation {:?} == {:?})",
            self.pattern, self.derived_trait
        )
    }
}

#[derive(TypeInfo, Debug, Clone, Default)]
pub struct Relations(pub Vec<Relation>);

core_env_key!(pub relations for Relations {
    visibility: EnvironmentVisibility::Public(UseFn::from(|parent: &Relations, new| {
        Relations(parent.0.clone().into_iter().chain(new.0.clone()).collect())
    })),
});

impl EnvironmentInner {
    pub fn add_relation(
        &mut self,
        pattern: Pattern,
        derived_trait: Trait,
        derive_value: impl Fn(Value, &Environment, &Stack) -> Result + 'static,
    ) {
        // New relations always have a lower precedence than older relations
        self.relations()
            .0
            .push(Relation::new(pattern, derived_trait, derive_value))
    }

    pub fn add_primitive_relation<A: TypeInfo, B: TypeInfo>(
        &mut self,
        derive_trait_value: impl Fn(A) -> B + 'static,
    ) {
        self.add_relation(
            Pattern::for_trait(Trait::of::<A>()),
            Trait::of::<B>(),
            move |value, _, _| {
                let a = value.into_primitive::<A>().unwrap();
                let b = derive_trait_value(a);
                Ok(Value::of(b))
            },
        );
    }

    pub fn add_text_relation<T: TypeInfo>(&mut self, value_name: &'static str) {
        self.add_primitive_relation(move |_: T| Text::new(&format!("<{}>", value_name)));
    }
}
