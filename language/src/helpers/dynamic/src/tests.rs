use crate as dynamic;
use dynamic::*;

#[derive(TypeInfo, Debug, Clone, PartialEq, Eq)]
struct A(String);

#[derive(TypeInfo, Debug, Clone, PartialEq, Eq)]
struct B;

#[derive(TypeInfo, Debug, Clone, PartialEq, Eq)]
struct C<T>(T);

#[derive(TypeInfo, Debug, Clone, PartialEq, Eq)]
struct X;

#[derive(TypeInfo, Debug, Clone, PartialEq, Eq)]
struct Y;

#[test]
fn test_cast() {
    let a = A("Hi".to_string());
    let any = Any::from(a.clone());
    assert_eq!(any.cast::<A>(), &a);
    assert!(any.try_cast::<B>().is_none());
}

#[test]
fn test_into_cast() {
    let a = A("Hi".to_string());
    let any = Any::from(a.clone());
    assert_eq!(any.into_cast::<A>(), a);
}

#[test]
fn test_cast_mut() {
    let a = A("Hi".to_string());
    let mut any = Any::from(a);
    any.cast_mut::<A>().0 = "Hello".to_string();
    assert_eq!(any.into_cast::<A>(), A("Hello".to_string()));
}

#[test]
fn test_clone() {
    let a = A("Hi".to_string());
    let any1 = Any::from(a);
    let any2 = any1.clone();
    assert_eq!(any1.into_cast::<A>(), any2.into_cast::<A>());
}

#[test]
fn test_cast_generic() {
    let cx = C(X);
    let cy = C(Y);
    let any_cx = Any::from(cx.clone());
    let any_cy = Any::from(cy.clone());
    assert_eq!(any_cx.try_cast::<C<X>>(), Some(&cx));
    assert_eq!(any_cy.try_cast::<C<Y>>(), Some(&cy));
    assert_eq!(any_cx.try_cast::<C<Y>>(), None);
    assert_eq!(any_cy.try_cast::<C<X>>(), None);
}
