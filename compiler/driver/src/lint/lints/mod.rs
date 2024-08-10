//! The available lints.

mod naming_conventions;

pub use naming_conventions::*;

use crate::lint::Lint;
use wipple_linker::{Driver, UnlinkedItem};
use wipple_util::WithInfo;

pub(super) trait Rule<D: Driver> {
    fn lint_item(
        &self,
        path: &wipple_lower::Path,
        item: &UnlinkedItem<D>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = path;
        let _ = item;
        let _ = add_lint;
    }

    fn lint_type_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: WithInfo<D::Info, &wipple_typecheck::TypeDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = declaration;
        let _ = path;
        let _ = add_lint;
    }

    fn lint_trait_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: WithInfo<D::Info, &wipple_typecheck::TraitDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = declaration;
        let _ = path;
        let _ = add_lint;
    }

    fn lint_constant_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: WithInfo<D::Info, &wipple_typecheck::ConstantDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = declaration;
        let _ = path;
        let _ = add_lint;
    }

    fn lint_instance_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: WithInfo<D::Info, &wipple_typecheck::InstanceDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = declaration;
        let _ = path;
        let _ = add_lint;
    }

    fn lint_type_parameter_declaration(
        &self,
        path: &wipple_lower::Path,
        declaration: WithInfo<D::Info, &wipple_typecheck::TypeParameterDeclaration<D>>,
        add_lint: AddLint<'_, D>,
    ) {
        let _ = declaration;
        let _ = path;
        let _ = add_lint;
    }
}

pub(super) type AddLint<'a, D> =
    &'a mut dyn FnMut(WithInfo<<D as wipple_typecheck::Driver>::Info, Lint>);

pub(super) fn rules<D: Driver>() -> &'static [&'static dyn Rule<D>] {
    &[&naming_conventions::NamingConventionsRule]
}
