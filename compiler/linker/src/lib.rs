//! Combine multiple library files into a single executable.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wipple_codegen::TypeDescriptor;
use wipple_util::WithInfo;

/// Provides the linker with information about the program.
pub trait Driver: wipple_codegen::Driver {}

/// A standalone, unlinked library.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnlinkedLibrary<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, UnlinkedItem<D>>,

    /// The type descriptors required for various intrinsics.
    pub intrinsic_type_descriptors: HashMap<String, TypeDescriptor<D>>,

    /// The variants required for various intrinsics.
    pub intrinsic_variants: HashMap<String, D::Path>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<Instance<D>>>,

    /// Any code to be run when the program starts.
    pub code: Vec<UnlinkedItem<D>>,
}

/// An analyzed expression along with its compiled IR.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnlinkedItem<D: Driver> {
    /// The analyzed expression.
    pub expression: WithInfo<D::Info, wipple_typecheck::TypedExpression<D>>,

    /// The compiled IR.
    pub ir: Option<Vec<Vec<wipple_codegen::Instruction<D>>>>,
}

/// A linked executable.
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Executable<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, LinkedItem<D>>,

    /// The type descriptors required for various intrinsics.
    pub intrinsic_type_descriptors: HashMap<String, TypeDescriptor<D>>,

    /// The variants required for various intrinsics.
    pub intrinsic_variants: HashMap<String, D::Path>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<Instance<D>>>,

    /// Any code to be run when the program starts.
    pub code: Vec<LinkedItem<D>>,
}

/// An item in an [`Executable`].
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct LinkedItem<D: Driver> {
    /// The compiled IR.
    pub ir: Vec<Vec<wipple_codegen::Instruction<D>>>,
}

/// An instance.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct Instance<D: Driver> {
    /// The type descriptor for the trait if this instance matches.
    pub type_descriptor: wipple_codegen::TypeDescriptor<D>,

    /// The path to the instance's implementation.
    pub item: D::Path,
}

/// Link multiple [`UnlinkedLibrary`]s into a single [`Executable`].
pub fn link<D: Driver>(
    libraries: impl IntoIterator<Item = UnlinkedLibrary<D>>,
) -> Option<Executable<D>> {
    libraries
        .into_iter()
        .try_fold(Executable::default(), |mut executable, library| {
            for (path, item) in library.items {
                executable.items.insert(path, convert_item(item)?);
            }

            executable
                .intrinsic_type_descriptors
                .extend(library.intrinsic_type_descriptors);

            executable
                .intrinsic_variants
                .extend(library.intrinsic_variants);

            executable.instances.extend(library.instances);

            for item in library.code {
                executable.code.push(convert_item(item)?);
            }

            Some(executable)
        })
}

fn convert_item<D: Driver>(item: UnlinkedItem<D>) -> Option<LinkedItem<D>> {
    Some(LinkedItem { ir: item.ir? })
}
