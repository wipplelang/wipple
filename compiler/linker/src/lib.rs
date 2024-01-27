//! Combine multiple library files into a single executable.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
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
    pub intrinsic_type_descriptors: HashMap<String, D::Path>,

    /// The variants required for various intrinsics.
    pub intrinsic_variants: HashMap<String, D::Path>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<D::Path>>,

    /// Any code to be run when the program starts.
    pub code: Vec<UnlinkedItem<D>>,
}

/// An analyzed expression along with its compiled IR.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct UnlinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

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
    pub intrinsic_type_descriptors: HashMap<String, D::Path>,

    /// The variants required for various intrinsics.
    pub intrinsic_variants: HashMap<String, D::Path>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<D::Path>>,

    /// Any code to be run when the program starts.
    pub code: Vec<LinkedItem<D>>,
}

/// An item in an [`Executable`].
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
pub struct LinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

    /// The item's type descriptor.
    pub type_descriptor: wipple_codegen::TypeDescriptor<D>,

    /// The compiled IR.
    pub ir: Vec<Vec<wipple_codegen::Instruction<D>>>,
}

/// The linked executable, or the linking error.
pub type Result<T, D> =
    std::result::Result<T, WithInfo<<D as wipple_typecheck::Driver>::Info, Error>>;

/// A linking error.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Error {
    /// Failed to produce a type descriptor for the item.
    TypeDescriptor,

    /// Failed to produce IR for the item.
    Ir,
}

/// Link multiple [`UnlinkedLibrary`]s into a single [`Executable`].
pub fn link<D: Driver>(
    libraries: impl IntoIterator<Item = UnlinkedLibrary<D>>,
) -> Result<Executable<D>, D> {
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

            Ok(executable)
        })
}

fn convert_item<D: Driver>(item: UnlinkedItem<D>) -> Result<LinkedItem<D>, D> {
    Ok(LinkedItem {
        parameters: item.parameters,
        type_descriptor: wipple_codegen::type_descriptor(&item.expression.item.r#type).ok_or_else(
            || WithInfo {
                info: item.expression.info.clone(),
                item: Error::TypeDescriptor,
            },
        )?,
        ir: item.ir.ok_or(WithInfo {
            info: item.expression.info,
            item: Error::Ir,
        })?,
    })
}
