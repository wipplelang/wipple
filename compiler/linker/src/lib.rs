//! Combine multiple library files into a single executable.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use ts_rs::TS;
use wipple_util::WithInfo;

/// Provides the linker with information about the program.
pub trait Driver: wipple_ir::Driver {}

impl Driver for wipple_util::TsAny {}

/// A standalone, unlinked library.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "linker_UnlinkedLibrary", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct UnlinkedLibrary<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, UnlinkedItem<D>>,

    /// The layouts of each declared type.
    pub layouts: HashMap<D::Path, wipple_ir::LayoutDescriptor<D>>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<D::Path>>,

    /// The default instance for each trait.
    pub default_instances: HashMap<D::Path, Vec<D::Path>>,

    /// The item to run when the program starts.
    pub entrypoint: Option<D::Path>,

    /// Items exported by the executable.
    pub exports: HashMap<String, D::Path>,
}

/// An analyzed expression along with its compiled IR.
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "linker_UnlinkedItem", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct UnlinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

    /// The analyzed expression.
    pub expression: WithInfo<D::Info, wipple_typecheck::TypedExpression<D>>,

    /// The compiled IR.
    pub ir: Vec<wipple_ir::Instruction<D>>,

    /// The variables captured by this item.
    pub captures: Vec<wipple_ir::TypeDescriptor<D>>,
}

/// A linked executable.
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "linker_Executable", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct Executable<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, LinkedItem<D>>,

    /// The layouts of each declared type.
    pub layouts: HashMap<D::Path, wipple_ir::LayoutDescriptor<D>>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<D::Path>>,

    /// The default instance for each trait.
    pub default_instances: HashMap<D::Path, Vec<D::Path>>,

    /// The item to run when the program starts.
    pub entrypoint: Option<D::Path>,

    /// Items exported by the executable.
    pub exports: HashMap<String, D::Path>,
}

/// An item in an [`Executable`].
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative, TS)]
#[derivative(Debug(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[ts(export, rename = "linker_LinkedItem", concrete(D = wipple_util::TsAny), bound = "D::Info: TS")]
pub struct LinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

    /// The item's type descriptor.
    pub type_descriptor: wipple_ir::TypeDescriptor<D>,

    /// The compiled IR.
    pub ir: Vec<wipple_ir::Instruction<D>>,

    /// The variables captured by this item.
    pub captures: Vec<wipple_ir::TypeDescriptor<D>>,
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

    /// Multiple libraries contained an entrypoint.
    MultipleEntrypoints,
}

/// Link multiple [`UnlinkedLibrary`]s into a single [`Executable`].
pub fn link<D: Driver>(
    libraries: impl IntoIterator<Item = UnlinkedLibrary<D>>,
) -> Result<Executable<D>, D> {
    libraries
        .into_iter()
        .try_fold(Executable::default(), |mut executable, library| {
            if let Some(entrypoint) = library.entrypoint {
                if executable.entrypoint.is_some() {
                    let info = library
                        .items
                        .get(&entrypoint)
                        .unwrap()
                        .expression
                        .info
                        .clone();

                    return Err(WithInfo {
                        info,
                        item: Error::MultipleEntrypoints,
                    });
                }

                executable.entrypoint = Some(entrypoint);
            }

            for (path, item) in library.items {
                executable.items.insert(path, convert_item(item)?);
            }

            executable.layouts.extend(library.layouts);

            executable.exports.extend(library.exports);

            for (r#trait, instances) in library.instances {
                executable
                    .instances
                    .entry(r#trait)
                    .or_default()
                    .extend(instances);
            }

            for (r#trait, instances) in library.default_instances {
                executable
                    .default_instances
                    .entry(r#trait)
                    .or_default()
                    .extend(instances);
            }

            Ok(executable)
        })
}

fn convert_item<D: Driver>(item: UnlinkedItem<D>) -> Result<LinkedItem<D>, D> {
    Ok(LinkedItem {
        parameters: item.parameters,
        type_descriptor: wipple_ir::type_descriptor(&item.expression.item.r#type).ok_or_else(
            || WithInfo {
                info: item.expression.info.clone(),
                item: Error::TypeDescriptor,
            },
        )?,
        ir: item.ir,
        captures: item.captures,
    })
}
