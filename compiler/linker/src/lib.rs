//! Combine multiple library files into a single executable.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wipple_util::WithInfo;

/// Provides the linker with information about the program.
pub trait Driver: wipple_ir::Driver {}

/// A standalone, unlinked library.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnlinkedLibrary<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, UnlinkedItem<D>>,

    /// The layouts of each declared type.
    pub layouts: HashMap<D::Path, wipple_ir::LayoutDescriptor<D>>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, Vec<UnlinkedInstance<D>>>,

    /// The default instances for each trait.
    pub default_instances: HashMap<D::Path, Vec<UnlinkedInstance<D>>>,

    /// The items to run when the program starts.
    pub entrypoints: Vec<D::Path>,

    /// Items exported by the executable.
    pub exports: HashMap<String, D::Path>,
}

/// An analyzed instance.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnlinkedInstance<D: Driver> {
    /// The path to the associated [`UnlinkedItem`].
    pub path: D::Path,

    /// The list of parameters the instance provides to the trait.
    pub trait_parameters: Vec<(D::Path, WithInfo<D::Info, wipple_typecheck::Type<D>>)>,
}

/// An analyzed expression along with its compiled IR.
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct UnlinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

    /// The list of bounds required by this item.
    pub bounds: Vec<wipple_ir::InstanceDescriptor<D>>,

    /// The analyzed expression.
    pub expression: WithInfo<D::Info, wipple_typecheck::TypedExpression<D>>,

    /// The compiled IR.
    pub ir: Option<Vec<wipple_ir::Instruction<D>>>,

    /// Whether to evaluate this item every time it is referenced, or just once.
    pub evaluate_once: bool,
}

/// A linked executable.
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""), Default(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct Executable<D: Driver> {
    /// The implementations of constants and instances.
    pub items: HashMap<D::Path, LinkedItem<D>>,

    /// The layouts of each declared type.
    pub layouts: HashMap<D::Path, wipple_ir::LayoutDescriptor<D>>,

    /// The list of instances for each trait.
    pub instances: HashMap<D::Path, HashMap<D::Path, LinkedInstance<D>>>,

    /// The default instances for each trait.
    pub default_instances: HashMap<D::Path, HashMap<D::Path, LinkedInstance<D>>>,

    /// The items to run when the program starts.
    pub entrypoints: Vec<D::Path>,

    /// Items exported by the executable.
    pub exports: HashMap<String, D::Path>,
}

/// An instance in an [`Executable`].
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct LinkedInstance<D: Driver> {
    /// The path to the associated [`LinkedItem`].
    pub path: D::Path,

    /// The list of parameters the instance provides to the trait.
    pub trait_parameters: Vec<(D::Path, wipple_ir::TypeDescriptor<D>)>,
}

/// An item in an [`Executable`].
#[non_exhaustive]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct LinkedItem<D: Driver> {
    /// The list of type parameters declared by the item.
    pub parameters: Vec<D::Path>,

    /// The list of bounds required by this item.
    pub bounds: Vec<wipple_ir::InstanceDescriptor<D>>,

    /// The compiled IR.
    pub ir: Vec<wipple_ir::Instruction<D>>,

    /// Whether to evaluate this item every time it is referenced, or just once.
    pub evaluate_once: bool,
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

            executable.layouts.extend(library.layouts);

            for (r#trait, instances) in library.instances {
                executable.instances.entry(r#trait).or_default().extend(
                    instances
                        .into_iter()
                        .map(|instance| {
                            let instance = convert_instance(instance)?;
                            Ok((instance.path.clone(), instance))
                        })
                        .collect::<Result<HashMap<_, _>, D>>()?,
                );
            }

            for (r#trait, instances) in library.default_instances {
                executable
                    .default_instances
                    .entry(r#trait)
                    .or_default()
                    .extend(
                        instances
                            .into_iter()
                            .map(|instance| {
                                let instance = convert_instance(instance)?;
                                Ok((instance.path.clone(), instance))
                            })
                            .collect::<Result<HashMap<_, _>, D>>()?,
                    );
            }

            executable.entrypoints.extend(library.entrypoints);

            executable.exports.extend(library.exports);

            Ok(executable)
        })
}

fn convert_instance<D: Driver>(instance: UnlinkedInstance<D>) -> Result<LinkedInstance<D>, D> {
    Ok(LinkedInstance {
        path: instance.path,
        trait_parameters: instance
            .trait_parameters
            .into_iter()
            .map(|(parameter, r#type)| {
                let type_descriptor = wipple_ir::type_descriptor(&r#type.item).ok_or(WithInfo {
                    info: r#type.info,
                    item: Error::TypeDescriptor,
                })?;

                Ok((parameter, type_descriptor))
            })
            .collect::<Result<_, D>>()?,
    })
}

fn convert_item<D: Driver>(item: UnlinkedItem<D>) -> Result<LinkedItem<D>, D> {
    Ok(LinkedItem {
        parameters: item.parameters,
        bounds: item.bounds,
        ir: item.ir.ok_or_else(|| item.expression.replace(Error::Ir))?,
        evaluate_once: item.evaluate_once,
    })
}
