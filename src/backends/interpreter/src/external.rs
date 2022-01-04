use crate::*;
use libffi::high::call::*;
use libloading::{Library, Symbol};
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use std::{ffi::CString, os::raw::c_char, sync::Arc};
use wipple_frontend::typecheck::{ExternalItem, Type, BUILTIN_TYPES};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternalFunction<'a> {
    pub namespace: &'a str,
    pub identifier: &'a str,
    pub params: Vec<ExternalValueType>,
    pub returns: Option<ExternalValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternalValueType {
    Number,
    Text,
}

impl<'a> ExternalFunction<'a> {
    pub fn from_item(
        item: &'a ExternalItem,
        inputs: Vec<&Type>,
        return_ty: &Type,
    ) -> Result<ExternalFunction<'a>, Error> {
        macro_rules! type_id {
            ($ty:expr) => {
                match $ty {
                    Type::Constructed(name, _) => name.id,
                    Type::Variable(_) => {
                        return Err(Error::from("Type of input or result is unknown"))
                    }
                }
            };
        }

        let params = inputs
            .iter()
            .map(|ty| {
                if type_id!(ty) == type_id!(BUILTIN_TYPES.number) {
                    Ok(ExternalValueType::Number)
                } else if type_id!(ty) == type_id!(BUILTIN_TYPES.text) {
                    Ok(ExternalValueType::Text)
                } else {
                    Err(Error::from("Only Number and Text are supported here"))
                }
            })
            .collect::<Result<_, _>>()?;

        let returns = if type_id!(return_ty) == type_id!(BUILTIN_TYPES.number) {
            Some(ExternalValueType::Number)
        } else if type_id!(return_ty) == type_id!(BUILTIN_TYPES.text) {
            Some(ExternalValueType::Text)
        } else if type_id!(return_ty) == type_id!(BUILTIN_TYPES.unit) {
            None
        } else {
            return Err(Error::from("Only Number, Text and () are supported here"));
        };

        Ok(ExternalFunction {
            namespace: &item.namespace,
            identifier: &item.identifier,
            params,
            returns,
        })
    }

    pub fn call_with(&self, inputs: Vec<Arc<Value>>, info: &Info) -> Result<Arc<Value>, Diverge> {
        unsafe {
            let lib = Library::new(&self.namespace).map_err(|error| {
                Diverge::new(
                    &info.callstack,
                    DivergeKind::Error(format!("Error loading external namespace: {}", error)),
                )
            })?;

            let symbol: Symbol<*const ()> =
                lib.get(self.identifier.as_bytes()).map_err(|error| {
                    Diverge::new(
                        &info.callstack,
                        DivergeKind::Error(format!("Error loading external identifier: {}", error)),
                    )
                })?;

            let ptr = CodePtr::from_ptr(symbol.into_raw().into_raw());

            assert!(inputs.len() == self.params.len());

            pub enum ExternalValueHolder {
                F64(f64),
                Str(*mut c_char),
            }

            let mut strings_to_cleanup = Vec::new();

            let args = inputs
                .into_iter()
                .zip(&self.params)
                .map(|(input, ty)| match ty {
                    ExternalValueType::Number => {
                        let number = match input.as_ref() {
                            Value::Number(number) => *number,
                            _ => unreachable!(),
                        };

                        let float = number.to_f64().ok_or_else(|| {
                            Diverge::new(
                                &info.callstack,
                                DivergeKind::Error(Error::from("Error converting Wipple value to external value: couldn't fit number into a 64-bit float")),
                            )
                        })?;

                        Ok(ExternalValueHolder::F64(float))
                    }
                    ExternalValueType::Text => {
                        let text = match input.as_ref() {
                            Value::Text(text) => text.clone(),
                            _ => unreachable!(),
                        };

                        let string = CString::new(text).map_err(|error| {
                            Diverge::new(&info.callstack, DivergeKind::Error(format!(
                                "Error converting Wipple value to external value: {}",
                                error
                            )))
                        })?;

                        let ptr = string.into_raw();
                        strings_to_cleanup.push(ptr);

                        Ok(ExternalValueHolder::Str(ptr))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;

            let args = args
                .iter()
                .map(|a| match a {
                    ExternalValueHolder::F64(n) => arg(n),
                    ExternalValueHolder::Str(s) => arg(s),
                })
                .collect::<Vec<_>>();

            let result = match self.returns {
                Some(ExternalValueType::Number) => {
                    let result = call::<f64>(ptr, &args);
                    Arc::new(Value::Number(Decimal::from_f64(result).unwrap()))
                }
                Some(ExternalValueType::Text) => {
                    let result = call::<*mut c_char>(ptr, &args);
                    Arc::new(Value::Text(
                        CString::from_raw(result).to_string_lossy().into_owned(),
                    ))
                }
                None => {
                    call::<()>(ptr, &args);
                    Arc::new(Value::Unit)
                }
            };

            for ptr in strings_to_cleanup {
                drop(CString::from_raw(ptr));
            }

            Ok(result)
        }
    }
}
