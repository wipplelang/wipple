use crate::*;
use libffi::high::call::*;
use libloading::{Library, Symbol};
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};
use std::{ffi::CString, os::raw::c_char, rc::Rc};
use wipple_compiler::compile::typecheck::{BuiltinType, Type};

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
    pub fn new(
        namespace: &'a str,
        identifier: &'a str,
        inputs: Vec<&Type>,
        return_ty: &Type,
    ) -> Result<ExternalFunction<'a>, Error> {
        let params = inputs
            .iter()
            .map(|ty| {
                if matches!(ty, Type::Builtin(BuiltinType::Number)) {
                    Ok(ExternalValueType::Number)
                } else if matches!(ty, Type::Builtin(BuiltinType::Text)) {
                    Ok(ExternalValueType::Text)
                } else {
                    Err(Error::from("only `Number` and `Text` are supported here"))
                }
            })
            .collect::<Result<_, _>>()?;

        let returns = if matches!(return_ty, Type::Builtin(BuiltinType::Number)) {
            Some(ExternalValueType::Number)
        } else if matches!(return_ty, Type::Builtin(BuiltinType::Text)) {
            Some(ExternalValueType::Text)
        } else if matches!(return_ty, Type::Builtin(BuiltinType::Unit)) {
            None
        } else {
            return Err(Error::from(
                "only `Number`, `Text` and `()` are supported here",
            ));
        };

        Ok(ExternalFunction {
            namespace,
            identifier,
            params,
            returns,
        })
    }

    pub fn call_with(&self, inputs: Vec<Rc<Value>>) -> Result<Rc<Value>, Error> {
        unsafe {
            let lib = Library::new(&self.namespace)
                .map_err(|error| format!("error loading external namespace: {}", error))?;

            let symbol: Symbol<*const ()> = lib
                .get(self.identifier.as_bytes())
                .map_err(|error| format!("error loading external identifier: {}", error))?;

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
                            Error::from("error converting Wipple value to external value: couldn't fit number into a 64-bit float")
                        })?;

                        Ok::<_, Error>(ExternalValueHolder::F64(float))
                    }
                    ExternalValueType::Text => {
                        let text = match input.as_ref() {
                            Value::Text(text) => text.clone(),
                            _ => unreachable!(),
                        };

                        let string = CString::new(text).map_err(|error| {
                            format!(
                                "error converting Wipple value to external value: {}",
                                error
                            )
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
                    Rc::new(Value::Number(Decimal::from_f64(result).unwrap()))
                }
                Some(ExternalValueType::Text) => {
                    let result = call::<*mut c_char>(ptr, &args);
                    Rc::new(Value::Text(
                        CString::from_raw(result).to_string_lossy().into_owned(),
                    ))
                }
                None => {
                    call::<()>(ptr, &args);
                    Rc::new(Value::Marker)
                }
            };

            for ptr in strings_to_cleanup {
                drop(CString::from_raw(ptr));
            }

            Ok(result)
        }
    }
}