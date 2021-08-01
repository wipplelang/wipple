mod resolver;

use bytecode::binary::Index;
use rand::Rng;
use resolver::{ClosureResolver, Object};
use std::{error::Error, io};
use wipple_bytecode as bytecode;
use wipple_interpreter::{self as interpreter, Context, Interpreter, Reference, Value};

fn main() -> Result<(), Box<dyn Error>> {
    let file: bytecode::BinFile = bincode::deserialize_from(io::stdin())?;

    unsafe fn get<T: Copy>(index: Index, context: &mut Context) -> interpreter::Result<T> {
        let value = context.get(index)?.reference().unwrap();
        let number = *(value.as_ptr() as *const T);
        Ok(number)
    }

    macro_rules! math {
        ($op:tt) => {
            |inputs, context| {
                let a: f64 = unsafe { get(inputs[0], context)? };
                let b: f64 = unsafe { get(inputs[1], context)? };

                let sum = a $op b;

                let value = Value::Reference(Reference::from_data(sum.to_ne_bytes().to_vec()));
                context.stack.current_mut().push(value);

                Ok(())
            }
        };
    }

    let interpreter = Interpreter::new(
        ClosureResolver::new()
            .with(
                "logic",
                Object::new().with("if", |inputs, context| {
                    let condition: bool = unsafe { get(inputs[0], context)? };
                    let then = context.get(inputs[1])?.thunk().unwrap();
                    let r#else = context.get(inputs[2])?.thunk().unwrap();

                    let thunk = if condition {
                        then.clone()
                    } else {
                        r#else.clone()
                    };

                    context.stack.current_mut().push(Value::Thunk(thunk));

                    Ok(())
                }),
            )
            .with(
                "math",
                Object::new()
                    .with("add", math!(+))
                    .with("sub", math!(-))
                    .with("lte", |inputs, context| {
                        let a: f64 = unsafe { get(inputs[0], context)? };
                        let b: f64 = unsafe { get(inputs[1], context)? };

                        let lte = a <= b;

                        let value = Value::Reference(Reference::from_data(
                            (lte as u8).to_ne_bytes().to_vec(),
                        ));

                        context.stack.current_mut().push(value);

                        Ok(())
                    }),
            )
            .with(
                "io",
                Object::new().with("show", |inputs, context| {
                    let n: f64 = unsafe { get(inputs[0], context)? };

                    println!("{}", n);

                    Ok(())
                }),
            )
            .with(
                "random",
                Object::new().with("random", move |inputs, context| {
                    let min: f64 = unsafe { get(inputs[0], context)? };
                    let max: f64 = unsafe { get(inputs[1], context)? };

                    let n = rand::thread_rng().gen_range(min..=max) as u64 as f64;

                    let value = Value::Reference(Reference::from_data(n.to_ne_bytes().to_vec()));
                    context.stack.current_mut().push(value);

                    Ok(())
                }),
            ),
    );

    interpreter.execute(&file)?;

    Ok(())
}
