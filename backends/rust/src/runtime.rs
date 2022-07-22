#![allow(unused, nonstandard_style)]

mod __runtime {
    use std::{
        cell::RefCell,
        cmp,
        mem::{self, MaybeUninit},
        rc::Rc,
        thread,
    };

    pub type Value<T> = MaybeUninit<T>;

    pub fn value<T>() -> Value<T> {
        Value::uninit()
    }

    #[track_caller]
    pub fn init<T>(value: &mut Value<T>, x: T) {
        value.write(x);
    }

    /// # Safety
    ///
    /// -  The value is already initialized as guaranteed by Wipple.
    /// -  Because Wipple guarantees type safety, transmuting only has an effect
    ///    when converting to a type containing the bottom type (`!`), so a
    ///    transmute that actually reinterprets the type or size of a value will
    ///    never occur because the program will never reach that point.
    #[track_caller]
    pub fn read<T, U: Clone>(value: &Value<T>) -> U {
        unsafe {
            let x = value.assume_init_ref();
            (*(x as *const T as *const U)).clone()
        }
    }

    #[track_caller]
    pub fn capture<T: Clone>(value: &Value<T>) -> Value<T> {
        unsafe { Value::new(value.assume_init_ref().clone()) }
    }

    #[track_caller]
    pub fn drop<T>(value: &mut Value<T>) {
        unsafe { value.assume_init_drop() }
    }

    pub type Constant<T> = thread::LocalKey<T>;

    /// # Safety
    ///
    /// See [`read`] above.
    #[track_caller]
    pub fn read_init<T, U: Clone>(value: &'static Constant<T>) -> U {
        value.with(|x| unsafe { (*(x as *const T as *const U)).clone() })
    }

    pub fn unreachable() -> ! {
        unreachable!()
    }

    #[derive(Clone, Copy)]
    pub struct Marker;

    pub type Number = f64;

    pub type Text = Rc<str>;

    pub fn text(s: &'static str) -> Text {
        Text::from(s)
    }

    pub type Discriminant = usize;

    pub type Boolean = bool;

    pub type Function<Input, Output> = Rc<dyn Fn(Input) -> Output>;

    pub fn function<Input, Output>(
        function: impl Fn(Input) -> Output + 'static,
    ) -> Function<Input, Output> {
        Rc::new(function)
    }

    pub fn call<Input, Output>(function: Function<Input, Output>, input: Input) -> Output {
        function(input)
    }

    pub type List<T> = Rc<Vec<T>>;

    pub type Mutable<T> = Rc<RefCell<T>>;

    pub struct Unreachable;

    pub trait Enumeration {
        fn discriminant(&self) -> Discriminant;
    }

    pub fn discriminant<T: Enumeration>(enumeration: &T) -> Discriminant {
        enumeration.discriminant()
    }

    pub mod builtins {
        use super::*;

        pub fn crash(message: Text) -> Unreachable {
            panic!("fatal error: {}", message);
        }

        pub fn write_stdout(text: Text) {
            print!("{}", text);
        }

        pub fn format(text: Text, inputs: List<Text>) -> Text {
            let mut text = text.split('_').collect::<Vec<_>>();
            let last = text.pop().unwrap();

            let result = text
                .into_iter()
                .zip(inputs.iter())
                .map(|(part, value)| part.to_string() + value.as_ref())
                .chain(std::iter::once(last.to_string()))
                .collect::<String>();

            Text::from(result)
        }

        pub fn number_to_text(n: Number) -> Text {
            Text::from(n.to_string())
        }

        pub fn add(a: Number, b: Number) -> Number {
            a + b
        }

        pub fn subtract(a: Number, b: Number) -> Number {
            a - b
        }

        pub fn multiply(a: Number, b: Number) -> Number {
            a * b
        }

        pub fn divide(a: Number, b: Number) -> Number {
            if b == 0. {
                panic!("division by zero is undefined");
            }

            a / b
        }

        pub fn power(a: Number, b: Number) -> Number {
            if a == 0. && b == 0. {
                panic!("raising zero to the power of zero is undefined");
            }

            a.powf(b)
        }

        pub fn floor(x: Number) -> Number {
            x.floor()
        }

        pub fn ceil(x: Number) -> Number {
            x.ceil()
        }

        pub fn sqrt(x: Number) -> Number {
            if x < 0. {
                panic!("cannot calculate the square root of a negative number");
            }

            x.sqrt()
        }

        pub fn text_equality() -> ! {
            todo!()
        }

        pub fn number_equality(a: Number, b: Number) -> Boolean {
            a == b
        }

        pub enum Ordering {
            Less(),
            Equal(),
            Greater(),
        }

        pub fn number_ordering<T: 'static>(a: Number, b: Number) -> T {
            let ordering = match a.partial_cmp(&b).unwrap() {
                cmp::Ordering::Less => Ordering::Less(),
                cmp::Ordering::Equal => Ordering::Equal(),
                cmp::Ordering::Greater => Ordering::Greater(),
            };

            // SAFETY: This relies on the `Ordering` type from Wipple.
            unsafe { mem::transmute_copy(&ordering) }
        }

        pub fn make_mutable<T>(value: T) -> Mutable<T> {
            Rc::new(RefCell::new(value))
        }

        pub fn get_mutable<T: Clone>(mutable: Mutable<T>) -> T {
            mutable.borrow().clone()
        }

        pub fn set_mutable<T>(mutable: Mutable<T>, value: T) {
            mutable.replace(value);
        }

        pub trait MakeList<T> {
            fn make_list(self) -> List<T>;
        }

        impl<T> MakeList<T> for (T,) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0])
            }
        }

        impl<T> MakeList<T> for (T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0, self.1])
            }
        }

        impl<T> MakeList<T> for (T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0, self.1, self.2])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0, self.1, self.2, self.3])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0, self.1, self.2, self.3, self.4])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![self.0, self.1, self.2, self.3, self.4, self.5])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![
                    self.0, self.1, self.2, self.3, self.4, self.5, self.6
                ])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![
                    self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7
                ])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![
                    self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7, self.8
                ])
            }
        }

        impl<T> MakeList<T> for (T, T, T, T, T, T, T, T, T, T) {
            fn make_list(self) -> List<T> {
                List::new(::std::vec![
                    self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7, self.8, self.9
                ])
            }
        }

        pub fn make_list<T>(tuple: impl MakeList<T>) -> List<T> {
            tuple.make_list()
        }

        pub fn list_first() -> ! {
            todo!()
        }

        pub fn list_last() -> ! {
            todo!()
        }

        pub fn list_initial() -> ! {
            todo!()
        }

        pub fn list_tail() -> ! {
            todo!()
        }

        pub fn list_at() -> ! {
            todo!()
        }

        pub fn list_append() -> ! {
            todo!()
        }

        pub fn list_insert() -> ! {
            todo!()
        }

        pub fn list_remove() -> ! {
            todo!()
        }
    }
}
