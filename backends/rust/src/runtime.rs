#![allow(unused, nonstandard_style)]

mod __runtime {
    use std::{
        cell::RefCell,
        cmp,
        mem::{self, MaybeUninit},
        os,
        rc::Rc,
        thread,
    };

    unsafe fn transmute_clone<T, U: Clone>(x: &T) -> U {
        (*(x as *const T as *const U)).clone()
    }

    pub type Value<T> = MaybeUninit<T>;
    pub type BoxedValue<T> = Box<MaybeUninit<T>>;

    pub trait ValueInner<T> {
        fn uninit() -> Self;
        fn init(&mut self, x: T);
        unsafe fn read(&self) -> &T;
    }

    impl<T> ValueInner<T> for Value<T> {
        fn uninit() -> Self {
            MaybeUninit::uninit()
        }

        fn init(&mut self, x: T) {
            self.write(x);
        }

        unsafe fn read(&self) -> &T {
            self.assume_init_ref()
        }
    }

    impl<T> ValueInner<T> for BoxedValue<T> {
        fn uninit() -> Self {
            Box::new(MaybeUninit::uninit())
        }

        fn init(&mut self, x: T) {
            self.write(x);
        }

        unsafe fn read(&self) -> &T {
            self.assume_init_ref()
        }
    }

    #[inline(always)]
    pub fn value<T, V: ValueInner<T>>() -> V {
        V::uninit()
    }

    #[inline(always)]
    pub fn init<T>(value: &mut impl ValueInner<T>, x: T) {
        value.init(x);
    }

    /// # Safety
    ///
    /// -  The value is already initialized as guaranteed by Wipple.
    /// -  Because Wipple guarantees type safety, transmuting only has an effect
    ///    when converting to a type containing the bottom type (`!`), so a
    ///    transmute that actually reinterprets the type or size of a value will
    ///    never occur because the program will never reach that point.
    #[inline(always)]
    pub fn read<T, U: Clone>(value: &impl ValueInner<T>) -> U {
        unsafe { transmute_clone(value.read()) }
    }

    #[inline(always)]
    pub fn capture<T: Clone>(value: &Value<T>) -> Value<T> {
        unsafe { Value::new(value.assume_init_ref().clone()) }
    }

    #[inline(always)]
    pub fn drop<T>(value: &mut Value<T>) {
        unsafe { value.assume_init_drop() }
    }

    pub type Constant<T> = thread::LocalKey<T>;

    /// # Safety
    ///
    /// See [`read`] above.
    #[inline(always)]
    pub fn read_init<T, U: Clone>(value: &'static Constant<T>) -> U {
        value.with(|x| unsafe { transmute_clone(x) })
    }

    #[inline(always)]
    pub fn unreachable() -> ! {
        unsafe { std::hint::unreachable_unchecked() }
    }

    #[derive(Clone, Copy)]
    pub struct Marker;

    pub type Number = f64;

    pub type Integer = i64;

    pub type Text = Rc<str>;

    #[inline(always)]
    pub fn text(s: &'static str) -> Text {
        Text::from(s)
    }

    pub type Discriminant = usize;

    pub type Boolean = bool;

    pub type Function<Input, Output> = Rc<dyn Fn(Input) -> Output>;

    #[inline(always)]
    pub fn function<Input, Output>(
        function: impl Fn(Input) -> Output + 'static,
    ) -> Function<Input, Output> {
        Rc::new(function)
    }

    #[inline(always)]
    pub fn call<Input, Output>(function: Function<Input, Output>, input: Input) -> Output {
        function(input)
    }

    pub type List<T> = Rc<Vec<T>>;

    pub type Mutable<T> = Rc<RefCell<T>>;

    pub struct Unreachable;

    pub trait Enumeration {
        fn discriminant(&self) -> Discriminant;
    }

    #[inline(always)]
    pub fn discriminant<T: Enumeration>(enumeration: &T) -> Discriminant {
        enumeration.discriminant()
    }

    pub trait FromC<T> {
        fn from_c(x: T) -> Self;
    }

    impl FromC<os::raw::c_int> for Integer {
        fn from_c(x: os::raw::c_int) -> Self {
            x as Integer
        }
    }

    impl FromC<os::raw::c_double> for Number {
        fn from_c(x: os::raw::c_double) -> Self {
            x as Number
        }
    }

    pub trait ToC<T> {
        fn to_c(self) -> T;
    }

    impl ToC<os::raw::c_int> for Integer {
        fn to_c(self) -> os::raw::c_int {
            self as os::raw::c_int
        }
    }

    impl ToC<os::raw::c_double> for Number {
        fn to_c(self) -> os::raw::c_double {
            self as os::raw::c_double
        }
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

        pub fn text_equality(a: Text, b: Text) -> Boolean {
            a == b
        }

        pub fn number_equality(a: Number, b: Number) -> Boolean {
            a == b
        }

        pub fn integer_to_number<T: Clone>(n: Integer) -> T {
            unsafe { transmute_clone(&(n as f64)) }
        }

        pub fn number_to_integer<T: Clone>(n: Number) -> T {
            let n = if n.trunc() == n { Some(n as i64) } else { None };

            // SAFETY: This relies on the `Maybe` type from Wipple
            unsafe { transmute_clone(&n) }
        }

        pub enum Ordering {
            Less(),
            Equal(),
            Greater(),
        }

        pub fn number_ordering<T: Clone>(a: Number, b: Number) -> T {
            let ordering = match a.partial_cmp(&b).unwrap() {
                cmp::Ordering::Less => Ordering::Less(),
                cmp::Ordering::Equal => Ordering::Equal(),
                cmp::Ordering::Greater => Ordering::Greater(),
            };

            // SAFETY: This relies on the `Ordering` type from Wipple
            unsafe { transmute_clone(&ordering) }
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

        pub fn list_first<T: Clone, U: Clone>(list: List<T>) -> U {
            // SAFETY: This relies on the `Maybe` type from Wipple
            unsafe { transmute_clone(&list.first().cloned()) }
        }

        pub fn list_last<T: Clone, U: Clone>(list: List<T>) -> U {
            // SAFETY: This relies on the `Maybe` type from Wipple
            unsafe { transmute_clone(&list.last().cloned()) }
        }

        pub fn list_initial<T: Clone, U: Clone>(list: List<T>) -> U {
            let initial = (!list.is_empty()).then(|| Rc::new(list[0..(list.len() - 1)].to_vec()));

            // SAFETY: This relies on the `Maybe` type from Wipple
            unsafe { transmute_clone(&initial) }
        }

        pub fn list_tail<T: Clone>(list: List<T>) -> List<T> {
            let tail = (!list.is_empty()).then(|| Rc::new(list[1..].to_vec()));

            // SAFETY: This relies on the `Maybe` type from Wipple
            unsafe { transmute_clone(&tail) }
        }

        pub fn list_nth<T: Clone, U: Clone>(list: List<T>, index: Number) -> U {
            struct IndexError;

            let index = if index >= 0. && index.floor() == index {
                index as usize
            } else {
                // SAFETY: This relies on the `Maybe` type from Wipple
                return unsafe { transmute_clone(&Err::<List<T>, _>(IndexError)) };
            };

            // SAFETY: This relies on the `Result (List A) Index-Error` type from Wipple
            unsafe { transmute_clone(&list.get(index).cloned().ok_or(IndexError)) }
        }

        pub fn list_append<T: Clone>(list: List<T>, item: T) -> List<T> {
            let mut list = (*list).clone();
            list.push(item);
            Rc::new(list)
        }

        pub fn list_insert<T: Clone, U: Clone>(list: List<T>, index: Number, item: T) -> U {
            struct IndexError;

            let index = if index >= 0. && index.floor() == index {
                index as usize
            } else {
                // SAFETY: This relies on the `Result (List A) Index-Error` type from Wipple
                return unsafe { transmute_clone(&Err::<List<T>, _>(IndexError)) };
            };

            let mut list = (*list).clone();
            list.insert(index, item);
            let list = Rc::new(list);

            // SAFETY: This relies on the `Result (List A) Index-Error` type from Wipple
            unsafe { transmute_clone(&Ok::<_, IndexError>(list)) }
        }

        pub fn list_remove<T: Clone, U: Clone>(list: List<T>, index: Number) -> U {
            struct IndexError;

            let index = if index >= 0. && index.floor() == index {
                index as usize
            } else {
                // SAFETY: This relies on the `Result (List A) Index-Error` type from Wipple
                return unsafe { transmute_clone(&Err::<List<T>, _>(IndexError)) };
            };

            let mut list = (*list).clone();
            list.remove(index);
            let list = Rc::new(list);

            // SAFETY: This relies on the `Result (List A) Index-Error` type from Wipple
            unsafe { transmute_clone(&Ok::<_, IndexError>(list)) }
        }
    }
}
