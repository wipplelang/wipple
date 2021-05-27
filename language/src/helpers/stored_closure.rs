#[macro_export]
macro_rules! stored_closure {
    ($vis:vis $(for<$($lt:lifetime),+>)? struct $name:ident $(<$($generic:ident $(: $bound:tt)?),*>)? ($($arg:ty),*) $(-> $ret:ty)?) => {
        #[derive($crate::TypeInfo, Clone)]
        $vis struct $name$(<$($generic $(: $bound)?),*>)?(::std::rc::Rc<dyn $(for <$($lt),+>)? Fn($($arg),*) $(-> $ret)?>);

        impl $(<$($generic $(: $bound)?),*>)? $name $(<$($generic),*>)? {
            $vis fn new(f: impl $(for <$($lt),+>)? Fn($($arg),*) $(-> $ret)? + 'static) -> Self {
                Self(::std::rc::Rc::new(f))
            }
        }

        impl $(<$($generic $(: $bound)?),*>)? ::std::ops::Deref for $name $(<$($generic),*>)? {
            type Target = dyn $(for <$($lt),+>)? Fn($($arg),*) $(-> $ret)?;

            fn deref(&self) -> &Self::Target {
                self.0.as_ref()
            }
        }

        impl $(<$($generic $(: $bound)?),*>)? ::std::fmt::Debug for $name $(<$($generic),*>)? {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                f.write_str(stringify!($name))
            }
        }
    };
}
