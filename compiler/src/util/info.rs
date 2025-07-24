use crate::syntax::Location;
use serde::Serialize;
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

/// An item with information from the driver attached.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct WithInfo<T> {
    /// Additional information about the item provided by the driver.
    pub info: Location,

    /// The parsed item.
    pub item: T,
}

impl<T> WithInfo<T> {
    /// Convert the value contained within the [`WithInfo`].
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithInfo<U> {
        WithInfo {
            info: self.info,
            item: f(self.item),
        }
    }

    /// Like [`map`], but returns `None` if the conversion fails,
    pub fn filter_map<U>(self, f: impl FnOnce(T) -> Option<U>) -> Option<WithInfo<U>> {
        Some(WithInfo {
            info: self.info,
            item: f(self.item)?,
        })
    }

    /// Box the value contained within the [`WithInfo`].
    pub fn boxed(self) -> WithInfo<Box<T>> {
        self.map(Box::new)
    }

    /// Replace the value contained within the [`WithInfo`], cloning the
    /// [`info`](WithInfo::info).
    pub fn replace<U>(&self, new: U) -> WithInfo<U> {
        WithInfo {
            info: self.info.clone(),
            item: new,
        }
    }

    /// Obtain a reference to the value contained within the [`WithInfo`],
    /// cloning the [`info`](WithInfo::info).
    pub fn as_ref(&self) -> WithInfo<&T> {
        WithInfo {
            info: self.info.clone(),
            item: &self.item,
        }
    }

    /// Obtain a mutable reference to the value contained within the
    /// [`WithInfo`], cloning the [`info`](WithInfo::info).
    pub fn as_mut(&mut self) -> WithInfo<&mut T> {
        WithInfo {
            info: self.info.clone(),
            item: &mut self.item,
        }
    }

    /// Dereference the value contained within the [`WithInfo`], cloning the
    /// [`info`](WithInfo::info).
    pub fn as_deref<'a>(&'a self) -> WithInfo<&'a T::Target>
    where
        T: Deref,
        T::Target: 'a,
    {
        WithInfo {
            info: self.info.clone(),
            item: self.item.deref(),
        }
    }

    /// Mutably dereference the value contained within the [`WithInfo`], cloning
    /// the [`info`](WithInfo::info).
    pub fn as_deref_mut<'a>(&'a mut self) -> WithInfo<&'a mut T::Target>
    where
        T: DerefMut,
        T::Target: 'a,
    {
        WithInfo {
            info: self.info.clone(),
            item: self.item.deref_mut(),
        }
    }
}

impl<T> WithInfo<Box<T>> {
    /// Unbox the value contained within the [`WithInfo`].
    pub fn unboxed(self) -> WithInfo<T> {
        self.map(|value| *value)
    }
}

impl<T> WithInfo<Option<T>> {
    /// Unwrap the [`Option`] value contained within the [`WithInfo`].
    pub fn try_unwrap(self) -> Option<WithInfo<T>> {
        self.item.map(|item| WithInfo {
            info: self.info,
            item,
        })
    }
}

impl<T> WithInfo<&T>
where
    T: Clone,
{
    /// Clone the value contained within the [`WithInfo`].
    pub fn cloned(&self) -> WithInfo<T> {
        WithInfo {
            info: self.info.clone(),
            item: self.item.clone(),
        }
    }
}

/// Like [`Default`], but for types wrapped in [`WithInfo`].
pub trait DefaultFromInfo: Sized {
    /// Produce the default value of `Self` with the given info.
    fn default_from_info(info: Location) -> WithInfo<Self>;
}

impl DefaultFromInfo for () {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo { info, item: () }
    }
}

impl<A, B> DefaultFromInfo for (WithInfo<A>, WithInfo<B>)
where
    A: DefaultFromInfo,
    B: DefaultFromInfo,
{
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info: info.clone(),
            item: (
                A::default_from_info(info.clone()),
                B::default_from_info(info),
            ),
        }
    }
}

impl<T> DefaultFromInfo for Option<T> {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo { info, item: None }
    }
}

impl<T> DefaultFromInfo for Vec<WithInfo<T>> {
    fn default_from_info(info: Location) -> WithInfo<Self> {
        WithInfo {
            info,
            item: Vec::new(),
        }
    }
}
