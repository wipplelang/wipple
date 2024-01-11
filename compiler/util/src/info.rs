use serde::{Deserialize, Serialize};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

/// An item with information from the driver attached.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WithInfo<I, T> {
    /// Additional information about the item provided by the driver.
    pub info: I,

    /// The parsed item.
    pub item: T,
}

impl<I, T> WithInfo<I, T> {
    /// Convert the value contained within the [`WithInfo`].
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithInfo<I, U> {
        WithInfo {
            info: self.info,
            item: f(self.item),
        }
    }

    /// Convert the info contained within the [`WithInfo`].
    pub fn map_info<J>(self, f: impl FnOnce(I) -> J) -> WithInfo<J, T> {
        WithInfo {
            info: f(self.info),
            item: self.item,
        }
    }

    /// Like [`map`], but returns `None` if the conversion fails,
    pub fn filter_map<U>(self, f: impl FnOnce(T) -> Option<U>) -> Option<WithInfo<I, U>> {
        Some(WithInfo {
            info: self.info,
            item: f(self.item)?,
        })
    }

    /// Box the value contained within the [`WithInfo`].
    pub fn boxed(self) -> WithInfo<I, Box<T>> {
        self.map(Box::new)
    }

    /// Replace the value contained within the [`WithInfo`], cloning the
    /// [`info`](WithInfo::info).
    pub fn replace<U>(&self, new: U) -> WithInfo<I, U>
    where
        I: Clone,
    {
        WithInfo {
            info: self.info.clone(),
            item: new,
        }
    }

    /// Obtain a reference to the value contained within the [`WithInfo`],
    /// cloning the [`info`](WithInfo::info).
    pub fn as_ref(&self) -> WithInfo<I, &T>
    where
        I: Clone,
    {
        WithInfo {
            info: self.info.clone(),
            item: &self.item,
        }
    }

    /// Obtain a mutable reference to the value contained within the
    /// [`WithInfo`], cloning the [`info`](WithInfo::info).
    pub fn as_mut(&mut self) -> WithInfo<I, &mut T>
    where
        I: Clone,
    {
        WithInfo {
            info: self.info.clone(),
            item: &mut self.item,
        }
    }

    /// Dereference the value contained within the [`WithInfo`], cloning the
    /// [`info`](WithInfo::info).
    pub fn as_deref<'a>(&'a self) -> WithInfo<I, &'a T::Target>
    where
        I: Clone,
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
    pub fn as_deref_mut<'a>(&'a mut self) -> WithInfo<I, &'a mut T::Target>
    where
        I: Clone,
        T: DerefMut,
        T::Target: 'a,
    {
        WithInfo {
            info: self.info.clone(),
            item: self.item.deref_mut(),
        }
    }
}

impl<I, T> WithInfo<I, Box<T>> {
    /// Unbox the value contained within the [`WithInfo`].
    pub fn unboxed(self) -> WithInfo<I, T> {
        self.map(|value| *value)
    }
}
