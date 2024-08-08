use serde::{Deserialize, Serialize};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

/// An item with information from the driver attached.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

impl<I, T> WithInfo<I, Option<T>> {
    /// Unwrap the [`Option`] value contained within the [`WithInfo`].
    pub fn try_unwrap(self) -> Option<WithInfo<I, T>> {
        self.item.map(|item| WithInfo {
            info: self.info,
            item,
        })
    }
}

impl<I, T> WithInfo<I, &T>
where
    I: Clone,
    T: Clone,
{
    /// Clone the value contained within the [`WithInfo`].
    pub fn cloned(&self) -> WithInfo<I, T> {
        WithInfo {
            info: self.info.clone(),
            item: self.item.clone(),
        }
    }
}

/// Like [`Default`], but for types wrapped in [`WithInfo`].
pub trait DefaultFromInfo<I>: Sized {
    /// Produce the default value of `Self` with the given info.
    fn default_from_info(info: I) -> WithInfo<I, Self>;
}

impl<I> DefaultFromInfo<I> for () {
    fn default_from_info(info: I) -> WithInfo<I, Self> {
        WithInfo { info, item: () }
    }
}

impl<I, A, B> DefaultFromInfo<I> for (WithInfo<I, A>, WithInfo<I, B>)
where
    I: Clone,
    A: DefaultFromInfo<I>,
    B: DefaultFromInfo<I>,
{
    fn default_from_info(info: I) -> WithInfo<I, Self> {
        WithInfo {
            info: info.clone(),
            item: (
                A::default_from_info(info.clone()),
                B::default_from_info(info),
            ),
        }
    }
}

impl<I, T> DefaultFromInfo<I> for Option<T> {
    fn default_from_info(info: I) -> WithInfo<I, Self> {
        WithInfo { info, item: None }
    }
}

impl<I, T> DefaultFromInfo<I> for Vec<WithInfo<I, T>> {
    fn default_from_info(info: I) -> WithInfo<I, Self> {
        WithInfo {
            info,
            item: Vec::new(),
        }
    }
}
