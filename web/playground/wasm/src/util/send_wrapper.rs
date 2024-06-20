use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub struct SendWrapper<T>(send_wrapper::SendWrapper<T>);

impl<T> SendWrapper<T> {
    pub fn new(value: T) -> Self {
        SendWrapper(send_wrapper::SendWrapper::new(value))
    }

    #[track_caller]
    pub fn into_inner(self) -> T {
        self.0.take()
    }
}

impl<T> std::ops::Deref for SendWrapper<T> {
    type Target = T;

    #[track_caller]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Serialize for SendWrapper<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (*self.0).serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for SendWrapper<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(SendWrapper::new(T::deserialize(deserializer)?))
    }
}

impl<T> std::future::IntoFuture for SendWrapper<T>
where
    T: std::future::Future,
{
    type Output = T::Output;
    type IntoFuture = send_wrapper::SendWrapper<T>;

    fn into_future(self) -> Self::IntoFuture {
        self.0
    }
}
