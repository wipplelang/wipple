use crate::database::{Db, NodeRef};
use std::{
    fmt::{self, Display},
    sync::Arc,
};

pub trait Render {
    fn write(&self, w: &mut dyn fmt::Write, db: &Db) -> fmt::Result {
        let _ = w;
        let _ = db;
        Err(fmt::Error)
    }

    fn link(&self) -> Option<&NodeRef> {
        None
    }

    fn to_string(&self, db: &Db) -> String {
        let mut buf = String::new();
        self.write(&mut buf, db).unwrap();
        buf
    }
}

#[derive(Clone)]
pub struct RenderConfig {
    write: Arc<dyn Fn(&Db, &dyn Render, &mut fmt::Formatter<'_>) -> fmt::Result + Send + Sync>,
}

impl RenderConfig {
    pub fn new(
        write: impl Fn(&Db, &dyn Render, &mut fmt::Formatter<'_>) -> fmt::Result + Send + Sync + 'static,
    ) -> Self {
        RenderConfig {
            write: Arc::new(write),
        }
    }

    pub fn wrap<'a>(&self, db: &'a Db, value: &'a dyn Render) -> impl Display + use<'a> {
        struct Wrapped<'a> {
            db: &'a Db,
            render: RenderConfig,
            value: &'a dyn Render,
        }

        impl<'a> Display for Wrapped<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                (self.render.write)(self.db, self.value, f)
            }
        }

        Wrapped {
            db,
            render: self.clone(),
            value,
        }
    }
}

impl Default for RenderConfig {
    fn default() -> Self {
        RenderConfig {
            write: Arc::new(|_, _, _| unimplemented!()),
        }
    }
}
