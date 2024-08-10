pub trait OrThrowExt {
    type Result;

    fn or_throw(self, msg: &str) -> Self::Result;
    fn or_warn(self, msg: &str);
}

impl<T, E: std::fmt::Debug> OrThrowExt for Result<T, E> {
    type Result = T;

    #[track_caller]
    fn or_throw(self, msg: &str) -> T {
        let location = std::panic::Location::caller();

        self.unwrap_or_else(|err| {
            wasm_bindgen::throw_str(&format!("{}: {}: {:?}", location, msg, err));
        })
    }

    #[track_caller]
    fn or_warn(self, msg: &str) {
        let location = std::panic::Location::caller();

        if let Err(err) = self {
            web_sys::console::warn_1(&format!("{}: {}: {:?}", location, msg, err).into());
        }
    }
}

impl<T> OrThrowExt for Option<T> {
    type Result = T;

    #[track_caller]
    fn or_throw(self, msg: &str) -> T {
        let location = std::panic::Location::caller();

        self.unwrap_or_else(|| {
            wasm_bindgen::throw_str(&format!("{}: {}", location, msg));
        })
    }

    #[track_caller]
    fn or_warn(self, msg: &str) {
        let location = std::panic::Location::caller();

        if self.is_none() {
            web_sys::console::warn_1(&format!("{}: {}", location, msg).into());
        }
    }
}
