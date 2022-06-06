use lsp_server::{ExtractError, Notification, Request, RequestId};

#[macro_export]
macro_rules! match_req {
    ($connection:ident, match $req:ident {
        $($($kind:ident)::+($id:tt, $params:ident) => $block:block)*
    }) => {
        $(
            #[allow(unused)]
            match $crate::macros::cast_req::<$($kind)::+>($req) {
                Ok(($id, $params)) => {
                    let result = $block;

                    let result = serde_json::to_value(&result).unwrap();
                    let resp = lsp_server::Response {
                        id: $id,
                        result: Some(result),
                        error: None,
                    };

                    $connection.sender.send(lsp_server::Message::Response(resp))?;

                    continue;
                }
                Err(err @ lsp_server::ExtractError::JsonError { .. }) => panic!("{:?}", err),
                Err(lsp_server::ExtractError::MethodMismatch(req)) => {
                    $req = req;
                }
            }
        )*
    };
    ($req:expr, {}) => {{
        let _ = $req;
    }};
}

#[macro_export]
macro_rules! match_notif {
    (match $notif:ident {
        $($($kind:ident)::+($params:ident) => $block:block)*
    }) => {
        $(
            #[allow(unused)]
            match $crate::macros::cast_notif::<$($kind)::+>($notif) {
                Ok($params) => {
                    $block;
                    continue;
                }
                Err(err @ lsp_server::ExtractError::JsonError { .. }) => panic!("{:?}", err),
                Err(lsp_server::ExtractError::MethodMismatch(notif)) => {
                    $notif = notif;
                }
            }
        )*
    };
    ($req:expr, {}) => {{
        let _ = $req;
    }};
}

pub fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

pub fn cast_notif<N>(notif: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}
