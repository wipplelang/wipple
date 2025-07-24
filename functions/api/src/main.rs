mod context;
mod libraries;
mod routes;

use aws_lambda_events::lambda_function_urls::LambdaFunctionUrlResponse;
use axum::{Json, Router, http, routing::post};
use lambda_runtime::{Error, LambdaEvent};
use serde::Deserialize;
use tower::service_fn;
use tower_http::trace::TraceLayer;
use tower_http::{compression::CompressionLayer, cors::CorsLayer};
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let lambda = std::env::var("LAMBDA_TASK_ROOT").is_ok();

    if lambda {
        #[derive(Deserialize)]
        struct LambdaRequest {
            body: String,
        }

        lambda_runtime::run(service_fn(
            async |event: LambdaEvent<LambdaRequest>| -> Result<LambdaFunctionUrlResponse, Error> {
                let (req, _context) = event.into_parts();
                let req = serde_json::from_str(&req.body)?;
                let response = routes::handle(req).await?;

                Ok(LambdaFunctionUrlResponse {
                    status_code: 200,
                    headers: Default::default(),
                    body: Some(serde_json::to_string(&response)?),
                    is_base64_encoded: false,
                    cookies: Default::default(),
                })
            },
        ))
        .await
        .unwrap();
    } else {
        let port = std::env::var("PORT")
            .expect("missing PORT")
            .parse()
            .unwrap();

        let router = Router::new()
            .route(
                "/",
                post(async |Json(req)| {
                    routes::handle(req)
                        .await
                        .map(Json)
                        .map_err(|e| (http::StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
                }),
            )
            .layer(CorsLayer::permissive())
            .layer(CompressionLayer::new())
            .layer(TraceLayer::new_for_http());

        let listener = tokio::net::TcpListener::bind(("0.0.0.0", port))
            .await
            .unwrap();

        axum::serve(listener, router).await.unwrap();
    }
}
