mod compile;
mod documentation;
mod format;
mod ide_info;
mod libraries;

use crate::libraries::fetch_library;
use dashmap::{DashMap, Entry};
use lambda_http::{RequestPayloadExt, aws_lambda_events::apigw::ApiGatewayProxyResponse};
use serde::Deserialize;
use std::{env, sync::LazyLock};
use wipple::{
    database::{Db, NodeRef},
    driver::{self},
    syntax::parse,
};

#[tokio::main]
async fn main() {
    if env::var("LAMBDA_TASK_ROOT").is_ok() {
        lambda_runtime::run(
            #[allow(deprecated)]
            lambda_runtime::handler_fn(async |event: serde_json::Value, _ctx| {
                let body = event
                    .get("body")
                    .ok_or_else(|| anyhow::format_err!("missing request body"))?
                    .as_str()
                    .ok_or_else(|| anyhow::format_err!("expected JSON string"))?;

                let response = serde_json::from_str::<Request>(body)?.handle().await?;

                let mut proxy_response = ApiGatewayProxyResponse::default();

                proxy_response.status_code = 200;

                proxy_response.headers.insert(
                    "Content-Type",
                    lambda_http::http::HeaderValue::from_static("application/json"),
                );

                proxy_response.body =
                    Some(lambda_http::Body::Text(serde_json::to_string(&response)?));

                Ok::<_, lambda_runtime::Error>(proxy_response)
            }),
        )
        .await
    } else {
        lambda_http::run(lambda_http::service_fn(
            async |request: lambda_http::Request| {
                request
                    .payload::<Request>()?
                    .ok_or_else(|| anyhow::format_err!("missing request body"))?
                    .handle()
                    .await
            },
        ))
        .await
    }
    .expect("failed to start Lambda service")
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
enum Request {
    Compile(compile::Request),
    Documentation(documentation::Request),
    Format(format::Request),
    IdeInfo(ide_info::Request),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct InputMetadata {
    library: Option<String>,
}

impl Request {
    async fn handle(self) -> anyhow::Result<serde_json::Value> {
        match self {
            Request::Compile(request) => compile::handle(request).await,
            Request::Documentation(request) => documentation::handle(request).await,
            Request::Format(request) => format::handle(request).await,
            Request::IdeInfo(request) => ide_info::handle(request).await,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct File {
    path: String,
    code: String,
}

async fn compile(
    files: &[File],
    library_name: Option<&str>,
) -> anyhow::Result<(Db, Vec<NodeRef>, Vec<NodeRef>)> {
    let (mut db, lib_files) = match library_name {
        Some(library_name) => compile_library(library_name).await?,
        None => (Db::new(), Vec::new()),
    };

    let files = files
        .iter()
        .map(|file| parse(&mut db, &file.path, &file.code))
        .collect::<Vec<_>>();

    driver::compile(&mut db, &files);

    Ok((db, files, lib_files))
}

static LIBRARY_CACHE: LazyLock<DashMap<String, (Db, Vec<NodeRef>)>> =
    LazyLock::new(Default::default);

async fn compile_library(name: &str) -> anyhow::Result<(Db, Vec<NodeRef>)> {
    let entry = LIBRARY_CACHE.entry(name.to_string());

    match entry {
        Entry::Occupied(entry) => Ok(entry.get().clone()),
        Entry::Vacant(entry) => {
            let library = fetch_library(name).await?;

            let (db, mut files, lib_files) =
                Box::pin(compile(&library.files, library.metadata.library.as_deref())).await?;

            files.extend(lib_files);

            Ok(entry.insert((db, files)).clone())
        }
    }
}
