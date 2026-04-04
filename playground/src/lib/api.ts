import axios, { type AxiosRequestConfig } from "axios";
import type { PlaygroundMetadata } from "./models/Playground";
import type { DocumentationItem } from "./models/Documentation";
import { PUBLIC_SERVER_URL } from "$env/static/public";

const request =
    <Request, Response>(type: string) =>
    (request: Request, config?: AxiosRequestConfig<any>) =>
        axios
            .post(PUBLIC_SERVER_URL, { [type]: request }, config)
            .then((response) => response.data as Response);

export type IdeInfoRequest = PlaygroundMetadata;

export interface IdeInfoResponse {
    info: Record<string, any>[];
}

export const ideInfo = request<IdeInfoRequest, IdeInfoResponse>("ideInfo");

export interface CompileRequest extends PlaygroundMetadata {
    code: string;
}

export type CompileResponse = CompileResponseSuccess | CompileResponseError;

export interface CompileResponseSuccess {
    graph: any;
    executable: string;
}

export interface CompileResponseError {
    graph: any;
    diagnostics: any[];
}

export const compile = request<CompileRequest, CompileResponse>("compile");

export interface FormatRequest {
    code: string;
}

export interface FormatResponse {
    code: string;
}

export const format = request<FormatRequest, FormatResponse>("format");

export type DocumentationRequest = PlaygroundMetadata;

export interface DocumentationResponse {
    items: Record<string, DocumentationItem>;
}

export const documentation = request<DocumentationRequest, DocumentationResponse>("documentation");
