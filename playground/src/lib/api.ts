import axios, { type AxiosRequestConfig } from "axios";
import type { PlaygroundMetadata } from "./models/Playground";
import { PUBLIC_SERVER_URL } from "$env/static/public";

const request =
    <Request, Response>(type: string) =>
    (request: Request, config?: AxiosRequestConfig<any>) =>
        axios
            .post(PUBLIC_SERVER_URL, { [type]: request }, config)
            .then((response) => response.data as Response);

type IdeInfoRequest = PlaygroundMetadata;

interface IdeInfoResponse {
    info: Record<string, any>[];
}

export const ideInfo = request<IdeInfoRequest, IdeInfoResponse>("ide-info");

interface CompileRequest extends PlaygroundMetadata {
    code: string;
}

type CompileResponse = CompileResponseSuccess | CompileResponseError;

interface CompileResponseSuccess {
    success: true;
    executable: any;
}

interface CompileResponseError {
    success: false;
    diagnostics: any[];
}

export const compile = request<CompileRequest, CompileResponse>("compile");

interface FormatRequest {
    code: string;
}

interface FormatResponse {
    code: string;
}

export const format = request<FormatRequest, FormatResponse>("format");

interface DocumentationRequest extends PlaygroundMetadata {
    name: string;
}

interface DocumentationResponse {
    documentation: Record<string, any> | null;
}

export const documentation = request<DocumentationRequest, DocumentationResponse>("documentation");
