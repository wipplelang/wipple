import axios, { type AxiosRequestConfig } from "axios";
import { PUBLIC_FUNCTIONS_URL } from "$env/static/public";
import type { RuntimeId } from "./runtimes";

const request =
    <Request, Response>(type: string) =>
    (request: Request, config?: AxiosRequestConfig<any>) =>
        axios
            .post(PUBLIC_FUNCTIONS_URL + "/share", { ...request, type }, config)
            .then((response) => response.data as Response);

export interface GetRequest {
    id: string;
}

export interface GetResponse {
    playground: {
        runtime: RuntimeId;
        code: string;
    };
}

export interface ShareRequest {
    runtime: RuntimeId;
    code: string;
}

export interface ShareResponse {
    id: string;
}

export const get = request<GetRequest, GetResponse>("get");
export const share = request<ShareRequest, ShareResponse>("share");
