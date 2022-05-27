// @ts-ignore
import sections from "./sections/index.yml";

export interface Section {
    title: string;
    path: string;
    pages: Page[];
}

export interface Page {
    title: string;
    path: string;
}

export default sections as Section[];
