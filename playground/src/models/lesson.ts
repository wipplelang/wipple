import { Playground } from "./playground";

export interface Lesson extends Pick<Playground, "name" | "pages"> {
    id: string;
    description: string;
}
