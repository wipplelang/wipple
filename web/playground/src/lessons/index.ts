import { Playground } from "../models";
import { learnToCodeLesson } from "./learn-to-code";

export interface Lesson extends Pick<Playground, "name" | "pages"> {
    description: string;
}

export const lessons = [learnToCodeLesson];
