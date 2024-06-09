import { Playground } from "../models";
import { learnToCodeLesson } from "./learn-to-code";
import { intermediateWippleLesson } from "./intermediate-wipple";
import { learnPhysicsLesson } from "./learn-physics";
import { wippleByExampleLesson } from "./wipple-by-example";

export interface Lesson extends Pick<Playground, "name" | "pages"> {
    id: string;
    description: string;
}

export const lessons = [
    learnToCodeLesson,
    intermediateWippleLesson,
    learnPhysicsLesson,
    wippleByExampleLesson,
];
