import { Playground } from "../models";
import { learnToCodeLesson } from "./learn-to-code";
import { intermediateWippleLesson } from "./intermediate-wipple";
import { learnPhysicsLesson } from "./learn-physics";

export interface Lesson extends Pick<Playground, "name" | "pages"> {
    description: string;
}

export const lessons = [learnToCodeLesson, intermediateWippleLesson, learnPhysicsLesson];
