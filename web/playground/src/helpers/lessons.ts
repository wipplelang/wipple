import { Lesson } from "wipple-playground";

export const getLessons = async (): Promise<Lesson[]> => {
    const index = await fetch("/playground/lessons/index.json").then((response) => response.json());
    return Promise.all(index.map(getLesson));
};

export const getLesson = (id: string): Promise<Lesson> =>
    fetch(`/playground/lessons/${id}.json`).then((response) => response.json());
