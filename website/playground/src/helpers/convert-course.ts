import fm from "front-matter";
import { Page } from "../app";
import { nanoid } from "nanoid";

export const convertCourse = (text: string): Page[] => {
    const pages: Page[] = [{ title: "", sections: [] }];

    do {
        const parsed = fm<any>(text);

        const index = parsed.body.indexOf("---");
        const value = parsed.body.slice(0, index).trim();

        if (pages[pages.length - 1].title !== parsed.attributes.page) {
            [pages.push({ title: "", sections: [] })];
        }

        pages[pages.length - 1].title = parsed.attributes.page;
        pages[pages.length - 1].sections.push({
            ...parsed.attributes,
            id: nanoid(8),
            value,
        });

        if (index === -1) {
            break;
        }

        text = parsed.body.slice(index);
    } while (text);

    if (pages.length > 1 && pages[0].title === "") {
        pages.shift();
    }

    return pages;
};
