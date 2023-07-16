import fm from "front-matter";

export const convertLesson = (text: string) => {
    const sections = [];
    do {
        const parsed = fm<any>(text);

        const index = parsed.body.indexOf("---");
        const value = parsed.body.slice(0, index).trim();

        sections.push({
            ...parsed.attributes,
            value,
        });

        if (index === -1) {
            break;
        }

        text = parsed.body.slice(index);
    } while (text);

    const options = sections.shift();
    delete options.value;

    return { ...options, sections };
};
