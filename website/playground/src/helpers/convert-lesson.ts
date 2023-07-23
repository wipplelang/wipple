import fm from "front-matter";

export const convertLesson = (text: string) => {
    const sections: any[] = [];
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

    let options;
    if (sections[0]?.id == null) {
        options = sections.shift();
        delete options.value;
    } else {
        options = {};
    }

    return { ...options, sections };
};
