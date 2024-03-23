const transformer = {
    process: () => ({
        code: `
            import path from "path";
            await import(path.resolve(process.cwd(), "./src/run-test"));
        `,
    }),
};

export default transformer;
