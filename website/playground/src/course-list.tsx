const courses = [
    {
        id: "learn-to-code",
        title: "Learn to code",
        description: "Learn coding with Wipple by making drawings and music.",
    },
    {
        id: "intermediate-wipple",
        title: "Intermediate Wipple",
        description:
            "Explore how to use functions, patterns and types to write more complex programs.",
    },
    {
        id: "advanced-wipple",
        title: "Advanced Wipple",
        description:
            "Use Wipple's advanced features to describe and manage your own data. At the end, you'll make your own video game.",
    },
    {
        id: "expert-wipple",
        title: "Expert Wipple",
        description: "Learn how Wipple works under the hood so you can design custom lessons.",
    },
];

export const CourseList = () => (
    <div className="flex flex-col mx-auto gap-4 w-full max-w-4xl mt-20 px-6 pb-6">
        <h1 className="text-4xl font-bold text-black dark:text-white">Courses</h1>

        {courses.map((course, index) => (
            <a
                key={index}
                href={`./?course=${course.id}`}
                className="relative flex max-w-none border-2 border-gray-100 dark:border-gray-700 rounded-lg p-4 overflow-clip group"
            >
                <img
                    src="./images/lesson-bg.svg"
                    className="absolute inset-0 w-full h-full object-cover -z-10 group-hover:scale-105 transition-transform"
                />

                <div className="flex flex-col gap-4 m-4 text-black dark:text-white">
                    <h1 className="text-3xl font-bold">{course.title}</h1>
                    <p className="text-xl opacity-50">{course.description}</p>
                </div>
            </a>
        ))}
    </div>
);
