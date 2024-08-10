import { useEffect } from "react";
import { useParams, useNavigate } from "react-router-dom";
import { createLesson } from "../../models";
import { CircularProgress } from "wipple-playground";
import { getLesson } from "../../helpers";

export const LessonPage = () => {
    const params = useParams();
    const id = params.id!;
    const selectedPageId = params.page;

    const navigate = useNavigate();

    useEffect(() => {
        (async () => {
            const lesson = await getLesson(id);
            const playgroundId = await createLesson(lesson);

            let path = `/playground/edit/${playgroundId}`;
            if (selectedPageId) {
                path += `/${selectedPageId}`;
            }

            navigate(path);
        })();
    }, [id, selectedPageId]);

    return (
        <div className="flex flex-col items-center justify-center gap-4 w-full h-full">
            <CircularProgress />

            <p className="text-lg text-gray-500">Loading lesson...</p>
        </div>
    );
};
