import { useParams } from "react-router-dom";
import { CodeEditor } from "./code-editor";
import { useEffect, useState } from "react";
import { useNavbar } from "../../components";
import { Playground, getPlayground } from "../../models";
import Skeleton from "react-loading-skeleton";

export const PlaygroundPage = () => {
    const id = useParams().id!;

    const [playground, setPlayground] = useState<Playground>();

    useEffect(() => {
        (async () => {
            const result = await getPlayground(id);
            if (!result) {
                console.error(`no such playground ${id}`);
                // TODO: Show 404 page
                return;
            }

            setPlayground(result);
        })();
    }, []);

    const { setPrimaryActions } = useNavbar();

    useEffect(() => {
        setPrimaryActions(<p className="text-lg font-semibold">Wipple</p>);

        return () => {
            setPrimaryActions(null);
        };
    }, []);

    return (
        <div className="flex flex-row justify-center">
            <div className="flex flex-col items-stretch gap-4 container max-w-4xl px-4">
                {playground ? (
                    <CodeEditor />
                ) : (
                    <>
                        <Skeleton height={160} />
                        <Skeleton height={80} />
                        <Skeleton height={120} />
                        <Skeleton height={80} />
                    </>
                )}
            </div>
        </div>
    );
};
