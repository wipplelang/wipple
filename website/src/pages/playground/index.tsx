import { useParams } from "react-router-dom";
import { CodeEditor } from "./code-editor";
import { useEffect, useState } from "react";
import { Animated, useNavbar } from "../../components";
import { Playground, getPlayground } from "../../models";
import Skeleton from "react-loading-skeleton";
import { MaterialSymbol } from "react-material-symbols";

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
        setPrimaryActions(
            playground?.name ? (
                <button
                    className="flex flex-row items-center gap-4 -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                    onClick={() => alert("TODO: Rename")}
                >
                    <p className="text-lg font-semibold">{playground.name}</p>
                </button>
            ) : null,
        );

        return () => {
            setPrimaryActions(null);
        };
    }, [playground?.name]);

    const [tempCode, setTempCode] = useState(`fib :: Number -> Number
fib : n -> when n (
    0 or 1 -> 1
    _ -> fib (n - 2) + fib (n - 1)
)

show (fib 5)`);

    return (
        <div className="flex flex-row justify-center">
            <div className="flex flex-col items-stretch gap-4 container max-w-4xl px-4">
                {playground ? (
                    <>
                        <CodeEditor onChange={setTempCode}>{tempCode}</CodeEditor>
                        <AddButton />
                    </>
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

const AddButton = () => {
    const [isHovering, setHovering] = useState(false);

    return (
        <button
            className="group flex items-center justify-center p-1 w-full rounded-md border-2 border-gray-100 dark:border-gray-800 hover:border-blue-500 transition-colors"
            onMouseEnter={() => setHovering(true)}
            onMouseLeave={() => setHovering(false)}
            onClick={() => {}}
        >
            <MaterialSymbol
                icon="add"
                className="text-xl text-gray-400 dark:text-gray-600 group-hover:text-blue-500 transition-colors"
            />

            <Animated direction="horizontal" open={isHovering}>
                <p className="text-blue-500 whitespace-nowrap">Add Section</p>
            </Animated>
        </button>
    );
};