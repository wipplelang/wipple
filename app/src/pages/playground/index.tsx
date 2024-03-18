import { Link, useNavigate, useParams } from "react-router-dom";
import { CodeEditor } from "./code-editor";
import { useEffect, useMemo, useState } from "react";
import { Animated, Skeleton, useAlert, useNavbar } from "../../components";
import { Playground, getPlayground } from "../../models";
import { MaterialSymbol } from "react-material-symbols";
import { defaultThemeConfig } from "./codemirror/theme";
import { produce } from "immer";

export const PlaygroundPage = () => {
    const params = useParams();
    const id = params.id!;
    const selectedPage = params.page;

    const navigate = useNavigate();

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
    }, [id]);

    useEffect(() => {
        if (!playground) {
            return;
        }

        if (!selectedPage) {
            navigate(`./${playground.pages[0].id}`, { replace: true });
        }
    }, [playground, selectedPage]);

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

    const [tempCode, setTempCode] = useState("");

    const theme = useMemo(() => defaultThemeConfig(), []);

    const { displayAlert } = useAlert();

    return (
        <div className="flex flex-row justify-center">
            <div className="flex flex-col items-center lg:items-start lg:flex-row w-full max-w-screen-lg">
                <ul className="flex flex-row gap-1 lg:flex-col w-full max-w-4xl lg:max-w-none lg:w-[240px] lg:max-h-[calc(100vh-140px)] overflow-scroll pt-3 px-4 pb-8 lg:pb-4">
                    {playground ? (
                        <>
                            {playground.pages.map((page) => {
                                const isActive = page.id === selectedPage;

                                return (
                                    <Link key={page.id} to={`../${page.id}`} relative="path">
                                        <li
                                            className={`lg:w-full px-3 py-1.5 rounded-lg text-nowrap lg:text-wrap transition-colors ${
                                                isActive
                                                    ? "bg-blue-500 hover:bg-blue-600 dark:hover:bg-blue-400 shadow-md shadow-blue-200 dark:shadow-blue-950 text-white font-semibold"
                                                    : "hover:bg-gray-100 active:bg-gray-200 dark:hover:bg-gray-900 dark:active:bg-gray-800"
                                            }`}
                                        >
                                            {page.name}
                                        </li>
                                    </Link>
                                );
                            })}

                            <button
                                className="flex flex-row items-center gap-1 lg:w-full px-1.5 py-1.5 rounded-lg hover:bg-gray-100 active:bg-gray-200 dark:hover:bg-gray-900 dark:active:bg-gray-800 transition-colors text-blue-500 text-nowrap lg:text-wrap"
                                onClick={() =>
                                    displayAlert(({ dismiss }) => (
                                        <AddPageAlert
                                            onAddPage={(page) =>
                                                setPlayground(
                                                    produce((playground) => {
                                                        playground!.pages.push(page);
                                                    }),
                                                )
                                            }
                                            dismiss={dismiss}
                                        />
                                    ))
                                }
                            >
                                <MaterialSymbol icon="add" size={22} /> Add Page
                            </button>
                        </>
                    ) : (
                        new Array(4)
                            .fill(null)
                            .map((_, index) => (
                                <Skeleton key={index} className="w-20 h-8 lg:w-full lg:h-10" />
                            ))
                    )}
                </ul>

                <div
                    className={`flex-1 flex flex-col items-stretch gap-4 container max-w-4xl px-4 pb-4 ${
                        playground ? "" : "pt-3"
                    }`}
                >
                    {playground ? (
                        <>
                            <CodeEditor onChange={setTempCode} theme={theme}>
                                {tempCode}
                            </CodeEditor>

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

const AddPageAlert = (props: {
    onAddPage: (page: import("../../models").PlaygroundPage) => void;
    dismiss: () => void;
}) => {
    return <p>TODO</p>;
};
