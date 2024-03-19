import { Link, useNavigate, useParams } from "react-router-dom";
import { CodeEditor } from "./code-editor";
import { useEffect, useLayoutEffect, useMemo, useRef, useState } from "react";
import { Animated, Button, Skeleton, useAlert, useNavbar } from "../../components";
import { Playground, PlaygroundPageItem, getPlayground, updatePlayground } from "../../models";
import { MaterialSymbol } from "react-material-symbols";
import { defaultThemeConfig } from "./codemirror/theme";
import { produce } from "immer";
import { turtleImage } from "../../runtimes/turtle";
import { nanoid } from "nanoid";
import { useDebounceCallback, useHover } from "usehooks-ts";
import { flushSync } from "react-dom";

export const PlaygroundPage = () => {
    const params = useParams();
    const id = params.id!;
    const selectedPageId = params.page;

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

    const savePlayground = useDebounceCallback(updatePlayground, 1000);

    useEffect(() => {
        if (playground) {
            (async () => {
                const handleOnBeforeUnload = (event: BeforeUnloadEvent) => {
                    event.returnValue = true;
                };

                window.addEventListener("beforeunload", handleOnBeforeUnload);
                await savePlayground(playground);
                window.removeEventListener("beforeunload", handleOnBeforeUnload);
            })();
        }
    }, [playground]);

    useEffect(() => {
        if (playground && !selectedPageId) {
            navigate(`./${playground.pages[0].id}`, { replace: true });
        }
    }, [playground, selectedPageId]);

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

    const playgroundPageIndex = (() => {
        if (!playground) {
            return undefined;
        }

        const index = playground.pages.findIndex((page) => page.id === selectedPageId);
        return index === -1 ? 0 : index;
    })();

    return (
        <div className="flex flex-row justify-center">
            <div className="flex flex-col items-center lg:items-start lg:flex-row w-full max-w-screen-lg">
                <PlaygroundPageList
                    playground={playground}
                    selectedPage={selectedPageId}
                    onAddPage={(name) => {
                        const id = nanoid(20);

                        setPlayground(
                            produce((playground) => {
                                playground!.pages.push({
                                    id,
                                    name,
                                    items: [],
                                });
                            }),
                        );

                        navigate(`../${id}`, { relative: "path" });
                    }}
                    onDeletePage={(pageIndex) => {
                        setPlayground(
                            produce((playground) => {
                                playground!.pages.splice(pageIndex, 1);

                                const newPageIndex = pageIndex === 0 ? 0 : pageIndex - 1;

                                navigate(`../${playground!.pages[newPageIndex].id}`, {
                                    relative: "path",
                                });
                            }),
                        );
                    }}
                />

                <PlaygroundPageEditor
                    items={
                        playgroundPageIndex != null
                            ? playground!.pages[playgroundPageIndex].items
                            : undefined
                    }
                    onAddItem={(item) => {
                        setPlayground(
                            produce((playground) => {
                                playground!.pages[playgroundPageIndex!].items.push(item);
                            }),
                        );
                    }}
                    onChangeItem={(index, item) => {
                        setPlayground(
                            produce((playground) => {
                                playground!.pages[playgroundPageIndex!].items[index] = item;
                            }),
                        );
                    }}
                    onMoveItemUp={(index) => {
                        setPlayground(
                            produce((playground) => {
                                const page = playground!.pages[playgroundPageIndex!];
                                const [item] = page.items.splice(index, 1);
                                page.items.splice(index - 1, 0, item);
                            }),
                        );
                    }}
                    onMoveItemDown={(index) => {
                        setPlayground(
                            produce((playground) => {
                                const page = playground!.pages[playgroundPageIndex!];
                                const [item] = page.items.splice(index, 1);
                                page.items.splice(index + 1, 0, item);
                            }),
                        );
                    }}
                    onDeleteItem={(index) => {
                        setPlayground(
                            produce((playground) => {
                                playground!.pages[playgroundPageIndex!].items.splice(index, 1);
                            }),
                        );
                    }}
                />
            </div>
        </div>
    );
};

const PlaygroundPageEditor = (props: {
    items?: PlaygroundPageItem[];
    onAddItem: (item: PlaygroundPageItem) => void;
    onChangeItem: (index: number, item: PlaygroundPageItem) => void;
    onMoveItemDown: (index: number) => void;
    onMoveItemUp: (index: number) => void;
    onDeleteItem: (index: number) => void;
}) => {
    // HACK: Prevent layout bugs by rendering one item at a time
    const [maxRenderIndex, setMaxRenderIndex] = useState(0);

    useLayoutEffect(() => {
        if (props.items && maxRenderIndex < props.items.length - 1) {
            requestAnimationFrame(() => {
                flushSync(() => {
                    setMaxRenderIndex(maxRenderIndex + 1);
                });
            });
        }
    }, [props.items, maxRenderIndex]);

    return (
        <div
            className={`flex-1 flex flex-col items-stretch gap-4 container max-w-4xl px-4 pb-4 ${
                props.items ? "" : "pt-3"
            }`}
        >
            {props.items ? (
                <>
                    {props.items.map((item, index) =>
                        index <= maxRenderIndex ? (
                            <PlaygroundPageItemEditor
                                key={index}
                                item={item}
                                onChange={(item) => props.onChangeItem(index, item)}
                                onMoveDown={() => props.onMoveItemDown(index)}
                                onMoveUp={() => props.onMoveItemUp(index)}
                                onDelete={() => props.onDeleteItem(index)}
                            />
                        ) : null,
                    )}

                    <AddPlaygroundPageItemButton onAddItem={props.onAddItem} />
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
    );
};

const AddPlaygroundPageItemButton = (props: { onAddItem: (item: PlaygroundPageItem) => void }) => {
    const [isHovering, setHovering] = useState(false);

    const { displayAlert } = useAlert();

    return (
        <button
            className="group flex items-center justify-center mt-3 p-1 w-full rounded-md border-2 border-gray-100 dark:border-gray-800 hover:border-blue-500 transition-colors"
            onMouseEnter={() => setHovering(true)}
            onMouseLeave={() => setHovering(false)}
            onClick={() =>
                displayAlert(({ dismiss }) => (
                    <AddPlaygroundPageItemAlert onAddItem={props.onAddItem} dismiss={dismiss} />
                ))
            }
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

const PlaygroundPageItemEditor = (props: {
    item: PlaygroundPageItem;
    onChange: (item: PlaygroundPageItem) => void;
    onMoveDown: () => void;
    onMoveUp: () => void;
    onDelete: () => void;
}) => {
    const theme = useMemo(() => defaultThemeConfig(), []);

    switch (props.item.type) {
        case "code":
            return (
                <CodeEditor
                    onChange={(code) =>
                        props.onChange(
                            produce(props.item, (item) => {
                                if (item.type !== "code") {
                                    return;
                                }

                                item.code = code;
                            }),
                        )
                    }
                    theme={theme}
                    runtime={"setup" in props.item ? props.item.setup : undefined}
                >
                    {props.item.code}
                </CodeEditor>
            );
        case "text":
            break;
        default:
            return null;
    }
};

const PlaygroundPageList = (props: {
    playground?: Playground;
    selectedPage?: string;
    onAddPage: (name: string) => void;
    onDeletePage: (pageIndex: number) => void;
}) => {
    const { displayAlert } = useAlert();

    return (
        <ul className="flex flex-shrink-0 flex-row gap-1 lg:flex-col max-w-4xl lg:max-w-none w-full lg:w-[240px] lg:max-h-[calc(100vh-140px)] overflow-scroll px-4 pb-8 lg:pb-4">
            {props.playground ? (
                <>
                    {props.playground.pages.map((page, pageIndex) => {
                        const isActive = page.id === props.selectedPage;

                        return (
                            <Link key={page.id} to={`../${page.id}`} relative="path">
                                <li
                                    className={`flex flex-row items-center justify-between lg:w-full px-3 py-1.5 rounded-lg transition-colors ${
                                        isActive
                                            ? "bg-blue-500 hover:bg-blue-600 dark:hover:bg-blue-400 shadow-md shadow-blue-200 dark:shadow-blue-950 text-white font-semibold"
                                            : "hover:bg-gray-100 active:bg-gray-200 dark:hover:bg-gray-900 dark:active:bg-gray-800"
                                    }`}
                                >
                                    <p className="text-nowrap lg:text-wrap">{page.name}</p>

                                    {isActive && props.playground!.pages.length > 1 ? (
                                        <button
                                            className="flex items-center justify-center"
                                            onClick={() =>
                                                displayAlert(({ dismiss }) => (
                                                    <ManagePlaygroundPageAlert
                                                        onDeletePage={() =>
                                                            props.onDeletePage(pageIndex)
                                                        }
                                                        dismiss={dismiss}
                                                    />
                                                ))
                                            }
                                        >
                                            <MaterialSymbol icon="more_horiz" size={22} />
                                        </button>
                                    ) : null}
                                </li>
                            </Link>
                        );
                    })}

                    <button
                        className="flex flex-row items-center gap-1 lg:w-full px-1.5 py-1.5 rounded-lg hover:bg-gray-100 active:bg-gray-200 dark:hover:bg-gray-900 dark:active:bg-gray-800 transition-colors text-blue-500 text-nowrap lg:text-wrap"
                        onClick={() => {
                            const name = prompt("Enter page name:");
                            if (name) {
                                props.onAddPage(name);
                            }
                        }}
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
    );
};

const AddPlaygroundPageItemAlert = (props: {
    onAddItem: (item: PlaygroundPageItem) => void;
    dismiss: () => void;
}) => (
    <div className="flex flex-col gap-4 w-[512px]">
        <div className="flex flex-col gap-2">
            <h1 className="text-2xl font-semibold">What would you like to create?</h1>

            <div className="grid grid-cols-3 auto-rows-max gap-4">
                <AddPlaygroundPageItemAlertButton
                    setup={undefined}
                    name="Blank"
                    description="Create a program from scratch."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="turtle"
                    name="Turtle"
                    description="Draw graphics on the screen."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "turtle",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="music"
                    name="Music"
                    description="Make a musical composition."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "music",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="graphing"
                    name="Math"
                    description="Plot mathematical functions."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "math",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="game"
                    name="Game"
                    description="Create a video game."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "game",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="physics"
                    name="Physics"
                    description="Experiment with physics."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "physics",
                        });

                        props.dismiss();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="text"
                    name="Text"
                    description="Write text alongside your code."
                    onSelect={() => {
                        props.onAddItem({
                            type: "text",
                            text: "",
                        });

                        props.dismiss();
                    }}
                />
            </div>
        </div>

        <Button role="secondary" fill onClick={props.dismiss}>
            Cancel
        </Button>
    </div>
);

const AddPlaygroundPageItemAlertButton = (props: {
    setup: string | undefined;
    name: string;
    description: string;
    onSelect: () => void;
}) => (
    <button
        className="group bg-gray-50 dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 rounded-lg p-3 transition-colors"
        onClick={props.onSelect}
    >
        <div className="flex flex-col items-start gap-1 w-full h-full">
            <div className="group-hover:scale-110 transition-transform">
                <AddPlaygroundPageItemAlertButtonIcon setup={props.setup} size="large" />
            </div>

            <div className="flex flex-col normal-case text-left ui-font tracking-normal">
                <p className="font-bold">{props.name}</p>
                <p className="text-sm opacity-75">{props.description}</p>
            </div>
        </div>
    </button>
);

const AddPlaygroundPageItemAlertButtonIcon = (props: {
    setup: string | undefined;
    size: "medium" | "large";
}) => {
    const size = props.size === "large" ? 31 : 24;
    const sizeClass = props.size === "large" ? "w-[31px]" : "w-[24px]";

    switch (props.setup) {
        case undefined:
            return (
                <MaterialSymbol icon="article" size={size} fill className="text-blue-500 pb-0.5" />
            );
        case "turtle":
            return (
                <img src={turtleImage} className={`${sizeClass} p-[2px] aspect-square mb-1.5`} />
            );
        case "music":
            return (
                <MaterialSymbol
                    icon="music_note"
                    size={size}
                    fill
                    className="text-orange-500 pb-0.5"
                />
            );
        case "graphing":
            return (
                <MaterialSymbol
                    icon="calculate"
                    size={size}
                    fill
                    className="text-green-500 pb-0.5"
                />
            );
        case "game":
            return (
                <MaterialSymbol
                    icon="sports_esports"
                    size={size}
                    fill
                    className="text-purple-500 pb-0.5"
                />
            );
        case "physics":
            return (
                <img
                    // src={atomIcon} // TODO: Physics runtime
                    className={`${sizeClass} p-[2px] aspect-square mb-1.5`}
                />
            );
        case "text":
            return <MaterialSymbol icon="edit_note" size={size} className="text-gray-500 pb-0.5" />;
        default:
            return null;
    }
};

const ManagePlaygroundPageAlert = (props: { onDeletePage: () => void; dismiss: () => void }) => (
    <div className="flex flex-col gap-2">
        <Button
            role="destructive"
            icon="delete"
            onClick={() => {
                props.dismiss();
                props.onDeletePage();
            }}
        >
            Delete
        </Button>

        <Button
            role="secondary"
            fill={false}
            onClick={() => {
                props.dismiss();
            }}
        >
            Cancel
        </Button>
    </div>
);
