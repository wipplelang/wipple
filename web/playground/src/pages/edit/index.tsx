import { Link, useBeforeUnload, useNavigate, useParams } from "react-router-dom";
import { CodeEditor } from "./code-editor";
import { useCallback, useEffect, useLayoutEffect, useMemo, useState } from "react";
import {
    Animated,
    Button,
    ContextMenuButton,
    Skeleton,
    TutorialItem,
    TutorialSection,
    useAlert,
    useNavbar,
} from "../../components";
import {
    Playground,
    PlaygroundPageItem,
    getPlayground,
    hexagonTutorialItem,
    newPlaygroundTutorialItem,
    updatePlayground,
    useOnTutorialAction,
} from "../../models";
import { MaterialSymbol } from "react-material-symbols";
import { defaultThemeConfig } from "./codemirror/theme";
import { produce } from "immer";
import { nanoid } from "nanoid";
import { useDebounceCallback } from "usehooks-ts";
import { flushSync } from "react-dom";
import { SetupIcon } from "./setup-icon";

export const EditPage = () => {
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

    const savePlayground = useDebounceCallback(async (playground: Playground) => {
        await updatePlayground(playground);
    }, 1000);

    useEffect(() => {
        if (playground) {
            savePlayground(playground);
        }
    }, [playground]);

    useBeforeUnload(() => {
        if (playground) {
            updatePlayground(playground);
        }
    });

    useEffect(() => {
        if (playground && !selectedPageId) {
            navigate(`./${playground.pages[0].id}`, { replace: true });
        }
    }, [playground, selectedPageId]);

    const { setPrimaryActions } = useNavbar();

    const rename = useCallback(() => {
        const name = prompt("Enter playground name:", playground?.name);
        if (name) {
            setPlayground(
                produce((playground) => {
                    if (!playground) return;
                    playground.name = name;
                }),
            );
        }
    }, []);

    useEffect(() => {
        setPrimaryActions(
            playground?.name ? (
                <button
                    className="flex flex-row items-center gap-4 -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                    onClick={rename}
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

    useOnTutorialAction(
        "newPlayground",
        () => {
            setPlayground(
                produce((playground) => {
                    const items = playground?.pages[0]?.items;
                    if (items) {
                        console.log("adding newPlaygroundTutorialItem");
                        items.push(newPlaygroundTutorialItem);
                    }
                }),
            );

            return playground?.pages[0]?.items != null;
        },
        [playground],
    );

    useOnTutorialAction(
        "hexagonCode",
        () => {
            let ran = false;
            setPlayground(
                produce((playground) => {
                    const items = playground?.pages[0]?.items;
                    if (items) {
                        items[0] = hexagonTutorialItem;
                        ran = true;
                    }
                }),
            );

            return ran;
        },
        [playground],
    );

    return (
        <div className="flex flex-col">
            <div className="w-full max-w-screen-lg mx-auto px-4 pb-8">
                <TutorialSection />
            </div>

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
                        onRenamePage={(pageIndex, name) => {
                            setPlayground(
                                produce((playground) => {
                                    playground!.pages[pageIndex].name = name;
                                }),
                            );
                        }}
                        onMovePageUp={(index) => {
                            setPlayground(
                                produce((playground) => {
                                    const [page] = playground!.pages.splice(index, 1);
                                    playground!.pages.splice(index - 1, 0, page);
                                }),
                            );
                        }}
                        onMovePageDown={(index) => {
                            setPlayground(
                                produce((playground) => {
                                    const [page] = playground!.pages.splice(index, 1);
                                    playground!.pages.splice(index + 1, 0, page);
                                }),
                            );
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
                        id={selectedPageId}
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
        </div>
    );
};

const PlaygroundPageEditor = (props: {
    id?: string;
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
        <div className="flex-1 flex flex-col items-stretch gap-4 container max-w-4xl px-4 pb-4">
            {props.id && props.items ? (
                props.items.length > 0 ? (
                    <>
                        {props.items.map((item, index) =>
                            index <= maxRenderIndex ? (
                                <TutorialItem
                                    key={`${props.id!}-${index}`}
                                    id={index === 0 ? "playgroundCodeEditor" : undefined}
                                >
                                    <PlaygroundPageItemEditor
                                        item={item}
                                        onChange={(item) => props.onChangeItem(index, item)}
                                        onMoveUp={
                                            index > 0 ? () => props.onMoveItemUp(index) : undefined
                                        }
                                        onMoveDown={
                                            index < props.items!.length - 1
                                                ? () => props.onMoveItemDown(index)
                                                : undefined
                                        }
                                        onDelete={() => props.onDeleteItem(index)}
                                    />
                                </TutorialItem>
                            ) : null,
                        )}

                        <AddPlaygroundPageItemButton onAddItem={props.onAddItem} />
                    </>
                ) : (
                    <div className="p-4 rounded-lg border-2 border-gray-100 dark:border-gray-800">
                        <AddPlaygroundPageItemAlert
                            gridClassName="grid-cols-2 md:grid-cols-3 lg:grid-cols-4"
                            onAddItem={props.onAddItem}
                        />
                    </div>
                )
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
                    <div className="w-[512px]">
                        <AddPlaygroundPageItemAlert
                            gridClassName="grid-cols-3"
                            onAddItem={props.onAddItem}
                            dismiss={dismiss}
                        />
                    </div>
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
    onMoveUp?: () => void;
    onMoveDown?: () => void;
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
                    onMoveUp={props.onMoveUp}
                    onMoveDown={props.onMoveDown}
                    onDelete={props.onDelete}
                    theme={theme}
                    runtime={
                        "setup" in props.item
                            ? {
                                  name: props.item.setup,
                                  settings: props.item.settings,
                                  onChangeSettings: (settings) => {
                                      props.onChange(
                                          produce(props.item, (item) => {
                                              (item as any).settings = settings;
                                          }),
                                      );
                                  },
                              }
                            : undefined
                    }
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
    onRenamePage: (pageIndex: number, name: string) => void;
    onMovePageUp: (pageIndex: number) => void;
    onMovePageDown: (pageIndex: number) => void;
    onDeletePage: (pageIndex: number) => void;
}) => (
    <TutorialItem id="playgroundPageList" className="w-full max-w-4xl lg:w-fit lg:max-w-none">
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

                                    {isActive ? (
                                        <ContextMenuButton
                                            className="flex items-center justify-center"
                                            items={[
                                                {
                                                    title: "Rename",
                                                    icon: "edit",
                                                    onClick: () => {
                                                        const name = prompt(
                                                            "Enter page name:",
                                                            page.name,
                                                        );

                                                        if (name) {
                                                            props.onRenamePage(pageIndex, name);
                                                        }
                                                    },
                                                },
                                                {
                                                    title: "Move Up",
                                                    icon: "arrow_upward",
                                                    disabled: pageIndex === 0,
                                                    onClick: () => props.onMovePageUp(pageIndex),
                                                },
                                                {
                                                    title: "Move Down",
                                                    icon: "arrow_downward",
                                                    disabled:
                                                        pageIndex ===
                                                        props.playground!.pages.length - 1,
                                                    onClick: () => props.onMovePageDown(pageIndex),
                                                },
                                                {
                                                    title: "Delete",
                                                    icon: "delete",
                                                    role: "destructive",
                                                    disabled: props.playground!.pages.length <= 1,
                                                    onClick: () => props.onDeletePage(pageIndex),
                                                },
                                            ]}
                                        >
                                            <MaterialSymbol icon="more_horiz" size={22} />
                                        </ContextMenuButton>
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
    </TutorialItem>
);

const AddPlaygroundPageItemAlert = (props: {
    onAddItem: (item: PlaygroundPageItem) => void;
    gridClassName: string;
    dismiss?: () => void;
}) => (
    <div className="flex flex-col gap-4">
        <div className="flex flex-col gap-2">
            <h1 className="text-2xl font-semibold">What would you like to create?</h1>

            <div className={`grid auto-rows-max gap-4 ${props.gridClassName}`}>
                <AddPlaygroundPageItemAlertButton
                    setup={undefined}
                    name="Blank"
                    description="Create a program from scratch."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                        });

                        props.dismiss?.();
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

                        props.dismiss?.();
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

                        props.dismiss?.();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="math"
                    name="Math"
                    description="Plot functions on a graph."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "math",
                        });

                        props.dismiss?.();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    disabled // TODO
                    setup="game"
                    name="Game"
                    description="Create a video game."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "game",
                        });

                        props.dismiss?.();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    disabled // TODO
                    setup="physics"
                    name="Physics"
                    description="Experiment with physics."
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "physics",
                        });

                        props.dismiss?.();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    disabled // TODO
                    setup="text"
                    name="Text"
                    description="Write text alongside your code."
                    onSelect={() => {
                        props.onAddItem({
                            type: "text",
                            text: "",
                        });

                        props.dismiss?.();
                    }}
                />
            </div>
        </div>

        {props.dismiss ? (
            <Button role="secondary" fill onClick={props.dismiss}>
                Cancel
            </Button>
        ) : null}
    </div>
);

const AddPlaygroundPageItemAlertButton = (props: {
    disabled?: boolean;
    setup: string | undefined;
    name: string;
    description: string;
    onSelect: () => void;
}) => (
    <button
        disabled={props.disabled}
        className="group bg-gray-50 dark:bg-gray-800 disabled:opacity-50 enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-700 rounded-lg p-3 transition-colors"
        onClick={props.onSelect}
    >
        <div className="flex flex-col items-start gap-1 w-full h-full">
            <div className="enabled:group-hover:scale-110 transition-transform">
                <SetupIcon setup={props.setup} size="lg" />
            </div>

            <div className="flex flex-col normal-case text-left ui-font tracking-normal">
                <p className="font-bold">{props.name}</p>
                <p className="text-sm opacity-75">{props.description}</p>
            </div>
        </div>
    </button>
);
