import React, { useCallback, useEffect, useLayoutEffect, useMemo, useState } from "react";
import {
    Animated,
    Button,
    ContextMenuButton,
    Skeleton,
    TutorialItem,
    TutorialSection,
    useAlert,
    useNavbar,
    useOnTutorialAction,
} from "../components";
import {
    Playground,
    PlaygroundPageItem,
    hexagonTutorialItem,
    newPlaygroundTutorialItem,
} from "../models";
import { MaterialSymbol } from "react-material-symbols";
import { defaultThemeConfig } from "./codemirror/theme";
import { produce } from "immer";
import { nanoid } from "nanoid";
import { flushSync } from "react-dom";
import { SetupIcon } from "./setup-icon";
import { TextEditor } from "./text-editor";
import { CodeEditor } from "./code-editor";

export const Editor = (props: {
    wipple: typeof import("wipple-wasm");
    playground?: Playground;
    setPlayground: React.Dispatch<React.SetStateAction<Playground | undefined>>;
    selectedPageId?: string;
    onSelectPage: (id: string) => void;
    onMakePublic?: () => void;
}) => {
    useEffect(() => {
        if (props.playground && !props.selectedPageId) {
            props.onSelectPage(props.playground.pages[0].id);
        }
    }, [props.playground, props.selectedPageId]);

    const { setPrimaryActions } = useNavbar();

    const rename = useCallback(() => {
        const name = prompt("Enter playground name:", props.playground?.name);
        if (name) {
            props.setPlayground(
                produce((playground) => {
                    if (!playground) return;
                    playground.name = name;
                }),
            );
        }
    }, []);

    const exportAsJson = useCallback(() => {
        if (!props.playground) return;

        (async () => {
            const json = JSON.stringify(props.playground, null, 4);
            await navigator.clipboard.writeText(json);
            alert("JSON copied to clipboard.");
        })();
    }, [props.playground]);

    const toggleLock = useCallback(() => {
        props.setPlayground(
            produce((playground) => {
                if (!playground) return;

                playground.locked = !playground.locked;
            }),
        );
    }, []);

    useEffect(() => {
        setPrimaryActions(
            props.playground?.name ? (
                <ContextMenuButton
                    className="flex flex-row items-center gap-4 -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                    items={[
                        {
                            title: "Rename",
                            icon: "edit",
                            onClick: rename,
                        },
                        {
                            title: props.playground.locked ? "Unlock Editing" : "Lock Editing",
                            icon: props.playground.locked ? "lock_open" : "lock",
                            onClick: toggleLock,
                        },
                        props.onMakePublic
                            ? {
                                  title: "Make Public",
                                  icon: "public",
                                  onClick: props.onMakePublic,
                              }
                            : undefined,
                        {
                            title: "Export as JSON",
                            icon: "data_object",
                            onClick: exportAsJson,
                        },
                    ]}
                >
                    <p className="text-lg font-semibold">{props.playground.name}</p>
                </ContextMenuButton>
            ) : null,
        );

        return () => {
            setPrimaryActions(null);
        };
    }, [props.playground?.name, props.playground?.locked]);

    const playgroundPageIndex = (() => {
        if (!props.playground) {
            return undefined;
        }

        const index = props.playground.pages.findIndex((page) => page.id === props.selectedPageId);
        return index === -1 ? 0 : index;
    })();

    useOnTutorialAction(
        "newPlayground",
        () => {
            props.setPlayground(
                produce((playground) => {
                    const items = playground?.pages[0]?.items;
                    if (items) {
                        items.push(newPlaygroundTutorialItem);
                    }
                }),
            );

            return props.playground?.pages[0]?.items != null;
        },
        [props.playground],
    );

    useOnTutorialAction(
        "hexagonCode",
        () => {
            let ran = false;
            props.setPlayground(
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
        [props.playground],
    );

    return (
        <div className="flex flex-col">
            <div className="w-full max-w-screen-lg mx-auto px-4 pb-8">
                <TutorialSection />
            </div>

            <div className="flex flex-row justify-center">
                <div className="flex flex-col items-center lg:items-start lg:flex-row w-full max-w-screen-lg">
                    <PlaygroundPageList
                        playground={props.playground}
                        selectedPage={props.selectedPageId}
                        onSelectPage={props.onSelectPage}
                        onAddPage={(name) => {
                            const id = nanoid(20);

                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages.push({
                                        id,
                                        name,
                                        items: [],
                                    });
                                }),
                            );

                            props.onSelectPage(id);
                        }}
                        onRenamePage={(pageIndex, name) => {
                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages[pageIndex].name = name;
                                }),
                            );
                        }}
                        onMovePageUp={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    const [page] = playground!.pages.splice(index, 1);
                                    playground!.pages.splice(index - 1, 0, page);
                                }),
                            );
                        }}
                        onMovePageDown={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    const [page] = playground!.pages.splice(index, 1);
                                    playground!.pages.splice(index + 1, 0, page);
                                }),
                            );
                        }}
                        onDeletePage={(pageIndex) => {
                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages.splice(pageIndex, 1);

                                    const newPageIndex = pageIndex === 0 ? 0 : pageIndex - 1;
                                    props.onSelectPage(playground!.pages[newPageIndex].id);
                                }),
                            );
                        }}
                    />

                    <PlaygroundPageEditor
                        wipple={props.wipple}
                        id={props.selectedPageId}
                        items={
                            playgroundPageIndex != null
                                ? props.playground!.pages[playgroundPageIndex].items
                                : undefined
                        }
                        locked={props.playground?.locked}
                        onAddItem={(item) => {
                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages[playgroundPageIndex!].items.push(item);
                                }),
                            );
                        }}
                        onChangeItem={(index, item) => {
                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages[playgroundPageIndex!].items[index] = item;
                                }),
                            );
                        }}
                        onMoveItemUp={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    const page = playground!.pages[playgroundPageIndex!];
                                    const [item] = page.items.splice(index, 1);
                                    page.items.splice(index - 1, 0, item);
                                }),
                            );
                        }}
                        onMoveItemDown={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    const page = playground!.pages[playgroundPageIndex!];
                                    const [item] = page.items.splice(index, 1);
                                    page.items.splice(index + 1, 0, item);
                                }),
                            );
                        }}
                        onDeleteItem={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    playground!.pages[playgroundPageIndex!].items.splice(index, 1);
                                }),
                            );
                        }}
                        canResetItem={(index) => {
                            const page = props.playground!.pages[playgroundPageIndex!];
                            const item = page.items[index];
                            return item.type === "code" && item.originalCode != null;
                        }}
                        onResetItem={(index) => {
                            props.setPlayground(
                                produce((playground) => {
                                    const page = playground!.pages[playgroundPageIndex!];
                                    const item = page.items[index];

                                    if (item.type === "code" && item.originalCode != null) {
                                        item.code = item.originalCode;
                                    }
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
    wipple: typeof import("wipple-wasm");
    id?: string;
    items?: PlaygroundPageItem[];
    locked?: boolean;
    onAddItem: (item: PlaygroundPageItem) => void;
    onChangeItem: (index: number, item: PlaygroundPageItem) => void;
    onMoveItemDown: (index: number) => void;
    onMoveItemUp: (index: number) => void;
    onDeleteItem: (index: number) => void;
    canResetItem: (index: number) => boolean;
    onResetItem: (index: number) => void;
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
                                        id={{ page: props.id!, index }}
                                        wipple={props.wipple}
                                        item={item}
                                        onChange={(item) => props.onChangeItem(index, item)}
                                        locked={props.locked}
                                        onMoveUp={
                                            index > 0 ? () => props.onMoveItemUp(index) : undefined
                                        }
                                        onMoveDown={
                                            index < props.items!.length - 1
                                                ? () => props.onMoveItemDown(index)
                                                : undefined
                                        }
                                        onDelete={() => props.onDeleteItem(index)}
                                        onReset={
                                            props.canResetItem(index)
                                                ? () => props.onResetItem(index)
                                                : undefined
                                        }
                                    />
                                </TutorialItem>
                            ) : null,
                        )}

                        {!props.locked ? (
                            <AddPlaygroundPageItemButton onAddItem={props.onAddItem} />
                        ) : null}
                    </>
                ) : (
                    <div className="p-4 rounded-lg border-2 border-gray-100 dark:border-gray-800">
                        <AddPlaygroundPageItemAlert
                            gridClassName="grid-cols-2 md:grid-cols-3"
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

export const PlaygroundPageItemEditor = (props: {
    wipple: typeof import("wipple-wasm");
    id: { page: string; index: number };
    item: PlaygroundPageItem;
    onChange: (item: PlaygroundPageItem) => void;
    locked?: boolean;
    readOnly?: boolean;
    onMoveUp?: () => void;
    onMoveDown?: () => void;
    onDelete?: () => void;
    onReset?: () => void;
    menu?: JSX.Element;
}) => {
    const theme = useMemo(() => defaultThemeConfig(), []);

    switch (props.item.type) {
        case "code":
            return (
                <CodeEditor
                    wipple={props.wipple}
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
                    locked={props.locked}
                    readOnly={props.readOnly}
                    onMoveUp={props.onMoveUp}
                    onMoveDown={props.onMoveDown}
                    onDelete={props.onDelete}
                    onReset={props.onReset}
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
                    menu={props.menu}
                >
                    {props.item.code}
                </CodeEditor>
            );
        case "text":
            return (
                <TextEditor
                    onChange={(text) =>
                        props.onChange(
                            produce(props.item, (item) => {
                                if (item.type !== "text") {
                                    return;
                                }

                                item.text = text;
                            }),
                        )
                    }
                    locked={props.locked || props.item.locked}
                    onToggleLock={
                        !props.locked
                            ? () =>
                                  props.onChange(
                                      produce(props.item, (item) => {
                                          if (item.type !== "text") {
                                              return;
                                          }

                                          item.locked = !item.locked;
                                      }),
                                  )
                            : undefined
                    }
                    onMoveUp={props.onMoveUp}
                    onMoveDown={props.onMoveDown}
                    onDelete={props.onDelete}
                    menu={props.menu}
                >
                    {props.item.text}
                </TextEditor>
            );
        default:
            return null;
    }
};

const PlaygroundPageList = (props: {
    playground?: Playground;
    selectedPage?: string;
    onSelectPage: (id: string) => void;
    onAddPage: (name: string) => void;
    onRenamePage: (pageIndex: number, name: string) => void;
    onMovePageUp: (pageIndex: number) => void;
    onMovePageDown: (pageIndex: number) => void;
    onDeletePage: (pageIndex: number) => void;
}) => (
    <TutorialItem id="playgroundPageList" className="w-full max-w-4xl lg:w-fit lg:max-w-none">
        <ul className="flex flex-shrink-0 flex-row gap-1 lg:flex-col max-w-4xl lg:max-w-none w-full lg:w-[240px] lg:max-h-[calc(100vh-140px)] overflow-scroll no-scrollbar px-4 pb-8 lg:pb-4">
            {props.playground ? (
                <>
                    {props.playground.pages.map((page, pageIndex) => {
                        const isActive = page.id === props.selectedPage;

                        return (
                            <button key={page.id} onClick={() => props.onSelectPage(page.id)}>
                                <li
                                    className={`flex flex-row items-center justify-between gap-2 lg:w-full px-3 py-1.5 rounded-lg transition-colors ${
                                        isActive
                                            ? "bg-blue-500 hover:bg-blue-600 dark:hover:bg-blue-400 shadow-md shadow-blue-200 dark:shadow-blue-950 text-white font-semibold"
                                            : "hover:bg-gray-100 active:bg-gray-200 dark:hover:bg-gray-900 dark:active:bg-gray-800"
                                    }`}
                                >
                                    <p className="text-left text-nowrap lg:text-wrap">
                                        {page.name}
                                    </p>

                                    {isActive && !props.playground!.locked ? (
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
                            </button>
                        );
                    })}

                    {!props.playground!.locked ? (
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
                    ) : null}
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
                    setup="turtle"
                    name="Turtle"
                    description="Draw graphics on the screen."
                    backgroundClassName="bg-green-50 dark:bg-green-800  enabled:hover:bg-green-100 enabled:dark:hover:bg-green-700"
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
                    backgroundClassName="bg-orange-50 dark:bg-orange-800  enabled:hover:bg-orange-100 enabled:dark:hover:bg-orange-700"
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
                    backgroundClassName="bg-cyan-50 dark:bg-teal-800  enabled:hover:bg-cyan-100 enabled:dark:hover:bg-cyan-700"
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "math",
                        });

                        props.dismiss?.();
                    }}
                />

                {/* <AddPlaygroundPageItemAlertButton
                    setup="game"
                    name="Game"
                    description="Create a video game."
                    backgroundClassName="bg-purple-50 dark:bg-purple-800  enabled:hover:bg-purple-100 enabled:dark:hover:bg-purple-700"
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                            setup: "game",
                        });

                        props.dismiss?.();
                    }}
                /> */}

                <AddPlaygroundPageItemAlertButton
                    setup="physics"
                    name="Physics"
                    description="Experiment with physics."
                    backgroundClassName="bg-yellow-50 dark:bg-yellow-800  enabled:hover:bg-yellow-100 enabled:dark:hover:bg-yellow-700"
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
                    setup={undefined}
                    name="Blank"
                    description="Create a program from scratch."
                    backgroundClassName="bg-blue-50 dark:bg-blue-800  enabled:hover:bg-blue-100 enabled:dark:hover:bg-blue-700"
                    onSelect={() => {
                        props.onAddItem({
                            type: "code",
                            code: "",
                        });

                        props.dismiss?.();
                    }}
                />

                <AddPlaygroundPageItemAlertButton
                    setup="text"
                    name="Text"
                    description="Write text alongside your code."
                    backgroundClassName="bg-gray-50 dark:bg-gray-800  enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-700"
                    onSelect={() => {
                        props.onAddItem({
                            type: "text",
                            text: "",
                            locked: false,
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
    backgroundClassName: string;
    onSelect: () => void;
}) => (
    <button
        disabled={props.disabled}
        className={`group disabled:opacity-50 rounded-lg p-3 transition-colors ${props.backgroundClassName}`}
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
