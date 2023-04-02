import React, { useEffect, useState } from "react";
import {
    DndContext,
    closestCenter,
    PointerSensor,
    useSensor,
    useSensors,
    DragOverlay,
} from "@dnd-kit/core";
import {
    arrayMove,
    SortableContext,
    useSortable,
    verticalListSortingStrategy,
} from "@dnd-kit/sortable";
import { CSS } from "@dnd-kit/utilities";
import DragIndicatorIcon from "@mui/icons-material/DragIndicator";
import AddIcon from "@mui/icons-material/AddRounded";
import TextIcon from "@mui/icons-material/TextFormatRounded";
import DeleteIcon from "@mui/icons-material/DeleteOutlineRounded";
import LockIcon from "@mui/icons-material/Lock";
import LockOpenIcon from "@mui/icons-material/LockOpen";
import CodeIcon from "@mui/icons-material/Code";
import CodeOffIcon from "@mui/icons-material/CodeOff";
import PopupState, { bindMenu, bindTrigger } from "material-ui-popup-state";
import { Menu, MenuItem } from "@mui/material";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import { nanoid } from "nanoid";
import { CodeEditor, TextEditor } from "./components";
import { useRefState } from "./helpers";

type Section = { id: string; value: string } & (
    | { type: "code"; lint?: boolean }
    | { type: "text"; locked?: boolean }
);

interface PageLink {
    name: string;
    link: string;
}

const App = () => {
    const [sections, setSections] = useState<Section[]>([]);
    const [previousPage, setPreviousPage] = useState<PageLink | undefined>();
    const [nextPage, setNextPage] = useState<PageLink | undefined>();

    const [query, setQuery] = useRefState<URLSearchParams | null>(null);

    useEffect(() => {
        const setup = async () => {
            const query = new URLSearchParams(window.location.search);
            setQuery(query);

            // For backward compatibility
            const codeParam = query.get("code");
            if (codeParam) {
                setSections([
                    {
                        id: nanoid(8),
                        type: "code",
                        value: codeParam,
                    },
                ]);

                return;
            }

            const sectionsParam = query.get("sections");
            if (sectionsParam) {
                setSections(JSON.parse(sectionsParam));

                const previousPage = query.get("previous");
                if (previousPage) {
                    setPreviousPage(JSON.parse(previousPage));
                }

                const nextPage = query.get("next");
                if (nextPage) {
                    setNextPage(JSON.parse(nextPage));
                }

                return;
            }

            const lessonParam = query.get("lesson");
            if (lessonParam) {
                const data = await (await fetch(`./lessons/${lessonParam}.json`)).text();

                const lesson: {
                    sections: Section[];
                    previous?: PageLink;
                    next?: PageLink;
                } = JSON.parse(data);

                setSections(lesson.sections);
                setPreviousPage(lesson.previous);
                setNextPage(lesson.next);
            } else {
                setSections([
                    {
                        id: nanoid(8),
                        type: "code",
                        value: "",
                    },
                ]);
            }
        };

        setup();
    }, []);

    useEffect(() => {
        if (!query.current) return;

        query.current.delete("code");
        query.current.set("sections", JSON.stringify(sections));
        previousPage && query.current.set("previous", JSON.stringify(previousPage));
        nextPage && query.current.set("next", JSON.stringify(nextPage));
        query.current.delete("lesson");

        const newURL =
            window.location.pathname +
            (sections.length ? "?" + (query.current.toString() ?? "") : "");

        window.history.replaceState(null, "", newURL);
    }, [sections]);

    const [activeId, setActiveId] = useState<string | undefined>(undefined);
    const sensors = useSensors(useSensor(PointerSensor, { activationConstraint: { distance: 1 } }));

    return (
        <main>
            <div className="mx-auto p-6 max-w-4xl">
                <div className="flex items-center justify-between pb-4">
                    <a
                        href="/playground"
                        className="flex items-center gap-3 text-black dark:text-white"
                    >
                        <img src="./images/logo.svg" alt="Wipple Playground" className="h-10" />
                        <h1 className="font-semibold">Wipple Playground</h1>
                    </a>

                    <div className="flex gap-4 text-gray-500 dark:text-gray-400">
                        <a href="?lesson=learn/toc">Learn</a>

                        <a target="_blank" href="/guide">
                            Guide
                        </a>

                        <a target="_blank" href="https://github.com/wipplelang/wipple">
                            GitHub
                        </a>
                    </div>
                </div>

                <DndContext
                    sensors={sensors}
                    collisionDetection={closestCenter}
                    onDragEnd={(event) => {
                        setActiveId(undefined);
                        const { active, over } = event;

                        if (active.id !== over?.id) {
                            setSections((items) => {
                                const oldIndex = items.findIndex((s) => s.id === active.id);
                                const newIndex = items.findIndex((s) => s.id === over?.id);

                                return arrayMove(items, oldIndex, newIndex);
                            });
                        }
                    }}
                    onDragStart={(event) => {
                        setActiveId(event.active.id as string);
                    }}
                    autoScroll
                >
                    <SortableContext items={sections} strategy={verticalListSortingStrategy}>
                        {sections.map((section, index) => (
                            <SortableItem
                                key={section.id}
                                id={section.id}
                                onPressAdd={(type) => {
                                    const newSections = [...sections];
                                    newSections.splice(index + 1, 0, {
                                        id: nanoid(8),
                                        type,
                                        value: "",
                                        locked: type === "text" ? false : undefined,
                                    });
                                    setSections(newSections);
                                }}
                                onPressRemove={
                                    sections.length > 1
                                        ? async () => {
                                              const newSections = [...sections];
                                              newSections.splice(index, 1);
                                              setSections(newSections);
                                          }
                                        : undefined
                                }
                                lock={
                                    section.type === "text"
                                        ? {
                                              isLocked: section.locked ?? false,
                                              onChangeLocked: (locked) => {
                                                  const newSections = [...sections];
                                                  newSections.splice(index, 1, {
                                                      ...section,
                                                      locked,
                                                  });
                                                  setSections(newSections);
                                              },
                                          }
                                        : undefined
                                }
                                lint={
                                    section.type === "code"
                                        ? {
                                              lintEnabled: section.lint ?? true,
                                              onChangeLintEnabled: (lint) => {
                                                  const newSections = [...sections];
                                                  newSections.splice(index, 1, {
                                                      ...section,
                                                      lint,
                                                  });
                                                  setSections(newSections);
                                              },
                                          }
                                        : undefined
                                }
                            >
                                <SectionContainer
                                    section={section}
                                    autoFocus={index === 0}
                                    onChange={(section) => {
                                        const newSections = [...sections];
                                        newSections.splice(index, 1, section);
                                        setSections(newSections);
                                    }}
                                />
                            </SortableItem>
                        ))}
                    </SortableContext>

                    <DragOverlay>
                        {activeId && (
                            <div className="flex items-center">
                                <SideMenu />

                                <SectionContainer
                                    section={sections.find((s) => s.id === activeId)!}
                                    autoFocus={false}
                                    onChange={() => {}}
                                />
                            </div>
                        )}
                    </DragOverlay>
                </DndContext>

                <div className="flex my-5 gap-4">
                    <div className="flex-1">
                        {previousPage && (
                            <a href={previousPage.link}>
                                <div
                                    className="p-4 rounded-md border-sky-100 dark:border-sky-900 text-sky-500"
                                    style={{ borderWidth: 1 }}
                                >
                                    <div>
                                        <ArrowBackIcon className="-ml-1 mb-2" />
                                    </div>
                                    {previousPage.name}
                                </div>
                            </a>
                        )}
                    </div>

                    <div className="flex-1">
                        {nextPage && (
                            <a href={nextPage.link}>
                                <div
                                    className="text-right p-4 rounded-md dark:border-sky-900 border-sky-100 text-sky-500"
                                    style={{ borderWidth: 1 }}
                                >
                                    <div className="ml-auto">
                                        <ArrowForwardIcon className="-mr-1 mb-2" />
                                    </div>
                                    {nextPage.name}
                                </div>
                            </a>
                        )}
                    </div>
                </div>

                <div className="mb-4 text-center text-gray-400 dark:text-gray-500">
                    Made by{" "}
                    <a
                        target="_blank"
                        href="https://gramer.dev"
                        className="text-gray-500 dark:text-gray-400"
                    >
                        Wilson Gramer
                    </a>
                </div>
            </div>
        </main>
    );
};

const SideMenu = (props: {
    grabberProps?: any;
    onPressAdd?: (type: Section["type"]) => void;
    onPressRemove?: () => void;
    lock?: {
        isLocked: boolean;
        onChangeLocked: (locked: boolean) => void;
    };
    lint?: {
        lintEnabled: boolean;
        onChangeLintEnabled: (lint: boolean) => void;
    };
}) => (
    <div className="w-6 -mt-4 -ml-6">
        <div {...props.grabberProps}>
            <PopupState variant="popover">
                {(popupState) => (
                    <>
                        <button {...bindTrigger(popupState)}>
                            <DragIndicatorIcon className="text-gray-300 hover:text-gray-500 dark:text-gray-500 dark:hover:text-gray-300" />
                        </button>

                        <Menu {...bindMenu(popupState)}>
                            <MenuItem
                                disabled={props.onPressAdd == null}
                                onClick={() => {
                                    popupState.close();
                                    props.onPressAdd?.("code");
                                }}
                            >
                                <AddIcon /> Add Code
                            </MenuItem>

                            <MenuItem
                                disabled={props.onPressAdd == null}
                                onClick={() => {
                                    popupState.close();
                                    props.onPressAdd?.("text");
                                }}
                            >
                                <TextIcon /> Add Text
                            </MenuItem>

                            {props.lock && (
                                <MenuItem
                                    onClick={() => {
                                        popupState.close();
                                        props.lock?.onChangeLocked(!props.lock.isLocked);
                                    }}
                                >
                                    {props.lock.isLocked ? (
                                        <>
                                            <LockOpenIcon /> Unlock
                                        </>
                                    ) : (
                                        <>
                                            <LockIcon /> Lock
                                        </>
                                    )}
                                </MenuItem>
                            )}

                            {props.lint && (
                                <MenuItem
                                    onClick={() => {
                                        popupState.close();
                                        props.lint?.onChangeLintEnabled(!props.lint.lintEnabled);
                                    }}
                                >
                                    {props.lint.lintEnabled ? (
                                        <>
                                            <CodeOffIcon /> Disable Lints
                                        </>
                                    ) : (
                                        <>
                                            <CodeIcon /> Enable Lints
                                        </>
                                    )}
                                </MenuItem>
                            )}

                            <MenuItem
                                disabled={props.onPressRemove == null}
                                onClick={() => {
                                    popupState.close();
                                    props.onPressRemove?.();
                                }}
                            >
                                <DeleteIcon /> Remove
                            </MenuItem>
                        </Menu>
                    </>
                )}
            </PopupState>
        </div>
    </div>
);

const SortableItem = (props: {
    id: string;
    onPressAdd?: (type: Section["type"]) => void;
    onPressRemove?: () => void;
    lock?: {
        isLocked: boolean;
        onChangeLocked: (locked: boolean) => void;
    };
    lint?: {
        lintEnabled: boolean;
        onChangeLintEnabled: (lint: boolean) => void;
    };
    children: React.ReactNode;
}) => {
    const { attributes, listeners, setNodeRef, transform, transition, isDragging } = useSortable({
        id: props.id,
    });

    const style = {
        transform: CSS.Transform.toString(transform),
        transition,
        opacity: isDragging ? 0 : 1,
    };

    const [showGrabber, setShowGrabber] = useState(false);

    return (
        <div
            ref={setNodeRef}
            style={style}
            className="flex items-center"
            onMouseEnter={() => setShowGrabber(true)}
            onMouseLeave={() => setShowGrabber(false)}
        >
            {showGrabber ? (
                <div>
                    <SideMenu
                        grabberProps={{ ...attributes, ...listeners }}
                        onPressAdd={props.onPressAdd}
                        onPressRemove={props.onPressRemove}
                        lock={props.lock}
                        lint={props.lint}
                    />
                </div>
            ) : null}

            {props.children}
        </div>
    );
};

const SectionContainer = (props: {
    section: Section;
    autoFocus: boolean;
    onChange: (section: Section) => void;
}) => {
    let content: JSX.Element;
    switch (props.section.type) {
        case "code":
            content = (
                <CodeEditor
                    id={props.section.id}
                    code={props.section.value}
                    lint={props.section.lint ?? true}
                    autoFocus={props.autoFocus}
                    onChange={(code) => {
                        props.onChange({
                            ...props.section,
                            value: code,
                        });
                    }}
                />
            );
            break;
        case "text":
            content = (
                <TextEditor
                    content={props.section.value}
                    onChange={(text) => {
                        props.onChange({
                            ...props.section,
                            value: text,
                        });
                    }}
                    isLocked={props.section.locked ?? false}
                />
            );
            break;
    }

    return <div className="mb-4 flex-1">{content}</div>;
};

export default App;
