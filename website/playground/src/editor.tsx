import React, { useEffect, useRef, useState } from "react";
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
import DeleteIcon from "@mui/icons-material/DeleteOutlineRounded";
import LockIcon from "@mui/icons-material/Lock";
import LockOpenIcon from "@mui/icons-material/LockOpen";
import CodeIcon from "@mui/icons-material/Code";
import CodeOffIcon from "@mui/icons-material/CodeOff";
import PopupState, { bindMenu, bindTrigger } from "material-ui-popup-state";
import { Menu, MenuItem } from "@mui/material";
import { nanoid } from "nanoid";
import { CodeEditor, Picker, TextEditor } from "./components";
import { produce } from "immer";
import { EditCommands, Page, Section, Settings } from "./app";
import { Helmet } from "react-helmet";
import { CourseList } from "./course-list";

export const Editor = (props: {
    pages: Page[];
    onChangePages: (pages: Page[]) => void;
    activePage: number;
    onChangeActivePage: (page: number) => void;
    settings: Settings;
    onChangeActiveEditor: (editCommands: EditCommands | undefined) => void;
}) => {
    const [viewCourses, setViewCourses] = useState(false);

    const containerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        setTimeout(() => {
            containerRef.current?.scrollTo({
                top: 0,
                behavior: "instant",
            });
        }, 50);
    }, [props.activePage]);

    return viewCourses ? (
        <CourseList />
    ) : props.pages.length === 0 || props.pages[0].sections.length === 0 ? (
        <div className="flex flex-col flex-1 gap-4 my-8 items-center justify-center text-center max-w-sm mx-auto">
            <img src="./images/logo.svg" alt="Wipple Playground" className="h-20" />

            <h1 className="text-2xl font-semibold text-gray-900 dark:text-gray-100">
                Welcome to Wipple
            </h1>

            <p className="xl text-gray-600 dark:text-gray-500">
                Learn to code with Wipple — make drawings, play music, explore math, and more.
            </p>

            <div className="flex flex-col gap-4 items-stretch w-full">
                <button
                    className="welcome-button"
                    onClick={() => {
                        props.onChangePages([
                            {
                                title: "",
                                sections: [
                                    {
                                        id: nanoid(8),
                                        page: "",
                                        type: "picker",
                                        value: "",
                                    },
                                ],
                            },
                        ]);
                    }}
                >
                    Start coding
                </button>

                <button
                    className="welcome-button"
                    onClick={() => {
                        setViewCourses(true);
                    }}
                >
                    View courses
                </button>

                <div className="text-gray-400 dark:text-gray-500 mt-4">
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
        </div>
    ) : (
        <>
            <Helmet>
                {props.pages.length > 0 && props.pages[props.activePage].title ? (
                    <title>{props.pages[props.activePage].title} — Wipple Playground</title>
                ) : null}
            </Helmet>

            <div ref={containerRef} className="w-full overflow-y-scroll">
                <div className="flex flex-col justify-center editor-breakpoint:flex-row gap-6 p-6 py-20 w-full max-w-4xl editor-breakpoint:w-auto editor-breakpoint:max-w-none mx-auto">
                    {props.pages.length > 1 ? (
                        <div className="flex flex-col gap-2 w-full editor-breakpoint:w-72 editor-breakpoint:px-4 pb-4 editor-breakpoint:sticky editor-breakpoint:top-[80px] editor-breakpoint:self-start h-full overflow-y-scroll">
                            {props.pages.map((page, pageIndex) => (
                                <button
                                    key={pageIndex}
                                    onClick={() => {
                                        props.onChangeActivePage(pageIndex);
                                    }}
                                    className={`text-left px-4 py-2 rounded-lg transition-colors ${
                                        props.activePage === pageIndex
                                            ? "bg-blue-500 shadow-lg shadow-blue-100 dark:shadow-none text-white font-semibold"
                                            : "hover:bg-gray-200 dark:hover:bg-gray-700 text-gray-900 dark:text-gray-100"
                                    }`}
                                >
                                    {page.title}
                                </button>
                            ))}
                        </div>
                    ) : null}

                    <div className="w-full h-full editor-breakpoint:w-[896px] editor-breakpoint:pr-4">
                        <div className="pb-20">
                            <Page
                                page={props.pages[props.activePage]}
                                onChange={(page) => {
                                    props.onChangePages(
                                        produce(props.pages, (pages) => {
                                            pages[props.activePage] = page;
                                        })
                                    );
                                }}
                                settings={props.settings}
                                onChangeActiveEditor={props.onChangeActiveEditor}
                            />
                        </div>
                    </div>
                </div>
            </div>
        </>
    );
};

const Page = (props: {
    page: Page;
    onChange: (page: Page) => void;
    settings: Settings;
    onChangeActiveEditor: (editCommands: EditCommands | undefined) => void;
}) => {
    const [activeId, setActiveId] = useState<string | undefined>(undefined);
    const sensors = useSensors(useSensor(PointerSensor, { activationConstraint: { distance: 1 } }));

    const handleFocus = props.onChangeActiveEditor;
    const handleBlur = () => props.onChangeActiveEditor(undefined);

    const onChange = (page: Page) => {
        props.onChange(page);
    };

    return (
        <div className="flex flex-col gap-6">
            <h1 className="text-3xl font-bold text-black dark:text-white mx-4">
                {props.page.title}
            </h1>

            <DndContext
                sensors={sensors}
                collisionDetection={closestCenter}
                onDragEnd={(event) => {
                    setActiveId(undefined);
                    const { active, over } = event;

                    if (active.id !== over?.id) {
                        onChange(
                            produce(props.page, (page) => {
                                const oldIndex = page.sections.findIndex((s) => s.id === active.id);
                                const newIndex = page.sections.findIndex((s) => s.id === over?.id);
                                page.sections = arrayMove(page.sections, oldIndex, newIndex);
                            })
                        );
                    }
                }}
                onDragStart={(event) => {
                    setActiveId(event.active.id as string);
                }}
                autoScroll
            >
                <SortableContext items={props.page.sections} strategy={verticalListSortingStrategy}>
                    {props.page.sections.map((section, sectionIndex) => {
                        const deleteSection = () => {
                            onChange(
                                produce(props.page, (page) => {
                                    page.sections.splice(sectionIndex, 1);
                                })
                            );
                        };

                        return (
                            <SortableItem
                                key={section.id}
                                id={section.id}
                                onPressAdd={(type) => {
                                    onChange(
                                        produce(props.page, (page) => {
                                            page.sections.push({
                                                id: nanoid(8),
                                                page: page.title ?? "",
                                                type,
                                                value: "",
                                                locked: type === "text" ? false : undefined,
                                            });
                                        })
                                    );
                                }}
                                onPressRemove={deleteSection}
                                lock={
                                    section.type === "text"
                                        ? {
                                              isLocked: section.locked ?? false,
                                              onChangeLocked: (locked) => {
                                                  onChange(
                                                      produce(props.page, (page) => {
                                                          const section =
                                                              page.sections[sectionIndex];

                                                          if (section.type !== "text") {
                                                              throw new Error(
                                                                  `section mismatch: ${JSON.stringify(
                                                                      section,
                                                                      null,
                                                                      4
                                                                  )}`
                                                              );
                                                          }

                                                          section.locked = locked;
                                                      })
                                                  );
                                              },
                                          }
                                        : undefined
                                }
                                lint={
                                    section.type === "code"
                                        ? {
                                              lintEnabled: section.lint ?? true,
                                              onChangeLintEnabled: (lint) => {
                                                  onChange(
                                                      produce(props.page, (page) => {
                                                          const section =
                                                              page.sections[sectionIndex];

                                                          if (section.type !== "code") {
                                                              throw new Error(
                                                                  `section mismatch: ${JSON.stringify(
                                                                      section,
                                                                      null,
                                                                      4
                                                                  )}`
                                                              );
                                                          }

                                                          section.lint = lint;
                                                      })
                                                  );
                                              },
                                          }
                                        : undefined
                                }
                            >
                                <SectionContainer
                                    section={section}
                                    autoFocus={sectionIndex === 0}
                                    settings={props.settings}
                                    onChange={(newSection) => {
                                        onChange(
                                            produce(props.page, (page) => {
                                                if (
                                                    newSection.id !== page.sections[sectionIndex].id
                                                ) {
                                                    throw new Error("wrong section");
                                                }

                                                page.sections[sectionIndex] = newSection;
                                            })
                                        );
                                    }}
                                    onDelete={deleteSection}
                                    onFocus={handleFocus}
                                    onBlur={handleBlur}
                                />
                            </SortableItem>
                        );
                    })}
                </SortableContext>

                <DragOverlay>
                    {activeId && (
                        <div className="flex items-center">
                            <SideMenu />

                            <SectionContainer
                                section={props.page.sections.find((s) => s.id === activeId)!}
                                autoFocus={false}
                                settings={props.settings}
                                onChange={() => {}}
                                onDelete={() => {}}
                                onFocus={() => {}}
                                onBlur={() => {}}
                            />
                        </div>
                    )}
                </DragOverlay>
            </DndContext>
        </div>
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
                                    props.onPressAdd?.("picker");
                                }}
                            >
                                <AddIcon /> Add Section
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
    settings: Settings;
    onChange: (newSection: Section) => void;
    onDelete: () => void;
    onFocus: (commands: EditCommands) => void;
    onBlur: (e: FocusEvent) => void;
}) => {
    let content: JSX.Element;
    switch (props.section.type) {
        case "picker":
            content = (
                <Picker
                    onSelect={(options) => {
                        props.onChange(
                            produce(props.section, (section) => {
                                switch (options.type) {
                                    case "code":
                                        section.type = "code";
                                        (section as Section & { type: "code" }).setup =
                                            options.setup;
                                        break;
                                    case "text":
                                        section.type = "text";
                                        break;
                                }
                            })
                        );
                    }}
                    onCancel={props.onDelete}
                />
            );
            break;
        case "code":
            content = (
                <CodeEditor
                    id={props.section.id}
                    code={props.section.value}
                    lint={props.section.lint ?? true}
                    setup={props.section.setup}
                    autoRun={props.section.autoRun ?? true}
                    onChangeAutoRun={(autoRun) => {
                        props.onChange(
                            produce(props.section, (section) => {
                                if (section.type !== "code") {
                                    throw new Error(
                                        `section mismatch: ${JSON.stringify(section, null, 4)}`
                                    );
                                }

                                section.autoRun = autoRun;
                            })
                        );
                    }}
                    collapse={props.section.collapse ?? false}
                    onChangeCollapse={(collapse) => {
                        props.onChange(
                            produce(props.section, (section) => {
                                if (section.type !== "code") {
                                    throw new Error(
                                        `section mismatch: ${JSON.stringify(section, null, 4)}`
                                    );
                                }

                                section.collapse = collapse;
                            })
                        );
                    }}
                    autoFocus={props.autoFocus}
                    settings={props.settings}
                    onChange={(code) => {
                        props.onChange(
                            produce(props.section, (section) => {
                                if (section.type !== "code") {
                                    throw new Error(
                                        `section mismatch: ${JSON.stringify(section, null, 4)}`
                                    );
                                }

                                section.value = code;
                            })
                        );
                    }}
                    onFocus={props.onFocus}
                    onBlur={props.onBlur}
                />
            );
            break;
        case "text":
            content = (
                <TextEditor
                    content={props.section.value}
                    onChange={(text) => {
                        props.onChange(
                            produce(props.section, (section) => {
                                if (section.type !== "text") {
                                    throw new Error(
                                        `section mismatch: ${JSON.stringify(section, null, 4)}`
                                    );
                                }

                                section.value = text;
                            })
                        );
                    }}
                    isLocked={props.section.locked ?? false}
                    onFocus={props.onFocus}
                    onBlur={props.onBlur}
                />
            );
            break;
    }

    return <div className="mb-4 flex-1 w-full">{content}</div>;
};
