import { useCallback, useEffect, useState } from "react";
import PopupState, { bindDialog, bindMenu, bindTrigger } from "material-ui-popup-state";
import {
    Button,
    Checkbox,
    CircularProgress,
    Dialog,
    Menu,
    MenuItem,
    debounce,
} from "@mui/material";
import LinkIcon from "@mui/icons-material/Link";
import DataObjectIcon from "@mui/icons-material/DataObject";
import ListAltIcon from "@mui/icons-material/ListAlt";
import NoteAddOutlinedIcon from "@mui/icons-material/NoteAddOutlined";
import VerticalAlignTopIcon from "@mui/icons-material/VerticalAlignTop";
import VerticalAlignBottomIcon from "@mui/icons-material/VerticalAlignBottom";
import UndoRoundedIcon from "@mui/icons-material/UndoRounded";
import RedoRoundedIcon from "@mui/icons-material/RedoRounded";
import ContentCutRoundedIcon from "@mui/icons-material/ContentCutRounded";
import ContentCopyRoundedIcon from "@mui/icons-material/ContentCopyRounded";
import ContentPasteRoundedIcon from "@mui/icons-material/ContentPasteRounded";
import SelectAllRoundedIcon from "@mui/icons-material/SelectAllRounded";
import { nanoid } from "nanoid";
import { convertCourse } from "./helpers";
import { useMemo } from "react";
import { produce } from "immer";
import { Editor } from "./editor";

export interface Page {
    title: string;
    sections: Section[];
}

export type Section = { id: string; page: string; value: string } & (
    | { type: "picker" }
    | { type: "code"; setup?: string; lint?: boolean; autoRun?: boolean; collapse?: boolean }
    | { type: "text"; locked?: boolean }
);

export interface Settings {
    beginner?: boolean;
    focus?: boolean;
    autocomplete?: boolean;
    analytics?: boolean;
}

export interface EditCommands {
    jumpToBeginning?: () => void;
    jumpToEnd?: () => void;
    undo?: () => void;
    redo?: () => void;
    cut?: () => string | undefined;
    copy?: () => string | undefined;
    paste?: (text: string) => void;
    selectAll?: () => void;
}

const App = () => {
    const [course, setCourse] = useState<string>();
    const [pages, setPages] = useState<Page[]>([]);
    const [hasChangedPages, setHasChangedPages] = useState(false);
    const [activePage, setActivePage] = useState(0);

    const pageSlug = (page: Page) =>
        page.title
            .toLowerCase()
            .replaceAll(/[^A-Za-z0-9]/g, "-")
            .replace(/^\-+/, "")
            .replace(/\-+$/, "")
            .replace(/\-+/, "-");

    const [isLoading, setLoading] = useState(true);

    useEffect(() => {
        const setup = async () => {
            try {
                const pages: Page[] | undefined = await (async () => {
                    const query = new URLSearchParams(window.location.search);

                    // For backward compatibility
                    const codeParam = query.get("code");
                    if (codeParam) {
                        setHasChangedPages(true);
                        return [
                            {
                                title: "",
                                sections: [
                                    {
                                        id: nanoid(8),
                                        page: "",
                                        type: "code",
                                        value: codeParam,
                                    },
                                ],
                            },
                        ];
                    }

                    const pagesParam = query.get("pages");
                    if (pagesParam) {
                        setHasChangedPages(true);
                        return JSON.parse(pagesParam);
                    }

                    const getFile = async (path: string) => {
                        const data = await (await fetch(path)).text();
                        const pages = convertCourse(data);
                        return pages;
                    };

                    const fileParam = query.get("file");
                    if (fileParam) {
                        return await getFile(fileParam);
                    }

                    const courseParam = query.get("course");
                    if (courseParam) {
                        setCourse(courseParam);
                        return await getFile(`./courses/${courseParam}.txt`);
                    }
                })();

                if (!pages) {
                    return;
                }

                setPages(pages);

                const hash = window.location.hash;
                if (hash) {
                    const activePage = pages.findIndex((page) => pageSlug(page) === hash.slice(1));
                    if (activePage != -1) {
                        setActivePage(activePage);
                    }
                }
            } finally {
                setLoading(false);
            }
        };

        setup();
    }, []);

    const query = useMemo(() => {
        const query = new URLSearchParams();

        if (course && !hasChangedPages) {
            query.set("course", course);
        }

        if (pages.length > 0 && hasChangedPages) {
            query.set("pages", JSON.stringify(pages));
        }

        return query.toString();
    }, [course, pages, hasChangedPages]);

    const hash = useMemo(
        () => (pages.length > 0 ? pageSlug(pages[activePage]) : ""),
        [pages, activePage]
    );

    const updateHistory = useMemo(
        () => debounce((url: string) => window.history.replaceState(null, "", url), 250),
        []
    );

    useEffect(() => {
        let url = window.location.pathname;

        if (query) {
            url += "?";
            url += query;
        }

        if (hash) {
            url += "#";
            url += hash;
        }

        updateHistory(url);
    }, [hash, query]);

    const [settings, setSettings] = useState<Settings>(() => {
        const settings = localStorage.getItem("settings");
        if (!settings) {
            return {};
        }

        return JSON.parse(settings);
    });

    useEffect(() => {
        localStorage.setItem("settings", JSON.stringify(settings));
    }, [settings]);

    const onViewPage = useCallback(
        (page: Page) => {
            if (!page.title || !(settings.analytics ?? true)) return;

            window.gtag("event", "page_view", {
                page_title: page.title,
            });
        },
        [settings.analytics]
    );

    useEffect(() => {
        const consent = settings.analytics ?? true ? "granted" : "denied";

        window.gtag("consent", "update", {
            ad_storage: consent,
            analytics_storage: consent,
        });
    }, [settings.analytics]);

    const [activeEditor, setActiveEditor] = useState<EditCommands>();

    return (
        <div>
            <div
                className="fixed top-0 left-0 right-0 z-50"
                onMouseDown={(e) => e.preventDefault()}
            >
                <div className="flex items-center gap-4 p-4 flex-shrink-0 overflow-x-scroll text-gray-500 dark:text-gray-400 bg-white dark:bg-gray-900 bg-opacity-75 dark:bg-opacity-75 backdrop-blur-lg disable-scrollbars">
                    <a href="https://wipple.dev" target="_blank" className="flex-shrink-0">
                        <img src="./images/logo.svg" alt="Wipple" className="w-6 h-6" />
                    </a>

                    <PopupState variant="popover" disableAutoFocus>
                        {(popupState) => (
                            <>
                                <button {...bindTrigger(popupState)}>File</button>

                                <Menu {...bindMenu(popupState)} sx={{ marginTop: 1 }}>
                                    <MenuItem
                                        component="a"
                                        href="."
                                        target="_blank"
                                        onClick={popupState.close}
                                    >
                                        <NoteAddOutlinedIcon sx={{ marginRight: 1 }} />
                                        New
                                    </MenuItem>

                                    <MenuItem
                                        onClick={async () => {
                                            await navigator.clipboard.writeText(
                                                window.location.href
                                            );
                                            popupState.close();
                                        }}
                                    >
                                        <LinkIcon sx={{ marginRight: 1 }} /> Copy Link
                                    </MenuItem>

                                    <MenuItem
                                        onClick={async () => {
                                            const json = JSON.stringify({ pages }, null, 4);
                                            await navigator.clipboard.writeText(json);
                                            popupState.close();
                                        }}
                                    >
                                        <DataObjectIcon sx={{ marginRight: 1 }} /> Copy JSON
                                    </MenuItem>

                                    <MenuItem
                                        onClick={async () => {
                                            let text = "";
                                            for (const page of pages) {
                                                for (const section of page.sections) {
                                                    text += "---\n";
                                                    for (const [key, value] of Object.entries(
                                                        section
                                                    )) {
                                                        if (
                                                            key === "id" ||
                                                            key === "value" ||
                                                            value == null
                                                        ) {
                                                            continue;
                                                        }

                                                        text += `${key}: ${value}\n`;
                                                    }

                                                    text += `---\n\n${section.value}\n\n`;
                                                }
                                            }

                                            await navigator.clipboard.writeText(text);
                                            popupState.close();
                                        }}
                                    >
                                        <ListAltIcon sx={{ marginRight: 1 }} /> Copy text
                                    </MenuItem>
                                </Menu>
                            </>
                        )}
                    </PopupState>

                    <PopupState variant="popover" disableAutoFocus>
                        {(popupState) => (
                            <>
                                <button {...bindTrigger(popupState)}>Edit</button>

                                <Menu
                                    {...bindMenu(popupState)}
                                    sx={{ marginTop: 1 }}
                                    disableAutoFocus
                                >
                                    <MenuItem
                                        disabled={activeEditor?.jumpToBeginning == null}
                                        onClick={(e) => {
                                            e.preventDefault();
                                            activeEditor?.jumpToBeginning?.();
                                            popupState.close();
                                        }}
                                    >
                                        <VerticalAlignTopIcon sx={{ marginRight: 1 }} />
                                        Jump to Beginning
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.jumpToEnd == null}
                                        onClick={(e) => {
                                            e.preventDefault();
                                            activeEditor?.jumpToEnd?.();
                                            popupState.close();
                                        }}
                                    >
                                        <VerticalAlignBottomIcon sx={{ marginRight: 1 }} />
                                        Jump to End
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.undo == null}
                                        onClick={(e) => {
                                            e.preventDefault();
                                            activeEditor?.undo?.();
                                            popupState.close();
                                        }}
                                    >
                                        <UndoRoundedIcon sx={{ marginRight: 1 }} /> Undo
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.redo == null}
                                        onClick={(e) => {
                                            e.preventDefault();
                                            activeEditor?.redo?.();
                                            popupState.close();
                                        }}
                                    >
                                        <RedoRoundedIcon sx={{ marginRight: 1 }} /> Redo
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.cut == null}
                                        onClick={(e) => {
                                            e.preventDefault();

                                            const text = activeEditor?.cut?.();
                                            if (text != null) {
                                                navigator.clipboard.writeText(text);
                                            }

                                            popupState.close();
                                        }}
                                    >
                                        <ContentCutRoundedIcon sx={{ marginRight: 1 }} /> Cut
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.copy == null}
                                        onClick={(e) => {
                                            e.preventDefault();

                                            const text = activeEditor?.copy?.();
                                            if (text != null) {
                                                navigator.clipboard.writeText(text);
                                            }

                                            popupState.close();
                                        }}
                                    >
                                        <ContentCopyRoundedIcon sx={{ marginRight: 1 }} /> Copy
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.paste == null}
                                        onClick={async (e) => {
                                            e.preventDefault();

                                            const text = await navigator.clipboard.readText();
                                            activeEditor?.paste?.(text);

                                            popupState.close();
                                        }}
                                    >
                                        <ContentPasteRoundedIcon sx={{ marginRight: 1 }} /> Paste
                                    </MenuItem>

                                    <MenuItem
                                        disabled={activeEditor?.selectAll == null}
                                        onClick={(e) => {
                                            e.preventDefault();
                                            activeEditor?.selectAll?.();
                                            popupState.close();
                                        }}
                                    >
                                        <SelectAllRoundedIcon sx={{ marginRight: 1 }} /> Select All
                                    </MenuItem>
                                </Menu>
                            </>
                        )}
                    </PopupState>

                    <PopupState variant="popover">
                        {(popupState) => (
                            <>
                                <button {...bindTrigger(popupState)}>Settings</button>

                                <Dialog {...bindDialog(popupState)} fullWidth>
                                    <div className="flex flex-col gap-8 p-8 text-black dark:text-white">
                                        <h1 className="text-2xl font-semibold w-full text-center">
                                            Settings
                                        </h1>

                                        <div className="flex flex-row gap-2">
                                            <div className="flex flex-col flex-1">
                                                <p className="font-semibold">Beginner mode</p>
                                                <p className="opacity-50">
                                                    Simplify the code editor and highlight blocks.
                                                </p>
                                            </div>

                                            <Checkbox
                                                checked={settings.beginner ?? true}
                                                onChange={(_e, checked) => {
                                                    setSettings(
                                                        produce((settings) => {
                                                            settings.beginner = checked;
                                                        })
                                                    );
                                                }}
                                            />
                                        </div>

                                        <div className="flex flex-row gap-2">
                                            <div className="flex flex-col flex-1">
                                                <p className="font-semibold">Focus mode</p>
                                                <p className="opacity-50">
                                                    Focus on one line of code at a time.
                                                </p>
                                            </div>

                                            <Checkbox
                                                checked={settings.focus ?? false}
                                                onChange={(_e, checked) => {
                                                    setSettings(
                                                        produce((settings) => {
                                                            settings.focus = checked;
                                                        })
                                                    );
                                                }}
                                            />
                                        </div>

                                        <div className="flex flex-row gap-2">
                                            <div className="flex flex-col flex-1">
                                                <p className="font-semibold">Autocomplete</p>
                                                <p className="opacity-50">
                                                    Automatically insert closing brackets and
                                                    suggest what to type next in the code editor.
                                                    Press Tab to complete the suggestion.
                                                </p>
                                            </div>

                                            <Checkbox
                                                checked={settings.autocomplete ?? false}
                                                onChange={(_e, checked) => {
                                                    setSettings(
                                                        produce((settings) => {
                                                            settings.autocomplete = checked;
                                                        })
                                                    );
                                                }}
                                            />
                                        </div>

                                        <div className="flex flex-row gap-2">
                                            <div className="flex flex-col flex-1">
                                                <p className="font-semibold">Allow analytics</p>
                                                <p className="opacity-50">
                                                    Help make Wipple better by reporting analytics.
                                                </p>
                                                <p className="text-sm text-blue-500">
                                                    <a
                                                        target="_blank"
                                                        href="https://support.google.com/analytics/answer/6004245"
                                                    >
                                                        {"Google Analytics Privacy Policy ->"}
                                                    </a>
                                                </p>
                                            </div>

                                            <Checkbox
                                                checked={settings.analytics ?? true}
                                                onChange={(_e, checked) => {
                                                    setSettings(
                                                        produce((settings) => {
                                                            settings.analytics = checked;
                                                        })
                                                    );
                                                }}
                                            />
                                        </div>

                                        <Button variant="contained" onClick={popupState.close}>
                                            Done
                                        </Button>
                                    </div>
                                </Dialog>
                            </>
                        )}
                    </PopupState>

                    <a target="_blank" href="https://forms.gle/ijfLtvJ5FT6heJsD7">
                        Feedback
                    </a>
                </div>
            </div>

            <main className="flex flex-col flex-1 mx-auto w-screen h-screen">
                {isLoading ? (
                    <div className="flex flex-col flex-1 items-center justify-center">
                        <CircularProgress />
                    </div>
                ) : (
                    <Editor
                        pages={pages}
                        onChangePages={(newPages) => {
                            // HACK: Only update the URL when the pages have
                            // actually changed
                            if (JSON.stringify(newPages) !== JSON.stringify(pages)) {
                                setHasChangedPages(true);
                                setPages(newPages);
                            }
                        }}
                        activePage={activePage}
                        onChangeActivePage={(page) => {
                            setActivePage(page);
                            onViewPage(pages[page]);
                        }}
                        settings={settings}
                        onChangeActiveEditor={setActiveEditor}
                    />
                )}
            </main>
        </div>
    );
};

export default App;
