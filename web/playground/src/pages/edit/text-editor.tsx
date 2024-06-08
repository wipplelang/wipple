import { useEffect, useState } from "react";
import { ContextMenuButton, Markdown, Tooltip } from "../../components";
import { MaterialSymbol } from "react-material-symbols";
import TextareaAutosize from "@mui/material/TextareaAutosize";

export const TextEditor = (props: {
    children: string;
    onChange: (value: string) => void;
    locked: boolean;
    onToggleLock?: () => void;
    autofocus?: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
    onMoveUp?: () => void;
    onMoveDown?: () => void;
    onDelete: () => void;
}) => {
    const [isFocused, setFocused] = useState(props.autofocus ?? false);

    useEffect(() => {
        if (isFocused) {
            props.onFocus?.();
        } else {
            props.onBlur?.();
        }
    }, [isFocused]);

    return (
        <div
            autoFocus={props.autofocus}
            className="flex flex-col w-full"
            tabIndex={0}
            onFocus={() => setFocused(true)}
            onBlur={() => setFocused(false)}
        >
            <div
                className={`flex flex-col overflow-clip ${
                    !props.locked ? "border-2 border-gray-100 dark:border-gray-800 rounded-md" : ""
                }`}
            >
                {props.onToggleLock ? (
                    <div className="flex flex-row items-center justify-between w-full p-1">
                        <ContextMenuButton
                            items={[
                                {
                                    title: "Move Up",
                                    icon: "arrow_upward",
                                    disabled: props.onMoveUp == null,
                                    onClick: () => props.onMoveUp!(),
                                },
                                {
                                    title: "Move Down",
                                    icon: "arrow_downward",
                                    disabled: props.onMoveDown == null,
                                    onClick: () => props.onMoveDown!(),
                                },
                                {
                                    title: "Delete",
                                    icon: "delete",
                                    role: "destructive",
                                    onClick: props.onDelete,
                                },
                            ]}
                        >
                            <MenuButton>
                                <MaterialSymbol icon="more_vert" />
                            </MenuButton>
                        </ContextMenuButton>

                        <Tooltip description={props.locked ? "Locked" : "Unlocked"}>
                            <MenuButton onClick={props.onToggleLock}>
                                <MaterialSymbol icon={props.locked ? "lock" : "lock_open"} />
                            </MenuButton>
                        </Tooltip>
                    </div>
                ) : null}

                <div className="px-3 py-1 pb-2">
                    {props.locked ? (
                        <Markdown
                            className="prose dark:prose-invert prose-blue prose-code:text-gray-900 dark:prose-code:text-gray-100 max-w-full first:prose-headings:mt-0"
                            enableMath
                        >
                            {props.children}
                        </Markdown>
                    ) : (
                        <TextareaAutosize
                            className="w-full resize-none outline-none bg-inherit dark:text-white min-h-[1lh]"
                            value={props.children}
                            onChange={(e) => props.onChange(e.target.value)}
                            placeholder="Write your text here!"
                        />
                    )}
                </div>
            </div>
        </div>
    );
};

const MenuButton = (props: React.HTMLAttributes<HTMLButtonElement>) => (
    <button
        className="flex items-center justify-center rounded-md px-1 h-7 hover:bg-gray-100 dark:hover:bg-gray-800 transition-colors text-lg text-gray-400 dark:text-gray-600"
        {...props}
    />
);
