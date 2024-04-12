import { MaterialSymbol } from "react-material-symbols";
import { ContextMenuButton } from "../../../components";

export const DropdownAsset = (props: {
    disabled?: boolean;
    value: string;
    options: string[];
    onChange: (value: string) => void;
}) => {
    return (
        <ContextMenuButton
            disabled={props.disabled}
            items={[
                ...props.options.map((option) => ({
                    title: () => (
                        <code>
                            <span
                                className={`ui-font text-blue-500 ${
                                    option === props.value ? "" : "opacity-0"
                                }`}
                            >
                                {"✓ "}
                            </span>
                            {option}
                        </code>
                    ),
                    onClick: () => props.onChange(option),
                })),
                {
                    title: () => (
                        <code>
                            <span
                                className={`ui-font text-blue-500 ${
                                    props.value != null && !props.options.includes(props.value)
                                        ? ""
                                        : "opacity-0"
                                }`}
                            >
                                {"✓ "}
                            </span>

                            <span className="opacity-50">Custom...</span>
                        </code>
                    ),
                    onClick: () => {
                        const value = prompt("Enter a custom value:");
                        if (!value) return;

                        props.onChange(value);
                    },
                },
            ]}
        >
            <div className="flex flex-row items-center justify-center rounded-lg h-[calc(1lh-2pt)] border-2 gap-0.5 pl-1.5 pr-0.5 border-gray-100 dark:border-gray-800 overflow-clip hover:bg-gray-50 dark:hover:bg-gray-900 transition-colors">
                <p>{props.value ?? props.options[0]}</p>

                <MaterialSymbol icon="expand_more" size={20} className="text-blue-500" />
            </div>
        </ContextMenuButton>
    );
};
