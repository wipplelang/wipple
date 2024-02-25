import { useMemo } from "react";
import { Tooltip } from "../../components";

export const Asset = (props: {
    children: string;
    onClick?: (type: string, value: string) => void;
}) => {
    const [type, value] = useMemo(() => props.children.split(":", 2), [props.children]);

    let content: JSX.Element;
    switch (type) {
        case "color":
            content = <ColorAsset color={value} />;
            break;
        default:
            content = <UnknownAsset value={value} />;
            break;
    }

    return (
        <div
            className="inline-block align-text-bottom w-fit hover:scale-110 transition-transform"
            onClick={() => props.onClick?.(type, value)}
        >
            <div className="rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip">
                {content}
            </div>
        </div>
    );
};

const ColorAsset = (props: { color: string }) => (
    <Tooltip description={props.color}>
        <div className="ui-font w-4 h-4" style={{ backgroundColor: props.color }} />
    </Tooltip>
);

const UnknownAsset = (props: { value: string }) => (
    <div className="ui-font w-4 h-4">{props.value}</div>
);
