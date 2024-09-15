import { useMemo } from "react";
import { Tooltip } from "../../components";

export const objects = ["Block 1", "Block 2"];

export const objectImageUrl = (object: string) => `/playground/images/objects/${object}.png`;

export const ObjectAsset = (props: {
    object: string;
    tooltip?: boolean;
    disabled?: boolean;
    onClick?: () => void;
}) => {
    const imageUrl = useMemo(() => objectImageUrl(props.object), [props.object]);

    const object = <img className="block w-4 h-4 p-0.5" src={imageUrl} />;

    return (
        <div
            className={`inline-block items-center justify-center align-text-bottom rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip ${
                props.tooltip ?? true ? "hover:scale-110 transition-transform" : ""
            }`}
        >
            {props.tooltip ?? true ? (
                <Tooltip
                    disabled={props.disabled}
                    description={props.object}
                    onClick={props.onClick}
                >
                    {object}
                </Tooltip>
            ) : (
                object
            )}
        </div>
    );
};
