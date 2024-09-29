import { useMemo } from "react";
import { decodeRhythm } from "./melody-picker";
import rhythmIcon from "./rhythm.png";

export const RhythmAsset = (props: {
    rhythm: string;
    disabled?: boolean;
    onClick?: () => void;
}) => {
    const color = useMemo(() => decodeRhythm(props.rhythm).options.color ?? "gray", [props.rhythm]);

    return (
        <div className="inline-block align-text-bottom rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip hover:scale-110 transition-transform">
            <div
                className="flex items-center justify-center w-4 h-4"
                style={{ backgroundColor: color }}
                onClick={props.onClick}
            >
                <img src={rhythmIcon} className="w-[14px] h-[14px]" />
            </div>
        </div>
    );
};
