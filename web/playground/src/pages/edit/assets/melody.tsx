import { useMemo } from "react";
import { MaterialSymbol } from "react-material-symbols";
import { decodeMelody } from "./melody-picker";

export const MelodyAsset = (props: {
    melody: string;
    disabled?: boolean;
    onClick?: () => void;
}) => {
    const color = useMemo(() => decodeMelody(props.melody).options.color ?? "gray", [props.melody]);

    return (
        <div className="inline-block align-text-bottom rounded-md border-[1.5px] border-gray-100 dark:border-gray-800 overflow-clip hover:scale-110 transition-transform">
            <div
                className="flex items-center justify-center w-4 h-4"
                style={{ backgroundColor: color }}
                onClick={props.onClick}
            >
                <MaterialSymbol icon="music_note" className="text-white" />
            </div>
        </div>
    );
};
