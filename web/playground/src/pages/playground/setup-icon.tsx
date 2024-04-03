import { MaterialSymbol } from "react-material-symbols";
import { turtleImage } from "../../runtimes/turtle";

export const SetupIcon = (props: { setup: string | undefined; size: "sm" | "lg" }) => {
    const size = props.size === "lg" ? 31 : 20;
    const sizeClass = props.size === "lg" ? "w-[31px]" : "w-[20px]";
    const margin = props.size === "lg" ? "mb-1.5" : "mb-0.5";
    const padding = props.size === "lg" ? "pb-0.5" : "pb-[1px]";

    switch (props.setup) {
        case undefined:
            return (
                <MaterialSymbol
                    icon="article"
                    size={size}
                    fill
                    className={`text-blue-500 ${padding}`}
                />
            );
        case "turtle":
            return (
                <img src={turtleImage} className={`${sizeClass} p-[2px] aspect-square ${margin}`} />
            );
        case "music":
            return (
                <MaterialSymbol
                    icon="music_note"
                    size={size}
                    fill
                    className={`text-orange-500 ${padding}`}
                />
            );
        case "graphing":
            return (
                <MaterialSymbol
                    icon="calculate"
                    size={size}
                    fill
                    className={`text-green-500 ${padding}`}
                />
            );
        case "game":
            return (
                <MaterialSymbol
                    icon="sports_esports"
                    size={size}
                    fill
                    className={`text-purple-500 ${padding}`}
                />
            );
        case "physics":
            return (
                <img
                    // src={atomIcon} // TODO: Physics runtime
                    className={`${sizeClass} p-[2px] aspect-square ${margin}`}
                />
            );
        case "text":
            return (
                <MaterialSymbol
                    icon="edit_note"
                    size={size}
                    className={`text-gray-500 ${padding}`}
                />
            );
        default:
            return null;
    }
};
