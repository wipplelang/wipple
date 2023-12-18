import { Button, IconButton } from "@mui/material";
import CloseRounded from "@mui/icons-material/CloseRounded";
import ArticleRounded from "@mui/icons-material/ArticleRounded";
import turtleIcon from "../../ui/turtle/src/assets/turtle.png";
import MusicNoteRounded from "@mui/icons-material/MusicNoteRounded";
import CalculateRounded from "@mui/icons-material/CalculateRounded";
import SportsEsportsRounded from "@mui/icons-material/SportsEsportsRounded";
import EditNoteRounded from "@mui/icons-material/EditNoteRounded";
import ElectricBoltRounded from "@mui/icons-material/ElectricBoltRounded";
import atomIcon from "../../ui/physics/src/assets/atom.png";

export interface PickerProps {
    onSelect: (section: { type: "code"; setup?: string } | { type: "text" }) => void;
    onCancel: () => void;
}

export const Picker = (props: PickerProps) => {
    return (
        <div className="relative flex flex-col gap-4 p-4 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-black dark:text-white">
            <div className="flex items-center justify-between">
                <h1 className="text-2xl font-bold">What would you like to create?</h1>

                <IconButton onClick={props.onCancel}>
                    <CloseRounded />
                </IconButton>
            </div>

            <div className="grid grid-cols-2 md:grid-cols-4 auto-rows-max gap-4">
                <PickerItem
                    setup={undefined}
                    name="Blank"
                    description="Create a new program from scratch."
                    onSelect={() => props.onSelect({ type: "code", setup: undefined })}
                />

                <PickerItem
                    setup="turtle"
                    name="Turtle"
                    description="Draw graphics on the screen."
                    onSelect={() => props.onSelect({ type: "code", setup: "turtle" })}
                />

                <PickerItem
                    setup="music"
                    name="Music"
                    description="Make a musical composition."
                    onSelect={() => props.onSelect({ type: "code", setup: "music" })}
                />

                <PickerItem
                    setup="graphing"
                    name="Math"
                    description="Plot mathematical functions."
                    onSelect={() => props.onSelect({ type: "code", setup: "graphing" })}
                />

                <PickerItem
                    setup="game"
                    name="Game"
                    description="Create a video game."
                    onSelect={() => props.onSelect({ type: "code", setup: "game" })}
                />

                <PickerItem
                    setup="physics"
                    name="Physics"
                    description="Learn physics by building your own demonstrations."
                    onSelect={() => props.onSelect({ type: "code", setup: "physics" })}
                />

                <PickerItem
                    setup="energy"
                    name="Energy"
                    description="Measure the energy use of your program."
                    onSelect={() => props.onSelect({ type: "code", setup: "energy" })}
                />

                <PickerItem
                    setup="text"
                    name="Text"
                    description="Write text alongside your code."
                    onSelect={() => props.onSelect({ type: "text" })}
                />
            </div>
        </div>
    );
};

const PickerItem = (props: {
    setup: string | undefined;
    name: string;
    description: string;
    onSelect: () => void;
}) => (
    <div className="group bg-gray-50 dark:bg-gray-800 rounded-lg">
        <Button
            variant="outlined"
            color="inherit"
            sx={{ border: "none", padding: 2, width: "100%", height: "100%" }}
            onClick={props.onSelect}
        >
            <div className="flex flex-col items-start gap-2 w-full h-full">
                <div className="group-hover:scale-110 transition-transform">
                    <SetupIcon setup={props.setup} size="large" />
                </div>

                <div className="flex flex-col normal-case text-left ui-font tracking-normal">
                    <p className="text-lg font-bold">{props.name}</p>
                    <p className="text-sm opacity-75">{props.description}</p>
                </div>
            </div>
        </Button>
    </div>
);

export const SetupIcon = (props: { setup: string | undefined; size: "medium" | "large" }) => {
    switch (props.setup) {
        case undefined:
            return (
                <ArticleRounded fontSize={props.size} color="inherit" className="text-blue-500" />
            );
        case "turtle":
            return (
                <img
                    src={turtleIcon}
                    className={`${
                        props.size === "large" ? "w-[31px]" : "w-[24px]"
                    } p-[2px] aspect-square`}
                />
            );
        case "music":
            return (
                <MusicNoteRounded
                    fontSize={props.size}
                    color="inherit"
                    className="text-orange-500 pb-0.5"
                />
            );
        case "graphing":
            return (
                <CalculateRounded
                    fontSize={props.size}
                    color="inherit"
                    className="text-green-500 pb-0.5"
                />
            );
        case "game":
            return (
                <SportsEsportsRounded
                    fontSize={props.size}
                    color="inherit"
                    className="text-purple-500 pb-0.5"
                />
            );
        case "physics":
            return (
                <img
                    src={atomIcon}
                    className={`${
                        props.size === "large" ? "w-[31px]" : "w-[24px]"
                    } p-[2px] aspect-square`}
                />
            );
        case "energy":
            return (
                <ElectricBoltRounded
                    fontSize={props.size}
                    color="inherit"
                    className="text-yellow-500 pb-0.5"
                />
            );
        case "text":
            return (
                <EditNoteRounded
                    fontSize={props.size}
                    color="inherit"
                    className="text-gray-500 pb-0.5"
                />
            );
        default:
            return null;
    }
};
