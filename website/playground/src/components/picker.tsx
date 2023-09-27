import { Button, IconButton } from "@mui/material";
import CloseRounded from "@mui/icons-material/CloseRounded";
import ArticleRounded from "@mui/icons-material/ArticleRounded";
import turtleIcon from "../../ui/turtle/src/assets/turtle.png";
import MusicNoteRounded from "@mui/icons-material/MusicNoteRounded";
import CalculateRounded from "@mui/icons-material/CalculateRounded";
import SportsEsportsRounded from "@mui/icons-material/SportsEsportsRounded";
import EditNoteRounded from "@mui/icons-material/EditNoteRounded";

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
                    icon={ArticleRounded}
                    iconClassName="text-blue-500"
                    name="Blank"
                    description="Create a new program from scratch."
                    onSelect={() => props.onSelect({ type: "code", setup: undefined })}
                />

                <PickerItem
                    icon={turtleIcon}
                    iconClassName="w-[31px] p-[2px] aspect-square"
                    name="Turtle"
                    description="Draw graphics on the screen."
                    onSelect={() => props.onSelect({ type: "code", setup: "turtle" })}
                />

                <PickerItem
                    icon={MusicNoteRounded}
                    iconClassName="text-orange-500"
                    name="Music"
                    description="Make a musical composition."
                    onSelect={() => props.onSelect({ type: "code", setup: "music" })}
                />

                <PickerItem
                    icon={CalculateRounded}
                    iconClassName="text-green-500"
                    name="Math"
                    description="Plot mathematical functions."
                    onSelect={() => props.onSelect({ type: "code", setup: "graphing" })}
                />

                <PickerItem
                    icon={SportsEsportsRounded}
                    iconClassName="text-purple-500"
                    name="Game"
                    description="Create a video game."
                    onSelect={() => props.onSelect({ type: "code", setup: "game" })}
                />

                <PickerItem
                    icon={EditNoteRounded}
                    iconClassName="text-gray-500"
                    name="Text"
                    description="Write text alongside your code."
                    onSelect={() => props.onSelect({ type: "text" })}
                />
            </div>
        </div>
    );
};

const PickerItem = (props: {
    icon: string | typeof CloseRounded;
    iconClassName: string;
    name: string;
    description: string;
    onSelect: () => void;
}) => (
    <div className="bg-gray-50 dark:bg-gray-800 rounded-lg">
        <Button
            variant="outlined"
            color="inherit"
            sx={{ border: "none", padding: 2, width: "100%", height: "100%" }}
            onClick={props.onSelect}
        >
            <div className="flex flex-col gap-2 w-full h-full">
                {typeof props.icon === "string" ? (
                    <img src={props.icon} className={props.iconClassName} />
                ) : (
                    <props.icon className={props.iconClassName} fontSize="large" color="inherit" />
                )}

                <div className="flex flex-col normal-case text-left ui-font tracking-normal">
                    <p className="text-lg font-bold">{props.name}</p>
                    <p className="text-sm opacity-75">{props.description}</p>
                </div>
            </div>
        </Button>
    </div>
);
