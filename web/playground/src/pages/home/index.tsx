import { useCallback, useEffect, useState } from "react";
import { PlaygroundSetup, playgroundSetups } from "../../models";
import { createPlayground } from "../../models";
import { useNavigate } from "react-router-dom";
import { useStore } from "../../store";
import { SetupIcon } from "../edit/setup-icon";
import { signInAsGuest } from "../../helpers";
import { Box } from "../../components/box";
import { Logo } from "../../components";

export const HomePage = () => {
    const [store, _setStore] = useStore();
    const navigate = useNavigate();

    useEffect(() => {
        if (!store.user) {
            signInAsGuest();
        }
    }, []);

    const [isCreatingNewPlayground, setCreatingNewPlayground] = useState(false);

    const handleNewPlayground = useCallback(
        async (setup: PlaygroundSetup) => {
            if (isCreatingNewPlayground) {
                return;
            }

            setCreatingNewPlayground(true);

            try {
                const id = await createPlayground(setup);
                navigate(`edit/${id}`);
            } finally {
                setCreatingNewPlayground(false);
            }
        },
        [isCreatingNewPlayground],
    );

    return (
        <div className="flex flex-col items-center w-screen h-screen">
            <div className="self-start flex justify-center mb-4 p-2.5">
                <Logo />
            </div>

            <div className="mx-auto flex flex-1 w-full max-w-[600px] flex-col gap-2.5 pt-[80px] px-4 overflow-y-scroll">
                <h1 className="mb-3.5 text-center text-2xl font-semibold flex flex-col items-center gap-2.5">
                    What do you want to create?
                </h1>

                <div className="flex flex-row gap-2.5">
                    {Object.entries(setupDescriptions).map(([key, { name, className }]) => {
                        const setup = key === "blank" ? null : (key as PlaygroundSetup);

                        return (
                            <button
                                key={key}
                                className="flex-1 flex"
                                onClick={() => handleNewPlayground(setup)}
                            >
                                <Box fill>
                                    <div
                                        className={`flex-1 relative hover:bg-gray-50 dark:hover:bg-gray-800 -m-3 p-2.5`}
                                    >
                                        <div
                                            className={`absolute inset-0 bg-gradient-to-b ${className} z-0 opacity-50 hover:opacity-100 transition-opacity`}
                                        />

                                        <h2 className="flex flex-col justify-between gap-1 h-full font-semibold text-left *:z-[1] pointer-events-none">
                                            <SetupIcon setup={setup} size="lg" />
                                            <span className="text-[large]">{name}</span>
                                        </h2>
                                    </div>
                                </Box>
                            </button>
                        );
                    })}
                </div>
            </div>
        </div>
    );
};

const setupDescriptions: {
    [K in NonNullable<PlaygroundSetup> | "blank"]: {
        name: string;
        className: string;
    };
} = {
    turtle: {
        name: "Turtle",
        className: "from-green-100/50 to-green-100",
    },
    music: {
        name: "Music",
        className: "from-orange-100/50 to-orange-100",
    },
    math: {
        name: "Math",
        className: "from-blue-100/50 to-blue-100",
    },
    blank: {
        name: "Blank",
        className: "from-slate-100/50 to-slate-100",
    },
};
