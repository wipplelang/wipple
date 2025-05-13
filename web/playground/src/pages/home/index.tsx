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
            <div className="mx-auto flex h-full w-full max-w-[450px] flex-1 flex-col [justify-content:safe_center] gap-2.5 py-8">
                <div className="flex justify-center mb-4">
                    <Logo title="Welcome to Wipple" />
                </div>

                <h1 className="mb-3.5 text-center text-2xl font-semibold flex flex-col items-center gap-2.5">
                    What do you want to create?
                </h1>

                {playgroundSetups.map((setup) => {
                    const { name, description } = setupDescriptions[setup];

                    return (
                        <button key={setup} onClick={() => handleNewPlayground(setup)}>
                            <Box shadow={false}>
                                <div className="flex flex-1 flex-col justify-start items-stretch text-left hover:bg-gray-50 dark:hover:bg-gray-800 -m-3 p-2.5">
                                    <h2 className="flex flex-col gap-1 font-semibold">
                                        <SetupIcon setup={setup} size="lg" />

                                        <span className="text-[large]">{name}</span>
                                    </h2>
                                    <p>{description}</p>
                                </div>
                            </Box>
                        </button>
                    );
                })}
            </div>
        </div>
    );
};

const setupDescriptions: Record<
    NonNullable<PlaygroundSetup>,
    { name: string; description: string }
> = {
    turtle: {
        name: "Turtle",
        description: "Create drawings",
    },
    music: {
        name: "Music",
        description: "Make a composition",
    },
    math: {
        name: "Math",
        description: "Explore graphs",
    },
};
