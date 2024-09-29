import { useCallback, useEffect, useState } from "react";
import { Button, ContextMenuButton, Skeleton } from "../../components";
import { Playground, PlaygroundSetup } from "../../models";
import { MaterialSymbol } from "react-material-symbols";
import {
    createPlayground,
    deletePlayground,
    duplicatePlayground,
    listPlaygrounds,
} from "../../models";
import { useNavigate } from "react-router-dom";
import { useStore } from "../../store";
import { UserButton } from "../../components/user";
import turtleIcon from "../../runtimes/turtle/assets/turtle.png";
import { SetupIcon } from "../edit/setup-icon";
import { signIn, signInAsGuest } from "../../helpers";
import { produce } from "immer";

export const HomePage = () => {
    const [store, setStore] = useStore();
    const navigate = useNavigate();

    const handleSignIn = useCallback(async () => {
        await signIn();
    }, []);

    const handleContinueAsGuest = useCallback(async () => {
        const user = await signInAsGuest();

        setStore(
            produce((store) => {
                store.user = user;
            }),
        );
    }, []);

    const [playgrounds, setPlaygrounds] = useState<Playground[]>();

    const loadPlaygrounds = useCallback(async () => {
        if (!store.user) {
            return;
        }

        try {
            const playgrounds = await listPlaygrounds();
            setPlaygrounds(playgrounds);
        } catch (error) {
            console.error(error);
        }
    }, [store.user?.uid]);

    useEffect(() => {
        loadPlaygrounds();
    }, [loadPlaygrounds]);

    const [isCreatingNewPlayground, setCreatingNewPlayground] = useState(false);

    const handleNewPlayground = useCallback(
        async (setup: PlaygroundSetup) => {
            if (isCreatingNewPlayground) {
                return;
            }

            setCreatingNewPlayground(true);

            try {
                const name = prompt("What do you want to call your playground?");

                if (!name) {
                    return;
                }

                const id = await createPlayground(name, setup);

                navigate(`edit/${id}`);
            } finally {
                setCreatingNewPlayground(false);
            }
        },
        [isCreatingNewPlayground],
    );

    const handleSelectPlayground = useCallback(
        (playground: Playground) => {
            navigate(`edit/${playground.id}`);
        },
        [navigate],
    );

    const handleDuplicatePlayground = useCallback(async (playground: Playground) => {
        await duplicatePlayground(playground.id);
        await loadPlaygrounds();
    }, []);

    const handleDeletePlayground = useCallback(async (playground: Playground) => {
        await deletePlayground(playground.id);
        await loadPlaygrounds();
    }, []);

    return (
        <div className="flex flex-col items-center">
            <div
                className={`relative flex flex-col w-full overflow-clip bg-gray-50 dark:bg-gray-900 ${
                    store.user ? "" : "h-screen justify-center pb-36"
                }`}
            >
                <img
                    src="/playground/images/background.svg"
                    className="absolute inset-0 w-full h-full object-cover object-center opacity-30"
                />

                <div className="flex flex-row items-center justify-end px-4 h-20">
                    <UserButton />
                </div>

                <div className="flex flex-col items-center justify-center gap-4 pb-14 z-10">
                    <img
                        src="/playground/images/logo.svg"
                        alt="Wipple"
                        className="w-16 h-16 mb-2"
                    />

                    <h1 className="text-3xl font-semibold">
                        {store.user?.displayName
                            ? `Welcome back, ${store.user.displayName}`
                            : "Welcome to Wipple"}
                    </h1>

                    {/* TODO: Practice Points */}
                    {/* <div className="flex flex-row items-center gap-0.5 font-semibold text-blue-500">
                        <MaterialSymbol icon="star" fill className="text-xl -translate-y-px" />
                        <p>10 Practice Points</p>
                    </div> */}
                </div>

                {!store.user ? (
                    <div className="flex flex-col items-center w-full bg-gray-50 dark:bg-gray-900 z-10">
                        <div className="flex flex-col items-stretch justify-center gap-2">
                            <Button role="primary" onClick={handleSignIn}>
                                Sign In
                            </Button>

                            <Button role="secondary" onClick={handleContinueAsGuest}>
                                Continue as Guest
                            </Button>
                        </div>
                    </div>
                ) : null}
            </div>

            {store.user ? (
                <div className="flex flex-col gap-12 w-full max-w-screen-lg mt-8">
                    <Section title="Get Started">
                        <NewPlaygroundCard
                            title="Blank"
                            backgroundClassName="bg-gradient-to-b from-blue-50 to-blue-100 dark:from-blue-900 dark:to-blue-950"
                            onClick={() => handleNewPlayground(null)}
                        >
                            <MaterialSymbol
                                icon="article"
                                className="text-6xl font-semibold text-blue-500"
                            />
                        </NewPlaygroundCard>

                        <NewPlaygroundCard
                            title="Turtle"
                            backgroundClassName="bg-gradient-to-b from-green-50 to-green-100 dark:from-green-900 dark:to-green-950"
                            onClick={() => handleNewPlayground("turtle")}
                        >
                            <img src={turtleIcon} className="w-12 h-12" />
                        </NewPlaygroundCard>

                        <NewPlaygroundCard
                            title="Music"
                            backgroundClassName="bg-gradient-to-b from-orange-50 to-orange-100 dark:from-orange-900 dark:to-orange-950"
                            onClick={() => handleNewPlayground("music")}
                        >
                            <MaterialSymbol
                                icon="music_note"
                                className="text-6xl font-semibold text-orange-500"
                            />
                        </NewPlaygroundCard>
                    </Section>

                    {playgrounds ? (
                        playgrounds.length > 0 ? (
                            <Section title="Recents">
                                {playgrounds.map((playground) => (
                                    <PlaygroundCard
                                        key={playground.id}
                                        playground={playground}
                                        onClick={() => handleSelectPlayground(playground)}
                                        onDuplicate={() => handleDuplicatePlayground(playground)}
                                        onDelete={
                                            playground.owner === store.user?.uid
                                                ? () => handleDeletePlayground(playground)
                                                : undefined
                                        }
                                    />
                                ))}
                            </Section>
                        ) : null
                    ) : (
                        <Section>
                            <SkeletonCard />
                            <SkeletonCard />
                            <SkeletonCard />
                        </Section>
                    )}

                    <Footer />
                </div>
            ) : null}
        </div>
    );
};

const Footer = () => (
    <p className="my-8 text-center text-gray-400 dark:text-gray-600">
        Made by{" "}
        <a
            href="https://gramer.dev"
            className="font-semibold text-gray-500 hover:text-gray-600 dark:text-gray-400"
        >
            Wilson Gramer
        </a>
    </p>
);

const Section = (props: React.PropsWithChildren<{ title?: string }>) => (
    <div className="flex flex-col gap-4">
        {props.title != null ? <h2 className="text-2xl font-semibold">{props.title}</h2> : null}

        <div className="grid grid-cols-3 gap-5">{props.children}</div>
    </div>
);

const cardHeightClassName = "h-[100px] md:h-[200px]";

const Card = (
    props: React.PropsWithChildren<{
        title: string;
        onClick: () => void;
    }>,
) => (
    <div className="flex flex-col items-center gap-2.5">
        <button
            onClick={props.onClick}
            className={`w-full ${cardHeightClassName} bg-white dark:bg-gray-800 rounded-lg border-[1px] border-gray-100 dark:border-gray-900 shadow-sm hover:scale-105 transition-scale duration-150`}
        >
            <div className="flex items-center justify-center w-full h-full rounded-[4px] overflow-clip">
                {props.children}
            </div>
        </button>

        <p>{props.title}</p>
    </div>
);

const SkeletonCard = () => <Skeleton className={`${cardHeightClassName} opacity-25`} />;

const NewPlaygroundCard = (
    props: React.PropsWithChildren<{
        title: string;
        backgroundClassName: string;
        onClick: () => void;
    }>,
) => (
    <Card title={props.title} onClick={props.onClick}>
        <div
            className={`flex-1 w-full h-full flex items-center justify-center ${props.backgroundClassName}`}
        >
            {props.children}
        </div>
    </Card>
);

const PlaygroundCard = (props: {
    playground: Playground;
    onClick: () => void;
    onDuplicate: () => void;
    onDelete?: () => void;
}) => (
    <Card title={props.playground.name} onClick={props.onClick}>
        <div className="relative flex-1 w-full h-full">
            <div className="flex items-center justify-center">
                <SetupIcon setup={props.playground?.setup ?? null} size="lg" />
            </div>

            <ContextMenuButton
                className="absolute top-2 right-1 z-10"
                items={[
                    {
                        title: "Duplicate",
                        icon: "file_copy",
                        onClick: props.onDuplicate,
                    },
                    {
                        title: "Delete",
                        icon: "delete",
                        role: "destructive",
                        disabled: props.onDelete == null,
                        onClick: props.onDelete,
                    },
                ]}
            >
                <div
                    className="flex items-center justify-center text-2xl leading-3 w-8 h-8 rounded-full hover:bg-gray-200 dark:hover:bg-gray-800 transition-colors"
                    onClick={(e) => e.preventDefault()}
                >
                    <MaterialSymbol icon="more_vert" />
                </div>
            </ContextMenuButton>
        </div>
    </Card>
);
