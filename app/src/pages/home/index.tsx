import { useCallback, useEffect, useState } from "react";
import { Button, Footer, Skeleton, useAlert, useNavbar } from "../../components";
import { MaterialSymbol } from "react-material-symbols";
import {
    ListPlaygroundsFilter,
    PlaygroundListItem,
    createPlayground,
    deletePlayground,
    duplicatePlayground,
    listPlaygrounds,
} from "../../models";
import { Link, useNavigate } from "react-router-dom";
import { format } from "date-fns";
import { useStore } from "../../store";

export const HomePage = () => {
    const { displayAlert } = useAlert();
    const { setPrimaryActions } = useNavbar();

    useEffect(() => {
        setPrimaryActions(<p className="text-lg font-semibold">Wipple</p>);

        return () => {
            setPrimaryActions(null);
        };
    }, []);

    const [store, _setStore] = useStore();

    const [filter, setFilter] = useState<ListPlaygroundsFilter>("all");

    const [playgrounds, setPlaygrounds] = useState<PlaygroundListItem[]>();

    const loadPlaygrounds = useCallback(async () => {
        const playgrounds = await listPlaygrounds({ filter });
        setPlaygrounds(playgrounds);
    }, [store.user?.uid, filter]);

    useEffect(() => {
        loadPlaygrounds();
    }, [loadPlaygrounds]);

    const navigate = useNavigate();

    const handleNewPlayground = async () => {
        const id = await createPlayground();
        navigate(`edit/${id}`);
    };

    const handleDuplicate = async (playground: PlaygroundListItem) => {
        await duplicatePlayground(playground.id);
        await loadPlaygrounds();
    };

    const handleDelete = (playground: PlaygroundListItem) => {
        const handleConfirm = async () => {
            await deletePlayground(playground.id);
            await loadPlaygrounds();
        };

        displayAlert(({ dismiss }) => (
            <ConfirmDeleteAlert
                dismiss={dismiss}
                playground={playground}
                onConfirm={handleConfirm}
            />
        ));
    };

    return (
        <div className="flex flex-col gap-2.5">
            <div className="bg-gray-50 dark:bg-gray-900 flex flex-col items-center">
                <div className="w-full max-w-screen-lg">
                    <div className="flex flex-row gap-4 p-4">
                        <PrimaryCard title="New Playground" onClick={handleNewPlayground}>
                            <MaterialSymbol
                                icon="add"
                                className="text-blue-500 font-semibold text-6xl"
                            />
                        </PrimaryCard>

                        <PrimaryCard title="Browse Lessons" onClick={() => alert("TODO")}>
                            <img src="/playground/images/lesson-bg.png" />
                        </PrimaryCard>

                        <PrimaryCard title="Latest News" onClick={() => alert("TODO")}>
                            <div className="w-full h-full bg-gray-200 dark:bg-gray-600">
                                {/* TODO */}
                            </div>
                        </PrimaryCard>
                    </div>
                </div>
            </div>

            <div className="flex flex-col items-center">
                <div className="w-full max-w-screen-lg">
                    <div className="flex flex-col gap-4 p-4">
                        <FilterControl filter={filter} onChange={setFilter} />

                        {playgrounds ? (
                            playgrounds.length > 0 ? (
                                <div className="flex flex-col">
                                    {playgrounds.map((playground) => (
                                        <PlaygroundCard
                                            key={playground.id}
                                            playground={playground}
                                            onDuplicate={() => handleDuplicate(playground)}
                                            onDelete={
                                                playground.owner === store.user?.uid
                                                    ? () => handleDelete(playground)
                                                    : undefined
                                            }
                                        />
                                    ))}
                                </div>
                            ) : (
                                <div className="flex flex-col items-center justify-center gap-4 p-4 my-8 text-center text-gray-400 dark:text-gray-600">
                                    <MaterialSymbol icon="code_blocks" className="text-8xl" />
                                    <p className="text-3xl">No Playgrounds</p>
                                    <p className="text-xl">
                                        You can create a new playground or start a lesson.
                                    </p>
                                </div>
                            )
                        ) : (
                            <div className="flex flex-col gap-1">
                                <Skeleton height={60} />
                                <Skeleton height={60} />
                                <Skeleton height={60} />
                                <Skeleton height={60} />
                            </div>
                        )}
                    </div>
                </div>
            </div>

            <Footer />
        </div>
    );
};

const PrimaryCard = (props: React.PropsWithChildren<{ title: string; onClick: () => void }>) => (
    <div className="flex flex-col items-center gap-2.5 flex-1">
        <button
            onClick={props.onClick}
            className="w-full h-[100px] md:h-[200px] bg-white dark:bg-gray-800 rounded-lg border-[1px] border-gray-100 dark:border-gray-900 shadow-sm p-1 transition-scale ease-in-out duration-100 hover:scale-105"
        >
            <div className="flex items-center justify-center w-full h-full rounded-[4px] overflow-clip">
                {props.children}
            </div>
        </button>

        <p>{props.title}</p>
    </div>
);

const FilterControl = (props: {
    filter: ListPlaygroundsFilter;
    onChange: (filter: ListPlaygroundsFilter) => void;
}) => (
    <div className="flex flex-row gap-4">
        <Button
            role={props.filter === "all" ? "primary" : "secondary"}
            fill={props.filter === "all"}
            onClick={() => props.onChange("all")}
        >
            All
        </Button>

        <Button
            role={props.filter === "owned" ? "primary" : "secondary"}
            fill={props.filter === "owned"}
            onClick={() => props.onChange("owned")}
        >
            Created by me
        </Button>

        <Button
            role={props.filter === "shared" ? "primary" : "secondary"}
            fill={props.filter === "shared"}
            onClick={() => props.onChange("shared")}
        >
            Shared with me
        </Button>
    </div>
);

const PlaygroundCard = (props: {
    playground: PlaygroundListItem;
    onDuplicate: () => void;
    onDelete?: () => void;
}) => {
    const { displayAlert } = useAlert();

    return (
        <Link
            to={`/playground/edit/${props.playground.id}`}
            className="flex flex-row items-center justify-between p-4 hover:bg-gray-100 dark:hover:bg-gray-900 transition-colors hover:rounded-md border-b hover:border-b-transparent"
        >
            <p>{props.playground.name}</p>

            <div className="flex flex-row items-center gap-2.5 text-gray-500 dark:text-gray-400">
                <p>{format(props.playground.lastModified, "MMMM d, yyyy")}</p>

                <button
                    onClick={(e) => {
                        e.preventDefault();
                        displayAlert(({ dismiss }) => (
                            <ManagePlaygroundAlert
                                dismiss={dismiss}
                                onDuplicate={props.onDuplicate}
                                onDelete={props.onDelete}
                            />
                        ));
                    }}
                    className="text-2xl leading-3 w-8 h-8 rounded-full hover:bg-gray-200 dark:hover:bg-gray-800 transition-colors"
                >
                    <MaterialSymbol icon="more_vert" />
                </button>
            </div>
        </Link>
    );
};

const ManagePlaygroundAlert = (props: {
    dismiss: () => void;
    onDuplicate: () => void;
    onDelete?: () => void;
}) => (
    <div className="flex flex-col gap-4 p-4">
        <Button
            role="primary"
            icon="file_copy"
            onClick={() => {
                props.dismiss();

                requestAnimationFrame(() => {
                    props.onDuplicate();
                });
            }}
        >
            Duplicate
        </Button>

        {props.onDelete ? (
            <Button
                role="destructive"
                icon="delete"
                onClick={() => {
                    props.dismiss();

                    requestAnimationFrame(() => {
                        props.onDelete!();
                    });
                }}
            >
                Delete
            </Button>
        ) : null}

        <Button
            role="secondary"
            fill={false}
            onClick={() => {
                props.dismiss();
            }}
        >
            Cancel
        </Button>
    </div>
);

const ConfirmDeleteAlert = (props: {
    dismiss: () => void;
    playground: PlaygroundListItem;
    onConfirm: () => void;
}) => (
    <div className="flex flex-col gap-4 p-4">
        <p>Delete “{props.playground.name}”?</p>

        <Button
            role="destructive"
            onClick={() => {
                props.dismiss();
                props.onConfirm();
            }}
        >
            Delete
        </Button>

        <Button
            role="secondary"
            fill={false}
            onClick={() => {
                props.dismiss();
            }}
        >
            Cancel
        </Button>
    </div>
);
