import { useEffect } from "react";
import { useNavbar } from "../../components";
import { MaterialSymbol } from "react-material-symbols";

export const Home = () => {
    const { setPrimaryActions } = useNavbar();

    useEffect(() => {
        setPrimaryActions(<p className="text-lg font-semibold">Wipple</p>);

        return () => {
            setPrimaryActions(null);
        };
    }, []);

    return (
        <div className="flex flex-col gap-2.5">
            <div className="bg-gray-50 dark:bg-gray-900 flex flex-col items-center">
                <div className="w-full max-w-screen-lg">
                    <div className="flex flex-row gap-4 p-4">
                        <PrimaryCard title="New Playground" onClick={() => alert("TODO")}>
                            <MaterialSymbol
                                icon="add"
                                className="text-blue-500 font-semibold text-6xl"
                            />
                        </PrimaryCard>

                        <PrimaryCard title="Browse Lessons" onClick={() => alert("TODO")}>
                            <img src="/images/lesson-bg.png" />
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
                        <p>Hello</p>
                        <p>Hello</p>
                        <p>Hello</p>
                        <p>Hello</p>
                    </div>
                </div>
            </div>
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
