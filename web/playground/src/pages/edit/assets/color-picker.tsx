import { MaterialSymbol } from "react-material-symbols";
import { Button, Transition } from "../../../components";
import { colorName, colors } from "./color";
import { useState } from "react";

export const ColorPicker = (props: { selection: string; onDismiss: (color: string) => void }) => {
    const [selection, setSelection] = useState(props.selection);

    return (
        <div className="flex flex-col gap-4 w-[512px]">
            <div className="flex flex-col">
                <h1 className="text-2xl font-semibold">Choose a Color</h1>

                <div className="relative h-[380px]">
                    <div className="grid grid-cols-3 w-full gap-4 h-full -mx-1 px-1 py-4 overflow-y-scroll">
                        {Object.values(colors).map((shades) => (
                            <div className="flex flex-col w-full gap-2">
                                <p className="capitalize">{colorName(shades["500"])}</p>

                                <div className="grid grid-cols-6 gap-1 overflow-x-visible">
                                    {Object.values(shades).map((color) => (
                                        <button
                                            className="flex items-center justify-center w-full aspect-square rounded-md border-2 border-gray-100 dark:border-gray-800 hover:scale-110 transition-transform"
                                            style={{ backgroundColor: color }}
                                            onClick={() => setSelection(color)}
                                        >
                                            <Transition
                                                in={color === selection}
                                                inStyle={{ opacity: 1, scale: 1 }}
                                                outStyle={{ opacity: 0.5, scale: 0 }}
                                            >
                                                <MaterialSymbol
                                                    icon="check"
                                                    color="white"
                                                    size={18}
                                                    className="drop-shadow-md"
                                                />
                                            </Transition>
                                        </button>
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>

                    <div className="absolute left-0 right-0 top-0 h-5 bg-gradient-to-t from-transparent to-white dark:to-gray-900" />
                    <div className="absolute left-0 right-0 bottom-0 h-5 bg-gradient-to-b from-transparent to-white dark:to-gray-900" />
                </div>
            </div>

            <Button role="primary" fill onClick={() => props.onDismiss(selection)}>
                Done
            </Button>
        </div>
    );
};
