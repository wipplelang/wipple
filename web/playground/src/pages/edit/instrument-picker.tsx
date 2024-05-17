import { Button } from "../../components";
import { instrumentName, instruments } from "./assets/instrument";
import { useState } from "react";

export const InstrumentPicker = (props: {
    selection: string;
    onDismiss: (instrument: string) => void;
}) => {
    const [selection, setSelection] = useState(props.selection);

    return (
        <div className="flex flex-col gap-4 w-[512px]">
            <div className="flex flex-col">
                <h1 className="text-2xl font-semibold">Choose an Instrument</h1>

                <div className="relative h-[380px]">
                    <div className="grid grid-cols-2 w-full gap-2 h-full -mx-1 px-1 py-4 overflow-y-scroll">
                        {instruments.map((instrument) => (
                            <button
                                className={`text-left self-start ${
                                    selection === instrument ? "font-bold" : ""
                                }`}
                                style={{ backgroundColor: instrument }}
                                onClick={() => setSelection(instrument)}
                            >
                                <p className="capitalize">{instrumentName(instrument)}</p>
                            </button>
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
