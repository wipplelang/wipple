import { Button, Tooltip } from "../components";
import { useState } from "react";
import { objects, objectImageUrl } from "./assets/object";

export const ObjectPicker = (props: { selection: string; onDismiss: (color: string) => void }) => {
    const [selection, setSelection] = useState(props.selection);

    return (
        <div className="flex flex-col gap-4 w-[512px]">
            <div className="flex flex-col">
                <h1 className="text-2xl font-semibold">Choose an Object</h1>

                <div className="relative h-[380px] overflow-y-scroll">
                    {Object.values(objects).map((object) => (
                        <Tooltip
                            key={object}
                            description={<span className="capitalize">{object}</span>}
                        >
                            <button className="w-8 h-8 m-2" onClick={() => setSelection(object)}>
                                <img
                                    src={objectImageUrl(object)}
                                    className={`w-8 h-8 ${
                                        object === selection
                                            ? "scale-125"
                                            : "scale-100 hover:scale-110"
                                    } transition-transform`}
                                />
                            </button>
                        </Tooltip>
                    ))}
                </div>
            </div>

            <Button role="primary" fill onClick={() => props.onDismiss(selection)}>
                Done
            </Button>
        </div>
    );
};
