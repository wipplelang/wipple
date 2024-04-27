import { Button } from "../../components";
import { useState } from "react";
import { animalImageUrl, animals } from "./assets/animal";

export const AnimalPicker = (props: { selection: string; onDismiss: (color: string) => void }) => {
    const [selection, setSelection] = useState(props.selection);

    return (
        <div className="flex flex-col gap-4 w-[512px]">
            <div className="flex flex-col">
                <h1 className="text-2xl font-semibold">Choose an Animal</h1>

                <div className="relative h-[380px] overflow-y-scroll">
                    {Object.values(animals).map((animal) => (
                        <button className="w-8 h-8 m-2" onClick={() => setSelection(animal)}>
                            <img
                                src={animalImageUrl(animal)}
                                className={`w-8 h-8 ${
                                    animal === selection ? "scale-125" : "scale-100"
                                } transition-transform`}
                            />
                        </button>
                    ))}
                </div>
            </div>

            <Button role="primary" fill onClick={() => props.onDismiss(selection)}>
                Done
            </Button>
        </div>
    );
};
