import Slider from "@mui/material/Slider";
import { useState } from "react";

export const SliderAsset = (props: {
    disabled?: boolean;
    value: number;
    min: number;
    max: number;
    onChange: (value: number) => void;
}) => {
    const [value, setValue] = useState(props.value);

    return (
        <div className="flex flex-row items-center justify-center rounded-lg w-20 h-[calc(1lh-2pt)] border-[1.5px] gap-0.5 px-2.5 border-gray-100 dark:border-gray-800 overflow-clip">
            <Slider
                size="small"
                value={value}
                min={props.min}
                max={props.max}
                step={0.01}
                disabled={props.disabled}
                onChange={(_, value) => {
                    if (typeof value !== "number") return;
                    setValue(value);
                }}
                onChangeCommitted={() => {
                    props.onChange(value);
                }}
                sx={{
                    "& .MuiSlider-thumb:hover": {
                        boxShadow: "0 0 0 3px rgba(25, 118, 210, 0.15)",
                    },
                }}
            />
        </div>
    );
};
