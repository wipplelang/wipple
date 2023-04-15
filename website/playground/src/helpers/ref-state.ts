import { useRef, useState } from "react";

export const useRefState = <T>(initialValue: T) => {
    const [state, setState] = useState(initialValue);
    const stateRef = useRef(state);

    const setRefState = (value: React.SetStateAction<T>) => {
        stateRef.current = typeof value === "function" ? (value as any)(stateRef.current) : value;
        setState(value);
    };

    return [stateRef, setRefState] as const;
};
