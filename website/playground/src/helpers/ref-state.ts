import { useEffect, useRef, useState } from "react";

export const useRefState = <T>(initialValue: T) => {
    const [state, setState] = useState(initialValue);
    const stateRef = useRef(state);

    useEffect(() => {
        stateRef.current = state;
    }, [state]);

    return [stateRef, setState] as const;
};
