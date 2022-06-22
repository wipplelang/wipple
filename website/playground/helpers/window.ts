import { useEffect, useState } from "react";

export const useWindow = () => {
    const [window, setWindow] = useState<Window | undefined>();

    useEffect(() => {
        setWindow(window);
    }, []);

    return window;
};
