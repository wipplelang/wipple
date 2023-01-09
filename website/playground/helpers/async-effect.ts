import { DependencyList, useEffect } from "react";

export const useAsyncEffect = (callback: () => Promise<void>, deps?: DependencyList) => {
    useEffect(() => {
        try {
            callback();
        } catch (error) {
            console.error(error);
        }
    }, deps);
};
