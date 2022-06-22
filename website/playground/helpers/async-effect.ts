import { DependencyList, useEffect, useLayoutEffect } from "react";

export const useAsyncEffect = (callback: () => Promise<void>, deps?: DependencyList) => {
    useEffect(() => {
        callback();
    }, deps);
};
