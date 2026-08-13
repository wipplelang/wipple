export const debounce = <Args extends unknown[]>(delay: number, func: (...args: Args) => void) => {
    let timeout: NodeJS.Timeout | undefined;
    return (...args: Args) => {
        if (timeout !== undefined) {
            clearTimeout(timeout);
        }

        timeout = setTimeout(() => {
            func(...args);
            timeout = undefined;
        }, delay);
    };
};
