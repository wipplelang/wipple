import { useEffect, useState } from "react";

export const useIsOffline = () => {
    const [offline, setOffline] = useState(!navigator.onLine);

    useEffect(() => {
        const handleOffline = () => {
            console.log("Offline");
            setOffline(true);
        };

        const handleOnline = () => {
            console.log("Online");
            setOffline(false);
        };

        window.addEventListener("offline", handleOffline);
        window.addEventListener("online", handleOnline);

        return () => {
            window.removeEventListener("offline", handleOffline);
            window.removeEventListener("online", handleOnline);
        };
    }, []);

    return offline;
};
