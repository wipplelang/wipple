import { io } from "socket.io-client";
import { PlaygroundPageItem } from "wipple-playground";
import { getUserInfo } from "../models";

import { User } from "firebase/auth";

export const initializeSocketClient = async (
    user: User,
    options: {
        // TODO: Event handlers
        onDisconnect: () => void;
    },
) => {
    if (!user) return undefined;

    const userInfo = await getUserInfo();
    if (!userInfo) {
        console.error(`User ${user.uid} does not have any info`);
        return undefined;
    }

    const idToken = await user.getIdToken();

    const socket = io(`${import.meta.env.VITE_CLASSROOM_SOCKET_BASE_URL}/student`, {
        query: {
            classroomCode: userInfo.classroomCode,
            idToken,
        },
    });

    socket.on("disconnect", () => {
        options.onDisconnect();
    });

    return {
        update: (data: PlaygroundPageItem) => {
            socket.emit("update", { data });
        },
        askForHelp: () => {
            socket.emit("askForHelp", {});
        },
        disconnect: () => {
            socket.disconnect();
        },
    };
};
