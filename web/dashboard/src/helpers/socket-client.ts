import { useEffect, useState } from "react";
import { io, Socket } from "socket.io-client";
import { PlaygroundPageItem } from "wipple-playground";
import { useStore } from "../store";
import { getUserInfo } from "./database";

export const useSocketClient = (options: {
    onUpdateStudents: (students: { id: string; name: string }[]) => void;
    onUpdateStudent: (studentId: string, data: PlaygroundPageItem) => void;
    onAskForHelp: (studentId: string) => void;
}) => {
    const [store, setStore] = useStore();
    const [socket, setSocket] = useState<Socket>();

    useEffect(() => {
        (async () => {
            if (!store.user) {
                console.error("Not signed in");
                return;
            }

            const userInfo = await getUserInfo();

            if (!userInfo) {
                console.error(`User ${store.user.uid} does not have any info`);
                setSocket(undefined);
                return;
            }

            const idToken = await store.user.getIdToken();

            const socket = io(`${import.meta.env.VITE_CLASSROOM_SOCKET_BASE_URL}/teacher`, {
                query: {
                    classroomCode: userInfo.classroomCode,
                    idToken,
                },
                autoConnect: false,
            });

            socket.on("updateStudents", (input: { students: { id: string; name: string }[] }) => {
                options.onUpdateStudents(input.students);
            });

            socket.on("updateStudent", (input: { studentId: string; data: PlaygroundPageItem }) => {
                options.onUpdateStudent(input.studentId, input.data);
            });

            socket.on("askForHelp", (input: { studentId: string }) => {
                options.onAskForHelp(input.studentId);
            });

            socket.connect();

            setSocket(socket);
        })();
    }, [store.user]);

    return socket
        ? {
              stopSharing: (studentId: string) => {
                  socket.emit("stopSharing", { studentId });
              },
              subscribeToStudent: (studentId: string) => {
                  socket.emit("subscribeToStudent", { studentId });
              },
              unsubscribeFromStudent: (studentId: string) => {
                  socket.emit("unsubscribeFromStudent", { studentId });
              },
          }
        : undefined;
};
