import Peer, { DataConnection } from "peerjs";
import { useRef } from "react";

export const useCollaboration = (options: {
    onJoined: (userId: string) => void;
    onReceive: (userId: string, fromHost: boolean, data: any) => void;
}) => {
    const connection = useRef<Peer | null>(null);
    const hostPeer = useRef<DataConnection | null>(null);
    const joinedPeers = useRef<Record<string, DataConnection>>({});

    return {
        initialize: async () => {
            const peer = new Peer({
                host: import.meta.env.VITE_P2P_SERVER,
                secure: true,
            });

            connection.current = peer;

            const userId = await new Promise<string>((resolve, reject) => {
                peer.on("open", resolve);
                peer.on("error", reject);
            });

            peer.on("connection", (connection) => {
                const userId = connection.peer;
                joinedPeers.current[userId] = connection;

                connection.on("open", () => {
                    options.onJoined(userId);
                });

                connection.on("data", (data) => {
                    options.onReceive(userId, false, data);
                });

                connection.on("error", console.error);
            });

            return { userId };
        },
        connect: async (peer: string) => {
            if (!connection.current) return;

            hostPeer.current = connection.current.connect(peer, {
                serialization: "json",
                metadata: { userId: connection.current.id },
            });

            hostPeer.current.on("data", (data) => {
                options.onReceive(peer, true, data);
            });

            hostPeer.current.on("error", console.error);
        },
        send: async (data: any, include: (peer: string | null) => boolean) => {
            if (include(null)) {
                hostPeer.current?.send(data);
            }

            for (const [peer, connection] of Object.entries(joinedPeers.current)) {
                if (include(peer)) {
                    connection.send(data);
                }
            }
        },
        disconnect: async () => {
            if (!connection.current) return;

            connection.current.disconnect();
            connection.current = null;

            for (const [peer, connection] of Object.entries(joinedPeers.current)) {
                connection.close();
                delete joinedPeers.current[peer];
            }
        },
    };
};
