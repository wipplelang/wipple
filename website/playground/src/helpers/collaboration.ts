import Peer, { DataConnection } from "peerjs";
import { useRef } from "react";

export const useCollaboration = (options: {
    onHostReceive: (data: any) => void;
    onHostLeft: () => void;
    onPeerJoined: (userId: string) => void;
    onPeerReceive: (userId: string, data: any) => void;
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
                    options.onPeerJoined(userId);
                });

                connection.on("data", (data) => {
                    options.onPeerReceive(userId, data);
                });

                connection.on("error", console.error);
            });

            return { userId };
        },
        connectToHost: async (peer: string) => {
            if (!connection.current) return;

            hostPeer.current = connection.current.connect(peer, {
                serialization: "json",
                metadata: { userId: connection.current.id },
            });

            hostPeer.current.on("data", (data) => {
                options.onHostReceive(data);
            });

            hostPeer.current.on("close", () => {
                options.onHostLeft();
            });

            hostPeer.current.on("error", console.error);
        },
        sendToHost: async (data: any) => {
            hostPeer.current?.send(data);
        },
        sendToPeer: async (user: string, data: any) => {
            joinedPeers.current?.[user]?.send(data);
        },
        sendToAllPeers: async (data: any, options?: { except: string }) => {
            for (const [peer, connection] of Object.entries(joinedPeers.current)) {
                if (peer !== options?.except) {
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
