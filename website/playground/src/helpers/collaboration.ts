import Peer, { DataConnection } from "peerjs";
import { useRef } from "react";

export const useCollaboration = (options: {
    onJoined: () => void;
    onReceive: (data: any) => void;
}) => {
    const connection = useRef<Peer | null>(null);
    const peerConnection = useRef<DataConnection | null>(null);

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
                peerConnection.current = connection;
                peerConnection.current.on("open", options.onJoined);
                peerConnection.current.on("data", options.onReceive);
                peerConnection.current.on("error", console.error);
            });

            return { userId };
        },
        connect: async (peer: string) => {
            if (!connection.current) return;

            peerConnection.current = connection.current.connect(peer, {
                serialization: "json",
            });

            peerConnection.current.on("data", options.onReceive);
            peerConnection.current.on("error", console.error);
        },
        send: async (data: any) => {
            if (!peerConnection.current) return;

            peerConnection.current.send(data);
        },
        disconnect: async () => {
            if (!connection.current || !peerConnection.current) return;

            connection.current.disconnect();
            connection.current = null;

            peerConnection.current.close();
            peerConnection.current = null;
        },
    };
};
