import { useEffect, useState } from "react";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

type Status = "pending" | "running" | "stopped";

const arduinoUnoFilters = [
    { usbVendorId: 0x2341, usbProductId: 0x0043 },
    { usbVendorId: 0x2341, usbProductId: 0x0001 },
];

export const App = (props: AppProps) => {
    const [handleStatus, setHandleStatus] = useState<(status: Status) => void>();
    const [port, setPort] = useState<{
        readable: ReadableStream<Uint8Array>;
        writable: WritableStream<Uint8Array>;
    }>();
    const [status, setStatus] = useState<Status>("pending");

    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                switch (message) {
                    case "wait-for-run-button":
                        return new Promise<void>((resolve) => {
                            setHandleStatus(() => async (status: Status) => {
                                if (!("serial" in navigator)) {
                                    alert(
                                        "Error: Your browser doesn't support connecting to Arduino devices. Please try using the latest version of Google Chrome or another supported browser."
                                    );

                                    return;
                                }

                                if (status === "running") {
                                    try {
                                        const port = await navigator.serial.requestPort({
                                            filters: arduinoUnoFilters,
                                        });

                                        await port.open({ baudRate: 9600 });

                                        const { readable, writable } = port;

                                        if (!readable) {
                                            throw new Error("Device is not readable");
                                        }

                                        if (!writable) {
                                            throw new Error("Device is not writable");
                                        }

                                        setPort({ readable, writable });
                                        setStatus("running");
                                    } catch (error) {
                                        console.error(error);
                                        alert(`Error connecting to Arduino: ${error}`);
                                        setStatus("stopped");
                                    }

                                    resolve();
                                } else {
                                    // Disconnect from Arduino
                                }
                            });
                        });
                    case "todo":
                        console.warn("TODO", value);
                        break;
                    case "done":
                        setStatus("stopped");
                        break;
                    default:
                        throw new Error("unknown message");
                }
            });
        })();
    }, []);

    return (
        <div>
            {status !== "stopped" ? (
                <button
                    onClick={() => {
                        if (!handleStatus) {
                            return;
                        }

                        switch (status) {
                            case "pending":
                                handleStatus("running");
                                break;
                            case "running":
                                handleStatus("stopped");
                                break;
                        }
                    }}
                >
                    {status === "running" ? "⏹ Stop" : "▶️ Run"}
                </button>
            ) : null}
        </div>
    );
};
