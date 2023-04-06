import { useEffect, useState } from "react";
import { CartesianGrid, Line, LineChart, ResponsiveContainer, XAxis, YAxis } from "recharts";
import { produce } from "immer";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    const [minX, setMinX] = useState(-10);
    const [maxX, setMaxX] = useState(10);
    const [minY, setMinY] = useState(-10);
    const [maxY, setMaxY] = useState(10);
    const [resolution, setResolution] = useState(0.25);
    const [func, setFunc] = useState<(x: number) => Promise<number>>();
    const [colors, setColors] = useState<string[]>([]);
    const [resolve, setResolve] = useState(() => () => {});

    useEffect(() => {
        (async () => {
            props.setOnMessage(
                (message, value) =>
                    new Promise((resolve) => {
                        setResolve(() => resolve);

                        try {
                            switch (message) {
                                case "min-x":
                                    setMinX(value);
                                    break;
                                case "max-x":
                                    setMaxX(value);
                                    break;
                                case "min-y":
                                    setMinY(value);
                                    break;
                                case "max-y":
                                    setMaxY(value);
                                    break;
                                case "resolution":
                                    setResolution(value);
                                    break;
                                case "function":
                                    setFunc(() => value);
                                    break;
                                case "color":
                                    setColors((colors) => [...colors, value]);
                                    break;
                                default:
                                    throw new Error("unknown message");
                            }
                        } catch (error) {
                            console.error("[graphing] error:", error);
                        }
                    })
            );
        })();
    }, []);

    const [data, setData] = useState<[number, number[]][]>([]);

    useEffect(() => {
        (async () => {
            if (!func) {
                resolve();
                return;
            }

            for (let x = minX - resolution; x <= maxX + resolution; x += resolution) {
                const y = await func(x);
                setData((data) =>
                    produce(data, (data) => {
                        let ys: [number, number[]] | undefined = data.find(
                            ([dataX]) => dataX === x
                        );

                        if (!ys) {
                            ys = [x, []];
                            data.push(ys);
                        }

                        ys[1].push(y);
                    })
                );
            }

            setFunc(undefined);

            resolve();
        })();
    }, [minX, maxX, resolution, func, resolve]);

    return (
        <div style={{ backgroundColor: "white" }}>
            <ResponsiveContainer width="100%" aspect={4 / 3}>
                <LineChart data={data}>
                    <XAxis type="number" domain={[minX, maxX]} dataKey={0} allowDataOverflow />
                    <YAxis type="number" domain={[minY, maxY]} allowDataOverflow />
                    <CartesianGrid />

                    {data.map((_, index) => (
                        <Line
                            key={index}
                            dataKey={(data) => data[1][index]}
                            stroke={colors[index] ?? colors[colors.length - 1] ?? "black"}
                            strokeWidth={2}
                            dot={false}
                            isAnimationActive={false}
                        />
                    ))}
                </LineChart>
            </ResponsiveContainer>
        </div>
    );
};
