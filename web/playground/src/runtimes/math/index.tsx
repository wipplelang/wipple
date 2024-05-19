import type { RuntimeComponent } from "..";
import { forwardRef, useEffect, useImperativeHandle, useState } from "react";
import { CartesianGrid, Line, LineChart, ResponsiveContainer, XAxis, YAxis } from "recharts";
import { PaletteItem } from "../../models";
import { Resizable } from "react-resizable";
import { produce } from "immer";
import { ColorAsset } from "../../pages/edit/assets/color";

export interface Settings {
    canvasWidth: number;
    canvasHeight: number;
}

const defaultSettings: Settings = {
    canvasWidth: 400,
    canvasHeight: 300,
};

const defaultMinX = -10;
const defaultMaxX = 10;
const defaultMinY = -10;
const defaultMaxY = 10;
const defaultResolution = 0.25;

export const Math: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const [minX, setMinX] = useState(defaultMinX);
    const [maxX, setMaxX] = useState(defaultMaxX);
    const [minY, setMinY] = useState(defaultMinY);
    const [maxY, setMaxY] = useState(defaultMaxY);
    const [resolution, setResolution] = useState(defaultResolution);
    const [func, setFunc] = useState<(x: number) => Promise<number>>();
    const [colors, setColors] = useState<string[]>([]);
    const [resolve, setResolve] = useState(() => () => {});

    const [containerWidth, setContainerWidth] = useState(
        (props.settings ?? defaultSettings).canvasWidth,
    );

    const [containerHeight, setContainerHeight] = useState(
        (props.settings ?? defaultSettings).canvasHeight,
    );

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
                            ([dataX]) => dataX === x,
                        );

                        if (!ys) {
                            ys = [x, []];
                            data.push(ys);
                        }

                        ys[1].push(y);
                    }),
                );
            }

            setFunc(undefined);

            resolve();
        })();
    }, [minX, maxX, resolution, func, resolve]);

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            resolve();
            setMinX(defaultMinX);
            setMaxX(defaultMaxX);
            setMinY(defaultMinY);
            setMaxY(defaultMaxY);
            setResolution(defaultResolution);
            setFunc(undefined);
            setColors([]);
            setData([]);
        },
        onMessage: (message, value) =>
            new Promise((resolve) => {
                setResolve(() => resolve);

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
                        setFunc(() => (x: number) => props.call(value, x));
                        break;
                    case "color":
                        setColors((colors) => [...colors, value]);
                        break;
                    default:
                        throw new Error(`unsupported message: ${message}`);
                }
            }),
        cleanup: async () => {
            resolve();
        },
    }));

    return (
        <Resizable
            width={containerWidth}
            height={containerHeight}
            minConstraints={[200, 150]}
            maxConstraints={[700, 525]}
            lockAspectRatio
            onResize={(_event, data) => {
                setContainerWidth(data.size.width);
                setContainerHeight(data.size.height);
            }}
            onResizeStop={(_event, data) => {
                if (
                    data.size.width !== props.settings?.canvasWidth ||
                    data.size.height !== props.settings?.canvasHeight
                ) {
                    props.onChangeSettings({
                        canvasWidth: data.size.width,
                        canvasHeight: data.size.height,
                    });
                }
            }}
        >
            <div
                className={`relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
                style={{ width: containerWidth, height: containerHeight, aspectRatio: 4 / 3 }}
            >
                <div style={{ backgroundColor: "white" }}>
                    <ResponsiveContainer width="100%" aspect={4 / 3}>
                        <LineChart data={data}>
                            <XAxis
                                type="number"
                                domain={[minX, maxX]}
                                dataKey={0}
                                allowDataOverflow
                            />

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
            </div>
        </Resizable>
    );
});

export const assetItems: PaletteItem[] = [
    {
        title: <ColorAsset color="#3b82f6" tooltip={false} />,
        code: `[Color "#3b82f6"]`,
    },
];

export const paletteItems: PaletteItem[] = [
    {
        title: "plot",
        code: `plot (x -> x + 1)`,
    },
    {
        title: "color",
        code: `color [Color "#3b82f6"]`,
    },
];
