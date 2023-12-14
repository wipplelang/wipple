import { useEffect, useState } from "react";
import { averageCarbonEmissionsInKilogramsPerHouseholdPerMonth, energySources } from "./data";
import { useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import ReactMarkdown from "react-markdown";

export interface AppProps {
    id: string;
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    const [running, setRunning] = useState(false);
    const [handleStart, setHandleStart] = useState<() => void>();
    const [energyUsage, setEnergyUsage] = useState<number>();

    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                try {
                    switch (message) {
                        case "begin":
                            if (!window.electron) {
                                break;
                            }

                            await new Promise<void>((resolve) => {
                                setHandleStart(() => async () => {
                                    setRunning(true);
                                    setEnergyUsage(undefined);

                                    await window.electron!.wipple.energyUsage.beginMeasuring(
                                        props.id
                                    );

                                    resolve();
                                });
                            });

                            break;
                        case "end":
                            if (!window.electron) {
                                break;
                            }

                            const energyUsage =
                                await window.electron.wipple.energyUsage.endMeasuring(props.id);

                            setRunning(false);
                            setEnergyUsage(energyUsage);

                            break;
                        default:
                            throw new Error("unknown message");
                    }
                } catch (error) {
                    console.error("[graphing] error:", error);
                }
            });
        })();
    }, []);

    return !window.electron ? (
        <div className="prose prose-sky dark:prose-invert">
            <h3>App required</h3>

            <p>
                This lesson needs extra features not available in your browser. Please continue in
                the Wipple Playground app.
            </p>

            <a
                href="https://wipple.dev/playground/downloads/mac.zip"
                className="block my-2 px-2 py-1 rounded-lg bg-opacity-10 bg-sky-500 text-sky-500"
            >
                Download for Mac
            </a>
        </div>
    ) : running ? (
        <p>Running...</p>
    ) : energyUsage != null ? (
        <EnergySummary energyUsage={energyUsage} />
    ) : (
        <button onClick={() => handleStart!()}>Begin Measuring</button>
    );
};

const scale = 1000;

const secondsPerMonth = 2592000;
const joulesToKilowattHours = (joules: number) => joules / (3.6 * 10 ** 6);

const EnergySummary = (props: { energyUsage: number }) => {
    const watts = props.energyUsage;
    const kilowattHoursAtScaleForMonth = joulesToKilowattHours(watts * secondsPerMonth * scale);

    return (
        <div className="w-full mb-4">
            <div className="flex flex-col gap-6 bg-white dark:bg-gray-900 p-4 rounded-lg shadow-lg shadow-gray-100 dark:shadow-gray-900">
                <div className="flex flex-col gap-4">
                    <h1 className="text-lg font-bold">Energy use report</h1>

                    <div className="flex gap-2">
                        <div className="flex flex-col items-center justify-center bg-green-500 p-3 rounded-lg">
                            <p className="text-3xl font-bold text-white">{watts.toFixed(2)} W</p>
                        </div>

                        <div className="flex flex-col items-center justify-center bg-sky-500 p-3 rounded-lg text-center">
                            <p className="text-lg font-bold text-white">
                                {kilowattHoursAtScaleForMonth.toFixed(0)} kWh
                            </p>

                            <p className="text-xs text-white text-opacity-70">1000 computers/mo</p>
                        </div>
                    </div>
                </div>

                <div className="flex flex-col gap-4 w-full">
                    <h1 className="text-lg font-bold">Compare energy sources</h1>

                    <ul className="flex flex-col w-full">
                        {energySources.map((energySource) => (
                            <EnergySourceInfo
                                key={energySource.name}
                                energySource={energySource}
                                kilowattHoursAtScaleForMonth={kilowattHoursAtScaleForMonth}
                            />
                        ))}
                    </ul>
                </div>

                <p className="text-xs text-gray-700 dark:text-gray-500">
                    Made by Wilson Gramer and Demetrios Kennedy at{" "}
                    <a href="https://wpi.edu" target="_blank" className="font-bold">
                        Worcester Polytechnic Institute
                    </a>
                </p>
            </div>
        </div>
    );
};

const EnergySourceInfo = (props: {
    energySource: (typeof energySources)[number];
    kilowattHoursAtScaleForMonth: number;
}) => {
    const {
        name,
        image,
        costInDollarsPerKilowattHours,
        costSource,
        carbonEmissionsInKilogramsPerKilowattHours,
        emissionsSource,
        info,
    } = props.energySource;

    const cost = costInDollarsPerKilowattHours(props.kilowattHoursAtScaleForMonth);

    const emissions = carbonEmissionsInKilogramsPerKilowattHours?.(
        props.kilowattHoursAtScaleForMonth
    );

    const [showMore, setShowMore] = useState(false);

    const [infoRef, { height: infoHeight }] = useMeasure();

    const animatedInfoStyle = useSpring(
        showMore ? { opacity: 1, height: infoHeight } : { opacity: 0, height: 0 }
    );

    return (
        <li className="flex flex-row items-start gap-2">
            <img src={image} alt={name} className="w-10 h-10" />

            <div className="flex flex-col justify-center mt-1">
                <h2 className="font-bold">{name}</h2>

                <div className="text-sm">
                    <p>
                        <strong>Price:</strong> ${cost.toFixed(2)}{" "}
                        <a href={costSource} target="_blank" className="text-sky-500">
                            (source)
                        </a>
                    </p>

                    {emissions != null ? (
                        <p>
                            <strong>
                                CO<sub>2</sub> emissions:
                            </strong>{" "}
                            {emissions.toFixed(2)} kg
                            <span className="text-gray-500">
                                {" • "}
                                {(
                                    emissions /
                                    averageCarbonEmissionsInKilogramsPerHouseholdPerMonth
                                ).toFixed(2)}
                                x the emissions of the average US household
                            </span>{" "}
                            <a href={emissionsSource} target="_blank" className="text-sky-500">
                                (source)
                            </a>
                        </p>
                    ) : (
                        <p className="text-green-500">
                            <strong>No emissions</strong>
                        </p>
                    )}

                    <button
                        className="my-2 px-2 py-1 rounded-lg bg-opacity-10 bg-sky-500 text-sky-500"
                        onClick={() => {
                            setShowMore((showMore) => !showMore);
                        }}
                    >
                        {showMore ? "Show less" : "Show more"}
                    </button>

                    <animated.div style={animatedInfoStyle} className="overflow-clip">
                        <div ref={infoRef}>
                            <ReactMarkdown
                                className="prose prose-sm prose-sky dark:prose-invert"
                                // @ts-ignore
                                linkTarget="_blank"
                            >
                                {info}
                            </ReactMarkdown>
                        </div>
                    </animated.div>
                </div>
            </div>
        </li>
    );
};
