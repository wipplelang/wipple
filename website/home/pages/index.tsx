import { NextPage } from "next";
import Head from "next/head";
import { useEffect, useRef, useState } from "react";
import HomeDoc from "./home.mdx";

const Home: NextPage = () => {
    const [backgroundHeight, setBackgroundHeight] = useState(0);
    const $background = useRef<HTMLDivElement>(null);

    useEffect(() => {
        setBackgroundHeight($background.current!.clientHeight);

        window.addEventListener("resize", () => {
            setBackgroundHeight($background.current!.clientHeight);
        });
    }, []);

    return (
        <>
            <Head>
                <title>Wipple</title>

                <meta
                    name="description"
                    content="Wipple is a programming language that’s natural to read, write and learn."
                />

                <link rel="icon" href="/favicon.ico" />
            </Head>

            <div>
                <div
                    style={{
                        background: "linear-gradient(157.58deg, #00C2FF 8.94%, #0085FF 91.79%)",
                        height: backgroundHeight,
                    }}
                    className="absolute top-0 bottom-0 left-0 w-1/2 -z-10"
                />

                <div
                    style={{
                        background: "linear-gradient(157.58deg, #FFB800 8.94%, #FF9900 91.79%)",
                        height: backgroundHeight,
                    }}
                    className="absolute top-0 bottom-0 right-0 w-1/2 -z-10"
                />

                <div className="container max-w-6xl mx-auto" ref={$background}>
                    <div className="flex flex-col items-start gap-4 p-10 md:p-20">
                        <img src="logo.svg" alt="Wipple logo" />

                        <h1 className="text-3xl md:text-5xl leading-tight md:leading-tight font-bold text-white">
                            Wipple is a programming language that’s natural to read, write and
                            learn.
                        </h1>

                        <div className="flex gap-2">
                            <a
                                href="https://playground.wipple.gramer.dev"
                                className="font-bold py-2 px-4 rounded bg-white"
                            >
                                Playground
                            </a>

                            <a
                                href="https://guide.wipple.gramer.dev"
                                className="font-bold py-2 px-4 rounded bg-white"
                            >
                                Guide
                            </a>
                        </div>
                    </div>
                </div>

                <div className="prose prose-sky prose-md md:prose-lg container max-w-6xl mx-auto p-10 md:p-20">
                    <HomeDoc />
                </div>

                <p className="text-sm text-center text-gray-500 mb-8">
                    Made by{" "}
                    <a href="https://gramer.dev" className="font-semibold text-gray-700">
                        Wilson Gramer
                    </a>
                </p>
            </div>
        </>
    );
};

export default Home;
