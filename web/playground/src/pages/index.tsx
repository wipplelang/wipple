export * from "./home";
export * from "./edit";

import { useEffect, useState } from "react";
import { Outlet, ScrollRestoration } from "react-router-dom";
import { produce } from "immer";
import { Button, Navbar, useAlert as useAlert } from "../components";
import { useStore } from "../store";
import { getUser, signInAsGuest, signInWithGoogle } from "../helpers";

export const RootPage = () => {
    const [store, setStore] = useStore();
    const { displayAlert } = useAlert();

    useEffect(() => {
        (async () => {
            try {
                const user = await getUser();

                if (user) {
                    setStore(
                        produce((store) => {
                            store.user = user;
                        }),
                    );
                } else {
                    displayAlert(WelcomeAlert);
                }
            } catch (error) {
                console.error(error);
            }
        })();
    }, [store.user]);

    return (
        <div className="w-screen flex flex-col items-stretch">
            <Navbar />
            <Outlet />

            <ScrollRestoration />
        </div>
    );
};

const WelcomeAlert = (props: { dismiss: () => void }) => {
    const [_store, setStore] = useStore();

    const handleSignIn = async () => {
        const user = await signInWithGoogle();

        setStore(
            produce((store) => {
                store.user = user;
            }),
        );

        props.dismiss();
    };

    const handleContinueAsGuest = async () => {
        const user = await signInAsGuest();

        setStore(
            produce((store) => {
                store.user = user;
            }),
        );

        props.dismiss();
    };

    return (
        <div className="flex flex-col items-center p-4 w-[400px] md:w-[700px]">
            <WelcomeAnimation />

            <p className="mx-8 mb-8 text-center text-lg text-gray-800 dark:text-gray-100">
                Wipple is a programming language designed for learning. With Wipple, you can make
                art and music, explore math and science, design video games, and more.
            </p>

            <div className="flex flex-col items-stretch gap-2">
                <Button role="primary" onClick={handleSignIn}>
                    Sign In
                </Button>

                <Button role="secondary" onClick={handleContinueAsGuest}>
                    Continue as Guest
                </Button>
            </div>
        </div>
    );
};

const WelcomeAnimation = () => {
    const content = [
        { image: "/playground/images/laptop-screen-wipple.png", text: "Learn to code" },
        { image: "/playground/images/laptop-screen-art.png", text: "Make art" },
        { image: "/playground/images/laptop-screen-music.png", text: "Make music" },
        { image: "/playground/images/laptop-screen-math.png", text: "Explore math" },
    ];

    const [activeIndex, setActiveIndex] = useState(0);
    const [visible, setVisible] = useState(true);

    useEffect(() => {
        let activeIndex = 0;

        const interval = setInterval(() => {
            activeIndex = (activeIndex + 1) % 4;
            setVisible(false);

            setTimeout(() => {
                setActiveIndex(activeIndex);

                setTimeout(() => {
                    setVisible(true);
                }, 250);
            }, 500);

            if (activeIndex == 0) {
                clearInterval(interval);
            }
        }, 5500);
    }, []);

    const { image, text } = content[activeIndex];

    return (
        <div className="flex flex-col items-center w-full gap-8 py-10 background-[url(/images/background.svg)] bg-cover bg-center">
            <div className="flex flex-col items-center w-full">
                <div className="border-4 md:border-[6px] border-gray-900 dark:border-gray-700 rounded-t-lg bg-white">
                    <img
                        src={image}
                        className={`w-[224px] h-[150px] md:w-[360px] md:h-[240px] rounded-md transition-opacity duration-500 ${
                            visible ? "" : "opacity-0"
                        }`}
                    />
                </div>

                <div className="w-[272px] h-[7px] md:w-[438px] md:h-[11px] bg-gradient-to-b from-gray-200 to-gray-300 dark:from-gray-500 dark:to-gray-600 rounded-md shadow-md" />
            </div>

            <div className={`transition-opacity duration-500 ${visible ? "" : "opacity-0"}`}>
                <h1 className="text-center text-3xl md:text-5xl leading-tight md:leading-tight font-semibold">
                    <span className="bg-gradient-to-br from-sky-400 to-blue-500 inline-block text-transparent bg-clip-text">
                        {text}&nbsp;
                    </span>

                    <br className="md:hidden" />

                    <span className="bg-gradient-to-br from-yellow-500 to-orange-400 inline-block text-transparent bg-clip-text">
                        <span>with Wipple</span>
                    </span>
                </h1>
            </div>
        </div>
    );
};
