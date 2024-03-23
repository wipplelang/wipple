import { useState, useEffect } from "react";
import { AnimatePresence, HTMLMotionProps, motion } from "framer-motion";

export const defaultAnimationDuration = 150;

const transition = {
    type: "linear",
    duration: defaultAnimationDuration / 1000,
};

export interface TransitionProps {
    in: boolean;
    animateOnMount?: boolean;
    inStyle?: HTMLMotionProps<"span">["animate"];
    outStyle?: HTMLMotionProps<"span">["initial"] & HTMLMotionProps<"span">["exit"];
    children: JSX.Element | null;
}

export const Transition = (props: TransitionProps) => {
    const [mounted, setMounted] = useState(!props.animateOnMount);

    useEffect(() => {
        if (props.animateOnMount) {
            requestAnimationFrame(() => {
                setMounted(true);
            });
        }
    }, [props.animateOnMount]);

    const [key, setKey] = useState(0);

    useEffect(() => {
        setKey(key + 1);
    }, [props.children]);

    return (
        <AnimatePresence mode="wait">
            {props.in && mounted ? (
                <motion.div
                    key={key}
                    transition={transition}
                    initial={props.outStyle}
                    animate={props.inStyle}
                    exit={props.outStyle}
                >
                    {props.children}
                </motion.div>
            ) : null}
        </AnimatePresence>
    );
};
