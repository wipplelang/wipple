import { useState, useEffect, useMemo } from "react";
import { AnimatePresence, HTMLMotionProps, motion } from "framer-motion";
import { produce } from "immer";

export const defaultAnimationDuration = 150;

const defaultTransition = {
    type: "linear",
    duration: defaultAnimationDuration / 1000,
};

export interface TransitionProps {
    in: boolean;
    animateOnMount?: boolean;
    inStyle?: HTMLMotionProps<"span">["animate"];
    outStyle?: HTMLMotionProps<"span">["initial"] & HTMLMotionProps<"span">["exit"];
    className?: string;
    dynamicChildren?: boolean;
    children: React.ReactNode;
    duration?: number;
    delay?: number;
}

export const Transition = (props: TransitionProps) => {
    const [mounted, setMounted] = useState(!props.animateOnMount);

    useEffect(() => {
        if (props.animateOnMount) {
            setTimeout(() => {
                setMounted(true);
            }, props.delay ?? 0);
        }
    }, [props.animateOnMount]);

    const [key, setKey] = useState(0);

    useEffect(() => {
        if (props.dynamicChildren) {
            setKey(key + 1);
        }
    }, [props.dynamicChildren, props.children]);

    const transition = useMemo(
        () =>
            produce(defaultTransition, (transition) => {
                if (props.duration != null) {
                    transition.duration = props.duration / 1000;
                }
            }),
        [props.duration],
    );

    return (
        <AnimatePresence mode="wait" initial={props.animateOnMount}>
            {props.in && mounted ? (
                <motion.div
                    key={props.dynamicChildren ? key : null}
                    className={`w-fit h-fit ${props.className ?? ""}`}
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
