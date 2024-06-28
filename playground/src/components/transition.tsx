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
    dynamicChildren?: boolean;
    children: React.ReactNode;
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
        if (props.dynamicChildren) {
            setKey(key + 1);
        }
    }, [props.dynamicChildren, props.children]);

    return (
        <AnimatePresence mode="wait">
            {props.in && mounted ? (
                <motion.div
                    key={props.dynamicChildren ? key : null}
                    className="w-fit h-fit"
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
