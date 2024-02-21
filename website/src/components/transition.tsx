import React, { useEffect, useState } from "react";
import { CSSTransition } from "react-transition-group";

export const defaultAnimationDuration = 150; // FIXME: Obtain from tailwindcss-animate

export interface TransitionProps {
    in: boolean;
    exitAnimationDuration: number;
    inClassName?: string;
    outClassName?: string;
}

export const Transition = (props: React.PropsWithChildren<TransitionProps>) => {
    const [display, setDisplay] = useState(props.in);
    const [cachedChildren, setCachedChildren] = useState<React.ReactNode>(null);

    useEffect(() => {
        if (props.in) {
            setCachedChildren(props.children);
            setDisplay(true);
        } else {
            setDisplay(false);

            setTimeout(() => {
                setCachedChildren(null);
            }, props.exitAnimationDuration);
        }
    }, [props.in, props.children, props.exitAnimationDuration]);

    return (
        <CSSTransition
            in={display}
            classNames={{
                enter: props.inClassName,
                enterActive: props.inClassName,
                exit: props.outClassName,
                exitActive: props.outClassName,
            }}
            addEndListener={(node, done) => node.addEventListener("transitionend", done, false)}
            timeout={{ exit: props.exitAnimationDuration }}
        >
            <div>{cachedChildren}</div>
        </CSSTransition>
    );
};
