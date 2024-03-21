import { Mutex } from "async-mutex";
import { useEffect, useMemo, useRef, useState } from "react";
import { CSSTransition } from "react-transition-group";

export const defaultAnimationDuration = 150; // FIXME: Obtain from tailwindcss-animate

export interface TransitionProps {
    in: boolean;
    waitForLayout?: boolean;
    animateOnMount?: boolean;
    exitAnimationDuration: number;
    inClassName?: string;
    outClassName?: string;
    children: JSX.Element | null;
}

export const Transition = (props: TransitionProps) => {
    const [hasWaitedForLayout, setHasWaitedForLayout] = useState(!props.waitForLayout);
    const [display, setDisplay] = useState(props.animateOnMount ? false : props.in);
    const mutex = useMemo(() => new Mutex(), []);
    const [cachedChildren, setCachedChildren] = useState<JSX.Element | null>(null);

    useEffect(() => {
        if (props.waitForLayout) {
            setTimeout(() => {
                setHasWaitedForLayout(true);
            }, props.exitAnimationDuration);
        }
    }, [props.waitForLayout]);

    useEffect(() => {
        if (!hasWaitedForLayout) return;

        (async () => {
            await mutex.runExclusive(
                () =>
                    new Promise<void>((resolve) => {
                        console.log("props.in:", props.in);

                        if (props.in) {
                            setCachedChildren(() => props.children);
                            setDisplay(true);
                            resolve();
                        } else {
                            setDisplay(false);

                            setTimeout(() => {
                                setCachedChildren(() => props.children);
                                resolve();
                            }, props.exitAnimationDuration);
                        }
                    }),
            );
        })();
    }, [mutex, hasWaitedForLayout, props.in, props.children, props.exitAnimationDuration]);

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
            <>{cachedChildren}</>
        </CSSTransition>
    );
};
