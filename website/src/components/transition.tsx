import { useEffect, useRef, useState } from "react";
import { CSSTransition } from "react-transition-group";

export const defaultAnimationDuration = 150; // FIXME: Obtain from tailwindcss-animate

export interface TransitionProps<T> {
    value: T | undefined;
    exitAnimationDuration: number;
    inClassName?: string;
    outClassName?: string;
    children: (value: T) => JSX.Element;
}

export function Transition<T>(props: TransitionProps<T>) {
    const [display, setDisplay] = useState(props.value != null);
    const [contents, setContents] = useState(<></>);

    useEffect(() => {
        if (props.value != null) {
            setContents(() => props.children(props.value!));
            setDisplay(true);
        } else {
            setDisplay(false);
        }
    }, [props.value]);

    const ref = useRef<HTMLDivElement>(null);

    return (
        <CSSTransition
            in={display}
            classNames={{
                enter: props.inClassName,
                enterActive: props.inClassName,
                exit: props.outClassName,
                exitActive: props.outClassName,
            }}
            nodeRef={ref}
            timeout={{ exit: props.exitAnimationDuration }}
            onExited={() => setContents(<></>)}
        >
            <div ref={ref}>{contents}</div>
        </CSSTransition>
    );
}
