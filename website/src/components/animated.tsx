import { useRef, useState } from "react";
import { animated, useSpring } from "@react-spring/web";
import { useResizeObserver } from "usehooks-ts";

export const Animated = (
    props: React.PropsWithChildren<{
        direction: "horizontal" | "vertical";
        clip?: boolean;
        open?: boolean;
    }>,
) => {
    const isOpen = props.open ?? true;
    const [initialIsOpen, setInitialIsOpen] = useState(() => isOpen);

    const ref = useRef<HTMLDivElement>(null);
    const { width, height } = useResizeObserver({ ref });

    const style = useSpring({
        width: props.direction === "horizontal" ? (isOpen ? width : 0) : undefined,
        height: props.direction === "vertical" ? (isOpen ? height : 0) : undefined,
        opacity: isOpen ? 1 : 0,
        immediate: initialIsOpen,
        onRest: () => setInitialIsOpen(false),
        config: { tension: 300, friction: 30, bounce: 0 },
    });

    return (
        <animated.div style={style} className={props.clip ? "overflow-clip" : ""}>
            <div
                ref={ref}
                className={props.direction === "horizontal" ? "w-fit h-full" : "w-full h-fit"}
            >
                {props.children}
            </div>
        </animated.div>
    );
};
