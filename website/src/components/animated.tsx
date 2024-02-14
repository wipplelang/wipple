import { useState } from "react";
import { animated, useSpring } from "@react-spring/web";
import useMeasure from "react-use-measure";

export const Animated = (
    props: React.PropsWithChildren<{ direction: "horizontal" | "vertical"; open?: boolean }>,
) => {
    const isOpen = props.open ?? true;
    const [initialIsOpen, setInitialIsOpen] = useState(() => isOpen);

    const [ref, { width, height }] = useMeasure();

    const style = useSpring({
        width: props.direction === "horizontal" ? (isOpen ? width : 0) : undefined,
        height: props.direction === "vertical" ? (isOpen ? height : 0) : undefined,
        opacity: isOpen ? 1 : 0,
        immediate: initialIsOpen,
        onRest: () => setInitialIsOpen(false),
        config: { tension: 300, friction: 30, bounce: 0 },
    });

    return (
        <animated.div style={style}>
            <div ref={ref} className="w-fit">
                {props.children}
            </div>
        </animated.div>
    );
};
