import { useRef, useState } from "react";
import { animated, useSpring } from "@react-spring/web";
import { useResizeObserver } from "usehooks-ts";

export const Animated = (
    props: React.PropsWithChildren<{
        direction: "horizontal" | "vertical" | ("horizontal" | "vertical")[];
        clip?: boolean;
        unsized?: boolean;
        open?: boolean;
    }>,
) => {
    const isOpen = props.open ?? true;
    const [initialIsOpen, setInitialIsOpen] = useState(() => isOpen);

    const ref = useRef<HTMLDivElement>(null);
    const { width, height } = useResizeObserver({ ref });

    const expandHorizontal =
        (Array.isArray(props.direction) && props.direction.includes("horizontal")) ||
        props.direction === "horizontal";

    const expandVertical =
        (Array.isArray(props.direction) && props.direction.includes("vertical")) ||
        props.direction === "vertical";

    const style = useSpring({
        width: expandHorizontal ? (isOpen ? width : 0) : undefined,
        height: expandVertical ? (isOpen ? height : 0) : undefined,
        opacity: isOpen ? 1 : 0,
        immediate: initialIsOpen,
        onRest: () => setInitialIsOpen(false),
        config: { tension: 300, friction: 30, bounce: 0 },
    });

    return (
        <animated.div style={style} className={props.clip ? "overflow-clip" : ""}>
            <div
                ref={ref}
                className={
                    expandHorizontal
                        ? `w-fit ${props.unsized ? "" : "h-full"}`
                        : `${props.unsized ? "" : "w-full"} h-fit`
                }
            >
                {props.children}
            </div>
        </animated.div>
    );
};
