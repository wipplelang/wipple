import { useEffect, useRef, useState } from "react";
import { animated, useSpring, useSpringValue } from "@react-spring/web";
import { defaultAnimationDuration } from ".";
import { useInView } from "framer-motion";

const config = { tension: 300, friction: 30, bounce: 0 };

export const Animated = (
    props: React.PropsWithChildren<{
        waitForLayout?: boolean;
        direction: "horizontal" | "vertical" | ("horizontal" | "vertical")[];
        clip?: boolean;
        unsized?: boolean;
        open?: boolean;
    }>,
) => {
    const [hasWaitedForLayout, setHasWaitedForLayout] = useState(!props.waitForLayout);

    const isOpen = props.open ?? true;
    const [initialIsOpen, setInitialIsOpen] = useState(() => isOpen);

    const ref = useRef<HTMLDivElement>(null);
    const isInView = useInView(ref);

    const width = useSpringValue(0, { config });
    const height = useSpringValue(0, { config });

    useEffect(() => {
        if (!isInView) return;

        const handler = () => {
            if (!ref.current) return;

            const rect = ref.current.getBoundingClientRect();

            if (expandHorizontal) {
                width.start({ to: rect.width });
            }

            if (expandVertical) {
                height.start({ to: rect.height });
            }
        };

        handler();

        const poll = setInterval(handler, 100);

        return () => {
            clearInterval(poll);
        };
    }, [isInView]);

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
        config,
    });

    useEffect(() => {
        if (props.waitForLayout) {
            setTimeout(() => {
                setHasWaitedForLayout(true);
            }, defaultAnimationDuration);
        }
    }, [props.waitForLayout]);

    return hasWaitedForLayout ? (
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
    ) : null;
};
