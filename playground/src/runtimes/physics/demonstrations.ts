import matter from "matter-js";
import { worldHeight, worldWidth } from ".";

export interface Demonstration {
    bodies: Record<string, matter.Body>;
}

// https://kdesign.co/blog/pastel-color-palette-examples/
const colors = ["#C9E4DE", "#F7D9C4", "#C6DEF1", "#F2C6DE", "#DBCDFO", "#FAEDCB"];

const createBlock = (props: {
    x: number;
    y: number;
    width: number;
    height: number;
    centerX: number;
    centerY: number;
    color: string;
    mass: number;
    elastic: boolean;
    rotates: boolean;
    solid: boolean;
}) => {
    const body = matter.Bodies.rectangle(
        (isNaN(props.x) ? 0 : props.x) + worldWidth / 2,
        worldHeight / 2 - (isNaN(props.y) ? 0 : props.y),
        props.width,
        props.height,
        {
            render: {
                fillStyle: props.color,
            },

            // @ts-ignore (don't multiply force, velocity, etc. by anything)
            deltaTime: 1,

            isStatic: isNaN(props.mass),
            restitution: props.elastic ? 1 : 0,
            inertia: props.rotates ? undefined : Infinity,
            isSensor: !props.solid,

            // TODO: Let user specify these
            friction: 0,
            frictionAir: 0,
            frictionStatic: 0,
        },
    );

    matter.Body.setCentre(
        body,
        { x: props.centerX * (props.width / 2), y: -props.centerY * (props.height / 2) },
        true,
    );

    if (!isNaN(props.mass)) {
        matter.Body.setMass(body, props.mass);
    }

    return body;
};

export const demonstrations: Record<string, () => Demonstration> = {
    "One Block": () => ({
        bodies: {
            "Block 1": createBlock({
                x: 0,
                y: 0,
                centerX: 0,
                centerY: 0,
                width: 1,
                height: 1,
                color: colors[0],
                mass: 1,
                elastic: false,
                rotates: true,
                solid: true,
            }),
        },
    }),
    "Two Blocks": () => ({
        bodies: {
            "Block 1": createBlock({
                x: -1,
                y: 0,
                centerX: 0,
                centerY: 0,
                width: 1,
                height: 1,
                color: colors[0],
                mass: 1,
                elastic: false,
                rotates: true,
                solid: true,
            }),
            "Block 2": createBlock({
                x: 1,
                y: 0,
                centerX: 0,
                centerY: 0,
                width: 1,
                height: 1,
                color: colors[1],
                mass: 2,
                elastic: false,
                rotates: true,
                solid: true,
            }),
        },
    }),
};
