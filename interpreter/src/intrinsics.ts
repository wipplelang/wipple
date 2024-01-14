import type { Context, TypedValue } from "./index.js";

export type Intrinsic = (inputs: TypedValue[], context: Context) => Promise<TypedValue>;

export const intrinsics: Record<string, Intrinsic> = {
    crash: async ([message], context) => {
        if (message.type !== "text") {
            throw context.error("expected text");
        }

        throw context.error(`error: ${message.value}`);
    },
    "number-equality": async ([left, right], context) => {
        if (left.type !== "number" || right.type !== "number") {
            throw context.error("expected numbers");
        }

        return {
            typeDescriptor: context.executable.intrinsicTypeDescriptors.boolean,
            type: "variant",
            variant: left.value.eq(right.value)
                ? context.executable.intrinsicVariants.true
                : context.executable.intrinsicVariants.false,
            values: [],
        };
    },
    "text-equality": async ([left, right], context) => {
        if (left.type !== "text" || right.type !== "text") {
            throw context.error("expected text");
        }

        return {
            typeDescriptor: context.executable.intrinsicTypeDescriptors.boolean,
            type: "variant",
            variant:
                left.value === right.value
                    ? context.executable.intrinsicVariants.true
                    : context.executable.intrinsicVariants.false,
            values: [],
        };
    },
};
