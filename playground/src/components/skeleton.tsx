import BaseSkeleton, { SkeletonProps } from "react-loading-skeleton";
import { useDarkMode } from "usehooks-ts";

export const Skeleton = (props: SkeletonProps) => {
    const { isDarkMode } = useDarkMode();

    return <BaseSkeleton baseColor={isDarkMode ? "#1f2937" : "#e5e7eb"} {...props} />;
};
