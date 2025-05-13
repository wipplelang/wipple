import { MaterialSymbol, SymbolCodepoints } from "react-material-symbols";

export interface ToolbarButtonProps {
    onClick: () => void;
    prominent?: boolean;
    square?: boolean;
    icon?: SymbolCodepoints;
    children?: React.ReactNode;
}

export const ToolbarButton = (props: ToolbarButtonProps) => (
    <button
        data-prominent={props.prominent}
        data-square={props.square}
        onClick={props.onClick}
        className="h-full cursor-pointer rounded-[8px] hover:bg-gray-200 dark:hover:bg-gray-800 bg-gray-100 dark:bg-gray-900 data-[prominent]:bg-blue-500 data-[prominent]:dark:bg-blue-400 data-[prominent]:hover:bg-blue-600 data-[prominent]:dark:hover:bg-blue-400 data-[prominent]:text-white px-2 data-[square]:size-8 transition-colors"
    >
        <ToolbarButtonContent>
            {props.icon && <MaterialSymbol icon={props.icon} className="text-[1.4em]" />}
            {props.children}
        </ToolbarButtonContent>
    </button>
);

export const ToolbarButtonContent = (props: { children: React.ReactNode }) => (
    <div className="flex flex-row items-center justify-center gap-1">{props.children}</div>
);
