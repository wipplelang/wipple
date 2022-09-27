import _ = require("lodash");
import { Expression, Program, Type, UnresolvedType } from "./program";

export const traverseExpr = (expr: Expression, f: (expr: Expression) => void) => {
    f(expr);

    switch (expr.kind.type) {
        case "Marker":
            break;
        case "Variable":
            break;
        case "Constant":
            break;
        case "Text":
            break;
        case "Number":
            break;
        case "Block":
            for (const inner of expr.kind.value) {
                traverseExpr(inner, f);
            }
            break;
        case "Call":
            traverseExpr(expr.kind.value[0], f);
            traverseExpr(expr.kind.value[1], f);
            break;
        case "Function":
            traverseExpr(expr.kind.value[1], f);
            break;
        case "When":
            traverseExpr(expr.kind.value[0], f);
            for (const arm of expr.kind.value[1]) {
                traverseExpr(arm.body, f);
            }
            break;
        case "External":
            for (const inner of expr.kind.value[2]) {
                traverseExpr(inner, f);
            }
            break;
        case "Initialize":
            traverseExpr(expr.kind.value[1], f);
            break;
        case "Structure":
            for (const inner of expr.kind.value) {
                traverseExpr(inner, f);
            }
            break;
        case "Variant":
            for (const inner of expr.kind.value[1]) {
                traverseExpr(inner, f);
            }
            break;
        case "Tuple":
            for (const inner of expr.kind.value) {
                traverseExpr(inner, f);
            }
            break;
    }
};

export const formatType = (
    ty: Type | UnresolvedType | [number, Type[] | UnresolvedType[]],
    program: Program
) => {
    const typeNames = (id: number) => program.declarations.types[id].name!;
    const traitNames = (id: number) => program.declarations.traits[id].name!;
    const paramNames = (id: number) => program.declarations.type_parameters[id].name!;

    const formatNamedType = (
        name: string,
        params: Type[] | UnresolvedType[],
        isTopLevel = true
    ) => {
        for (const param of params) {
            name += ` ${format(param, false, false)}`;
        }

        return isTopLevel || params.length === 0 ? name : `(${name})`;
    };

    const format = (ty: Type | UnresolvedType, isTopLevel = true, isReturn = true): string => {
        switch (ty.type) {
            case "Variable":
                return "_";
            case "Parameter":
                return paramNames(ty.value);
            case "Named":
                return formatNamedType(typeNames(ty.value[0]), ty.value[1], isTopLevel);
            case "Builtin":
                switch (ty.value.type) {
                    case "Text":
                        return formatNamedType("Text", [], isTopLevel);
                    case "Number":
                        return formatNamedType("Number", [], isTopLevel);
                    case "List":
                        return formatNamedType("List", [ty.value.value], isTopLevel);

                    case "Mutable":
                        return formatNamedType("Mutable", [ty.value.value], isTopLevel);
                }

            case "Function":
                const input = format(ty.value[0], isTopLevel, false);
                const output = format(ty.value[1], isTopLevel, true);
                return isTopLevel && isReturn ? `${input} -> ${output}` : `(${input} -> ${output})`;
            case "Tuple":
                let inner: string;
                switch (ty.value.length) {
                    case 0:
                        inner = "()";
                        break;
                    case 1:
                        inner = format(ty.value[0], isTopLevel, isReturn) + " ,";
                        break;
                    default:
                        inner = ty.value.map((ty) => format(ty, isTopLevel, isReturn)).join(" , ");
                        break;
                }

                return isTopLevel ? inner : `(${inner})`;
            case "Bottom":
                return "!";
        }
    };

    const getParametersInType = (
        ty: UnresolvedType | Type | [number, UnresolvedType[] | Type[]]
    ): UnresolvedType[] | Type[] => {
        if (Array.isArray(ty)) {
            return ty[1];
        } else {
            switch (ty.type) {
                case "Parameter":
                    return [ty];
                case "Named":
                    return ty.value[1].flatMap(getParametersInType);
                case "Function":
                    return [
                        ...getParametersInType(ty.value[0]),
                        ...getParametersInType(ty.value[1]),
                    ];
                case "Tuple":
                    return ty.value.flatMap(getParametersInType);
                case "Builtin":
                    switch (ty.value.type) {
                        case "List":
                            return [ty.value.value];
                        case "Mutable":
                            return [ty.value.value];
                        default:
                            return [];
                    }
                default:
                    return [];
            }
        }
    };

    const params = _.uniqBy(getParametersInType(ty), JSON.stringify);

    const showParams = params.length === 0 || Array.isArray(ty);

    let formatted: string;
    if (Array.isArray(ty)) {
        formatted = formatNamedType(traitNames(ty[0]), ty[1]);
    } else {
        formatted = format(ty);
    }

    return showParams
        ? formatted
        : `${params.map((param) => format(param, false, false) + " ").join("")}=> ${formatted}`;
};
