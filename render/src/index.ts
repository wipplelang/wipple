import * as compiler from "wipple-compiler";
import { LinesAndColumns, SourceLocation } from "lines-and-columns";

export type AnyDeclaration = { name: string | null; path: compiler.lower_Path } & (
    | { type: "type"; declaration: compiler.typecheck_TypeDeclaration }
    | { type: "trait"; declaration: compiler.typecheck_TraitDeclaration }
    | { type: "typeParameter"; declaration: compiler.typecheck_TypeParameterDeclaration }
    | { type: "constant"; declaration: compiler.typecheck_ConstantDeclaration }
    | { type: "instance"; declaration: compiler.typecheck_InstanceDeclaration }
);

export interface RenderedSourceLocation {
    path: string;
    visiblePath: string;
    start: SourceLocation & { index: number };
    end: SourceLocation & { index: number };
}

export interface RenderedDiagnostic {
    location: RenderedSourceLocation;
    severity: "warning" | "error";
    message: string;
    fix: RenderedFix | null;
}

export interface RenderedFix {
    message: string;
    before?: string;
    replacement?: string;
    after?: string;
}

export interface RenderedDocumentation {
    docs: string;
    attributes: RenderedDocumentationAttribute[];
}

export interface RenderedDocumentationAttribute {
    name: string;
    input: string | null;
    value: RenderedDocumentationAttributeValue;
}

export type RenderedDocumentationAttributeValue =
    | { type: "string"; value: string }
    | { type: "formattedString"; segments: [string, string][]; trailing: string }
    | { type: "boolean"; value: boolean };

export class Render {
    private files: Record<string, compiler.File & { linesAndColumns: LinesAndColumns }> = {};
    private declarations: compiler.WithInfo<compiler.Info, AnyDeclaration>[] = [];
    private libraries: compiler.linker_UnlinkedLibrary[] = [];

    update(interface_: compiler.Interface, libraries: compiler.linker_UnlinkedLibrary[]) {
        this.files = {};
        for (const file of interface_.files) {
            this.files[file.path] = { ...file, linesAndColumns: new LinesAndColumns(file.code) };
        }

        this.declarations = [];
        for (const type of ["type", "trait", "typeParameter", "constant", "instance"] as const) {
            for (const [path, declaration] of Object.entries<compiler.WithInfo<compiler.Info, any>>(
                interface_[`${type}Declarations`],
            )) {
                this.declarations.push({
                    info: declaration.info,
                    item: {
                        name: this.nameForPath(path),
                        path,
                        type,
                        declaration: declaration.item,
                    },
                });
            }
        }

        this.libraries = libraries;
    }

    getDeclarationFromPath(
        path: compiler.lower_Path,
    ): compiler.WithInfo<compiler.Info, AnyDeclaration> | null {
        for (const declaration of this.declarations) {
            if (declaration.item.path === path) {
                return declaration;
            }
        }

        return null;
    }

    getDeclarationFromInfo(
        info: compiler.Info,
    ): compiler.WithInfo<compiler.Info, AnyDeclaration> | null {
        for (const declaration of this.declarations) {
            if (this.compareInfo(declaration.info, info)) {
                return declaration;
            }
        }

        return null;
    }

    getInfoAtCursor(path: string, index: number): compiler.Info | null {
        for (const item of this.libraries.flatMap((library) => [
            ...Object.values(library.items),
            ...library.code,
        ])) {
            if (
                path !== item.expression.info.location.path ||
                (path !== "top-level" &&
                    (index < item.expression.info.location.span.start ||
                        index >= item.expression.info.location.span.end))
            ) {
                continue;
            }

            const candidates: compiler.WithInfo<
                compiler.Info,
                compiler.typecheck_TypedExpression
            >[] = [];

            this.traverseExpression(item.expression, (expression) => {
                if (
                    index >= expression.info.location.span.start &&
                    index < expression.info.location.span.end
                ) {
                    candidates.push(expression);
                }

                return false;
            });

            if (candidates.length === 0) {
                continue;
            }

            candidates.sort((left, right) => {
                const length = (
                    expression: compiler.WithInfo<
                        compiler.Info,
                        compiler.typecheck_TypedExpression
                    >,
                ) => expression.info.location.span.end - expression.info.location.span.start;

                return length(left) - length(right);
            });

            const expression = candidates[0];
            switch (expression.item.kind.type) {
                case "constant":
                case "trait":
                    return (
                        this.getDeclarationFromPath(expression.item.kind.value.path)?.info ?? null
                    );
                case "variable":
                    return this.getDeclarationFromPath(expression.item.kind.value[1])?.info ?? null;
                default:
                    return expression.info;
            }
        }

        return null;
    }

    renderSourceLocation(
        value: compiler.WithInfo<compiler.Info, unknown>,
    ): RenderedSourceLocation | null {
        const file = this.files[value.info.location.path];
        if (!file) {
            return null;
        }

        const startLocation = file.linesAndColumns.locationForIndex(value.info.location.span.start);
        if (!startLocation) {
            return null;
        }

        const endLocation = file.linesAndColumns.locationForIndex(value.info.location.span.end);
        if (!endLocation) {
            return null;
        }

        return {
            path: file.path,
            visiblePath: file.visiblePath,
            start: { ...startLocation, index: value.info.location.span.start },
            end: { ...endLocation, index: value.info.location.span.end },
        };
    }

    renderDeclaration(
        declaration: compiler.WithInfo<compiler.Info, AnyDeclaration>,
    ): string | null {
        switch (declaration.item.type) {
            case "type": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    [],
                    { kind: "arrow" },
                );

                return `${declaration.item.name} : ${typeFunction}type`;
            }
            case "trait": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    [],
                    { kind: "arrow" },
                );

                return `${declaration.item.name} : ${typeFunction}trait`;
            }
            case "typeParameter": {
                return declaration.item.name;
            }
            case "constant": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    declaration.item.declaration.bounds,
                    { kind: "arrow" },
                );

                const type = this.renderType(declaration.item.declaration.type, true, false);

                return `${declaration.item.name} :: ${typeFunction}${type}`;
            }
            case "instance": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    declaration.item.declaration.bounds,
                    { kind: "arrow" },
                );

                const instance = this.renderInstance(declaration.item.declaration.instance);

                return `${typeFunction}instance ${instance}`;
            }
            default:
                declaration.item satisfies never;
                return null;
        }
    }

    renderPattern(pattern: compiler.exhaustiveness_Pattern, isTopLevel: boolean): string {
        switch (pattern.type) {
            case "constructor": {
                const [constructor, values] = pattern.value;
                switch (constructor.type) {
                    case "variant": {
                        const declaration = this.getDeclarationFromPath(constructor.value);
                        if (!declaration) {
                            return "<unknown>";
                        }

                        const name = declaration.item.name ?? "<unknown>";

                        const rendered =
                            values.length === 0
                                ? name
                                : `${name} ${values
                                      .map((pattern) => this.renderPattern(pattern, false))
                                      .join(" ")}`;

                        return isTopLevel || values.length === 0 ? rendered : `(${rendered})`;
                    }
                    case "tuple": {
                        const rendered =
                            values.length === 0
                                ? "()"
                                : values.length === 1
                                ? `${this.renderPattern(values[0], isTopLevel)} ;`
                                : `${values
                                      .map((value) => this.renderPattern(value, false))
                                      .join(" ; ")}`;

                        return isTopLevel || values.length === 0 ? rendered : `(${rendered})`;
                    }
                    case "structure": {
                        return "{ ... }";
                    }
                    case "wrapper": {
                        const declaration = this.getDeclarationFromPath(constructor.value);
                        if (!declaration) {
                            return "<unknown>";
                        }

                        const rendered = `${declaration.item.name} ${this.renderPattern(
                            values[0],
                            false,
                        )}`;

                        return isTopLevel ? rendered : `(${rendered})`;
                    }
                    case "unbounded": {
                        return "_";
                    }
                    default:
                        constructor satisfies never;
                        return "<unknown>";
                }
            }
            case "binding": {
                return "_";
            }
            case "or": {
                return "<unknown>";
            }
            default:
                pattern satisfies never;
                return "<unknown>";
        }
    }

    renderType(
        type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
        isTopLevel: boolean,
        renderAsCode: boolean,
    ): string {
        let isDescription = false;
        const render = (
            type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
            isTopLevel: boolean,
            isReturn: boolean,
        ): string => {
            switch (type.item.type) {
                case "unknown": {
                    return "_";
                }
                case "parameter": {
                    return this.nameForPath(type.item.value);
                }
                case "declared": {
                    if (isTopLevel) {
                        const typeDeclaration = this.getDeclarationFromPath(type.item.value.path);
                        if (typeDeclaration) {
                            const documentation = this.renderDocumentation(typeDeclaration);
                            if (documentation) {
                                for (const attribute of documentation.attributes) {
                                    if (
                                        attribute.name === "description" &&
                                        attribute.value.type === "string"
                                    ) {
                                        isDescription = true;
                                        return attribute.value.value;
                                    }
                                }
                            }
                        }
                    }

                    const name = this.nameForPath(type.item.value.path);
                    if (!name) {
                        return "_";
                    }

                    const rendered =
                        type.item.value.parameters.length === 0
                            ? name
                            : `${name} ${type.item.value.parameters
                                  .map((parameter) => render(parameter, false, false))
                                  .join(" ")}`;

                    return isTopLevel || type.item.value.parameters.length === 0
                        ? rendered
                        : `(${rendered})`;
                }
                case "function": {
                    const inputs = type.item.value.inputs
                        .map((input) => render(input, false, false))
                        .join(" ");

                    const output = render(type.item.value.output, false, true);

                    const rendered = `${inputs} -> ${output}`;

                    return isTopLevel && isReturn ? rendered : `(${rendered})`;
                }
                case "tuple": {
                    const rendered =
                        type.item.value.length === 0
                            ? "()"
                            : type.item.value.length === 1
                            ? `${render(type.item.value[0], isTopLevel, isReturn)} ;`
                            : `(${type.item.value
                                  .map((value) => render(value, false, false))
                                  .join(" ; ")})`;

                    return isTopLevel || type.item.value.length === 0 ? rendered : `(${rendered})`;
                }
                case "block": {
                    return `{${render(type.item.value, true, false)}}`;
                }
                case "intrinsic": {
                    return "intrinsic";
                }
            }
        };

        const rendered = render(type, isTopLevel, true);
        return renderAsCode && !isDescription ? `\`${rendered}\`` : rendered;
    }

    renderTypeFunction(
        parameters: compiler.lower_Path[],
        bounds: compiler.WithInfo<compiler.Info, compiler.typecheck_Instance>[],
        format:
            | { kind: "arrow" }
            | {
                  kind: "description";
                  type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>;
              },
    ): string {
        switch (format.kind) {
            case "arrow": {
                if (parameters.length === 0) {
                    return "";
                }

                const renderedParameters = parameters
                    .map((parameter) => this.nameForPath(parameter))
                    .join(" ");

                let renderedBounds = bounds
                    .map((bound) => `${this.renderInstance(bound)}`)
                    .join(" ");

                if (renderedBounds) {
                    renderedBounds = ` where ${renderedBounds}`;
                }

                return `${renderedParameters}${renderedBounds} => `;
            }
            case "description": {
                const renderedType = this.renderType(format.type, true, true);

                switch (parameters.length) {
                    case 0: {
                        return renderedType;
                    }
                    case 1: {
                        return `${renderedType} for any type \`${this.nameForPath(
                            parameters[0],
                        )}\``;
                    }
                    default: {
                        const last = parameters.pop()!;

                        return `${this.renderType(
                            format.type,
                            true,
                            true,
                        )} for any types ${parameters
                            .map((parameter) => `\`${this.nameForPath(parameter)}\``)
                            .join(", ")} and \`${this.nameForPath(last)}\``;
                    }
                }
            }
            default:
                format satisfies never;
                throw new Error("unknown format");
        }
    }

    renderInstance(
        instance: compiler.WithInfo<compiler.Info, compiler.typecheck_Instance>,
    ): string {
        const trait = this.nameForPath(instance.item.trait);

        const parameters = instance.item.parameters
            .map((type) => this.renderType(type, false, false))
            .join(" ");

        return parameters.length === 0 ? trait : `(${trait} ${parameters})`;
    }

    renderDiagnostic(
        diagnostic: compiler.WithInfo<compiler.Info, compiler.Diagnostic>,
    ): RenderedDiagnostic | null {
        const renderedSourceLocation = this.renderSourceLocation(diagnostic);
        if (!renderedSourceLocation) {
            return null;
        }

        let severity: "warning" | "error";
        let message: string;
        let fix: RenderedFix | null = null;
        switch (diagnostic.item.type) {
            case "tokenize": {
                switch (diagnostic.item.value.type) {
                    case "invalidToken": {
                        severity = "error";
                        message = "unrecognized symbol";
                        fix = {
                            message: "remove this symbol",
                            replacement: "",
                        };
                        break;
                    }
                    case "mismatch": {
                        severity = "error";

                        const { expected, found } = diagnostic.item.value.value;
                        if (expected && found) {
                            message = `expected ${this.renderToken(
                                expected,
                                "a",
                            )} here, but found ${this.renderToken(found, "a")}`;
                        } else if (expected && !found) {
                            message = `expected ${this.renderToken(expected, "a")} here`;
                        } else if (!expected && found) {
                            message = `unexpected ${this.renderToken(found, "a")} here`;
                            fix = {
                                message: `remove ${this.renderToken(found, "this")}`,
                                replacement: "",
                            };
                        } else {
                            return null;
                        }

                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "parse": {
                const { expected, stack, direction } = diagnostic.item.value;
                const context =
                    stack.length > 1
                        ? ` while reading this ${this.renderSyntaxKind(
                              stack[stack.length - 2].item,
                          )}`
                        : "";

                switch (direction) {
                    case "before": {
                        severity = "error";
                        message = `expected ${this.renderSyntaxKind(
                            expected,
                        )} before this${context}`;
                        break;
                    }
                    case "after": {
                        severity = "error";
                        message = `expected ${this.renderSyntaxKind(
                            expected,
                        )} after this${context}`;
                        break;
                    }
                    case null: {
                        severity = "error";
                        message = `expected ${this.renderSyntaxKind(expected)} here${context}`;
                        break;
                    }
                    default:
                        direction satisfies never;
                        return null;
                }

                break;
            }
            case "syntax": {
                switch (diagnostic.item.value.type) {
                    case "unexpectedBound": {
                        severity = "error";
                        message = "bounds aren't allowed on type and trait definitions";
                        break;
                    }
                    case "expectedConstantValue": {
                        severity = "error";
                        message = `missing a value for \`${diagnostic.item.value.value}\` on the next line`;
                        break;
                    }
                    case "emptyTypeRepresentation": {
                        severity = "error";
                        message = "missing a field or variant between the braces in this type";
                        break;
                    }
                    case "expectedField": {
                        severity = "error";
                        message = "expected a field of the form `name :: Type` here";
                        break;
                    }
                    case "expectedVariant": {
                        severity = "error";
                        message = "expected a variant of the form `Name` here";
                        break;
                    }
                    case "invalidTextLiteral": {
                        severity = "error";
                        message = diagnostic.item.value.value.error;
                        break;
                    }
                    case "invalidPlaceholderText": {
                        const { expected, found } = diagnostic.item.value.value;
                        severity = "error";
                        message = `text has ${expected} placeholders, but ${found} inputs were provided here`;
                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "lower": {
                switch (diagnostic.item.value.type) {
                    case "unresolvedName": {
                        severity = "error";
                        message = `can't find \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedType": {
                        severity = "error";
                        message = `can't find type \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedTrait": {
                        severity = "error";
                        message = `can't find trait \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedVariant": {
                        severity = "error";
                        message = `can't find variant \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedLanguageItem": {
                        severity = "error";
                        message = `can't find language item \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "ambiguousName": {
                        severity = "error";
                        message = `\`${diagnostic.item.value.value}\` has multiple definitions`;
                        break;
                    }
                    case "alreadyDefined": {
                        severity = "error";
                        message = `\`${diagnostic.item.value.value}\` is already defined`;
                        break;
                    }
                    case "nestedLanguageDeclaration": {
                        severity = "error";
                        message = "language items must be declared at the top level";
                        break;
                    }
                    case "notAWrapper": {
                        severity = "error";
                        message =
                            "this pattern matches a structure or enumeration, not a wrapper type";
                        break;
                    }
                    case "wrapperExpectsASinglePattern": {
                        severity = "error";
                        message = "expected a single pattern after the name of the type";
                        break;
                    }
                    case "invalidMutatePattern": {
                        severity = "error";
                        message = "`!` only works when assigning to a variable using `:`";
                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "typecheck": {
                switch (diagnostic.item.value.type) {
                    case "recursionLimit": {
                        severity = "error";
                        message = "this code is too complex to check; try simplifying it";
                        break;
                    }
                    case "missingLanguageItem": {
                        severity = "error";
                        message = `checking this code requires the \`${diagnostic.item.value.value}\` language item`;
                        break;
                    }
                    case "unknownType": {
                        severity = "error";

                        const renderedType = this.renderType(
                            { info: diagnostic.info, item: diagnostic.item.value.value },
                            true,
                            true,
                        );

                        // TODO: Roles (eg. convert a `Text -> _` on the function into a `_` on the function call)
                        if (renderedType === "_") {
                            message = "could not determine what kind of value this code produces";
                        } else {
                            message = `this code produces a ${renderedType}, but the \`_\`s are unknown`;
                        }

                        break;
                    }
                    case "undeclaredTypeParameter": {
                        severity = "error";
                        message = `this code references the type parameter \`${diagnostic.item.value.value}\`, which isn't available here`;
                        break;
                    }
                    case "mismatch": {
                        const { expected, actual, expectedRoles } = diagnostic.item.value.value;

                        severity = "error";

                        const { message: customMessage, fix: customFix } =
                            this.renderOnMismatchDiagnostic(actual, expected);

                        if (customMessage && customFix) {
                            message = customMessage;
                            fix = customFix;
                            break;
                        } else if (customMessage) {
                            message = customMessage;
                            break;
                        } else if (customFix) {
                            fix = customFix;
                        }

                        const renderedRole =
                            expectedRoles.length > 0
                                ? this.renderTypeRole(expectedRoles[0].item)
                                : "";

                        message = renderedRole
                            ? `expected this ${renderedRole} to be a ${this.renderType(
                                  expected,
                                  true,
                                  true,
                              )} here, but found a ${this.renderType(actual, true, true)}`
                            : `expected a ${this.renderType(
                                  expected,
                                  true,
                                  true,
                              )} here, but found a ${this.renderType(actual, true, true)}`;

                        break;
                    }
                    case "wrongNumberOfInputs": {
                        const { expected, actual } = diagnostic.item.value.value;
                        severity = "error";
                        message = `this function takes ${expected} inputs, but ${actual} were provided`;
                        break;
                    }
                    case "unresolvedInstance": {
                        const renderedInstance = this.renderInstance({
                            info: diagnostic.info,
                            item: diagnostic.item.value.value.instance,
                        });
                        severity = "error";
                        message = `this code requires \`${renderedInstance}\``;
                        break;
                    }
                    case "notAStructure": {
                        const renderedType = this.renderType(
                            diagnostic.item.value.value,
                            true,
                            true,
                        );
                        severity = "error";
                        message = `${renderedType} is not a structure`;
                        break;
                    }
                    case "missingFields": {
                        const renderedFields = diagnostic.item.value.value
                            .map((field) => `\`${field}\``)
                            .join(", ");
                        severity = "error";
                        message = `missing fields ${renderedFields}`;
                        break;
                    }
                    case "extraField": {
                        severity = "error";
                        message = "extra field";
                        break;
                    }
                    case "overlappingInstances": {
                        severity = "error";
                        message =
                            "this instance already exists elsewhere; try making it more specific";
                        break;
                    }
                    case "missingPatterns": {
                        const patterns = diagnostic.item.value.value;
                        const last = patterns.pop()!;

                        severity = "error";
                        message =
                            patterns.length === 0
                                ? `this code doesn't handle ${this.renderPattern(last, true)}`
                                : `this code doesn't handle ${patterns
                                      .map((pattern) => this.renderPattern(pattern, true))
                                      .join(", ")} or ${this.renderPattern(last, true)}`;
                        break;
                    }
                    case "extraPattern": {
                        severity = "warning";
                        message = "this pattern is unnecessary because it is already handled above";
                        fix = {
                            message: "remove this pattern",
                            replacement: "",
                        };
                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            default:
                diagnostic.item satisfies never;
                return null;
        }

        return {
            location: renderedSourceLocation,
            severity,
            message,
            fix,
        };
    }

    renderOnMismatchDiagnostic(
        actualType: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
        expectedType: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
    ): { message?: string; fix?: RenderedFix } {
        if (expectedType.item.type !== "declared") {
            return {};
        }

        const expectedTypeDeclaration = this.getDeclarationFromPath(expectedType.item.value.path);
        if (!expectedTypeDeclaration || expectedTypeDeclaration.item.type !== "type") {
            return {};
        }

        const expectedTypeParameters = expectedTypeDeclaration.item.declaration.parameters;

        const expectedDocumentation = this.renderDocumentation(expectedTypeDeclaration);
        if (!expectedDocumentation) {
            return {};
        }

        let message: string | undefined;
        let fix: RenderedFix | undefined;
        for (const attribute of expectedDocumentation.attributes) {
            if (message && fix) {
                break;
            }

            if (attribute.name !== "on-mismatch" && attribute.name !== "fix-mismatch") {
                continue;
            }

            if (attribute.input) {
                const inputMatch = /`(.*)`/.exec(attribute.input);

                if (inputMatch) {
                    const [_, renderedMismatchedType] = inputMatch;

                    const mismatchedParsedType = compiler.parseType(renderedMismatchedType);
                    if (!mismatchedParsedType) {
                        continue;
                    }

                    const actualParsedType = compiler.parsedTypeFromCompiled(actualType);

                    if (!compiler.parsedTypesAreEqual(mismatchedParsedType, actualParsedType)) {
                        continue;
                    }
                }
            }

            switch (attribute.value.type) {
                case "string": {
                    message = attribute.value.value;
                    break;
                }
                case "formattedString": {
                    switch (attribute.name) {
                        case "on-mismatch": {
                            const segments = attribute.value.segments.map(
                                ([string, typeParameterName]) => {
                                    let value = "`_`";
                                    if (
                                        actualType.item.type === "declared" &&
                                        actualType.item.value.path ===
                                            expectedTypeDeclaration.item.path
                                    ) {
                                        const typeParameterIndex = expectedTypeParameters.findIndex(
                                            (path) => typeParameterName === this.nameForPath(path),
                                        );

                                        if (typeParameterIndex !== -1) {
                                            value = `\`${actualType.item.value.parameters[typeParameterIndex]}\``;
                                        }
                                    }

                                    return string + value;
                                },
                            );

                            message = segments.join("") + attribute.value.trailing;

                            break;
                        }
                        case "fix-mismatch": {
                            if (attribute.value.segments.length !== 1) {
                                continue;
                            }

                            const [message, replacement] = attribute.value.segments[0];
                            fix = { message, replacement };

                            break;
                        }
                        default: {
                            continue;
                        }
                    }

                    break;
                }
                case "boolean": {
                    continue;
                }
            }
        }

        return { message, fix };
    }

    renderTypeRole(role: compiler.typecheck_Role): string {
        switch (role) {
            case "pattern":
                return "pattern";
            case "annotation":
                return "annotation";
            case "trait":
                return "trait";
            case "instance":
                return "instance";
            case "structureField":
                return "structure field";
            case "variantElement":
                return "variant element";
            case "wrappedType":
                return "wrapped type";
            case "functionInput":
                return "function input";
            case "functionOutput":
                return "function output";
            case "bound":
                return "bound";
            case "defaultType":
                return "default type";
            case "variable":
                return "variable";
            case "typeParameter":
                return "type parameter";
            case "emptyBlock":
                return "empty block";
            case "whenArm":
                return "when arm";
            case "collectionElement":
                return "collection element";
        }
    }

    renderToken(token: compiler.Token, prefix: "a" | "this"): string {
        const altPrefix = prefix === "a" ? "the" : "this";

        switch (token.type) {
            case "number":
                return `${prefix} number`;
            case "leftParenthesis":
                return `${prefix} opening parenthesis (\`(\`)`;
            case "rightParenthesis":
                return `${prefix} closing parenthesis (\`)\`)`;
            case "leftBracket":
                return `${prefix} opening bracket (\`[\`)`;
            case "rightBracket":
                return `${prefix} closing bracket (\`]\`)`;
            case "leftBrace":
                return `${prefix} opening brace (\`{\`)`;
            case "rightBrace":
                return `${prefix} closing brace (\`}\`)`;
            case "lineBreak":
                return `${altPrefix} end of the line`;
            case "comment":
                return `${prefix} comment`;
            case "keyword":
                return `${altPrefix} word \`${token.value}\``;
            case "operator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "variadicOperator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "nonAssociativeOperator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "name":
                return `${prefix} name`;
            case "text":
                return `${prefix} piece of text`;
        }
    }

    renderSyntaxKind(kind: compiler.syntax_SyntaxKind): string {
        switch (kind) {
            case "number":
                return "number";
            case "topLevel":
                return "top level";
            case "name":
                return "name";
            case "text":
                return "text";
            case "statement":
                return "statement";
            case "keyword":
                return "keyword";
            case "operator":
                return "operator";
            case "instance":
                return "instance";
            case "typeParameter":
                return "type parameter";
            case "pattern":
                return "pattern";
            case "wildcardPattern":
                return "wildcard pattern";
            case "numberPattern":
                return "number pattern";
            case "textPattern":
                return "text pattern";
            case "variantPattern":
                return "variant pattern";
            case "destructurePattern":
                return "destructure pattern";
            case "tuplePattern":
                return "tuple pattern";
            case "orPattern":
                return "or pattern";
            case "mutatePattern":
                return "mutate pattern";
            case "expression":
                return "expression";
            case "type":
                return "type";
            case "placeholderType":
                return "placeholder type";
            case "declaredType":
                return "declared type";
            case "functionType":
                return "function type";
            case "tupleType":
                return "tuple type";
            case "blockType":
                return "block type";
            case "intrinsicType":
                return "intrinsic type";
            case "typeMember":
                return "type member";
            case "fieldDeclaration":
                return "field declaration";
            case "variantDeclaration":
                return "variant declaration";
            case "arm":
                return "arm";
            case "typeFunction":
                return "type function";
            case "typeRepresentation":
                return "type representation";
            case "typeDeclaration":
                return "type declaration";
            case "traitDeclaration":
                return "trait declaration";
            case "instanceDeclaration":
                return "instance declaration";
            case "constantDeclaration":
                return "constant declaration";
            case "languageDeclaration":
                return "language declaration";
            case "assignment":
                return "assignment";
            case "annotateExpression":
                return "annotate expression";
            case "nameExpression":
                return "name expression";
            case "numberExpression":
                return "number expression";
            case "textExpression":
                return "text expression";
            case "doExpression":
                return "do expression";
            case "callExpression":
                return "call expression";
            case "applyExpression":
                return "apply expression";
            case "binaryOperatorExpression":
                return "binary operator expression";
            case "asExpression":
                return "as expression";
            case "isExpression":
                return "is expression";
            case "whenExpression":
                return "when expression";
            case "intrinsicExpression":
                return "intrinsic expression";
            case "tupleExpression":
                return "tuple expression";
            case "collectionExpression":
                return "collection expression";
            case "structureExpression":
                return "structure expression";
            case "structureField":
                return "structure field";
            case "whenBody":
                return "when body";
            case "whenArm":
                return "when arm";
            case "blockExpression":
                return "block expression";
            case "functionExpression":
                return "function expression";
            case "functionInputs":
                return "function inputs";
            case "nothing":
                return "nothing";
        }
    }

    renderDiagnosticToDebugString(diagnostic: RenderedDiagnostic): string {
        return `${diagnostic.location.visiblePath}:${diagnostic.location.start.line}:${diagnostic.location.start.column}: ${diagnostic.severity}: ${diagnostic.message}`;
    }

    renderDocumentation(
        value: compiler.WithInfo<compiler.Info, unknown>,
    ): RenderedDocumentation | null {
        const renderedSourceLocation = this.renderSourceLocation(value);
        if (!renderedSourceLocation) {
            return null;
        }

        const lines = this.files[renderedSourceLocation.path].code.split("\n");

        let line = renderedSourceLocation.start.line - 1;
        if (line < 0) {
            return null;
        }

        const docLines: string[] = [];
        while (true) {
            const code = lines[line];
            if (!code) {
                break;
            }

            if (code.startsWith("--")) {
                docLines.push(code.slice(2));
                line--;
            } else {
                break;
            }
        }

        if (docLines.length === 0) {
            return null;
        }

        docLines.reverse();

        const docs: string[] = [];
        const attributes: RenderedDocumentationAttribute[] = [];

        for (let docLine of docLines) {
            const attributeMatch = /^\s*\[(.*)\]:(.*)/.exec(docLine);
            if (attributeMatch) {
                let [_, name, value] = attributeMatch;
                name = name.trim();

                let input: string | null = null;
                if (name.includes(" ")) {
                    [name, input] = name.split(" ", 2);
                    name = name.trim();
                    input = input.trim();
                }

                value = value.trim();

                const stringMatch = /^"((?:[^"\\]|\\.)*)"(.*)$/.exec(value);
                if (stringMatch) {
                    let [_, string, rest] = stringMatch;

                    const values = rest
                        .split(/(`[^`]*`)|("[^"]*")/)
                        .filter(Boolean)
                        .flatMap((s) => {
                            s = s.trim();
                            if (s.startsWith('"') || s.startsWith("`")) {
                                s = s.slice(1, s.length - 1);
                            }
                            return s === "" ? [] : [s];
                        });

                    if (values.length === 0) {
                        attributes.push({
                            name,
                            input,
                            value: { type: "string", value: string },
                        });
                    } else {
                        const stringSegments = string.split("_");

                        const trailing = stringSegments.pop() ?? "";

                        let segments: [string, string][];
                        // Special case: "message" followed by `fix`
                        if (stringSegments.length === 0 && values.length === 1) {
                            segments = [[trailing, values[0]]];
                        } else {
                            segments = stringSegments.map(
                                (s, i) => [s, values[i] ?? ""] as [string, string],
                            );
                        }

                        attributes.push({
                            name,
                            input,
                            value: { type: "formattedString", segments, trailing },
                        });
                    }

                    continue;
                }

                if (value === "True" || value === "False") {
                    attributes.push({
                        name,
                        input,
                        value: { type: "boolean", value: value === "True" },
                    });

                    continue;
                }
            }

            docs.push(docLine);
        }

        return { docs: docs.join("\n"), attributes };
    }

    private traverseExpression(
        expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        f: (
            expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        ) => boolean,
    ): compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression> | null {
        const traverse = (
            expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        ) => {
            if (f(expression)) {
                throw expression;
            }

            switch (expression.item.kind.type) {
                case "function":
                    traverse(expression.item.kind.value.body);
                    break;
                case "block":
                    for (const statement of expression.item.kind.value) {
                        traverse(statement);
                    }

                    break;
                case "do":
                    traverse(expression.item.kind.value);
                    break;
                case "call":
                    traverse(expression.item.kind.value.function);

                    for (const input of expression.item.kind.value.inputs) {
                        traverse(input);
                    }

                    break;
                case "when":
                    traverse(expression.item.kind.value.input);

                    for (const arm of expression.item.kind.value.arms) {
                        traverse(arm.item.body);
                    }

                    break;
                case "intrinsic":
                    for (const input of expression.item.kind.value.inputs) {
                        traverse(input);
                    }

                    break;
                case "initialize":
                    traverse(expression.item.kind.value.value);
                    break;
                case "mutate":
                    traverse(expression.item.kind.value.value);
                    break;
                case "marker":
                    break;
                case "structure":
                    for (const field of expression.item.kind.value.fields) {
                        traverse(field.item.value);
                    }

                    break;
                case "variant":
                    for (const value of expression.item.kind.value.values) {
                        traverse(value);
                    }

                    break;
                case "wrapper":
                    traverse(expression.item.kind.value);
                    break;
                case "tuple":
                    for (const value of expression.item.kind.value) {
                        traverse(value);
                    }

                    break;
                case "format":
                    for (const segment of expression.item.kind.value.segments) {
                        traverse(segment.value);
                    }

                    break;
                case "number":
                    break;
                case "unknown":
                    break;
                case "variable":
                    break;
                case "constant":
                    break;
                case "trait":
                    break;
                case "text":
                    break;
                default:
                    expression.item.kind satisfies never;
                    break;
            }
        };

        try {
            traverse(expression);
        } catch (expression: any) {
            return expression;
        }

        return null;
    }

    private compareInfo(left: compiler.Info, right: compiler.Info): boolean {
        return (
            left.location.visiblePath === right.location.visiblePath &&
            left.location.span.start === right.location.span.start &&
            left.location.span.end === right.location.span.end
        );
    }

    private nameForPath(path: compiler.lower_Path): string {
        return path.split(" / ").pop()?.split(" ").pop() ?? "<unknown>";
    }
}
