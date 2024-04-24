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

export interface RenderedHighlight {
    category?: string;
    icon?: string;
}

export class Render {
    private files: Record<string, compiler.File & { linesAndColumns: LinesAndColumns }> = {};
    private declarations: compiler.WithInfo<compiler.Info, AnyDeclaration>[] = [];
    private interface: compiler.Interface | null = null;
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

        this.interface = interface_;
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

    renderCode(value: compiler.WithInfo<compiler.Info, unknown>): string | null {
        const renderedSourceLocation = this.renderSourceLocation(value);
        if (!renderedSourceLocation) {
            return null;
        }

        return this.files[renderedSourceLocation.path].code.slice(
            renderedSourceLocation.start.index,
            renderedSourceLocation.end.index,
        );
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
                const usedParameters = compiler.listTypeParameters(
                    declaration.item.declaration.simplifiedType,
                );

                const usedBounds = declaration.item.declaration.bounds.filter((bound) =>
                    bound.item.parameters.some((type) =>
                        compiler
                            .listTypeParameters(type)
                            .some((parameter) => usedParameters.includes(parameter)),
                    ),
                );

                if (usedParameters.length > 0 || usedBounds.length > 0) {
                    const typeFunction = this.renderTypeFunction(
                        declaration.item.declaration.parameters,
                        declaration.item.declaration.bounds,
                        { kind: "arrow" },
                    );

                    const type = this.renderType(
                        declaration.item.declaration.type,
                        false,
                        false,
                        false,
                    );

                    return `${declaration.item.name} :: ${typeFunction}${type}`;
                } else {
                    const type = this.renderType(
                        declaration.item.declaration.simplifiedType,
                        true,
                        true,
                        false,
                    );

                    return `${declaration.item.name} :: ${type}`;
                }
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
        describe: boolean,
        renderAsCode: boolean,
    ): string {
        if (isTopLevel && describe && this.interface) {
            const result = compiler.resolveAttributeLikeTrait(
                "describe-type",
                type,
                1,
                this.interface,
            );

            if (result) {
                const [description] = result;
                if (description.item.type === "message") {
                    return this.renderTypeLevelText(description.item.value, false);
                }
            }
        }

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
                case "message": {
                    let message = "";
                    for (const segment of type.item.value.segments) {
                        message += segment.text + render(segment.type, true, true);
                    }

                    message += type.item.value.trailing;

                    return message;
                }
                case "constant": {
                    return this.nameForPath(type.item.value);
                }
            }
        };

        const rendered = render(type, isTopLevel, true);
        return renderAsCode ? `\`${rendered}\`` : rendered;
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
                const renderedType = this.renderType(format.type, true, false, true);

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
                            false,
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
            .map((type) => this.renderType(type, false, false, false))
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
                            false,
                            true,
                        );

                        if (renderedType === "`_`") {
                            message = "could not determine what kind of value this code produces";
                        } else {
                            message = `this code produces ${renderedType}, but the \`_\`s are unknown`;
                        }

                        break;
                    }
                    case "undeclaredTypeParameter": {
                        severity = "error";
                        message = `this code references the type parameter \`${diagnostic.item.value.value}\`, which isn't available here`;
                        break;
                    }
                    case "mismatch": {
                        const { expected, actual } = diagnostic.item.value.value;

                        severity = "error";

                        let expectedMessage = this.renderType(expected, true, true, true);
                        let actualMessage = this.renderType(actual, true, true, true);

                        // If the type descriptions are equal, try rendering the
                        // actual type by setting `describe` to false
                        if (expectedMessage === actualMessage) {
                            expectedMessage = this.renderType(expected, true, false, true);
                            actualMessage = this.renderType(actual, true, false, true);
                        }

                        message = `expected ${expectedMessage} here, but found ${actualMessage}`;

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
                    case "traitHasNoValue": {
                        severity = "error";
                        message = "this trait can't be used as a value";
                        break;
                    }
                    case "expectedInstanceValue": {
                        severity = "error";
                        message = "`instance` declaration is missing a value";
                        break;
                    }
                    case "unexpectedInstanceValue": {
                        severity = "error";
                        message = "`instance` declaration declares a value, but the trait doesn't";
                        break;
                    }
                    case "notAStructure": {
                        const renderedType = this.renderType(
                            diagnostic.item.value.value,
                            true,
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
                    case "custom": {
                        severity = "error";

                        message = this.renderTypeLevelText(
                            diagnostic.item.value.value.message,
                            true,
                        );

                        if (diagnostic.item.value.value.fix) {
                            fix = {
                                message: this.renderTypeLevelText(
                                    diagnostic.item.value.value.fix[0],
                                    true,
                                ),
                                replacement: this.renderTypeLevelText(
                                    diagnostic.item.value.value.fix[1],
                                    false,
                                ),
                            };
                        }

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

    renderTypeLevelText(text: compiler.typecheck_MessageType, renderAsCode: boolean): string {
        const renderSegmentsAsCode =
            (text.segments.length === 0
                ? text.trailing.startsWith("`")
                : text.segments[0].text.startsWith("`")) && text.trailing.endsWith("`");

        let message = "";
        for (const segment of text.segments) {
            const code =
                renderSegmentsAsCode || segment.text.slice(segment.text.length - 1) === "`"
                    ? this.renderCode(segment.type)
                    : null;
            message += segment.text + (code ?? this.renderType(segment.type, true, true, true));
        }
        message += text.trailing;

        if (renderSegmentsAsCode && !renderAsCode) {
            message = message.slice(1, -1);
        }

        return message;
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
            case "annotatePattern":
                return "annotate pattern";
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
            case "messageType":
                return "message type";
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

        return { docs: docLines.join("\n") };
    }

    renderHighlight(value: compiler.WithInfo<compiler.Info, unknown>): RenderedHighlight | null {
        if (!this.interface) {
            return null;
        }

        const declaration = this.getDeclarationFromInfo(value.info);

        if (!declaration) {
            return null;
        }

        const constantReferenceType: compiler.WithInfo<compiler.Info, compiler.typecheck_Type> = {
            info: value.info,
            item: {
                type: "constant",
                value: declaration.item.path,
            },
        };

        const result = compiler.resolveAttributeLikeTrait(
            "highlight",
            constantReferenceType,
            1,
            this.interface,
        );

        if (!result) {
            return null;
        }

        const [highlightOptions] = result;

        const getOption = (
            name: string,
            type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
        ) =>
            type.item.type === "declared" &&
            type.item.value.path === this.interface!.languageDeclarations[name] &&
            type.item.value.parameters[0]?.item.type === "message"
                ? this.renderTypeLevelText(type.item.value.parameters[0].item.value, false)
                : null;

        switch (highlightOptions.item.type) {
            case "declared": {
                const category = getOption("highlight-category", highlightOptions);
                if (category) {
                    return { category };
                }

                const icon = getOption("highlight-icon", highlightOptions);
                if (icon) {
                    return { icon };
                }

                return null;
            }
            case "tuple": {
                const options: RenderedHighlight = {};

                for (const parameter of highlightOptions.item.value) {
                    const category = getOption("highlight-category", parameter);
                    if (category) {
                        options.category = category;
                    }

                    const icon = getOption("highlight-icon", parameter);
                    if (icon) {
                        options.icon = icon;
                    }
                }

                return options;
            }
            default:
                return null;
        }
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
