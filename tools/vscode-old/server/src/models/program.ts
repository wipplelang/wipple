import { Span } from "./span";

export interface Program {
    valid: boolean;
    body: Expression[];
    declarations: Declarations;
}

export interface Expression {
    span: Span;
    ty: Type;
    kind: ExpressionKind;
}

export type ExpressionKind =
    | { type: "Marker" }
    | { type: "Variable"; value: number }
    | { type: "Constant"; value: number }
    | { type: "Text"; value: string }
    | { type: "Number"; value: number }
    | { type: "Block"; value: Expression[] }
    | { type: "Call"; value: [Expression, Expression] }
    | { type: "Function"; value: [Pattern, Expression] }
    | { type: "When"; value: [Expression, Arm[]] }
    | { type: "External"; value: [string, string, Expression[]] }
    | { type: "Initialize"; value: [Pattern, Expression] }
    | { type: "Structure"; value: Expression[] }
    | { type: "Variant"; value: [number, Expression[]] }
    | { type: "Return"; value: Expression }
    | { type: "Loop"; value: Expression }
    | { type: "Break"; value: Expression }
    | { type: "Continue" }
    | { type: "Tuple"; value: Expression[] };

export interface Arm {
    span: Span;
    pattern: Pattern;
    body: Expression;
}

export interface Pattern {
    span: Span;
    kind: PatternKind;
}

export type PatternKind =
    | { type: "Wildcard" }
    | { type: "Number"; value: number }
    | { type: "Text"; value: string }
    | { type: "Variable"; value: number }
    | { type: "Or"; value: [Pattern, Pattern] }
    | { type: "Where"; value: [Pattern, Expression] }
    | { type: "Tuple"; value: Pattern[] }
    | { type: "Destructure"; value: Record<number, Pattern> }
    | { type: "Variant"; value: [number, Pattern[]] };

export type UnresolvedType =
    | { type: "Variable"; value: number }
    | { type: "Parameter"; value: number }
    | { type: "Named"; value: [number, UnresolvedType[], TypeStructure] }
    | { type: "Function"; value: [UnresolvedType, UnresolvedType] }
    | { type: "Tuple"; value: UnresolvedType[] }
    | { type: "Builtin"; value: BuiltinType<UnresolvedType> }
    | { type: "Bottom"; value: BottomTypeReason };

export type Type =
    | { type: "Parameter"; value: number }
    | { type: "Named"; value: [number, Type[], TypeStructure] }
    | { type: "Function"; value: [Type, Type] }
    | { type: "Tuple"; value: Type[] }
    | { type: "Builtin"; value: BuiltinType<Type> }
    | { type: "Bottom"; value: BottomTypeReason };

export type TypeStructure =
    | { type: "Marker" }
    | { type: "Structure"; value: Type[] }
    | { type: "Enumeration"; value: Type[][] };

export type BuiltinType<Ty> =
    | { type: "Number" }
    | { type: "Text" }
    | { type: "List"; value: Ty }
    | { type: "Mutable"; value: Ty };

export type BottomTypeReason = "Annotated" | "Error" | "Placeholder";

export interface Declarations {
    operators: Record<string, Operator>;
    templates: Record<string, TemplateDeclaration>;
    types: Record<string, Declaration<TypeDeclaration>>;
    type_parameters: Record<string, Declaration<undefined>>;
    traits: Record<string, Declaration<TraitDeclaration>>;
    generic_constants: Record<string, GenericConstantDeclaration>;
    monomorphized_constants: Record<string, [undefined, number, Declaration<Expression>]>;
    variables: Record<string, Declaration<Type>>;
}

export interface Operator {
    precedence: string;
    template: number;
}

export interface TemplateDeclaration {
    name: string;
    span: Span;
    attributes: TemplateDeclarationAttributes;
    uses: Span[];
}

export interface TemplateDeclarationAttributes {
    keyword: boolean;
}

export interface Declaration<T> {
    name?: string;
    span: Span;
    value: T;
    uses: Span[];
}

export interface DeclarationAttributes {
    help: string[];
}

export interface TraitAttributes {
    decl_attributes: DeclarationAttributes;
    on_unimplemented?: string;
}

export interface TypeDeclaration {
    attributes: DeclarationAttributes;
}

export interface TraitDeclaration {
    ty: UnresolvedType;
    params: number[];
    attributes: TraitAttributes;
}

export interface GenericConstantDeclaration {
    file: undefined;
    decl: Declaration<Expression>;
    attributes?: DeclarationAttributes;
}
