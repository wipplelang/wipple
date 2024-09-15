import { alreadyDefinedTemplate } from "./already-defined";
import { cannotAddTemplate } from "./cannot-add";
import { cannotCollectTemplate } from "./cannot-collect";
import { cannotConvertTemplate } from "./cannot-convert";
import { cannotCreateCollectionTemplate } from "./cannot-create-collection";
import { cannotDescribeTemplate } from "./cannot-describe";
import { cannotDivideTemplate } from "./cannot-divide";
import { cannotMakeSequenceTemplate } from "./cannot-make-sequence";
import { cannotMultiplyTemplate } from "./cannot-multiply";
import { cannotOrderTemplate } from "./cannot-order";
import { cannotPowerTemplate } from "./cannot-power";
import { cannotReadTemplate } from "./cannot-read";
import { cannotRemainderTemplate } from "./cannot-remainder";
import { cannotSubtractTemplate } from "./cannot-subtract";
import { expectedTimesAfterNumberTemplate } from "./expected-times-after-number";
import { extraBoundTemplate } from "./extra-bound";
import { extraClosingBraceTemplate } from "./extra-closing-brace";
import { extraClosingBracketTemplate } from "./extra-closing-bracket";
import { extraClosingParenthesisTemplate } from "./extra-closing-parenthesis";
import { extraFieldTemplate } from "./extra-field";
import { extraInputTemplate } from "./extra-input";
import { extraPatternTemplate } from "./extra-pattern";
import { extraSymbolTemplate } from "./extra-symbol";
import { extraTypeTemplate } from "./extra-type";
import { instanceAlreadyExistsTemplate } from "./instance-already-exists";
import { invalidMutatePatternTemplate } from "./invalid-mutate-pattern";
import { invalidPlaceholderTextTemplate } from "./invalid-placeholder-text";
import { invalidTextLiteralTemplate } from "./invalid-text-literal";
import { irErrorTemplate } from "./ir-error";
import { mismatchedSymbolTemplate } from "./mismatched-symbol";
import { mismatchedTypesTemplate } from "./mismatched-types";
import { missingClosingBraceTemplate } from "./missing-closing-brace";
import { missingClosingBracketTemplate } from "./missing-closing-bracket";
import { missingClosingParenthesisTemplate } from "./missing-closing-parenthesis";
import { missingConstantValueTemplate } from "./missing-constant-value";
import { missingFieldsTemplate } from "./missing-fields";
import { missingFieldTemplate } from "./missing-field";
import { missingInputsTemplate } from "./missing-inputs";
import { missingInstanceValueTemplate } from "./missing-instance-value";
import { missingLanguageItemTemplate } from "./missing-language-item";
import { missingParenthesesAroundOperatorTemplate } from "./missing-parentheses-around-operator";
import { missingPatternsTemplate } from "./missing-patterns";
import { missingPixelsTemplate } from "./missing-pixels";
import { missingRepeatTemplate } from "./missing-repeat";
import { missingSymbolTemplate } from "./missing-symbol";
import { missingTypeRepresentationTemplate } from "./missing-type-representation";
import { missingTypesTemplate } from "./missing-types";
import { missingUnitForAccelerationTemplate } from "./missing-unit-for-acceleration";
import { missingUnitForAngleTemplate } from "./missing-unit-for-angle";
import { missingUnitForDistanceTemplate } from "./missing-unit-for-distance";
import { missingUnitForForceTemplate } from "./missing-unit-for-force";
import { missingUnitForMassTemplate } from "./missing-unit-for-mass";
import { missingUnitForMomentumTemplate } from "./missing-unit-for-momentum";
import { missingUnitForStiffnessTemplate } from "./missing-unit-for-stiffness";
import { missingUnitForTimeTemplate } from "./missing-unit-for-time";
import { missingUnitForVelocityTemplate } from "./missing-unit-for-velocity";
import { missingVariableTemplate } from "./missing-variable";
import { missingVariantTemplate } from "./missing-variant";
import { multipleDefinitionsTemplate } from "./multiple-definitions";
import { namingConventionTemplate } from "./naming-convention";
import { nestedLanguageDeclarationTemplate } from "./nested-language-declaration";
import { noEmptyValueTemplate } from "./no-empty-value";
import { notAStructureTemplate } from "./not-a-structure";
import { notAWrapperTemplate } from "./not-a-wrapper";
import { partiallyUnknownTypeTemplate } from "./partially-unknown-type";
import { tooComplexTemplate } from "./too-complex";
import { traitHasNoValueTemplate } from "./trait-has-no-value";
import { undeclaredTypeParameterTemplate } from "./undeclared-type-parameter";
import { unexpectedInstanceValueTemplate } from "./unexpected-instance-value";
import { unexpectedSymbolAfterTemplate } from "./unexpected-symbol-after";
import { unexpectedSymbolBeforeTemplate } from "./unexpected-symbol-before";
import { unknownTypeTemplate } from "./unknown-type";
import { unrecognizedSymbolTemplate } from "./unrecognized-symbol";
import { unresolvedInstanceTemplate } from "./unresolved-instance";
import { unresolvedLanguageItemTemplate } from "./unresolved-language-item";
import { unresolvedNameTemplate } from "./unresolved-name";
import { unresolvedTraitTemplate } from "./unresolved-trait";
import { unresolvedTypeTemplate } from "./unresolved-type";
import { unresolvedVariantTemplate } from "./unresolved-variant";
import { unusedColorTemplate } from "./unused-color";
import { useShowInsteadTemplate } from "./use-show-instead";
import { wrapperExpectsASinglePatternTemplate } from "./wrapper-expects-a-single-pattern";

export interface DiagnosticTemplate {
    variants: DiagnosticVariant[];
}

export interface DiagnosticVariant {
    title: string;
    description: string;
    help: DiagnosticHelp;
}

export type DiagnosticHelp =
    | DiagnosticHelpMessage
    | DiagnosticHelpChoice
    | DiagnosticHelpPrompt
    | undefined;

export interface DiagnosticHelpMessage {
    type: "message";
    message: string;
}

export interface DiagnosticHelpChoice {
    type: "choice";
    question: string;
    choices: {
        name: string;
        then: DiagnosticHelp;
    }[];
}

export interface DiagnosticHelpPrompt {
    type: "prompt";
    question: string;
    then: DiagnosticHelp;
}

export const resolveDiagnosticTemplate = (
    diagnostic: any,
    code: string,
): { template: DiagnosticTemplate; data: Record<string, string> } | undefined => {
    const template =
        diagnosticTemplates[diagnostic.template.id as keyof typeof diagnosticTemplates];

    if (!template) {
        return undefined;
    }

    return {
        template,
        data: { ...diagnostic.template.data, code },
    };
};

const diagnosticTemplates = {
    "already-defined": alreadyDefinedTemplate,
    "cannot-add": cannotAddTemplate,
    "cannot-collect": cannotCollectTemplate,
    "cannot-convert": cannotConvertTemplate,
    "cannot-create-collection": cannotCreateCollectionTemplate,
    "cannot-describe": cannotDescribeTemplate,
    "cannot-divide": cannotDivideTemplate,
    "cannot-make-sequence": cannotMakeSequenceTemplate,
    "cannot-multiply": cannotMultiplyTemplate,
    "cannot-order": cannotOrderTemplate,
    "cannot-power": cannotPowerTemplate,
    "cannot-read": cannotReadTemplate,
    "cannot-remainder": cannotRemainderTemplate,
    "cannot-subtract": cannotSubtractTemplate,
    "expected-times-after-number": expectedTimesAfterNumberTemplate,
    "extra-bound": extraBoundTemplate,
    "extra-closing-brace": extraClosingBraceTemplate,
    "extra-closing-bracket": extraClosingBracketTemplate,
    "extra-closing-parenthesis": extraClosingParenthesisTemplate,
    "extra-field": extraFieldTemplate,
    "extra-input": extraInputTemplate,
    "extra-pattern": extraPatternTemplate,
    "extra-symbol": extraSymbolTemplate,
    "extra-type": extraTypeTemplate,
    "instance-already-exists": instanceAlreadyExistsTemplate,
    "invalid-mutate-pattern": invalidMutatePatternTemplate,
    "invalid-placeholder-text": invalidPlaceholderTextTemplate,
    "invalid-text-literal": invalidTextLiteralTemplate,
    "ir-error": irErrorTemplate,
    "mismatched-symbol": mismatchedSymbolTemplate,
    "mismatched-types": mismatchedTypesTemplate,
    "missing-closing-brace": missingClosingBraceTemplate,
    "missing-closing-bracket": missingClosingBracketTemplate,
    "missing-closing-parenthesis": missingClosingParenthesisTemplate,
    "missing-constant-value": missingConstantValueTemplate,
    "missing-field": missingFieldTemplate,
    "missing-fields": missingFieldsTemplate,
    "missing-inputs": missingInputsTemplate,
    "missing-instance-value": missingInstanceValueTemplate,
    "missing-language-item": missingLanguageItemTemplate,
    "missing-parentheses-around-operator": missingParenthesesAroundOperatorTemplate,
    "missing-patterns": missingPatternsTemplate,
    "missing-pixels": missingPixelsTemplate,
    "missing-repeat": missingRepeatTemplate,
    "missing-symbol": missingSymbolTemplate,
    "missing-type-representation": missingTypeRepresentationTemplate,
    "missing-types": missingTypesTemplate,
    "missing-unit-for-acceleration": missingUnitForAccelerationTemplate,
    "missing-unit-for-angle": missingUnitForAngleTemplate,
    "missing-unit-for-distance": missingUnitForDistanceTemplate,
    "missing-unit-for-force": missingUnitForForceTemplate,
    "missing-unit-for-mass": missingUnitForMassTemplate,
    "missing-unit-for-momentum": missingUnitForMomentumTemplate,
    "missing-unit-for-stiffness": missingUnitForStiffnessTemplate,
    "missing-unit-for-time": missingUnitForTimeTemplate,
    "missing-unit-for-velocity": missingUnitForVelocityTemplate,
    "missing-variable": missingVariableTemplate,
    "missing-variant": missingVariantTemplate,
    "multiple-definitions": multipleDefinitionsTemplate,
    "naming-convention": namingConventionTemplate,
    "nested-language-declaration": nestedLanguageDeclarationTemplate,
    "no-empty-value": noEmptyValueTemplate,
    "not-a-structure": notAStructureTemplate,
    "not-a-wrapper": notAWrapperTemplate,
    "partially-unknown-type": partiallyUnknownTypeTemplate,
    "too-complex": tooComplexTemplate,
    "trait-has-no-value": traitHasNoValueTemplate,
    "undeclared-type-parameter": undeclaredTypeParameterTemplate,
    "unexpected-instance-value": unexpectedInstanceValueTemplate,
    "unexpected-symbol-after": unexpectedSymbolAfterTemplate,
    "unexpected-symbol-before": unexpectedSymbolBeforeTemplate,
    "unknown-type": unknownTypeTemplate,
    "unrecognized-symbol": unrecognizedSymbolTemplate,
    "unresolved-instance": unresolvedInstanceTemplate,
    "unresolved-language-item": unresolvedLanguageItemTemplate,
    "unresolved-name": unresolvedNameTemplate,
    "unresolved-trait": unresolvedTraitTemplate,
    "unresolved-type": unresolvedTypeTemplate,
    "unresolved-variant": unresolvedVariantTemplate,
    "unused-color": unusedColorTemplate,
    "use-show-instead": useShowInsteadTemplate,
    "wrapper-expects-a-single-pattern": wrapperExpectsASinglePatternTemplate,
};
