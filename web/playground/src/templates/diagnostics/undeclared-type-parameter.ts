import { DiagnosticTemplate } from ".";

export const undeclaredTypeParameterTemplate: DiagnosticTemplate = ({ name }) => ({
    title: `Can't use \`${name}\` here because it's from an outer function`,
    description:
        "You can't use a type parameter that isn't declared directly within this function's type annotation. Try adding another type parameter to this function and add a type annotation where it's used if needed.",
    help: undefined,
});
