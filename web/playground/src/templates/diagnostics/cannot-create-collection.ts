import { DiagnosticTemplate } from ".";

export const cannotCreateCollectionTemplate: DiagnosticTemplate = ({ container, element }) => ({
    title: `Can't create ${container} from items that are ${element}`,
    description: `Creating ${container} from these items isn't supported. Double-check that you're providing the right kinds of items here.`,
    help: undefined,
});
