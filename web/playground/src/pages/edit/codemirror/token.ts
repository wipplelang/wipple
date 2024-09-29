import { syntaxTree } from "@codemirror/language";
import { EditorState } from "@codemirror/state";

export const getTokenAtPos = (state: EditorState, pos: number) => {
    const { from, to } = syntaxTree(state).resolveInner(pos, -1);
    return state.sliceDoc(from, to);
};
