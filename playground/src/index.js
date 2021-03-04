import CodeFlask from "codeflask";
import { run } from "wipple-playground-interpreter";

const query = new URLSearchParams(window.location.search);

/** @type {import("codeflask").CodeFlaskOptions} */
const options = {
  defaultTheme: false,
  selfClosingCharacters: {
    open: ["(", "[", "{", '"'],
    close: [")", "]", "}", '"'],
  },
};

const wippleGrammar = {
  comment: /--(?: [^\n]*|\n)/,
  string: /\"[^\"\n]*\"/,
  number: /-?[0-9]+(?:\.[0-9]+)?/,
  operator: /[^ A-Za-z0-9\t\r\n\(\)\[\]\{\}'"]+/,
  punctuation: /[\(\)\[\]\{\}']+/,
};

const inputEditor = new CodeFlask(document.getElementById("input"), {
  ...options,
  language: "wipple",
  lineNumbers: true,
});
inputEditor.addLanguage("wipple", wippleGrammar);
inputEditor.updateCode(query.get("code") || "-- Write your code here!");

requestAnimationFrame(() => {
  inputEditor.updateLanguage("wipple");
});

const outputEditor = new CodeFlask(document.getElementById("output"), {
  ...options,
  language: "wipple-output-inactive",
  readonly: true,
});
outputEditor.updateCode(
  "No output. Use 'show' to display values in this area."
);
outputEditor.addLanguage("wipple-output-inactive", {});
outputEditor.addLanguage("wipple-output-active", {});
outputEditor.addLanguage("wipple-output-error", {});

const handleLoading = () => {
  outputEditor.updateLanguage("wipple-output-inactive");
  outputEditor.updateCode("Running...");
};

const handleResult = (result) => {
  if (result.success) {
    if (result.output.length === 0) {
      outputEditor.updateLanguage("wipple-output-inactive");
      outputEditor.updateCode(
        "No output. Use 'show' to display values in this area."
      );
    } else {
      const text = result.output
        .map(({ input, output }) => `${input} ==> ${output}`)
        .join("\n");

      outputEditor.updateLanguage("wipple-output-active");
      outputEditor.updateCode(text);
    }
  } else {
    outputEditor.updateLanguage("wipple-output-error");
    outputEditor.updateCode(result.error);
  }
};

inputEditor.onUpdate((code) => {
  query.set("code", code);
  const newURL = window.location.pathname + "?" + query.toString();
  window.history.replaceState(null, "", newURL);

  handleLoading();
  const result = run(code);
  handleResult(result);
});
