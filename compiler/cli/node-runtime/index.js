import initRuntime from "./runtime.js";
import main from "./main.js";

const runtime = initRuntime({
  display: (message) => console.log(message),
});

main(runtime);
