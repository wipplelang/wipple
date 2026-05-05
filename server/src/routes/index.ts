import compileRoute from "./compile";
import documentationRoute from "./documentation";
import ideInfoRoute from "./ide-info";
import formatRoute from "./format";
import runtimeRoute from "./runtime";

const routes = {
    "/compile": compileRoute,
    "/documentation": documentationRoute,
    "/ide-info": ideInfoRoute,
    "/format": formatRoute,
    "/runtime": runtimeRoute,
};

export default routes;
