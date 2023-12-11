const { app, BrowserWindow, net, protocol, shell, ipcMain } = require("electron");
const path = require("path");
const url = require("url");
const energyUsage = require("./helpers/energy-usage");

protocol.registerSchemesAsPrivileged([
    {
        scheme: "wipple",
        privileges: {
            standard: true,
            secure: true,
            stream: true,
            supportFetchAPI: true,
            allowServiceWorkers: true,
        },
    },
]);

app.whenReady().then(() => {
    protocol.handle("wipple", (request) => {
        let resolvedPath = path.join(
            path.dirname(__dirname),
            "resources",
            decodeURIComponent(new URL(request.url).pathname)
        );

        if (path.extname(resolvedPath) === "") {
            resolvedPath = path.join(resolvedPath, "index.html");
        }

        return net.fetch(
            url.format({
                protocol: "file:",
                pathname: resolvedPath,
                slashes: true,
            })
        );
    });

    const windowOptions = {
        title: "Wipple Playground",
        width: 1024,
        height: 800,
        minWidth: 500,
        minHeight: 290,
        webPreferences: {
            preload: path.join(__dirname, "preload.js"),
        },
    };

    const configureWindow = (window) => {
        window.webContents.setWindowOpenHandler(({ url, disposition }) => {
            if (url.startsWith("wipple:")) {
                return {
                    action: "allow",
                    overrideBrowserWindowOptions:
                        disposition === "new-window" || disposition === "foreground-tab"
                            ? windowOptions
                            : undefined,
                };
            }

            shell.openExternal(url);
            return { action: "deny" };
        });

        window.webContents.on("did-create-window", (window) => {
            configureWindow(window);
        });
    };

    const browserWindow = new BrowserWindow(windowOptions);
    browserWindow.loadURL("wipple://playground/playground/index.html");
    configureWindow(browserWindow);
});

ipcMain.handle("wipple-energyUsage-beginMeasuring", async (_event, id) => {
    await energyUsage.beginMeasuring(id);
});

ipcMain.handle("wipple-energyUsage-endMeasuring", async (_event, id) => {
    return energyUsage.endMeasuring(id);
});
