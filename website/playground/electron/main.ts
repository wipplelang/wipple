import { app, BrowserWindow } from "electron";

app.whenReady().then(() => {
    const win = new BrowserWindow({
        title: "Wipple Playground",
        minWidth: 500,
        minHeight: 290,
    });

    win.loadURL(process.env.VITE_DEV_SERVER_URL || "dist/index.html");
});
