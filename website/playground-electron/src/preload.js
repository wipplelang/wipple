const { contextBridge, ipcRenderer } = require("electron/renderer");

contextBridge.exposeInMainWorld("electron", {
    wipple: {
        energyUsage: {
            beginMeasuring: (id) => ipcRenderer.invoke("wipple-energyUsage-beginMeasuring", id),
            endMeasuring: (id) => ipcRenderer.invoke("wipple-energyUsage-endMeasuring", id),
        },
    },
});
