const tmp = require("tmp");
const fs = require("fs");
const { spawn } = require("child_process");

const pollProcesses = {};

module.exports.beginMeasuring = (id) =>
    new Promise(async (resolve, reject) => {
        const { name: outputFileName } = tmp.fileSync();
        const { name: doneSignalFileName } = tmp.fileSync();

        // HACK: osascript doesn't kill powermetrics on its own, so we do it ourselves
        // NOTE: The '&>/dev/null' is important because osascript won't exit if stdio is open
        const script = `#!/bin/bash -e
touch '${outputFileName}'
touch '${doneSignalFileName}'
/usr/bin/powermetrics -i 10 --samplers cpu_power -o '${outputFileName}' &>/dev/null &
PID=$!

kill_powermetrics() {
    while [[ \\"$(< '${doneSignalFileName}')\\" != \\"done\\" ]]
    do
        echo 'waiting for signal'
        sleep 1
    done

    echo 'received signal'
    kill $PID
    echo 'killed powermetrics (PID $PID)'
}

kill_powermetrics &>/dev/null &`;

        const childProcess = spawn("/usr/bin/osascript", [
            "-e",
            `do shell script "${script}" with administrator privileges`,
        ]);

        childProcess.stderr.pipe(process.stderr);

        childProcess.on("spawn", () => {
            // Wait for 'touch' to be called, because then we know the user
            // granted administrator privileges
            const watcher = fs.watchFile(outputFileName, () => {
                fs.unwatchFile(outputFileName, watcher);
                resolve();
            });
        });

        childProcess.on("error", (error) => {
            reject(error);
        });

        pollProcesses[id] = { childProcess, outputFileName, doneSignalFileName };
    });

module.exports.endMeasuring = async (id) => {
    // Collect at least a few measurements
    await new Promise((resolve) => setTimeout(() => resolve(), 250));

    const { childProcess, outputFileName, doneSignalFileName } = pollProcesses[id];
    fs.writeFileSync(doneSignalFileName, "done");
    childProcess.kill("SIGINT");

    const output = fs.readFileSync(outputFileName, "utf8");

    const measurements = output
        .split("\n")
        .map((line) => {
            const match = /^CPU Power: (\d+) mW$/.exec(line);
            return match ? parseFloat(match[1]) / 1000 : undefined;
        })
        .filter((measurement) => measurement != null);

    if (measurements.length === 0) {
        return undefined;
    }

    return measurements.reduce((curr, next) => curr + next, 0) / measurements.length;
};
