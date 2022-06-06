/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ([
/* 0 */,
/* 1 */
/***/ ((module) => {

"use strict";
module.exports = require("path");

/***/ }),
/* 2 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(3);

/***/ }),
/* 3 */
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SettingMonitor = exports.LanguageClient = exports.TransportKind = void 0;
const cp = __webpack_require__(4);
const fs = __webpack_require__(5);
const path = __webpack_require__(1);
const SemVer = __webpack_require__(6);
const vscode_1 = __webpack_require__(53);
const Is = __webpack_require__(54);
const client_1 = __webpack_require__(55);
const processes_1 = __webpack_require__(156);
const node_1 = __webpack_require__(157);
__exportStar(__webpack_require__(157), exports);
__exportStar(__webpack_require__(158), exports);
const REQUIRED_VSCODE_VERSION = '^1.67.0'; // do not change format, updated by `updateVSCode` script
var TransportKind;
(function (TransportKind) {
    TransportKind[TransportKind["stdio"] = 0] = "stdio";
    TransportKind[TransportKind["ipc"] = 1] = "ipc";
    TransportKind[TransportKind["pipe"] = 2] = "pipe";
    TransportKind[TransportKind["socket"] = 3] = "socket";
})(TransportKind = exports.TransportKind || (exports.TransportKind = {}));
var Transport;
(function (Transport) {
    function isSocket(value) {
        const candidate = value;
        return candidate && candidate.kind === TransportKind.socket && Is.number(candidate.port);
    }
    Transport.isSocket = isSocket;
})(Transport || (Transport = {}));
var Executable;
(function (Executable) {
    function is(value) {
        return Is.string(value.command);
    }
    Executable.is = is;
})(Executable || (Executable = {}));
var NodeModule;
(function (NodeModule) {
    function is(value) {
        return Is.string(value.module);
    }
    NodeModule.is = is;
})(NodeModule || (NodeModule = {}));
var StreamInfo;
(function (StreamInfo) {
    function is(value) {
        let candidate = value;
        return candidate && candidate.writer !== undefined && candidate.reader !== undefined;
    }
    StreamInfo.is = is;
})(StreamInfo || (StreamInfo = {}));
var ChildProcessInfo;
(function (ChildProcessInfo) {
    function is(value) {
        let candidate = value;
        return candidate && candidate.process !== undefined && typeof candidate.detached === 'boolean';
    }
    ChildProcessInfo.is = is;
})(ChildProcessInfo || (ChildProcessInfo = {}));
class LanguageClient extends client_1.BaseLanguageClient {
    constructor(arg1, arg2, arg3, arg4, arg5) {
        let id;
        let name;
        let serverOptions;
        let clientOptions;
        let forceDebug;
        if (Is.string(arg2)) {
            id = arg1;
            name = arg2;
            serverOptions = arg3;
            clientOptions = arg4;
            forceDebug = !!arg5;
        }
        else {
            id = arg1.toLowerCase();
            name = arg1;
            serverOptions = arg2;
            clientOptions = arg3;
            forceDebug = arg4;
        }
        if (forceDebug === undefined) {
            forceDebug = false;
        }
        super(id, name, clientOptions);
        this._serverOptions = serverOptions;
        this._forceDebug = forceDebug;
        this._isInDebugMode = forceDebug;
        try {
            this.checkVersion();
        }
        catch (error) {
            if (Is.string(error.message)) {
                this.outputChannel.appendLine(error.message);
            }
            throw error;
        }
    }
    checkVersion() {
        const codeVersion = SemVer.parse(vscode_1.version);
        if (!codeVersion) {
            throw new Error(`No valid VS Code version detected. Version string is: ${vscode_1.version}`);
        }
        // Remove the insider pre-release since we stay API compatible.
        if (codeVersion.prerelease && codeVersion.prerelease.length > 0) {
            codeVersion.prerelease = [];
        }
        if (!SemVer.satisfies(codeVersion, REQUIRED_VSCODE_VERSION)) {
            throw new Error(`The language client requires VS Code version ${REQUIRED_VSCODE_VERSION} but received version ${vscode_1.version}`);
        }
    }
    get isInDebugMode() {
        return this._isInDebugMode;
    }
    async restart() {
        await this.stop();
        // We are in debug mode. Wait a little before we restart
        // so that the debug port can be freed. We can safely ignore
        // the disposable returned from start since it will call
        // stop on the same client instance.
        if (this.isInDebugMode) {
            await new Promise((resolve) => setTimeout(resolve, 1000));
            await this.start();
        }
        else {
            await this.start();
        }
    }
    stop(timeout = 2000) {
        return super.stop(timeout).finally(() => {
            if (this._serverProcess) {
                const toCheck = this._serverProcess;
                this._serverProcess = undefined;
                if (this._isDetached === undefined || !this._isDetached) {
                    this.checkProcessDied(toCheck);
                }
                this._isDetached = undefined;
            }
        });
    }
    checkProcessDied(childProcess) {
        if (!childProcess || childProcess.pid === undefined) {
            return;
        }
        setTimeout(() => {
            // Test if the process is still alive. Throws an exception if not
            try {
                if (childProcess.pid !== undefined) {
                    process.kill(childProcess.pid, 0);
                    (0, processes_1.terminate)(childProcess);
                }
            }
            catch (error) {
                // All is fine.
            }
        }, 2000);
    }
    handleConnectionClosed() {
        this._serverProcess = undefined;
        super.handleConnectionClosed();
    }
    fillInitializeParams(params) {
        super.fillInitializeParams(params);
        if (params.processId === null) {
            params.processId = process.pid;
        }
    }
    createMessageTransports(encoding) {
        function getEnvironment(env, fork) {
            if (!env && !fork) {
                return undefined;
            }
            const result = Object.create(null);
            Object.keys(process.env).forEach(key => result[key] = process.env[key]);
            if (fork) {
                result['ELECTRON_RUN_AS_NODE'] = '1';
                result['ELECTRON_NO_ASAR'] = '1';
            }
            if (env) {
                Object.keys(env).forEach(key => result[key] = env[key]);
            }
            return result;
        }
        const debugStartWith = ['--debug=', '--debug-brk=', '--inspect=', '--inspect-brk='];
        const debugEquals = ['--debug', '--debug-brk', '--inspect', '--inspect-brk'];
        function startedInDebugMode() {
            let args = process.execArgv;
            if (args) {
                return args.some((arg) => {
                    return debugStartWith.some(value => arg.startsWith(value)) ||
                        debugEquals.some(value => arg === value);
                });
            }
            return false;
        }
        function assertStdio(process) {
            if (process.stdin === null || process.stdout === null || process.stderr === null) {
                throw new Error('Process created without stdio streams');
            }
        }
        const server = this._serverOptions;
        // We got a function.
        if (Is.func(server)) {
            return server().then((result) => {
                if (client_1.MessageTransports.is(result)) {
                    this._isDetached = !!result.detached;
                    return result;
                }
                else if (StreamInfo.is(result)) {
                    this._isDetached = !!result.detached;
                    return { reader: new node_1.StreamMessageReader(result.reader), writer: new node_1.StreamMessageWriter(result.writer) };
                }
                else {
                    let cp;
                    if (ChildProcessInfo.is(result)) {
                        cp = result.process;
                        this._isDetached = result.detached;
                    }
                    else {
                        cp = result;
                        this._isDetached = false;
                    }
                    cp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                    return { reader: new node_1.StreamMessageReader(cp.stdout), writer: new node_1.StreamMessageWriter(cp.stdin) };
                }
            });
        }
        let json;
        let runDebug = server;
        if (runDebug.run || runDebug.debug) {
            if (this._forceDebug || startedInDebugMode()) {
                json = runDebug.debug;
                this._isInDebugMode = true;
            }
            else {
                json = runDebug.run;
                this._isInDebugMode = false;
            }
        }
        else {
            json = server;
        }
        return this._getServerWorkingDir(json.options).then(serverWorkingDir => {
            if (NodeModule.is(json) && json.module) {
                let node = json;
                let transport = node.transport || TransportKind.stdio;
                if (node.runtime) {
                    const args = [];
                    const options = node.options ?? Object.create(null);
                    if (options.execArgv) {
                        options.execArgv.forEach(element => args.push(element));
                    }
                    args.push(node.module);
                    if (node.args) {
                        node.args.forEach(element => args.push(element));
                    }
                    const execOptions = Object.create(null);
                    execOptions.cwd = serverWorkingDir;
                    execOptions.env = getEnvironment(options.env, false);
                    const runtime = this._getRuntimePath(node.runtime, serverWorkingDir);
                    let pipeName = undefined;
                    if (transport === TransportKind.ipc) {
                        // exec options not correctly typed in lib
                        execOptions.stdio = [null, null, null, 'ipc'];
                        args.push('--node-ipc');
                    }
                    else if (transport === TransportKind.stdio) {
                        args.push('--stdio');
                    }
                    else if (transport === TransportKind.pipe) {
                        pipeName = (0, node_1.generateRandomPipeName)();
                        args.push(`--pipe=${pipeName}`);
                    }
                    else if (Transport.isSocket(transport)) {
                        args.push(`--socket=${transport.port}`);
                    }
                    args.push(`--clientProcessId=${process.pid.toString()}`);
                    if (transport === TransportKind.ipc || transport === TransportKind.stdio) {
                        const serverProcess = cp.spawn(runtime, args, execOptions);
                        if (!serverProcess || !serverProcess.pid) {
                            return handleChildProcessStartError(serverProcess, `Launching server using runtime ${runtime} failed.`);
                        }
                        this._serverProcess = serverProcess;
                        serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        if (transport === TransportKind.ipc) {
                            serverProcess.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return Promise.resolve({ reader: new node_1.IPCMessageReader(serverProcess), writer: new node_1.IPCMessageWriter(serverProcess) });
                        }
                        else {
                            return Promise.resolve({ reader: new node_1.StreamMessageReader(serverProcess.stdout), writer: new node_1.StreamMessageWriter(serverProcess.stdin) });
                        }
                    }
                    else if (transport === TransportKind.pipe) {
                        return (0, node_1.createClientPipeTransport)(pipeName).then((transport) => {
                            const process = cp.spawn(runtime, args, execOptions);
                            if (!process || !process.pid) {
                                return handleChildProcessStartError(process, `Launching server using runtime ${runtime} failed.`);
                            }
                            this._serverProcess = process;
                            process.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            process.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return transport.onConnected().then((protocol) => {
                                return { reader: protocol[0], writer: protocol[1] };
                            });
                        });
                    }
                    else if (Transport.isSocket(transport)) {
                        return (0, node_1.createClientSocketTransport)(transport.port).then((transport) => {
                            const process = cp.spawn(runtime, args, execOptions);
                            if (!process || !process.pid) {
                                return handleChildProcessStartError(process, `Launching server using runtime ${runtime} failed.`);
                            }
                            this._serverProcess = process;
                            process.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            process.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            return transport.onConnected().then((protocol) => {
                                return { reader: protocol[0], writer: protocol[1] };
                            });
                        });
                    }
                }
                else {
                    let pipeName = undefined;
                    return new Promise((resolve, reject) => {
                        const args = (node.args && node.args.slice()) ?? [];
                        if (transport === TransportKind.ipc) {
                            args.push('--node-ipc');
                        }
                        else if (transport === TransportKind.stdio) {
                            args.push('--stdio');
                        }
                        else if (transport === TransportKind.pipe) {
                            pipeName = (0, node_1.generateRandomPipeName)();
                            args.push(`--pipe=${pipeName}`);
                        }
                        else if (Transport.isSocket(transport)) {
                            args.push(`--socket=${transport.port}`);
                        }
                        args.push(`--clientProcessId=${process.pid.toString()}`);
                        const options = node.options ?? Object.create(null);
                        options.env = getEnvironment(options.env, true);
                        options.execArgv = options.execArgv || [];
                        options.cwd = serverWorkingDir;
                        options.silent = true;
                        if (transport === TransportKind.ipc || transport === TransportKind.stdio) {
                            const sp = cp.fork(node.module, args || [], options);
                            assertStdio(sp);
                            this._serverProcess = sp;
                            sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                            if (transport === TransportKind.ipc) {
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                resolve({ reader: new node_1.IPCMessageReader(this._serverProcess), writer: new node_1.IPCMessageWriter(this._serverProcess) });
                            }
                            else {
                                resolve({ reader: new node_1.StreamMessageReader(sp.stdout), writer: new node_1.StreamMessageWriter(sp.stdin) });
                            }
                        }
                        else if (transport === TransportKind.pipe) {
                            (0, node_1.createClientPipeTransport)(pipeName).then((transport) => {
                                const sp = cp.fork(node.module, args || [], options);
                                assertStdio(sp);
                                this._serverProcess = sp;
                                sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                transport.onConnected().then((protocol) => {
                                    resolve({ reader: protocol[0], writer: protocol[1] });
                                }, reject);
                            }, reject);
                        }
                        else if (Transport.isSocket(transport)) {
                            (0, node_1.createClientSocketTransport)(transport.port).then((transport) => {
                                const sp = cp.fork(node.module, args || [], options);
                                assertStdio(sp);
                                this._serverProcess = sp;
                                sp.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                sp.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                                transport.onConnected().then((protocol) => {
                                    resolve({ reader: protocol[0], writer: protocol[1] });
                                }, reject);
                            }, reject);
                        }
                    });
                }
            }
            else if (Executable.is(json) && json.command) {
                const command = json;
                const args = json.args !== undefined ? json.args.slice(0) : [];
                let pipeName = undefined;
                const transport = json.transport;
                if (transport === TransportKind.stdio) {
                    args.push('--stdio');
                }
                else if (transport === TransportKind.pipe) {
                    pipeName = (0, node_1.generateRandomPipeName)();
                    args.push(`--pipe=${pipeName}`);
                }
                else if (Transport.isSocket(transport)) {
                    args.push(`--socket=${transport.port}`);
                }
                else if (transport === TransportKind.ipc) {
                    throw new Error(`Transport kind ipc is not support for command executable`);
                }
                const options = Object.assign({}, command.options);
                options.cwd = options.cwd || serverWorkingDir;
                if (transport === undefined || transport === TransportKind.stdio) {
                    const serverProcess = cp.spawn(command.command, args, options);
                    if (!serverProcess || !serverProcess.pid) {
                        return handleChildProcessStartError(serverProcess, `Launching server using command ${command.command} failed.`);
                    }
                    serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                    this._serverProcess = serverProcess;
                    this._isDetached = !!options.detached;
                    return Promise.resolve({ reader: new node_1.StreamMessageReader(serverProcess.stdout), writer: new node_1.StreamMessageWriter(serverProcess.stdin) });
                }
                else if (transport === TransportKind.pipe) {
                    return (0, node_1.createClientPipeTransport)(pipeName).then((transport) => {
                        const serverProcess = cp.spawn(command.command, args, options);
                        if (!serverProcess || !serverProcess.pid) {
                            return handleChildProcessStartError(serverProcess, `Launching server using command ${command.command} failed.`);
                        }
                        this._serverProcess = serverProcess;
                        this._isDetached = !!options.detached;
                        serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        serverProcess.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        return transport.onConnected().then((protocol) => {
                            return { reader: protocol[0], writer: protocol[1] };
                        });
                    });
                }
                else if (Transport.isSocket(transport)) {
                    return (0, node_1.createClientSocketTransport)(transport.port).then((transport) => {
                        const serverProcess = cp.spawn(command.command, args, options);
                        if (!serverProcess || !serverProcess.pid) {
                            return handleChildProcessStartError(serverProcess, `Launching server using command ${command.command} failed.`);
                        }
                        this._serverProcess = serverProcess;
                        this._isDetached = !!options.detached;
                        serverProcess.stderr.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        serverProcess.stdout.on('data', data => this.outputChannel.append(Is.string(data) ? data : data.toString(encoding)));
                        return transport.onConnected().then((protocol) => {
                            return { reader: protocol[0], writer: protocol[1] };
                        });
                    });
                }
            }
            return Promise.reject(new Error(`Unsupported server configuration ` + JSON.stringify(server, null, 4)));
        });
    }
    _getRuntimePath(runtime, serverWorkingDirectory) {
        if (path.isAbsolute(runtime)) {
            return runtime;
        }
        const mainRootPath = this._mainGetRootPath();
        if (mainRootPath !== undefined) {
            const result = path.join(mainRootPath, runtime);
            if (fs.existsSync(result)) {
                return result;
            }
        }
        if (serverWorkingDirectory !== undefined) {
            const result = path.join(serverWorkingDirectory, runtime);
            if (fs.existsSync(result)) {
                return result;
            }
        }
        return runtime;
    }
    _mainGetRootPath() {
        let folders = vscode_1.workspace.workspaceFolders;
        if (!folders || folders.length === 0) {
            return undefined;
        }
        let folder = folders[0];
        if (folder.uri.scheme === 'file') {
            return folder.uri.fsPath;
        }
        return undefined;
    }
    _getServerWorkingDir(options) {
        let cwd = options && options.cwd;
        if (!cwd) {
            cwd = this.clientOptions.workspaceFolder
                ? this.clientOptions.workspaceFolder.uri.fsPath
                : this._mainGetRootPath();
        }
        if (cwd) {
            // make sure the folder exists otherwise creating the process will fail
            return new Promise(s => {
                fs.lstat(cwd, (err, stats) => {
                    s(!err && stats.isDirectory() ? cwd : undefined);
                });
            });
        }
        return Promise.resolve(undefined);
    }
    getLocale() {
        const envValue = process.env['VSCODE_NLS_CONFIG'];
        if (envValue === undefined) {
            return 'en';
        }
        let config = undefined;
        try {
            config = JSON.parse(envValue);
        }
        catch (err) {
        }
        if (config === undefined || typeof config.locale !== 'string') {
            return 'en';
        }
        return config.locale;
    }
}
exports.LanguageClient = LanguageClient;
class SettingMonitor {
    constructor(_client, _setting) {
        this._client = _client;
        this._setting = _setting;
        this._listeners = [];
    }
    start() {
        vscode_1.workspace.onDidChangeConfiguration(this.onDidChangeConfiguration, this, this._listeners);
        this.onDidChangeConfiguration();
        return new vscode_1.Disposable(() => {
            if (this._client.needsStop()) {
                void this._client.stop();
            }
        });
    }
    onDidChangeConfiguration() {
        let index = this._setting.indexOf('.');
        let primary = index >= 0 ? this._setting.substr(0, index) : this._setting;
        let rest = index >= 0 ? this._setting.substr(index + 1) : undefined;
        let enabled = rest ? vscode_1.workspace.getConfiguration(primary).get(rest, false) : vscode_1.workspace.getConfiguration(primary);
        if (enabled && this._client.needsStart()) {
            this._client.start().catch((error) => this._client.error('Start failed after configuration change', error, 'force'));
        }
        else if (!enabled && this._client.needsStop()) {
            void this._client.stop().catch((error) => this._client.error('Stop failed after configuration change', error, 'force'));
        }
    }
}
exports.SettingMonitor = SettingMonitor;
function handleChildProcessStartError(process, message) {
    if (process === null) {
        return Promise.reject(message);
    }
    return new Promise((_, reject) => {
        process.on('error', (err) => {
            reject(`${message} ${err}`);
        });
        // the error event should always be run immediately,
        // but race on it just in case
        setImmediate(() => reject(message));
    });
}
//# sourceMappingURL=main.js.map

/***/ }),
/* 4 */
/***/ ((module) => {

"use strict";
module.exports = require("child_process");

/***/ }),
/* 5 */
/***/ ((module) => {

"use strict";
module.exports = require("fs");

/***/ }),
/* 6 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// just pre-load all the stuff that index.js lazily exports
const internalRe = __webpack_require__(7)
module.exports = {
  re: internalRe.re,
  src: internalRe.src,
  tokens: internalRe.t,
  SEMVER_SPEC_VERSION: (__webpack_require__(8).SEMVER_SPEC_VERSION),
  SemVer: __webpack_require__(10),
  compareIdentifiers: (__webpack_require__(12).compareIdentifiers),
  rcompareIdentifiers: (__webpack_require__(12).rcompareIdentifiers),
  parse: __webpack_require__(13),
  valid: __webpack_require__(14),
  clean: __webpack_require__(15),
  inc: __webpack_require__(16),
  diff: __webpack_require__(17),
  major: __webpack_require__(20),
  minor: __webpack_require__(21),
  patch: __webpack_require__(22),
  prerelease: __webpack_require__(23),
  compare: __webpack_require__(19),
  rcompare: __webpack_require__(24),
  compareLoose: __webpack_require__(25),
  compareBuild: __webpack_require__(26),
  sort: __webpack_require__(27),
  rsort: __webpack_require__(28),
  gt: __webpack_require__(29),
  lt: __webpack_require__(30),
  eq: __webpack_require__(18),
  neq: __webpack_require__(31),
  gte: __webpack_require__(32),
  lte: __webpack_require__(33),
  cmp: __webpack_require__(34),
  coerce: __webpack_require__(35),
  Comparator: __webpack_require__(36),
  Range: __webpack_require__(37),
  satisfies: __webpack_require__(41),
  toComparators: __webpack_require__(42),
  maxSatisfying: __webpack_require__(43),
  minSatisfying: __webpack_require__(44),
  minVersion: __webpack_require__(45),
  validRange: __webpack_require__(46),
  outside: __webpack_require__(47),
  gtr: __webpack_require__(48),
  ltr: __webpack_require__(49),
  intersects: __webpack_require__(50),
  simplifyRange: __webpack_require__(51),
  subset: __webpack_require__(52),
}


/***/ }),
/* 7 */
/***/ ((module, exports, __webpack_require__) => {

const { MAX_SAFE_COMPONENT_LENGTH } = __webpack_require__(8)
const debug = __webpack_require__(9)
exports = module.exports = {}

// The actual regexps go on exports.re
const re = exports.re = []
const src = exports.src = []
const t = exports.t = {}
let R = 0

const createToken = (name, value, isGlobal) => {
  const index = R++
  debug(name, index, value)
  t[name] = index
  src[index] = value
  re[index] = new RegExp(value, isGlobal ? 'g' : undefined)
}

// The following Regular Expressions can be used for tokenizing,
// validating, and parsing SemVer version strings.

// ## Numeric Identifier
// A single `0`, or a non-zero digit followed by zero or more digits.

createToken('NUMERICIDENTIFIER', '0|[1-9]\\d*')
createToken('NUMERICIDENTIFIERLOOSE', '[0-9]+')

// ## Non-numeric Identifier
// Zero or more digits, followed by a letter or hyphen, and then zero or
// more letters, digits, or hyphens.

createToken('NONNUMERICIDENTIFIER', '\\d*[a-zA-Z-][a-zA-Z0-9-]*')

// ## Main Version
// Three dot-separated numeric identifiers.

createToken('MAINVERSION', `(${src[t.NUMERICIDENTIFIER]})\\.` +
                   `(${src[t.NUMERICIDENTIFIER]})\\.` +
                   `(${src[t.NUMERICIDENTIFIER]})`)

createToken('MAINVERSIONLOOSE', `(${src[t.NUMERICIDENTIFIERLOOSE]})\\.` +
                        `(${src[t.NUMERICIDENTIFIERLOOSE]})\\.` +
                        `(${src[t.NUMERICIDENTIFIERLOOSE]})`)

// ## Pre-release Version Identifier
// A numeric identifier, or a non-numeric identifier.

createToken('PRERELEASEIDENTIFIER', `(?:${src[t.NUMERICIDENTIFIER]
}|${src[t.NONNUMERICIDENTIFIER]})`)

createToken('PRERELEASEIDENTIFIERLOOSE', `(?:${src[t.NUMERICIDENTIFIERLOOSE]
}|${src[t.NONNUMERICIDENTIFIER]})`)

// ## Pre-release Version
// Hyphen, followed by one or more dot-separated pre-release version
// identifiers.

createToken('PRERELEASE', `(?:-(${src[t.PRERELEASEIDENTIFIER]
}(?:\\.${src[t.PRERELEASEIDENTIFIER]})*))`)

createToken('PRERELEASELOOSE', `(?:-?(${src[t.PRERELEASEIDENTIFIERLOOSE]
}(?:\\.${src[t.PRERELEASEIDENTIFIERLOOSE]})*))`)

// ## Build Metadata Identifier
// Any combination of digits, letters, or hyphens.

createToken('BUILDIDENTIFIER', '[0-9A-Za-z-]+')

// ## Build Metadata
// Plus sign, followed by one or more period-separated build metadata
// identifiers.

createToken('BUILD', `(?:\\+(${src[t.BUILDIDENTIFIER]
}(?:\\.${src[t.BUILDIDENTIFIER]})*))`)

// ## Full Version String
// A main version, followed optionally by a pre-release version and
// build metadata.

// Note that the only major, minor, patch, and pre-release sections of
// the version string are capturing groups.  The build metadata is not a
// capturing group, because it should not ever be used in version
// comparison.

createToken('FULLPLAIN', `v?${src[t.MAINVERSION]
}${src[t.PRERELEASE]}?${
  src[t.BUILD]}?`)

createToken('FULL', `^${src[t.FULLPLAIN]}$`)

// like full, but allows v1.2.3 and =1.2.3, which people do sometimes.
// also, 1.0.0alpha1 (prerelease without the hyphen) which is pretty
// common in the npm registry.
createToken('LOOSEPLAIN', `[v=\\s]*${src[t.MAINVERSIONLOOSE]
}${src[t.PRERELEASELOOSE]}?${
  src[t.BUILD]}?`)

createToken('LOOSE', `^${src[t.LOOSEPLAIN]}$`)

createToken('GTLT', '((?:<|>)?=?)')

// Something like "2.*" or "1.2.x".
// Note that "x.x" is a valid xRange identifer, meaning "any version"
// Only the first item is strictly required.
createToken('XRANGEIDENTIFIERLOOSE', `${src[t.NUMERICIDENTIFIERLOOSE]}|x|X|\\*`)
createToken('XRANGEIDENTIFIER', `${src[t.NUMERICIDENTIFIER]}|x|X|\\*`)

createToken('XRANGEPLAIN', `[v=\\s]*(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:\\.(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:\\.(${src[t.XRANGEIDENTIFIER]})` +
                   `(?:${src[t.PRERELEASE]})?${
                     src[t.BUILD]}?` +
                   `)?)?`)

createToken('XRANGEPLAINLOOSE', `[v=\\s]*(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:\\.(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:\\.(${src[t.XRANGEIDENTIFIERLOOSE]})` +
                        `(?:${src[t.PRERELEASELOOSE]})?${
                          src[t.BUILD]}?` +
                        `)?)?`)

createToken('XRANGE', `^${src[t.GTLT]}\\s*${src[t.XRANGEPLAIN]}$`)
createToken('XRANGELOOSE', `^${src[t.GTLT]}\\s*${src[t.XRANGEPLAINLOOSE]}$`)

// Coercion.
// Extract anything that could conceivably be a part of a valid semver
createToken('COERCE', `${'(^|[^\\d])' +
              '(\\d{1,'}${MAX_SAFE_COMPONENT_LENGTH}})` +
              `(?:\\.(\\d{1,${MAX_SAFE_COMPONENT_LENGTH}}))?` +
              `(?:\\.(\\d{1,${MAX_SAFE_COMPONENT_LENGTH}}))?` +
              `(?:$|[^\\d])`)
createToken('COERCERTL', src[t.COERCE], true)

// Tilde ranges.
// Meaning is "reasonably at or greater than"
createToken('LONETILDE', '(?:~>?)')

createToken('TILDETRIM', `(\\s*)${src[t.LONETILDE]}\\s+`, true)
exports.tildeTrimReplace = '$1~'

createToken('TILDE', `^${src[t.LONETILDE]}${src[t.XRANGEPLAIN]}$`)
createToken('TILDELOOSE', `^${src[t.LONETILDE]}${src[t.XRANGEPLAINLOOSE]}$`)

// Caret ranges.
// Meaning is "at least and backwards compatible with"
createToken('LONECARET', '(?:\\^)')

createToken('CARETTRIM', `(\\s*)${src[t.LONECARET]}\\s+`, true)
exports.caretTrimReplace = '$1^'

createToken('CARET', `^${src[t.LONECARET]}${src[t.XRANGEPLAIN]}$`)
createToken('CARETLOOSE', `^${src[t.LONECARET]}${src[t.XRANGEPLAINLOOSE]}$`)

// A simple gt/lt/eq thing, or just "" to indicate "any version"
createToken('COMPARATORLOOSE', `^${src[t.GTLT]}\\s*(${src[t.LOOSEPLAIN]})$|^$`)
createToken('COMPARATOR', `^${src[t.GTLT]}\\s*(${src[t.FULLPLAIN]})$|^$`)

// An expression to strip any whitespace between the gtlt and the thing
// it modifies, so that `> 1.2.3` ==> `>1.2.3`
createToken('COMPARATORTRIM', `(\\s*)${src[t.GTLT]
}\\s*(${src[t.LOOSEPLAIN]}|${src[t.XRANGEPLAIN]})`, true)
exports.comparatorTrimReplace = '$1$2$3'

// Something like `1.2.3 - 1.2.4`
// Note that these all use the loose form, because they'll be
// checked against either the strict or loose comparator form
// later.
createToken('HYPHENRANGE', `^\\s*(${src[t.XRANGEPLAIN]})` +
                   `\\s+-\\s+` +
                   `(${src[t.XRANGEPLAIN]})` +
                   `\\s*$`)

createToken('HYPHENRANGELOOSE', `^\\s*(${src[t.XRANGEPLAINLOOSE]})` +
                        `\\s+-\\s+` +
                        `(${src[t.XRANGEPLAINLOOSE]})` +
                        `\\s*$`)

// Star ranges basically just allow anything at all.
createToken('STAR', '(<|>)?=?\\s*\\*')
// >=0.0.0 is like a star
createToken('GTE0', '^\\s*>=\\s*0\\.0\\.0\\s*$')
createToken('GTE0PRE', '^\\s*>=\\s*0\\.0\\.0-0\\s*$')


/***/ }),
/* 8 */
/***/ ((module) => {

// Note: this is the semver.org version of the spec that it implements
// Not necessarily the package version of this code.
const SEMVER_SPEC_VERSION = '2.0.0'

const MAX_LENGTH = 256
const MAX_SAFE_INTEGER = Number.MAX_SAFE_INTEGER ||
/* istanbul ignore next */ 9007199254740991

// Max safe segment length for coercion.
const MAX_SAFE_COMPONENT_LENGTH = 16

module.exports = {
  SEMVER_SPEC_VERSION,
  MAX_LENGTH,
  MAX_SAFE_INTEGER,
  MAX_SAFE_COMPONENT_LENGTH,
}


/***/ }),
/* 9 */
/***/ ((module) => {

const debug = (
  typeof process === 'object' &&
  process.env &&
  process.env.NODE_DEBUG &&
  /\bsemver\b/i.test(process.env.NODE_DEBUG)
) ? (...args) => console.error('SEMVER', ...args)
  : () => {}

module.exports = debug


/***/ }),
/* 10 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const debug = __webpack_require__(9)
const { MAX_LENGTH, MAX_SAFE_INTEGER } = __webpack_require__(8)
const { re, t } = __webpack_require__(7)

const parseOptions = __webpack_require__(11)
const { compareIdentifiers } = __webpack_require__(12)
class SemVer {
  constructor (version, options) {
    options = parseOptions(options)

    if (version instanceof SemVer) {
      if (version.loose === !!options.loose &&
          version.includePrerelease === !!options.includePrerelease) {
        return version
      } else {
        version = version.version
      }
    } else if (typeof version !== 'string') {
      throw new TypeError(`Invalid Version: ${version}`)
    }

    if (version.length > MAX_LENGTH) {
      throw new TypeError(
        `version is longer than ${MAX_LENGTH} characters`
      )
    }

    debug('SemVer', version, options)
    this.options = options
    this.loose = !!options.loose
    // this isn't actually relevant for versions, but keep it so that we
    // don't run into trouble passing this.options around.
    this.includePrerelease = !!options.includePrerelease

    const m = version.trim().match(options.loose ? re[t.LOOSE] : re[t.FULL])

    if (!m) {
      throw new TypeError(`Invalid Version: ${version}`)
    }

    this.raw = version

    // these are actually numbers
    this.major = +m[1]
    this.minor = +m[2]
    this.patch = +m[3]

    if (this.major > MAX_SAFE_INTEGER || this.major < 0) {
      throw new TypeError('Invalid major version')
    }

    if (this.minor > MAX_SAFE_INTEGER || this.minor < 0) {
      throw new TypeError('Invalid minor version')
    }

    if (this.patch > MAX_SAFE_INTEGER || this.patch < 0) {
      throw new TypeError('Invalid patch version')
    }

    // numberify any prerelease numeric ids
    if (!m[4]) {
      this.prerelease = []
    } else {
      this.prerelease = m[4].split('.').map((id) => {
        if (/^[0-9]+$/.test(id)) {
          const num = +id
          if (num >= 0 && num < MAX_SAFE_INTEGER) {
            return num
          }
        }
        return id
      })
    }

    this.build = m[5] ? m[5].split('.') : []
    this.format()
  }

  format () {
    this.version = `${this.major}.${this.minor}.${this.patch}`
    if (this.prerelease.length) {
      this.version += `-${this.prerelease.join('.')}`
    }
    return this.version
  }

  toString () {
    return this.version
  }

  compare (other) {
    debug('SemVer.compare', this.version, this.options, other)
    if (!(other instanceof SemVer)) {
      if (typeof other === 'string' && other === this.version) {
        return 0
      }
      other = new SemVer(other, this.options)
    }

    if (other.version === this.version) {
      return 0
    }

    return this.compareMain(other) || this.comparePre(other)
  }

  compareMain (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    return (
      compareIdentifiers(this.major, other.major) ||
      compareIdentifiers(this.minor, other.minor) ||
      compareIdentifiers(this.patch, other.patch)
    )
  }

  comparePre (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    // NOT having a prerelease is > having one
    if (this.prerelease.length && !other.prerelease.length) {
      return -1
    } else if (!this.prerelease.length && other.prerelease.length) {
      return 1
    } else if (!this.prerelease.length && !other.prerelease.length) {
      return 0
    }

    let i = 0
    do {
      const a = this.prerelease[i]
      const b = other.prerelease[i]
      debug('prerelease compare', i, a, b)
      if (a === undefined && b === undefined) {
        return 0
      } else if (b === undefined) {
        return 1
      } else if (a === undefined) {
        return -1
      } else if (a === b) {
        continue
      } else {
        return compareIdentifiers(a, b)
      }
    } while (++i)
  }

  compareBuild (other) {
    if (!(other instanceof SemVer)) {
      other = new SemVer(other, this.options)
    }

    let i = 0
    do {
      const a = this.build[i]
      const b = other.build[i]
      debug('prerelease compare', i, a, b)
      if (a === undefined && b === undefined) {
        return 0
      } else if (b === undefined) {
        return 1
      } else if (a === undefined) {
        return -1
      } else if (a === b) {
        continue
      } else {
        return compareIdentifiers(a, b)
      }
    } while (++i)
  }

  // preminor will bump the version up to the next minor release, and immediately
  // down to pre-release. premajor and prepatch work the same way.
  inc (release, identifier) {
    switch (release) {
      case 'premajor':
        this.prerelease.length = 0
        this.patch = 0
        this.minor = 0
        this.major++
        this.inc('pre', identifier)
        break
      case 'preminor':
        this.prerelease.length = 0
        this.patch = 0
        this.minor++
        this.inc('pre', identifier)
        break
      case 'prepatch':
        // If this is already a prerelease, it will bump to the next version
        // drop any prereleases that might already exist, since they are not
        // relevant at this point.
        this.prerelease.length = 0
        this.inc('patch', identifier)
        this.inc('pre', identifier)
        break
      // If the input is a non-prerelease version, this acts the same as
      // prepatch.
      case 'prerelease':
        if (this.prerelease.length === 0) {
          this.inc('patch', identifier)
        }
        this.inc('pre', identifier)
        break

      case 'major':
        // If this is a pre-major version, bump up to the same major version.
        // Otherwise increment major.
        // 1.0.0-5 bumps to 1.0.0
        // 1.1.0 bumps to 2.0.0
        if (
          this.minor !== 0 ||
          this.patch !== 0 ||
          this.prerelease.length === 0
        ) {
          this.major++
        }
        this.minor = 0
        this.patch = 0
        this.prerelease = []
        break
      case 'minor':
        // If this is a pre-minor version, bump up to the same minor version.
        // Otherwise increment minor.
        // 1.2.0-5 bumps to 1.2.0
        // 1.2.1 bumps to 1.3.0
        if (this.patch !== 0 || this.prerelease.length === 0) {
          this.minor++
        }
        this.patch = 0
        this.prerelease = []
        break
      case 'patch':
        // If this is not a pre-release version, it will increment the patch.
        // If it is a pre-release it will bump up to the same patch version.
        // 1.2.0-5 patches to 1.2.0
        // 1.2.0 patches to 1.2.1
        if (this.prerelease.length === 0) {
          this.patch++
        }
        this.prerelease = []
        break
      // This probably shouldn't be used publicly.
      // 1.0.0 'pre' would become 1.0.0-0 which is the wrong direction.
      case 'pre':
        if (this.prerelease.length === 0) {
          this.prerelease = [0]
        } else {
          let i = this.prerelease.length
          while (--i >= 0) {
            if (typeof this.prerelease[i] === 'number') {
              this.prerelease[i]++
              i = -2
            }
          }
          if (i === -1) {
            // didn't increment anything
            this.prerelease.push(0)
          }
        }
        if (identifier) {
          // 1.2.0-beta.1 bumps to 1.2.0-beta.2,
          // 1.2.0-beta.fooblz or 1.2.0-beta bumps to 1.2.0-beta.0
          if (compareIdentifiers(this.prerelease[0], identifier) === 0) {
            if (isNaN(this.prerelease[1])) {
              this.prerelease = [identifier, 0]
            }
          } else {
            this.prerelease = [identifier, 0]
          }
        }
        break

      default:
        throw new Error(`invalid increment argument: ${release}`)
    }
    this.format()
    this.raw = this.version
    return this
  }
}

module.exports = SemVer


/***/ }),
/* 11 */
/***/ ((module) => {

// parse out just the options we care about so we always get a consistent
// obj with keys in a consistent order.
const opts = ['includePrerelease', 'loose', 'rtl']
const parseOptions = options =>
  !options ? {}
  : typeof options !== 'object' ? { loose: true }
  : opts.filter(k => options[k]).reduce((o, k) => {
    o[k] = true
    return o
  }, {})
module.exports = parseOptions


/***/ }),
/* 12 */
/***/ ((module) => {

const numeric = /^[0-9]+$/
const compareIdentifiers = (a, b) => {
  const anum = numeric.test(a)
  const bnum = numeric.test(b)

  if (anum && bnum) {
    a = +a
    b = +b
  }

  return a === b ? 0
    : (anum && !bnum) ? -1
    : (bnum && !anum) ? 1
    : a < b ? -1
    : 1
}

const rcompareIdentifiers = (a, b) => compareIdentifiers(b, a)

module.exports = {
  compareIdentifiers,
  rcompareIdentifiers,
}


/***/ }),
/* 13 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const { MAX_LENGTH } = __webpack_require__(8)
const { re, t } = __webpack_require__(7)
const SemVer = __webpack_require__(10)

const parseOptions = __webpack_require__(11)
const parse = (version, options) => {
  options = parseOptions(options)

  if (version instanceof SemVer) {
    return version
  }

  if (typeof version !== 'string') {
    return null
  }

  if (version.length > MAX_LENGTH) {
    return null
  }

  const r = options.loose ? re[t.LOOSE] : re[t.FULL]
  if (!r.test(version)) {
    return null
  }

  try {
    return new SemVer(version, options)
  } catch (er) {
    return null
  }
}

module.exports = parse


/***/ }),
/* 14 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(13)
const valid = (version, options) => {
  const v = parse(version, options)
  return v ? v.version : null
}
module.exports = valid


/***/ }),
/* 15 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(13)
const clean = (version, options) => {
  const s = parse(version.trim().replace(/^[=v]+/, ''), options)
  return s ? s.version : null
}
module.exports = clean


/***/ }),
/* 16 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)

const inc = (version, release, options, identifier) => {
  if (typeof (options) === 'string') {
    identifier = options
    options = undefined
  }

  try {
    return new SemVer(
      version instanceof SemVer ? version.version : version,
      options
    ).inc(release, identifier).version
  } catch (er) {
    return null
  }
}
module.exports = inc


/***/ }),
/* 17 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(13)
const eq = __webpack_require__(18)

const diff = (version1, version2) => {
  if (eq(version1, version2)) {
    return null
  } else {
    const v1 = parse(version1)
    const v2 = parse(version2)
    const hasPre = v1.prerelease.length || v2.prerelease.length
    const prefix = hasPre ? 'pre' : ''
    const defaultResult = hasPre ? 'prerelease' : ''
    for (const key in v1) {
      if (key === 'major' || key === 'minor' || key === 'patch') {
        if (v1[key] !== v2[key]) {
          return prefix + key
        }
      }
    }
    return defaultResult // may be undefined
  }
}
module.exports = diff


/***/ }),
/* 18 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const eq = (a, b, loose) => compare(a, b, loose) === 0
module.exports = eq


/***/ }),
/* 19 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const compare = (a, b, loose) =>
  new SemVer(a, loose).compare(new SemVer(b, loose))

module.exports = compare


/***/ }),
/* 20 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const major = (a, loose) => new SemVer(a, loose).major
module.exports = major


/***/ }),
/* 21 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const minor = (a, loose) => new SemVer(a, loose).minor
module.exports = minor


/***/ }),
/* 22 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const patch = (a, loose) => new SemVer(a, loose).patch
module.exports = patch


/***/ }),
/* 23 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const parse = __webpack_require__(13)
const prerelease = (version, options) => {
  const parsed = parse(version, options)
  return (parsed && parsed.prerelease.length) ? parsed.prerelease : null
}
module.exports = prerelease


/***/ }),
/* 24 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const rcompare = (a, b, loose) => compare(b, a, loose)
module.exports = rcompare


/***/ }),
/* 25 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const compareLoose = (a, b) => compare(a, b, true)
module.exports = compareLoose


/***/ }),
/* 26 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const compareBuild = (a, b, loose) => {
  const versionA = new SemVer(a, loose)
  const versionB = new SemVer(b, loose)
  return versionA.compare(versionB) || versionA.compareBuild(versionB)
}
module.exports = compareBuild


/***/ }),
/* 27 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compareBuild = __webpack_require__(26)
const sort = (list, loose) => list.sort((a, b) => compareBuild(a, b, loose))
module.exports = sort


/***/ }),
/* 28 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compareBuild = __webpack_require__(26)
const rsort = (list, loose) => list.sort((a, b) => compareBuild(b, a, loose))
module.exports = rsort


/***/ }),
/* 29 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const gt = (a, b, loose) => compare(a, b, loose) > 0
module.exports = gt


/***/ }),
/* 30 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const lt = (a, b, loose) => compare(a, b, loose) < 0
module.exports = lt


/***/ }),
/* 31 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const neq = (a, b, loose) => compare(a, b, loose) !== 0
module.exports = neq


/***/ }),
/* 32 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const gte = (a, b, loose) => compare(a, b, loose) >= 0
module.exports = gte


/***/ }),
/* 33 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const compare = __webpack_require__(19)
const lte = (a, b, loose) => compare(a, b, loose) <= 0
module.exports = lte


/***/ }),
/* 34 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const eq = __webpack_require__(18)
const neq = __webpack_require__(31)
const gt = __webpack_require__(29)
const gte = __webpack_require__(32)
const lt = __webpack_require__(30)
const lte = __webpack_require__(33)

const cmp = (a, op, b, loose) => {
  switch (op) {
    case '===':
      if (typeof a === 'object') {
        a = a.version
      }
      if (typeof b === 'object') {
        b = b.version
      }
      return a === b

    case '!==':
      if (typeof a === 'object') {
        a = a.version
      }
      if (typeof b === 'object') {
        b = b.version
      }
      return a !== b

    case '':
    case '=':
    case '==':
      return eq(a, b, loose)

    case '!=':
      return neq(a, b, loose)

    case '>':
      return gt(a, b, loose)

    case '>=':
      return gte(a, b, loose)

    case '<':
      return lt(a, b, loose)

    case '<=':
      return lte(a, b, loose)

    default:
      throw new TypeError(`Invalid operator: ${op}`)
  }
}
module.exports = cmp


/***/ }),
/* 35 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const parse = __webpack_require__(13)
const { re, t } = __webpack_require__(7)

const coerce = (version, options) => {
  if (version instanceof SemVer) {
    return version
  }

  if (typeof version === 'number') {
    version = String(version)
  }

  if (typeof version !== 'string') {
    return null
  }

  options = options || {}

  let match = null
  if (!options.rtl) {
    match = version.match(re[t.COERCE])
  } else {
    // Find the right-most coercible string that does not share
    // a terminus with a more left-ward coercible string.
    // Eg, '1.2.3.4' wants to coerce '2.3.4', not '3.4' or '4'
    //
    // Walk through the string checking with a /g regexp
    // Manually set the index so as to pick up overlapping matches.
    // Stop when we get a match that ends at the string end, since no
    // coercible string can be more right-ward without the same terminus.
    let next
    while ((next = re[t.COERCERTL].exec(version)) &&
        (!match || match.index + match[0].length !== version.length)
    ) {
      if (!match ||
            next.index + next[0].length !== match.index + match[0].length) {
        match = next
      }
      re[t.COERCERTL].lastIndex = next.index + next[1].length + next[2].length
    }
    // leave it in a clean state
    re[t.COERCERTL].lastIndex = -1
  }

  if (match === null) {
    return null
  }

  return parse(`${match[2]}.${match[3] || '0'}.${match[4] || '0'}`, options)
}
module.exports = coerce


/***/ }),
/* 36 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const ANY = Symbol('SemVer ANY')
// hoisted class for cyclic dependency
class Comparator {
  static get ANY () {
    return ANY
  }

  constructor (comp, options) {
    options = parseOptions(options)

    if (comp instanceof Comparator) {
      if (comp.loose === !!options.loose) {
        return comp
      } else {
        comp = comp.value
      }
    }

    debug('comparator', comp, options)
    this.options = options
    this.loose = !!options.loose
    this.parse(comp)

    if (this.semver === ANY) {
      this.value = ''
    } else {
      this.value = this.operator + this.semver.version
    }

    debug('comp', this)
  }

  parse (comp) {
    const r = this.options.loose ? re[t.COMPARATORLOOSE] : re[t.COMPARATOR]
    const m = comp.match(r)

    if (!m) {
      throw new TypeError(`Invalid comparator: ${comp}`)
    }

    this.operator = m[1] !== undefined ? m[1] : ''
    if (this.operator === '=') {
      this.operator = ''
    }

    // if it literally is just '>' or '' then allow anything.
    if (!m[2]) {
      this.semver = ANY
    } else {
      this.semver = new SemVer(m[2], this.options.loose)
    }
  }

  toString () {
    return this.value
  }

  test (version) {
    debug('Comparator.test', version, this.options.loose)

    if (this.semver === ANY || version === ANY) {
      return true
    }

    if (typeof version === 'string') {
      try {
        version = new SemVer(version, this.options)
      } catch (er) {
        return false
      }
    }

    return cmp(version, this.operator, this.semver, this.options)
  }

  intersects (comp, options) {
    if (!(comp instanceof Comparator)) {
      throw new TypeError('a Comparator is required')
    }

    if (!options || typeof options !== 'object') {
      options = {
        loose: !!options,
        includePrerelease: false,
      }
    }

    if (this.operator === '') {
      if (this.value === '') {
        return true
      }
      return new Range(comp.value, options).test(this.value)
    } else if (comp.operator === '') {
      if (comp.value === '') {
        return true
      }
      return new Range(this.value, options).test(comp.semver)
    }

    const sameDirectionIncreasing =
      (this.operator === '>=' || this.operator === '>') &&
      (comp.operator === '>=' || comp.operator === '>')
    const sameDirectionDecreasing =
      (this.operator === '<=' || this.operator === '<') &&
      (comp.operator === '<=' || comp.operator === '<')
    const sameSemVer = this.semver.version === comp.semver.version
    const differentDirectionsInclusive =
      (this.operator === '>=' || this.operator === '<=') &&
      (comp.operator === '>=' || comp.operator === '<=')
    const oppositeDirectionsLessThan =
      cmp(this.semver, '<', comp.semver, options) &&
      (this.operator === '>=' || this.operator === '>') &&
        (comp.operator === '<=' || comp.operator === '<')
    const oppositeDirectionsGreaterThan =
      cmp(this.semver, '>', comp.semver, options) &&
      (this.operator === '<=' || this.operator === '<') &&
        (comp.operator === '>=' || comp.operator === '>')

    return (
      sameDirectionIncreasing ||
      sameDirectionDecreasing ||
      (sameSemVer && differentDirectionsInclusive) ||
      oppositeDirectionsLessThan ||
      oppositeDirectionsGreaterThan
    )
  }
}

module.exports = Comparator

const parseOptions = __webpack_require__(11)
const { re, t } = __webpack_require__(7)
const cmp = __webpack_require__(34)
const debug = __webpack_require__(9)
const SemVer = __webpack_require__(10)
const Range = __webpack_require__(37)


/***/ }),
/* 37 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// hoisted class for cyclic dependency
class Range {
  constructor (range, options) {
    options = parseOptions(options)

    if (range instanceof Range) {
      if (
        range.loose === !!options.loose &&
        range.includePrerelease === !!options.includePrerelease
      ) {
        return range
      } else {
        return new Range(range.raw, options)
      }
    }

    if (range instanceof Comparator) {
      // just put it in the set and return
      this.raw = range.value
      this.set = [[range]]
      this.format()
      return this
    }

    this.options = options
    this.loose = !!options.loose
    this.includePrerelease = !!options.includePrerelease

    // First, split based on boolean or ||
    this.raw = range
    this.set = range
      .split('||')
      // map the range to a 2d array of comparators
      .map(r => this.parseRange(r.trim()))
      // throw out any comparator lists that are empty
      // this generally means that it was not a valid range, which is allowed
      // in loose mode, but will still throw if the WHOLE range is invalid.
      .filter(c => c.length)

    if (!this.set.length) {
      throw new TypeError(`Invalid SemVer Range: ${range}`)
    }

    // if we have any that are not the null set, throw out null sets.
    if (this.set.length > 1) {
      // keep the first one, in case they're all null sets
      const first = this.set[0]
      this.set = this.set.filter(c => !isNullSet(c[0]))
      if (this.set.length === 0) {
        this.set = [first]
      } else if (this.set.length > 1) {
        // if we have any that are *, then the range is just *
        for (const c of this.set) {
          if (c.length === 1 && isAny(c[0])) {
            this.set = [c]
            break
          }
        }
      }
    }

    this.format()
  }

  format () {
    this.range = this.set
      .map((comps) => {
        return comps.join(' ').trim()
      })
      .join('||')
      .trim()
    return this.range
  }

  toString () {
    return this.range
  }

  parseRange (range) {
    range = range.trim()

    // memoize range parsing for performance.
    // this is a very hot path, and fully deterministic.
    const memoOpts = Object.keys(this.options).join(',')
    const memoKey = `parseRange:${memoOpts}:${range}`
    const cached = cache.get(memoKey)
    if (cached) {
      return cached
    }

    const loose = this.options.loose
    // `1.2.3 - 1.2.4` => `>=1.2.3 <=1.2.4`
    const hr = loose ? re[t.HYPHENRANGELOOSE] : re[t.HYPHENRANGE]
    range = range.replace(hr, hyphenReplace(this.options.includePrerelease))
    debug('hyphen replace', range)
    // `> 1.2.3 < 1.2.5` => `>1.2.3 <1.2.5`
    range = range.replace(re[t.COMPARATORTRIM], comparatorTrimReplace)
    debug('comparator trim', range)

    // `~ 1.2.3` => `~1.2.3`
    range = range.replace(re[t.TILDETRIM], tildeTrimReplace)

    // `^ 1.2.3` => `^1.2.3`
    range = range.replace(re[t.CARETTRIM], caretTrimReplace)

    // normalize spaces
    range = range.split(/\s+/).join(' ')

    // At this point, the range is completely trimmed and
    // ready to be split into comparators.

    let rangeList = range
      .split(' ')
      .map(comp => parseComparator(comp, this.options))
      .join(' ')
      .split(/\s+/)
      // >=0.0.0 is equivalent to *
      .map(comp => replaceGTE0(comp, this.options))

    if (loose) {
      // in loose mode, throw out any that are not valid comparators
      rangeList = rangeList.filter(comp => {
        debug('loose invalid filter', comp, this.options)
        return !!comp.match(re[t.COMPARATORLOOSE])
      })
    }
    debug('range list', rangeList)

    // if any comparators are the null set, then replace with JUST null set
    // if more than one comparator, remove any * comparators
    // also, don't include the same comparator more than once
    const rangeMap = new Map()
    const comparators = rangeList.map(comp => new Comparator(comp, this.options))
    for (const comp of comparators) {
      if (isNullSet(comp)) {
        return [comp]
      }
      rangeMap.set(comp.value, comp)
    }
    if (rangeMap.size > 1 && rangeMap.has('')) {
      rangeMap.delete('')
    }

    const result = [...rangeMap.values()]
    cache.set(memoKey, result)
    return result
  }

  intersects (range, options) {
    if (!(range instanceof Range)) {
      throw new TypeError('a Range is required')
    }

    return this.set.some((thisComparators) => {
      return (
        isSatisfiable(thisComparators, options) &&
        range.set.some((rangeComparators) => {
          return (
            isSatisfiable(rangeComparators, options) &&
            thisComparators.every((thisComparator) => {
              return rangeComparators.every((rangeComparator) => {
                return thisComparator.intersects(rangeComparator, options)
              })
            })
          )
        })
      )
    })
  }

  // if ANY of the sets match ALL of its comparators, then pass
  test (version) {
    if (!version) {
      return false
    }

    if (typeof version === 'string') {
      try {
        version = new SemVer(version, this.options)
      } catch (er) {
        return false
      }
    }

    for (let i = 0; i < this.set.length; i++) {
      if (testSet(this.set[i], version, this.options)) {
        return true
      }
    }
    return false
  }
}
module.exports = Range

const LRU = __webpack_require__(38)
const cache = new LRU({ max: 1000 })

const parseOptions = __webpack_require__(11)
const Comparator = __webpack_require__(36)
const debug = __webpack_require__(9)
const SemVer = __webpack_require__(10)
const {
  re,
  t,
  comparatorTrimReplace,
  tildeTrimReplace,
  caretTrimReplace,
} = __webpack_require__(7)

const isNullSet = c => c.value === '<0.0.0-0'
const isAny = c => c.value === ''

// take a set of comparators and determine whether there
// exists a version which can satisfy it
const isSatisfiable = (comparators, options) => {
  let result = true
  const remainingComparators = comparators.slice()
  let testComparator = remainingComparators.pop()

  while (result && remainingComparators.length) {
    result = remainingComparators.every((otherComparator) => {
      return testComparator.intersects(otherComparator, options)
    })

    testComparator = remainingComparators.pop()
  }

  return result
}

// comprised of xranges, tildes, stars, and gtlt's at this point.
// already replaced the hyphen ranges
// turn into a set of JUST comparators.
const parseComparator = (comp, options) => {
  debug('comp', comp, options)
  comp = replaceCarets(comp, options)
  debug('caret', comp)
  comp = replaceTildes(comp, options)
  debug('tildes', comp)
  comp = replaceXRanges(comp, options)
  debug('xrange', comp)
  comp = replaceStars(comp, options)
  debug('stars', comp)
  return comp
}

const isX = id => !id || id.toLowerCase() === 'x' || id === '*'

// ~, ~> --> * (any, kinda silly)
// ~2, ~2.x, ~2.x.x, ~>2, ~>2.x ~>2.x.x --> >=2.0.0 <3.0.0-0
// ~2.0, ~2.0.x, ~>2.0, ~>2.0.x --> >=2.0.0 <2.1.0-0
// ~1.2, ~1.2.x, ~>1.2, ~>1.2.x --> >=1.2.0 <1.3.0-0
// ~1.2.3, ~>1.2.3 --> >=1.2.3 <1.3.0-0
// ~1.2.0, ~>1.2.0 --> >=1.2.0 <1.3.0-0
const replaceTildes = (comp, options) =>
  comp.trim().split(/\s+/).map((c) => {
    return replaceTilde(c, options)
  }).join(' ')

const replaceTilde = (comp, options) => {
  const r = options.loose ? re[t.TILDELOOSE] : re[t.TILDE]
  return comp.replace(r, (_, M, m, p, pr) => {
    debug('tilde', comp, _, M, m, p, pr)
    let ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = `>=${M}.0.0 <${+M + 1}.0.0-0`
    } else if (isX(p)) {
      // ~1.2 == >=1.2.0 <1.3.0-0
      ret = `>=${M}.${m}.0 <${M}.${+m + 1}.0-0`
    } else if (pr) {
      debug('replaceTilde pr', pr)
      ret = `>=${M}.${m}.${p}-${pr
      } <${M}.${+m + 1}.0-0`
    } else {
      // ~1.2.3 == >=1.2.3 <1.3.0-0
      ret = `>=${M}.${m}.${p
      } <${M}.${+m + 1}.0-0`
    }

    debug('tilde return', ret)
    return ret
  })
}

// ^ --> * (any, kinda silly)
// ^2, ^2.x, ^2.x.x --> >=2.0.0 <3.0.0-0
// ^2.0, ^2.0.x --> >=2.0.0 <3.0.0-0
// ^1.2, ^1.2.x --> >=1.2.0 <2.0.0-0
// ^1.2.3 --> >=1.2.3 <2.0.0-0
// ^1.2.0 --> >=1.2.0 <2.0.0-0
const replaceCarets = (comp, options) =>
  comp.trim().split(/\s+/).map((c) => {
    return replaceCaret(c, options)
  }).join(' ')

const replaceCaret = (comp, options) => {
  debug('caret', comp, options)
  const r = options.loose ? re[t.CARETLOOSE] : re[t.CARET]
  const z = options.includePrerelease ? '-0' : ''
  return comp.replace(r, (_, M, m, p, pr) => {
    debug('caret', comp, _, M, m, p, pr)
    let ret

    if (isX(M)) {
      ret = ''
    } else if (isX(m)) {
      ret = `>=${M}.0.0${z} <${+M + 1}.0.0-0`
    } else if (isX(p)) {
      if (M === '0') {
        ret = `>=${M}.${m}.0${z} <${M}.${+m + 1}.0-0`
      } else {
        ret = `>=${M}.${m}.0${z} <${+M + 1}.0.0-0`
      }
    } else if (pr) {
      debug('replaceCaret pr', pr)
      if (M === '0') {
        if (m === '0') {
          ret = `>=${M}.${m}.${p}-${pr
          } <${M}.${m}.${+p + 1}-0`
        } else {
          ret = `>=${M}.${m}.${p}-${pr
          } <${M}.${+m + 1}.0-0`
        }
      } else {
        ret = `>=${M}.${m}.${p}-${pr
        } <${+M + 1}.0.0-0`
      }
    } else {
      debug('no pr')
      if (M === '0') {
        if (m === '0') {
          ret = `>=${M}.${m}.${p
          }${z} <${M}.${m}.${+p + 1}-0`
        } else {
          ret = `>=${M}.${m}.${p
          }${z} <${M}.${+m + 1}.0-0`
        }
      } else {
        ret = `>=${M}.${m}.${p
        } <${+M + 1}.0.0-0`
      }
    }

    debug('caret return', ret)
    return ret
  })
}

const replaceXRanges = (comp, options) => {
  debug('replaceXRanges', comp, options)
  return comp.split(/\s+/).map((c) => {
    return replaceXRange(c, options)
  }).join(' ')
}

const replaceXRange = (comp, options) => {
  comp = comp.trim()
  const r = options.loose ? re[t.XRANGELOOSE] : re[t.XRANGE]
  return comp.replace(r, (ret, gtlt, M, m, p, pr) => {
    debug('xRange', comp, ret, gtlt, M, m, p, pr)
    const xM = isX(M)
    const xm = xM || isX(m)
    const xp = xm || isX(p)
    const anyX = xp

    if (gtlt === '=' && anyX) {
      gtlt = ''
    }

    // if we're including prereleases in the match, then we need
    // to fix this to -0, the lowest possible prerelease value
    pr = options.includePrerelease ? '-0' : ''

    if (xM) {
      if (gtlt === '>' || gtlt === '<') {
        // nothing is allowed
        ret = '<0.0.0-0'
      } else {
        // nothing is forbidden
        ret = '*'
      }
    } else if (gtlt && anyX) {
      // we know patch is an x, because we have any x at all.
      // replace X with 0
      if (xm) {
        m = 0
      }
      p = 0

      if (gtlt === '>') {
        // >1 => >=2.0.0
        // >1.2 => >=1.3.0
        gtlt = '>='
        if (xm) {
          M = +M + 1
          m = 0
          p = 0
        } else {
          m = +m + 1
          p = 0
        }
      } else if (gtlt === '<=') {
        // <=0.7.x is actually <0.8.0, since any 0.7.x should
        // pass.  Similarly, <=7.x is actually <8.0.0, etc.
        gtlt = '<'
        if (xm) {
          M = +M + 1
        } else {
          m = +m + 1
        }
      }

      if (gtlt === '<') {
        pr = '-0'
      }

      ret = `${gtlt + M}.${m}.${p}${pr}`
    } else if (xm) {
      ret = `>=${M}.0.0${pr} <${+M + 1}.0.0-0`
    } else if (xp) {
      ret = `>=${M}.${m}.0${pr
      } <${M}.${+m + 1}.0-0`
    }

    debug('xRange return', ret)

    return ret
  })
}

// Because * is AND-ed with everything else in the comparator,
// and '' means "any version", just remove the *s entirely.
const replaceStars = (comp, options) => {
  debug('replaceStars', comp, options)
  // Looseness is ignored here.  star is always as loose as it gets!
  return comp.trim().replace(re[t.STAR], '')
}

const replaceGTE0 = (comp, options) => {
  debug('replaceGTE0', comp, options)
  return comp.trim()
    .replace(re[options.includePrerelease ? t.GTE0PRE : t.GTE0], '')
}

// This function is passed to string.replace(re[t.HYPHENRANGE])
// M, m, patch, prerelease, build
// 1.2 - 3.4.5 => >=1.2.0 <=3.4.5
// 1.2.3 - 3.4 => >=1.2.0 <3.5.0-0 Any 3.4.x will do
// 1.2 - 3.4 => >=1.2.0 <3.5.0-0
const hyphenReplace = incPr => ($0,
  from, fM, fm, fp, fpr, fb,
  to, tM, tm, tp, tpr, tb) => {
  if (isX(fM)) {
    from = ''
  } else if (isX(fm)) {
    from = `>=${fM}.0.0${incPr ? '-0' : ''}`
  } else if (isX(fp)) {
    from = `>=${fM}.${fm}.0${incPr ? '-0' : ''}`
  } else if (fpr) {
    from = `>=${from}`
  } else {
    from = `>=${from}${incPr ? '-0' : ''}`
  }

  if (isX(tM)) {
    to = ''
  } else if (isX(tm)) {
    to = `<${+tM + 1}.0.0-0`
  } else if (isX(tp)) {
    to = `<${tM}.${+tm + 1}.0-0`
  } else if (tpr) {
    to = `<=${tM}.${tm}.${tp}-${tpr}`
  } else if (incPr) {
    to = `<${tM}.${tm}.${+tp + 1}-0`
  } else {
    to = `<=${to}`
  }

  return (`${from} ${to}`).trim()
}

const testSet = (set, version, options) => {
  for (let i = 0; i < set.length; i++) {
    if (!set[i].test(version)) {
      return false
    }
  }

  if (version.prerelease.length && !options.includePrerelease) {
    // Find the set of versions that are allowed to have prereleases
    // For example, ^1.2.3-pr.1 desugars to >=1.2.3-pr.1 <2.0.0
    // That should allow `1.2.3-pr.2` to pass.
    // However, `1.2.4-alpha.notready` should NOT be allowed,
    // even though it's within the range set by the comparators.
    for (let i = 0; i < set.length; i++) {
      debug(set[i].semver)
      if (set[i].semver === Comparator.ANY) {
        continue
      }

      if (set[i].semver.prerelease.length > 0) {
        const allowed = set[i].semver
        if (allowed.major === version.major &&
            allowed.minor === version.minor &&
            allowed.patch === version.patch) {
          return true
        }
      }
    }

    // Version has a -pre, but it's not one of the ones we like.
    return false
  }

  return true
}


/***/ }),
/* 38 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";


// A linked list to keep track of recently-used-ness
const Yallist = __webpack_require__(39)

const MAX = Symbol('max')
const LENGTH = Symbol('length')
const LENGTH_CALCULATOR = Symbol('lengthCalculator')
const ALLOW_STALE = Symbol('allowStale')
const MAX_AGE = Symbol('maxAge')
const DISPOSE = Symbol('dispose')
const NO_DISPOSE_ON_SET = Symbol('noDisposeOnSet')
const LRU_LIST = Symbol('lruList')
const CACHE = Symbol('cache')
const UPDATE_AGE_ON_GET = Symbol('updateAgeOnGet')

const naiveLength = () => 1

// lruList is a yallist where the head is the youngest
// item, and the tail is the oldest.  the list contains the Hit
// objects as the entries.
// Each Hit object has a reference to its Yallist.Node.  This
// never changes.
//
// cache is a Map (or PseudoMap) that matches the keys to
// the Yallist.Node object.
class LRUCache {
  constructor (options) {
    if (typeof options === 'number')
      options = { max: options }

    if (!options)
      options = {}

    if (options.max && (typeof options.max !== 'number' || options.max < 0))
      throw new TypeError('max must be a non-negative number')
    // Kind of weird to have a default max of Infinity, but oh well.
    const max = this[MAX] = options.max || Infinity

    const lc = options.length || naiveLength
    this[LENGTH_CALCULATOR] = (typeof lc !== 'function') ? naiveLength : lc
    this[ALLOW_STALE] = options.stale || false
    if (options.maxAge && typeof options.maxAge !== 'number')
      throw new TypeError('maxAge must be a number')
    this[MAX_AGE] = options.maxAge || 0
    this[DISPOSE] = options.dispose
    this[NO_DISPOSE_ON_SET] = options.noDisposeOnSet || false
    this[UPDATE_AGE_ON_GET] = options.updateAgeOnGet || false
    this.reset()
  }

  // resize the cache when the max changes.
  set max (mL) {
    if (typeof mL !== 'number' || mL < 0)
      throw new TypeError('max must be a non-negative number')

    this[MAX] = mL || Infinity
    trim(this)
  }
  get max () {
    return this[MAX]
  }

  set allowStale (allowStale) {
    this[ALLOW_STALE] = !!allowStale
  }
  get allowStale () {
    return this[ALLOW_STALE]
  }

  set maxAge (mA) {
    if (typeof mA !== 'number')
      throw new TypeError('maxAge must be a non-negative number')

    this[MAX_AGE] = mA
    trim(this)
  }
  get maxAge () {
    return this[MAX_AGE]
  }

  // resize the cache when the lengthCalculator changes.
  set lengthCalculator (lC) {
    if (typeof lC !== 'function')
      lC = naiveLength

    if (lC !== this[LENGTH_CALCULATOR]) {
      this[LENGTH_CALCULATOR] = lC
      this[LENGTH] = 0
      this[LRU_LIST].forEach(hit => {
        hit.length = this[LENGTH_CALCULATOR](hit.value, hit.key)
        this[LENGTH] += hit.length
      })
    }
    trim(this)
  }
  get lengthCalculator () { return this[LENGTH_CALCULATOR] }

  get length () { return this[LENGTH] }
  get itemCount () { return this[LRU_LIST].length }

  rforEach (fn, thisp) {
    thisp = thisp || this
    for (let walker = this[LRU_LIST].tail; walker !== null;) {
      const prev = walker.prev
      forEachStep(this, fn, walker, thisp)
      walker = prev
    }
  }

  forEach (fn, thisp) {
    thisp = thisp || this
    for (let walker = this[LRU_LIST].head; walker !== null;) {
      const next = walker.next
      forEachStep(this, fn, walker, thisp)
      walker = next
    }
  }

  keys () {
    return this[LRU_LIST].toArray().map(k => k.key)
  }

  values () {
    return this[LRU_LIST].toArray().map(k => k.value)
  }

  reset () {
    if (this[DISPOSE] &&
        this[LRU_LIST] &&
        this[LRU_LIST].length) {
      this[LRU_LIST].forEach(hit => this[DISPOSE](hit.key, hit.value))
    }

    this[CACHE] = new Map() // hash of items by key
    this[LRU_LIST] = new Yallist() // list of items in order of use recency
    this[LENGTH] = 0 // length of items in the list
  }

  dump () {
    return this[LRU_LIST].map(hit =>
      isStale(this, hit) ? false : {
        k: hit.key,
        v: hit.value,
        e: hit.now + (hit.maxAge || 0)
      }).toArray().filter(h => h)
  }

  dumpLru () {
    return this[LRU_LIST]
  }

  set (key, value, maxAge) {
    maxAge = maxAge || this[MAX_AGE]

    if (maxAge && typeof maxAge !== 'number')
      throw new TypeError('maxAge must be a number')

    const now = maxAge ? Date.now() : 0
    const len = this[LENGTH_CALCULATOR](value, key)

    if (this[CACHE].has(key)) {
      if (len > this[MAX]) {
        del(this, this[CACHE].get(key))
        return false
      }

      const node = this[CACHE].get(key)
      const item = node.value

      // dispose of the old one before overwriting
      // split out into 2 ifs for better coverage tracking
      if (this[DISPOSE]) {
        if (!this[NO_DISPOSE_ON_SET])
          this[DISPOSE](key, item.value)
      }

      item.now = now
      item.maxAge = maxAge
      item.value = value
      this[LENGTH] += len - item.length
      item.length = len
      this.get(key)
      trim(this)
      return true
    }

    const hit = new Entry(key, value, len, now, maxAge)

    // oversized objects fall out of cache automatically.
    if (hit.length > this[MAX]) {
      if (this[DISPOSE])
        this[DISPOSE](key, value)

      return false
    }

    this[LENGTH] += hit.length
    this[LRU_LIST].unshift(hit)
    this[CACHE].set(key, this[LRU_LIST].head)
    trim(this)
    return true
  }

  has (key) {
    if (!this[CACHE].has(key)) return false
    const hit = this[CACHE].get(key).value
    return !isStale(this, hit)
  }

  get (key) {
    return get(this, key, true)
  }

  peek (key) {
    return get(this, key, false)
  }

  pop () {
    const node = this[LRU_LIST].tail
    if (!node)
      return null

    del(this, node)
    return node.value
  }

  del (key) {
    del(this, this[CACHE].get(key))
  }

  load (arr) {
    // reset the cache
    this.reset()

    const now = Date.now()
    // A previous serialized cache has the most recent items first
    for (let l = arr.length - 1; l >= 0; l--) {
      const hit = arr[l]
      const expiresAt = hit.e || 0
      if (expiresAt === 0)
        // the item was created without expiration in a non aged cache
        this.set(hit.k, hit.v)
      else {
        const maxAge = expiresAt - now
        // dont add already expired items
        if (maxAge > 0) {
          this.set(hit.k, hit.v, maxAge)
        }
      }
    }
  }

  prune () {
    this[CACHE].forEach((value, key) => get(this, key, false))
  }
}

const get = (self, key, doUse) => {
  const node = self[CACHE].get(key)
  if (node) {
    const hit = node.value
    if (isStale(self, hit)) {
      del(self, node)
      if (!self[ALLOW_STALE])
        return undefined
    } else {
      if (doUse) {
        if (self[UPDATE_AGE_ON_GET])
          node.value.now = Date.now()
        self[LRU_LIST].unshiftNode(node)
      }
    }
    return hit.value
  }
}

const isStale = (self, hit) => {
  if (!hit || (!hit.maxAge && !self[MAX_AGE]))
    return false

  const diff = Date.now() - hit.now
  return hit.maxAge ? diff > hit.maxAge
    : self[MAX_AGE] && (diff > self[MAX_AGE])
}

const trim = self => {
  if (self[LENGTH] > self[MAX]) {
    for (let walker = self[LRU_LIST].tail;
      self[LENGTH] > self[MAX] && walker !== null;) {
      // We know that we're about to delete this one, and also
      // what the next least recently used key will be, so just
      // go ahead and set it now.
      const prev = walker.prev
      del(self, walker)
      walker = prev
    }
  }
}

const del = (self, node) => {
  if (node) {
    const hit = node.value
    if (self[DISPOSE])
      self[DISPOSE](hit.key, hit.value)

    self[LENGTH] -= hit.length
    self[CACHE].delete(hit.key)
    self[LRU_LIST].removeNode(node)
  }
}

class Entry {
  constructor (key, value, length, now, maxAge) {
    this.key = key
    this.value = value
    this.length = length
    this.now = now
    this.maxAge = maxAge || 0
  }
}

const forEachStep = (self, fn, node, thisp) => {
  let hit = node.value
  if (isStale(self, hit)) {
    del(self, node)
    if (!self[ALLOW_STALE])
      hit = undefined
  }
  if (hit)
    fn.call(thisp, hit.value, hit.key, self)
}

module.exports = LRUCache


/***/ }),
/* 39 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";

module.exports = Yallist

Yallist.Node = Node
Yallist.create = Yallist

function Yallist (list) {
  var self = this
  if (!(self instanceof Yallist)) {
    self = new Yallist()
  }

  self.tail = null
  self.head = null
  self.length = 0

  if (list && typeof list.forEach === 'function') {
    list.forEach(function (item) {
      self.push(item)
    })
  } else if (arguments.length > 0) {
    for (var i = 0, l = arguments.length; i < l; i++) {
      self.push(arguments[i])
    }
  }

  return self
}

Yallist.prototype.removeNode = function (node) {
  if (node.list !== this) {
    throw new Error('removing node which does not belong to this list')
  }

  var next = node.next
  var prev = node.prev

  if (next) {
    next.prev = prev
  }

  if (prev) {
    prev.next = next
  }

  if (node === this.head) {
    this.head = next
  }
  if (node === this.tail) {
    this.tail = prev
  }

  node.list.length--
  node.next = null
  node.prev = null
  node.list = null

  return next
}

Yallist.prototype.unshiftNode = function (node) {
  if (node === this.head) {
    return
  }

  if (node.list) {
    node.list.removeNode(node)
  }

  var head = this.head
  node.list = this
  node.next = head
  if (head) {
    head.prev = node
  }

  this.head = node
  if (!this.tail) {
    this.tail = node
  }
  this.length++
}

Yallist.prototype.pushNode = function (node) {
  if (node === this.tail) {
    return
  }

  if (node.list) {
    node.list.removeNode(node)
  }

  var tail = this.tail
  node.list = this
  node.prev = tail
  if (tail) {
    tail.next = node
  }

  this.tail = node
  if (!this.head) {
    this.head = node
  }
  this.length++
}

Yallist.prototype.push = function () {
  for (var i = 0, l = arguments.length; i < l; i++) {
    push(this, arguments[i])
  }
  return this.length
}

Yallist.prototype.unshift = function () {
  for (var i = 0, l = arguments.length; i < l; i++) {
    unshift(this, arguments[i])
  }
  return this.length
}

Yallist.prototype.pop = function () {
  if (!this.tail) {
    return undefined
  }

  var res = this.tail.value
  this.tail = this.tail.prev
  if (this.tail) {
    this.tail.next = null
  } else {
    this.head = null
  }
  this.length--
  return res
}

Yallist.prototype.shift = function () {
  if (!this.head) {
    return undefined
  }

  var res = this.head.value
  this.head = this.head.next
  if (this.head) {
    this.head.prev = null
  } else {
    this.tail = null
  }
  this.length--
  return res
}

Yallist.prototype.forEach = function (fn, thisp) {
  thisp = thisp || this
  for (var walker = this.head, i = 0; walker !== null; i++) {
    fn.call(thisp, walker.value, i, this)
    walker = walker.next
  }
}

Yallist.prototype.forEachReverse = function (fn, thisp) {
  thisp = thisp || this
  for (var walker = this.tail, i = this.length - 1; walker !== null; i--) {
    fn.call(thisp, walker.value, i, this)
    walker = walker.prev
  }
}

Yallist.prototype.get = function (n) {
  for (var i = 0, walker = this.head; walker !== null && i < n; i++) {
    // abort out of the list early if we hit a cycle
    walker = walker.next
  }
  if (i === n && walker !== null) {
    return walker.value
  }
}

Yallist.prototype.getReverse = function (n) {
  for (var i = 0, walker = this.tail; walker !== null && i < n; i++) {
    // abort out of the list early if we hit a cycle
    walker = walker.prev
  }
  if (i === n && walker !== null) {
    return walker.value
  }
}

Yallist.prototype.map = function (fn, thisp) {
  thisp = thisp || this
  var res = new Yallist()
  for (var walker = this.head; walker !== null;) {
    res.push(fn.call(thisp, walker.value, this))
    walker = walker.next
  }
  return res
}

Yallist.prototype.mapReverse = function (fn, thisp) {
  thisp = thisp || this
  var res = new Yallist()
  for (var walker = this.tail; walker !== null;) {
    res.push(fn.call(thisp, walker.value, this))
    walker = walker.prev
  }
  return res
}

Yallist.prototype.reduce = function (fn, initial) {
  var acc
  var walker = this.head
  if (arguments.length > 1) {
    acc = initial
  } else if (this.head) {
    walker = this.head.next
    acc = this.head.value
  } else {
    throw new TypeError('Reduce of empty list with no initial value')
  }

  for (var i = 0; walker !== null; i++) {
    acc = fn(acc, walker.value, i)
    walker = walker.next
  }

  return acc
}

Yallist.prototype.reduceReverse = function (fn, initial) {
  var acc
  var walker = this.tail
  if (arguments.length > 1) {
    acc = initial
  } else if (this.tail) {
    walker = this.tail.prev
    acc = this.tail.value
  } else {
    throw new TypeError('Reduce of empty list with no initial value')
  }

  for (var i = this.length - 1; walker !== null; i--) {
    acc = fn(acc, walker.value, i)
    walker = walker.prev
  }

  return acc
}

Yallist.prototype.toArray = function () {
  var arr = new Array(this.length)
  for (var i = 0, walker = this.head; walker !== null; i++) {
    arr[i] = walker.value
    walker = walker.next
  }
  return arr
}

Yallist.prototype.toArrayReverse = function () {
  var arr = new Array(this.length)
  for (var i = 0, walker = this.tail; walker !== null; i++) {
    arr[i] = walker.value
    walker = walker.prev
  }
  return arr
}

Yallist.prototype.slice = function (from, to) {
  to = to || this.length
  if (to < 0) {
    to += this.length
  }
  from = from || 0
  if (from < 0) {
    from += this.length
  }
  var ret = new Yallist()
  if (to < from || to < 0) {
    return ret
  }
  if (from < 0) {
    from = 0
  }
  if (to > this.length) {
    to = this.length
  }
  for (var i = 0, walker = this.head; walker !== null && i < from; i++) {
    walker = walker.next
  }
  for (; walker !== null && i < to; i++, walker = walker.next) {
    ret.push(walker.value)
  }
  return ret
}

Yallist.prototype.sliceReverse = function (from, to) {
  to = to || this.length
  if (to < 0) {
    to += this.length
  }
  from = from || 0
  if (from < 0) {
    from += this.length
  }
  var ret = new Yallist()
  if (to < from || to < 0) {
    return ret
  }
  if (from < 0) {
    from = 0
  }
  if (to > this.length) {
    to = this.length
  }
  for (var i = this.length, walker = this.tail; walker !== null && i > to; i--) {
    walker = walker.prev
  }
  for (; walker !== null && i > from; i--, walker = walker.prev) {
    ret.push(walker.value)
  }
  return ret
}

Yallist.prototype.splice = function (start, deleteCount, ...nodes) {
  if (start > this.length) {
    start = this.length - 1
  }
  if (start < 0) {
    start = this.length + start;
  }

  for (var i = 0, walker = this.head; walker !== null && i < start; i++) {
    walker = walker.next
  }

  var ret = []
  for (var i = 0; walker && i < deleteCount; i++) {
    ret.push(walker.value)
    walker = this.removeNode(walker)
  }
  if (walker === null) {
    walker = this.tail
  }

  if (walker !== this.head && walker !== this.tail) {
    walker = walker.prev
  }

  for (var i = 0; i < nodes.length; i++) {
    walker = insert(this, walker, nodes[i])
  }
  return ret;
}

Yallist.prototype.reverse = function () {
  var head = this.head
  var tail = this.tail
  for (var walker = head; walker !== null; walker = walker.prev) {
    var p = walker.prev
    walker.prev = walker.next
    walker.next = p
  }
  this.head = tail
  this.tail = head
  return this
}

function insert (self, node, value) {
  var inserted = node === self.head ?
    new Node(value, null, node, self) :
    new Node(value, node, node.next, self)

  if (inserted.next === null) {
    self.tail = inserted
  }
  if (inserted.prev === null) {
    self.head = inserted
  }

  self.length++

  return inserted
}

function push (self, item) {
  self.tail = new Node(item, self.tail, null, self)
  if (!self.head) {
    self.head = self.tail
  }
  self.length++
}

function unshift (self, item) {
  self.head = new Node(item, null, self.head, self)
  if (!self.tail) {
    self.tail = self.head
  }
  self.length++
}

function Node (value, prev, next, list) {
  if (!(this instanceof Node)) {
    return new Node(value, prev, next, list)
  }

  this.list = list
  this.value = value

  if (prev) {
    prev.next = this
    this.prev = prev
  } else {
    this.prev = null
  }

  if (next) {
    next.prev = this
    this.next = next
  } else {
    this.next = null
  }
}

try {
  // add if support for Symbol.iterator is present
  __webpack_require__(40)(Yallist)
} catch (er) {}


/***/ }),
/* 40 */
/***/ ((module) => {

"use strict";

module.exports = function (Yallist) {
  Yallist.prototype[Symbol.iterator] = function* () {
    for (let walker = this.head; walker; walker = walker.next) {
      yield walker.value
    }
  }
}


/***/ }),
/* 41 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(37)
const satisfies = (version, range, options) => {
  try {
    range = new Range(range, options)
  } catch (er) {
    return false
  }
  return range.test(version)
}
module.exports = satisfies


/***/ }),
/* 42 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(37)

// Mostly just for testing and legacy API reasons
const toComparators = (range, options) =>
  new Range(range, options).set
    .map(comp => comp.map(c => c.value).join(' ').trim().split(' '))

module.exports = toComparators


/***/ }),
/* 43 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const Range = __webpack_require__(37)

const maxSatisfying = (versions, range, options) => {
  let max = null
  let maxSV = null
  let rangeObj = null
  try {
    rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach((v) => {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!max || maxSV.compare(v) === -1) {
        // compare(max, v, true)
        max = v
        maxSV = new SemVer(max, options)
      }
    }
  })
  return max
}
module.exports = maxSatisfying


/***/ }),
/* 44 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const Range = __webpack_require__(37)
const minSatisfying = (versions, range, options) => {
  let min = null
  let minSV = null
  let rangeObj = null
  try {
    rangeObj = new Range(range, options)
  } catch (er) {
    return null
  }
  versions.forEach((v) => {
    if (rangeObj.test(v)) {
      // satisfies(v, range, options)
      if (!min || minSV.compare(v) === 1) {
        // compare(min, v, true)
        min = v
        minSV = new SemVer(min, options)
      }
    }
  })
  return min
}
module.exports = minSatisfying


/***/ }),
/* 45 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const Range = __webpack_require__(37)
const gt = __webpack_require__(29)

const minVersion = (range, loose) => {
  range = new Range(range, loose)

  let minver = new SemVer('0.0.0')
  if (range.test(minver)) {
    return minver
  }

  minver = new SemVer('0.0.0-0')
  if (range.test(minver)) {
    return minver
  }

  minver = null
  for (let i = 0; i < range.set.length; ++i) {
    const comparators = range.set[i]

    let setMin = null
    comparators.forEach((comparator) => {
      // Clone to avoid manipulating the comparator's semver object.
      const compver = new SemVer(comparator.semver.version)
      switch (comparator.operator) {
        case '>':
          if (compver.prerelease.length === 0) {
            compver.patch++
          } else {
            compver.prerelease.push(0)
          }
          compver.raw = compver.format()
          /* fallthrough */
        case '':
        case '>=':
          if (!setMin || gt(compver, setMin)) {
            setMin = compver
          }
          break
        case '<':
        case '<=':
          /* Ignore maximum versions */
          break
        /* istanbul ignore next */
        default:
          throw new Error(`Unexpected operation: ${comparator.operator}`)
      }
    })
    if (setMin && (!minver || gt(minver, setMin))) {
      minver = setMin
    }
  }

  if (minver && range.test(minver)) {
    return minver
  }

  return null
}
module.exports = minVersion


/***/ }),
/* 46 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(37)
const validRange = (range, options) => {
  try {
    // Return '*' instead of '' so that truthiness works.
    // This will throw if it's invalid anyway
    return new Range(range, options).range || '*'
  } catch (er) {
    return null
  }
}
module.exports = validRange


/***/ }),
/* 47 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const SemVer = __webpack_require__(10)
const Comparator = __webpack_require__(36)
const { ANY } = Comparator
const Range = __webpack_require__(37)
const satisfies = __webpack_require__(41)
const gt = __webpack_require__(29)
const lt = __webpack_require__(30)
const lte = __webpack_require__(33)
const gte = __webpack_require__(32)

const outside = (version, range, hilo, options) => {
  version = new SemVer(version, options)
  range = new Range(range, options)

  let gtfn, ltefn, ltfn, comp, ecomp
  switch (hilo) {
    case '>':
      gtfn = gt
      ltefn = lte
      ltfn = lt
      comp = '>'
      ecomp = '>='
      break
    case '<':
      gtfn = lt
      ltefn = gte
      ltfn = gt
      comp = '<'
      ecomp = '<='
      break
    default:
      throw new TypeError('Must provide a hilo val of "<" or ">"')
  }

  // If it satisfies the range it is not outside
  if (satisfies(version, range, options)) {
    return false
  }

  // From now on, variable terms are as if we're in "gtr" mode.
  // but note that everything is flipped for the "ltr" function.

  for (let i = 0; i < range.set.length; ++i) {
    const comparators = range.set[i]

    let high = null
    let low = null

    comparators.forEach((comparator) => {
      if (comparator.semver === ANY) {
        comparator = new Comparator('>=0.0.0')
      }
      high = high || comparator
      low = low || comparator
      if (gtfn(comparator.semver, high.semver, options)) {
        high = comparator
      } else if (ltfn(comparator.semver, low.semver, options)) {
        low = comparator
      }
    })

    // If the edge version comparator has a operator then our version
    // isn't outside it
    if (high.operator === comp || high.operator === ecomp) {
      return false
    }

    // If the lowest version comparator has an operator and our version
    // is less than it then it isn't higher than the range
    if ((!low.operator || low.operator === comp) &&
        ltefn(version, low.semver)) {
      return false
    } else if (low.operator === ecomp && ltfn(version, low.semver)) {
      return false
    }
  }
  return true
}

module.exports = outside


/***/ }),
/* 48 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// Determine if version is greater than all the versions possible in the range.
const outside = __webpack_require__(47)
const gtr = (version, range, options) => outside(version, range, '>', options)
module.exports = gtr


/***/ }),
/* 49 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const outside = __webpack_require__(47)
// Determine if version is less than all the versions possible in the range
const ltr = (version, range, options) => outside(version, range, '<', options)
module.exports = ltr


/***/ }),
/* 50 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(37)
const intersects = (r1, r2, options) => {
  r1 = new Range(r1, options)
  r2 = new Range(r2, options)
  return r1.intersects(r2)
}
module.exports = intersects


/***/ }),
/* 51 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

// given a set of versions and a range, create a "simplified" range
// that includes the same versions that the original range does
// If the original range is shorter than the simplified one, return that.
const satisfies = __webpack_require__(41)
const compare = __webpack_require__(19)
module.exports = (versions, range, options) => {
  const set = []
  let first = null
  let prev = null
  const v = versions.sort((a, b) => compare(a, b, options))
  for (const version of v) {
    const included = satisfies(version, range, options)
    if (included) {
      prev = version
      if (!first) {
        first = version
      }
    } else {
      if (prev) {
        set.push([first, prev])
      }
      prev = null
      first = null
    }
  }
  if (first) {
    set.push([first, null])
  }

  const ranges = []
  for (const [min, max] of set) {
    if (min === max) {
      ranges.push(min)
    } else if (!max && min === v[0]) {
      ranges.push('*')
    } else if (!max) {
      ranges.push(`>=${min}`)
    } else if (min === v[0]) {
      ranges.push(`<=${max}`)
    } else {
      ranges.push(`${min} - ${max}`)
    }
  }
  const simplified = ranges.join(' || ')
  const original = typeof range.raw === 'string' ? range.raw : String(range)
  return simplified.length < original.length ? simplified : range
}


/***/ }),
/* 52 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const Range = __webpack_require__(37)
const Comparator = __webpack_require__(36)
const { ANY } = Comparator
const satisfies = __webpack_require__(41)
const compare = __webpack_require__(19)

// Complex range `r1 || r2 || ...` is a subset of `R1 || R2 || ...` iff:
// - Every simple range `r1, r2, ...` is a null set, OR
// - Every simple range `r1, r2, ...` which is not a null set is a subset of
//   some `R1, R2, ...`
//
// Simple range `c1 c2 ...` is a subset of simple range `C1 C2 ...` iff:
// - If c is only the ANY comparator
//   - If C is only the ANY comparator, return true
//   - Else if in prerelease mode, return false
//   - else replace c with `[>=0.0.0]`
// - If C is only the ANY comparator
//   - if in prerelease mode, return true
//   - else replace C with `[>=0.0.0]`
// - Let EQ be the set of = comparators in c
// - If EQ is more than one, return true (null set)
// - Let GT be the highest > or >= comparator in c
// - Let LT be the lowest < or <= comparator in c
// - If GT and LT, and GT.semver > LT.semver, return true (null set)
// - If any C is a = range, and GT or LT are set, return false
// - If EQ
//   - If GT, and EQ does not satisfy GT, return true (null set)
//   - If LT, and EQ does not satisfy LT, return true (null set)
//   - If EQ satisfies every C, return true
//   - Else return false
// - If GT
//   - If GT.semver is lower than any > or >= comp in C, return false
//   - If GT is >=, and GT.semver does not satisfy every C, return false
//   - If GT.semver has a prerelease, and not in prerelease mode
//     - If no C has a prerelease and the GT.semver tuple, return false
// - If LT
//   - If LT.semver is greater than any < or <= comp in C, return false
//   - If LT is <=, and LT.semver does not satisfy every C, return false
//   - If GT.semver has a prerelease, and not in prerelease mode
//     - If no C has a prerelease and the LT.semver tuple, return false
// - Else return true

const subset = (sub, dom, options = {}) => {
  if (sub === dom) {
    return true
  }

  sub = new Range(sub, options)
  dom = new Range(dom, options)
  let sawNonNull = false

  OUTER: for (const simpleSub of sub.set) {
    for (const simpleDom of dom.set) {
      const isSub = simpleSubset(simpleSub, simpleDom, options)
      sawNonNull = sawNonNull || isSub !== null
      if (isSub) {
        continue OUTER
      }
    }
    // the null set is a subset of everything, but null simple ranges in
    // a complex range should be ignored.  so if we saw a non-null range,
    // then we know this isn't a subset, but if EVERY simple range was null,
    // then it is a subset.
    if (sawNonNull) {
      return false
    }
  }
  return true
}

const simpleSubset = (sub, dom, options) => {
  if (sub === dom) {
    return true
  }

  if (sub.length === 1 && sub[0].semver === ANY) {
    if (dom.length === 1 && dom[0].semver === ANY) {
      return true
    } else if (options.includePrerelease) {
      sub = [new Comparator('>=0.0.0-0')]
    } else {
      sub = [new Comparator('>=0.0.0')]
    }
  }

  if (dom.length === 1 && dom[0].semver === ANY) {
    if (options.includePrerelease) {
      return true
    } else {
      dom = [new Comparator('>=0.0.0')]
    }
  }

  const eqSet = new Set()
  let gt, lt
  for (const c of sub) {
    if (c.operator === '>' || c.operator === '>=') {
      gt = higherGT(gt, c, options)
    } else if (c.operator === '<' || c.operator === '<=') {
      lt = lowerLT(lt, c, options)
    } else {
      eqSet.add(c.semver)
    }
  }

  if (eqSet.size > 1) {
    return null
  }

  let gtltComp
  if (gt && lt) {
    gtltComp = compare(gt.semver, lt.semver, options)
    if (gtltComp > 0) {
      return null
    } else if (gtltComp === 0 && (gt.operator !== '>=' || lt.operator !== '<=')) {
      return null
    }
  }

  // will iterate one or zero times
  for (const eq of eqSet) {
    if (gt && !satisfies(eq, String(gt), options)) {
      return null
    }

    if (lt && !satisfies(eq, String(lt), options)) {
      return null
    }

    for (const c of dom) {
      if (!satisfies(eq, String(c), options)) {
        return false
      }
    }

    return true
  }

  let higher, lower
  let hasDomLT, hasDomGT
  // if the subset has a prerelease, we need a comparator in the superset
  // with the same tuple and a prerelease, or it's not a subset
  let needDomLTPre = lt &&
    !options.includePrerelease &&
    lt.semver.prerelease.length ? lt.semver : false
  let needDomGTPre = gt &&
    !options.includePrerelease &&
    gt.semver.prerelease.length ? gt.semver : false
  // exception: <1.2.3-0 is the same as <1.2.3
  if (needDomLTPre && needDomLTPre.prerelease.length === 1 &&
      lt.operator === '<' && needDomLTPre.prerelease[0] === 0) {
    needDomLTPre = false
  }

  for (const c of dom) {
    hasDomGT = hasDomGT || c.operator === '>' || c.operator === '>='
    hasDomLT = hasDomLT || c.operator === '<' || c.operator === '<='
    if (gt) {
      if (needDomGTPre) {
        if (c.semver.prerelease && c.semver.prerelease.length &&
            c.semver.major === needDomGTPre.major &&
            c.semver.minor === needDomGTPre.minor &&
            c.semver.patch === needDomGTPre.patch) {
          needDomGTPre = false
        }
      }
      if (c.operator === '>' || c.operator === '>=') {
        higher = higherGT(gt, c, options)
        if (higher === c && higher !== gt) {
          return false
        }
      } else if (gt.operator === '>=' && !satisfies(gt.semver, String(c), options)) {
        return false
      }
    }
    if (lt) {
      if (needDomLTPre) {
        if (c.semver.prerelease && c.semver.prerelease.length &&
            c.semver.major === needDomLTPre.major &&
            c.semver.minor === needDomLTPre.minor &&
            c.semver.patch === needDomLTPre.patch) {
          needDomLTPre = false
        }
      }
      if (c.operator === '<' || c.operator === '<=') {
        lower = lowerLT(lt, c, options)
        if (lower === c && lower !== lt) {
          return false
        }
      } else if (lt.operator === '<=' && !satisfies(lt.semver, String(c), options)) {
        return false
      }
    }
    if (!c.operator && (lt || gt) && gtltComp !== 0) {
      return false
    }
  }

  // if there was a < or >, and nothing in the dom, then must be false
  // UNLESS it was limited by another range in the other direction.
  // Eg, >1.0.0 <1.0.1 is still a subset of <2.0.0
  if (gt && hasDomLT && !lt && gtltComp !== 0) {
    return false
  }

  if (lt && hasDomGT && !gt && gtltComp !== 0) {
    return false
  }

  // we needed a prerelease range in a specific tuple, but didn't get one
  // then this isn't a subset.  eg >=1.2.3-pre is not a subset of >=1.0.0,
  // because it includes prereleases in the 1.2.3 tuple
  if (needDomGTPre || needDomLTPre) {
    return false
  }

  return true
}

// >=1.2.3 is lower than >1.2.3
const higherGT = (a, b, options) => {
  if (!a) {
    return b
  }
  const comp = compare(a.semver, b.semver, options)
  return comp > 0 ? a
    : comp < 0 ? b
    : b.operator === '>' && a.operator === '>=' ? b
    : a
}

// <=1.2.3 is higher than <1.2.3
const lowerLT = (a, b, options) => {
  if (!a) {
    return b
  }
  const comp = compare(a.semver, b.semver, options)
  return comp < 0 ? a
    : comp > 0 ? b
    : b.operator === '<' && a.operator === '<=' ? b
    : a
}

module.exports = subset


/***/ }),
/* 53 */
/***/ ((module) => {

"use strict";
module.exports = require("vscode");

/***/ }),
/* 54 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.asPromise = exports.thenable = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function thenable(value) {
    return value && func(value.then);
}
exports.thenable = thenable;
function asPromise(value) {
    if (value instanceof Promise) {
        return value;
    }
    else if (thenable(value)) {
        return new Promise((resolve, reject) => {
            value.then((resolved) => resolve(resolved), (error) => reject(error));
        });
    }
    else {
        return Promise.resolve(value);
    }
}
exports.asPromise = asPromise;
//# sourceMappingURL=is.js.map

/***/ }),
/* 55 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProposedFeatures = exports.BaseLanguageClient = exports.MessageTransports = exports.SuspendMode = exports.State = exports.CloseAction = exports.ErrorAction = exports.RevealOutputChannelOn = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const c2p = __webpack_require__(103);
const p2c = __webpack_require__(114);
const Is = __webpack_require__(54);
const async_1 = __webpack_require__(104);
const UUID = __webpack_require__(115);
const progressPart_1 = __webpack_require__(116);
const features_1 = __webpack_require__(117);
const diagnostic_1 = __webpack_require__(118);
const notebook_1 = __webpack_require__(123);
const configuration_1 = __webpack_require__(124);
const textSynchronization_1 = __webpack_require__(125);
const completion_1 = __webpack_require__(126);
const hover_1 = __webpack_require__(127);
const definition_1 = __webpack_require__(128);
const signatureHelp_1 = __webpack_require__(129);
const documentHighlight_1 = __webpack_require__(130);
const documentSymbol_1 = __webpack_require__(131);
const workspaceSymbol_1 = __webpack_require__(132);
const reference_1 = __webpack_require__(133);
const codeAction_1 = __webpack_require__(134);
const codeLens_1 = __webpack_require__(135);
const formatting_1 = __webpack_require__(136);
const rename_1 = __webpack_require__(137);
const documentLink_1 = __webpack_require__(138);
const executeCommand_1 = __webpack_require__(139);
const fileSystemWatcher_1 = __webpack_require__(140);
const colorProvider_1 = __webpack_require__(141);
const configuration_2 = __webpack_require__(124);
const implementation_1 = __webpack_require__(142);
const typeDefinition_1 = __webpack_require__(143);
const workspaceFolder_1 = __webpack_require__(144);
const foldingRange_1 = __webpack_require__(145);
const declaration_1 = __webpack_require__(146);
const selectionRange_1 = __webpack_require__(147);
const progress_1 = __webpack_require__(148);
const callHierarchy_1 = __webpack_require__(149);
const semanticTokens_1 = __webpack_require__(150);
const fileOperations_1 = __webpack_require__(151);
const linkedEditingRange_1 = __webpack_require__(152);
const typeHierarchy_1 = __webpack_require__(153);
const inlineValue_1 = __webpack_require__(154);
const inlayHint_1 = __webpack_require__(155);
/**
 * Controls when the output channel is revealed.
 */
var RevealOutputChannelOn;
(function (RevealOutputChannelOn) {
    RevealOutputChannelOn[RevealOutputChannelOn["Info"] = 1] = "Info";
    RevealOutputChannelOn[RevealOutputChannelOn["Warn"] = 2] = "Warn";
    RevealOutputChannelOn[RevealOutputChannelOn["Error"] = 3] = "Error";
    RevealOutputChannelOn[RevealOutputChannelOn["Never"] = 4] = "Never";
})(RevealOutputChannelOn = exports.RevealOutputChannelOn || (exports.RevealOutputChannelOn = {}));
/**
 * An action to be performed when the connection is producing errors.
 */
var ErrorAction;
(function (ErrorAction) {
    /**
     * Continue running the server.
     */
    ErrorAction[ErrorAction["Continue"] = 1] = "Continue";
    /**
     * Shutdown the server.
     */
    ErrorAction[ErrorAction["Shutdown"] = 2] = "Shutdown";
})(ErrorAction = exports.ErrorAction || (exports.ErrorAction = {}));
/**
 * An action to be performed when the connection to a server got closed.
 */
var CloseAction;
(function (CloseAction) {
    /**
     * Don't restart the server. The connection stays closed.
     */
    CloseAction[CloseAction["DoNotRestart"] = 1] = "DoNotRestart";
    /**
     * Restart the server.
     */
    CloseAction[CloseAction["Restart"] = 2] = "Restart";
})(CloseAction = exports.CloseAction || (exports.CloseAction = {}));
/**
 * Signals in which state the language client is in.
 */
var State;
(function (State) {
    /**
     * The client is stopped or got never started.
     */
    State[State["Stopped"] = 1] = "Stopped";
    /**
     * The client is starting but not ready yet.
     */
    State[State["Starting"] = 3] = "Starting";
    /**
     * The client is running and ready.
     */
    State[State["Running"] = 2] = "Running";
})(State = exports.State || (exports.State = {}));
var SuspendMode;
(function (SuspendMode) {
    /**
     * Don't allow suspend mode.
     */
    SuspendMode["off"] = "off";
    /**
     * Support suspend mode even if not all
     * registered providers have a corresponding
     * activation listener.
     */
    SuspendMode["on"] = "on";
})(SuspendMode = exports.SuspendMode || (exports.SuspendMode = {}));
class DefaultErrorHandler {
    constructor(client, maxRestartCount) {
        this.client = client;
        this.maxRestartCount = maxRestartCount;
        this.restarts = [];
    }
    error(_error, _message, count) {
        if (count && count <= 3) {
            return { action: ErrorAction.Continue };
        }
        return { action: ErrorAction.Shutdown };
    }
    closed() {
        this.restarts.push(Date.now());
        if (this.restarts.length <= this.maxRestartCount) {
            return { action: CloseAction.Restart };
        }
        else {
            let diff = this.restarts[this.restarts.length - 1] - this.restarts[0];
            if (diff <= 3 * 60 * 1000) {
                return { action: CloseAction.DoNotRestart, message: `The ${this.client.name} server crashed ${this.maxRestartCount + 1} times in the last 3 minutes. The server will not be restarted. See the output for more information.` };
            }
            else {
                this.restarts.shift();
                return { action: CloseAction.Restart };
            }
        }
    }
}
var ClientState;
(function (ClientState) {
    ClientState["Initial"] = "initial";
    ClientState["Starting"] = "starting";
    ClientState["StartFailed"] = "startFailed";
    ClientState["Running"] = "running";
    ClientState["Stopping"] = "stopping";
    ClientState["Stopped"] = "stopped";
})(ClientState || (ClientState = {}));
var MessageTransports;
(function (MessageTransports) {
    function is(value) {
        let candidate = value;
        return candidate && vscode_languageserver_protocol_1.MessageReader.is(value.reader) && vscode_languageserver_protocol_1.MessageWriter.is(value.writer);
    }
    MessageTransports.is = is;
})(MessageTransports = exports.MessageTransports || (exports.MessageTransports = {}));
class BaseLanguageClient {
    constructor(id, name, clientOptions) {
        this._traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        this._diagnosticQueue = new Map();
        this._diagnosticQueueState = { state: 'idle' };
        this._features = [];
        this._dynamicFeatures = new Map();
        this.workspaceEditLock = new async_1.Semaphore(1);
        this._id = id;
        this._name = name;
        clientOptions = clientOptions || {};
        const markdown = { isTrusted: false, supportHtml: false };
        if (clientOptions.markdown !== undefined) {
            markdown.isTrusted = clientOptions.markdown.isTrusted === true;
            markdown.supportHtml = clientOptions.markdown.supportHtml === true;
        }
        // const defaultInterval = (clientOptions as TestOptions).$testMode ? 50 : 60000;
        this._clientOptions = {
            documentSelector: clientOptions.documentSelector ?? [],
            synchronize: clientOptions.synchronize ?? {},
            diagnosticCollectionName: clientOptions.diagnosticCollectionName,
            outputChannelName: clientOptions.outputChannelName ?? this._name,
            revealOutputChannelOn: clientOptions.revealOutputChannelOn ?? RevealOutputChannelOn.Error,
            stdioEncoding: clientOptions.stdioEncoding ?? 'utf8',
            initializationOptions: clientOptions.initializationOptions,
            initializationFailedHandler: clientOptions.initializationFailedHandler,
            progressOnInitialization: !!clientOptions.progressOnInitialization,
            errorHandler: clientOptions.errorHandler ?? this.createDefaultErrorHandler(clientOptions.connectionOptions?.maxRestartCount),
            middleware: clientOptions.middleware ?? {},
            uriConverters: clientOptions.uriConverters,
            workspaceFolder: clientOptions.workspaceFolder,
            connectionOptions: clientOptions.connectionOptions,
            markdown,
            // suspend: {
            // 	mode: clientOptions.suspend?.mode ?? SuspendMode.off,
            // 	callback: clientOptions.suspend?.callback ?? (() => Promise.resolve(true)),
            // 	interval: clientOptions.suspend?.interval ? Math.max(clientOptions.suspend.interval, defaultInterval) : defaultInterval
            // },
            diagnosticPullOptions: clientOptions.diagnosticPullOptions ?? { onChange: true, onSave: false },
            notebookDocumentOptions: clientOptions.notebookDocumentOptions ?? {}
        };
        this._clientOptions.synchronize = this._clientOptions.synchronize || {};
        this._state = ClientState.Initial;
        this._ignoredRegistrations = new Set();
        this._notificationHandlers = new Map();
        this._pendingNotificationHandlers = new Map();
        this._notificationDisposables = new Map();
        this._requestHandlers = new Map();
        this._pendingRequestHandlers = new Map();
        this._requestDisposables = new Map();
        this._progressHandlers = new Map();
        this._pendingProgressHandlers = new Map();
        this._progressDisposables = new Map();
        this._connection = undefined;
        // this._idleStart = undefined;
        this._initializeResult = undefined;
        if (clientOptions.outputChannel) {
            this._outputChannel = clientOptions.outputChannel;
            this._disposeOutputChannel = false;
        }
        else {
            this._outputChannel = undefined;
            this._disposeOutputChannel = true;
        }
        this._traceOutputChannel = clientOptions.traceOutputChannel;
        this._diagnostics = undefined;
        this._fileEvents = [];
        this._fileEventDelayer = new async_1.Delayer(250);
        this._onStop = undefined;
        this._telemetryEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._stateChangeEmitter = new vscode_languageserver_protocol_1.Emitter();
        this._trace = vscode_languageserver_protocol_1.Trace.Off;
        this._tracer = {
            log: (messageOrDataObject, data) => {
                if (Is.string(messageOrDataObject)) {
                    this.logTrace(messageOrDataObject, data);
                }
                else {
                    this.logObjectTrace(messageOrDataObject);
                }
            },
        };
        this._c2p = c2p.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.code2Protocol : undefined);
        this._p2c = p2c.createConverter(clientOptions.uriConverters ? clientOptions.uriConverters.protocol2Code : undefined, this._clientOptions.markdown.isTrusted, this._clientOptions.markdown.supportHtml);
        this._syncedDocuments = new Map();
        this.registerBuiltinFeatures();
    }
    get name() {
        return this._name;
    }
    get middleware() {
        return this._clientOptions.middleware ?? Object.create(null);
    }
    get clientOptions() {
        return this._clientOptions;
    }
    get protocol2CodeConverter() {
        return this._p2c;
    }
    get code2ProtocolConverter() {
        return this._c2p;
    }
    get onTelemetry() {
        return this._telemetryEmitter.event;
    }
    get onDidChangeState() {
        return this._stateChangeEmitter.event;
    }
    get outputChannel() {
        if (!this._outputChannel) {
            this._outputChannel = vscode_1.window.createOutputChannel(this._clientOptions.outputChannelName ? this._clientOptions.outputChannelName : this._name);
        }
        return this._outputChannel;
    }
    get traceOutputChannel() {
        if (this._traceOutputChannel) {
            return this._traceOutputChannel;
        }
        return this.outputChannel;
    }
    get diagnostics() {
        return this._diagnostics;
    }
    get state() {
        return this.getPublicState();
    }
    get $state() {
        return this._state;
    }
    set $state(value) {
        let oldState = this.getPublicState();
        this._state = value;
        let newState = this.getPublicState();
        if (newState !== oldState) {
            this._stateChangeEmitter.fire({ oldState, newState });
        }
    }
    getPublicState() {
        switch (this.$state) {
            case ClientState.Starting:
                return State.Starting;
            case ClientState.Running:
                return State.Running;
            default:
                return State.Stopped;
        }
    }
    get initializeResult() {
        return this._initializeResult;
    }
    async sendRequest(type, ...params) {
        if (this.$state === ClientState.StartFailed || this.$state === ClientState.Stopping || this.$state === ClientState.Stopped) {
            return Promise.reject(new vscode_languageserver_protocol_1.ResponseError(vscode_languageserver_protocol_1.ErrorCodes.ConnectionInactive, `Client is not running`));
        }
        try {
            // Ensure we have a connection before we force the document sync.
            const connection = await this.$start();
            this.forceDocumentSync();
            return connection.sendRequest(type, ...params);
        }
        catch (error) {
            this.error(`Sending request ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onRequest(type, handler) {
        const method = typeof type === 'string' ? type : type.method;
        this._requestHandlers.set(method, handler);
        const connection = this.activeConnection();
        let disposable;
        if (connection !== undefined) {
            this._requestDisposables.set(method, connection.onRequest(type, handler));
            disposable = {
                dispose: () => {
                    const disposable = this._requestDisposables.get(method);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._requestDisposables.delete(method);
                    }
                }
            };
        }
        else {
            this._pendingRequestHandlers.set(method, handler);
            disposable = {
                dispose: () => {
                    this._pendingRequestHandlers.delete(method);
                    const disposable = this._requestDisposables.get(method);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._requestDisposables.delete(method);
                    }
                }
            };
        }
        return {
            dispose: () => {
                this._requestHandlers.delete(method);
                disposable.dispose();
            }
        };
    }
    async sendNotification(type, params) {
        if (this.$state === ClientState.StartFailed || this.$state === ClientState.Stopping || this.$state === ClientState.Stopped) {
            return Promise.reject(new vscode_languageserver_protocol_1.ResponseError(vscode_languageserver_protocol_1.ErrorCodes.ConnectionInactive, `Client is not running`));
        }
        try {
            // Ensure we have a connection before we force the document sync.
            const connection = await this.$start();
            this.forceDocumentSync();
            return connection.sendNotification(type, params);
        }
        catch (error) {
            this.error(`Sending notification ${Is.string(type) ? type : type.method} failed.`, error);
            throw error;
        }
    }
    onNotification(type, handler) {
        const method = typeof type === 'string' ? type : type.method;
        this._notificationHandlers.set(method, handler);
        const connection = this.activeConnection();
        let disposable;
        if (connection !== undefined) {
            this._notificationDisposables.set(method, connection.onNotification(type, handler));
            disposable = {
                dispose: () => {
                    const disposable = this._notificationDisposables.get(method);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._notificationDisposables.delete(method);
                    }
                }
            };
        }
        else {
            this._pendingNotificationHandlers.set(method, handler);
            disposable = {
                dispose: () => {
                    this._pendingNotificationHandlers.delete(method);
                    const disposable = this._notificationDisposables.get(method);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._notificationDisposables.delete(method);
                    }
                }
            };
        }
        return {
            dispose: () => {
                this._notificationHandlers.delete(method);
                disposable.dispose();
            }
        };
    }
    async sendProgress(type, token, value) {
        try {
            // Ensure we have a connection before we force the document sync.
            const connection = await this.$start();
            return connection.sendProgress(type, token, value);
        }
        catch (error) {
            this.error(`Sending progress for token ${token} failed.`, error);
            throw error;
        }
    }
    onProgress(type, token, handler) {
        this._progressHandlers.set(token, { type, handler });
        const connection = this.activeConnection();
        let disposable;
        const handleWorkDoneProgress = this._clientOptions.middleware?.handleWorkDoneProgress;
        const realHandler = vscode_languageserver_protocol_1.WorkDoneProgress.is(type) && handleWorkDoneProgress !== undefined
            ? (params) => {
                handleWorkDoneProgress(token, params, () => handler(params));
            }
            : handler;
        if (connection !== undefined) {
            this._progressDisposables.set(token, connection.onProgress(type, token, realHandler));
            disposable = {
                dispose: () => {
                    const disposable = this._progressDisposables.get(token);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._progressDisposables.delete(token);
                    }
                }
            };
        }
        else {
            this._pendingProgressHandlers.set(token, { type, handler });
            disposable = {
                dispose: () => {
                    this._pendingProgressHandlers.delete(token);
                    const disposable = this._progressDisposables.get(token);
                    if (disposable !== undefined) {
                        disposable.dispose();
                        this._progressDisposables.delete(token);
                    }
                }
            };
        }
        return {
            dispose: () => {
                this._progressHandlers.delete(token);
                disposable.dispose();
            }
        };
    }
    createDefaultErrorHandler(maxRestartCount) {
        if (maxRestartCount !== undefined && maxRestartCount < 0) {
            throw new Error(`Invalid maxRestartCount: ${maxRestartCount}`);
        }
        return new DefaultErrorHandler(this, maxRestartCount ?? 4);
    }
    async setTrace(value) {
        this._trace = value;
        const connection = this.activeConnection();
        if (connection !== undefined) {
            await connection.trace(this._trace, this._tracer, {
                sendNotification: false,
                traceFormat: this._traceFormat
            });
        }
    }
    data2String(data) {
        if (data instanceof vscode_languageserver_protocol_1.ResponseError) {
            const responseError = data;
            return `  Message: ${responseError.message}\n  Code: ${responseError.code} ${responseError.data ? '\n' + responseError.data.toString() : ''}`;
        }
        if (data instanceof Error) {
            if (Is.string(data.stack)) {
                return data.stack;
            }
            return data.message;
        }
        if (Is.string(data)) {
            return data;
        }
        return data.toString();
    }
    info(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Info  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data !== null && data !== undefined) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Info) {
            this.showNotificationMessage(vscode_languageserver_protocol_1.MessageType.Info, message);
        }
    }
    warn(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Warn  - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data !== null && data !== undefined) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Warn) {
            this.showNotificationMessage(vscode_languageserver_protocol_1.MessageType.Warning, message);
        }
    }
    error(message, data, showNotification = true) {
        this.outputChannel.appendLine(`[Error - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data !== null && data !== undefined) {
            this.outputChannel.appendLine(this.data2String(data));
        }
        if (showNotification === 'force' || (showNotification && this._clientOptions.revealOutputChannelOn <= RevealOutputChannelOn.Error)) {
            this.showNotificationMessage(vscode_languageserver_protocol_1.MessageType.Error, message);
        }
    }
    showNotificationMessage(type, message) {
        message = message ?? 'A request has failed. See the output for more information.';
        const messageFunc = type === vscode_languageserver_protocol_1.MessageType.Error
            ? vscode_1.window.showErrorMessage
            : type === vscode_languageserver_protocol_1.MessageType.Warning
                ? vscode_1.window.showWarningMessage
                : vscode_1.window.showInformationMessage;
        void messageFunc(message, 'Go to output').then((selection) => {
            if (selection !== undefined) {
                this.outputChannel.show(true);
            }
        });
    }
    logTrace(message, data) {
        this.traceOutputChannel.appendLine(`[Trace - ${(new Date().toLocaleTimeString())}] ${message}`);
        if (data) {
            this.traceOutputChannel.appendLine(this.data2String(data));
        }
    }
    logObjectTrace(data) {
        if (data.isLSPMessage && data.type) {
            this.traceOutputChannel.append(`[LSP   - ${(new Date().toLocaleTimeString())}] `);
        }
        else {
            this.traceOutputChannel.append(`[Trace - ${(new Date().toLocaleTimeString())}] `);
        }
        if (data) {
            this.traceOutputChannel.appendLine(`${JSON.stringify(data)}`);
        }
    }
    needsStart() {
        return this.$state === ClientState.Initial || this.$state === ClientState.Stopping || this.$state === ClientState.Stopped;
    }
    needsStop() {
        return this.$state === ClientState.Starting || this.$state === ClientState.Running;
    }
    activeConnection() {
        return this.$state === ClientState.Running && this._connection !== undefined ? this._connection : undefined;
    }
    isRunning() {
        return this.$state === ClientState.Running;
    }
    async start() {
        if (this._onStart !== undefined) {
            return this._onStart;
        }
        const [promise, resolve, reject] = this.createOnStartPromise();
        this._onStart = promise;
        // We are currently stopping the language client. Await the stop
        // before continuing.
        if (this._onStop !== undefined) {
            await this._onStop;
            this._onStop = undefined;
        }
        // If we restart then the diagnostics collection is reused.
        if (this._diagnostics === undefined) {
            this._diagnostics = this._clientOptions.diagnosticCollectionName
                ? vscode_1.languages.createDiagnosticCollection(this._clientOptions.diagnosticCollectionName)
                : vscode_1.languages.createDiagnosticCollection();
        }
        // When we start make all buffer handlers pending so that they
        // get added.
        for (const [method, handler] of this._notificationHandlers) {
            if (!this._pendingNotificationHandlers.has(method)) {
                this._pendingNotificationHandlers.set(method, handler);
            }
        }
        for (const [method, handler] of this._requestHandlers) {
            if (!this._pendingRequestHandlers.has(method)) {
                this._pendingRequestHandlers.set(method, handler);
            }
        }
        for (const [token, data] of this._progressHandlers) {
            if (!this._pendingProgressHandlers.has(token)) {
                this._pendingProgressHandlers.set(token, data);
            }
        }
        this.$state = ClientState.Starting;
        try {
            const connection = await this.createConnection();
            connection.onNotification(vscode_languageserver_protocol_1.LogMessageNotification.type, (message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        this.error(message.message, undefined, false);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        this.warn(message.message, undefined, false);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        this.info(message.message, undefined, false);
                        break;
                    default:
                        this.outputChannel.appendLine(message.message);
                }
            });
            connection.onNotification(vscode_languageserver_protocol_1.ShowMessageNotification.type, (message) => {
                switch (message.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        void vscode_1.window.showErrorMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        void vscode_1.window.showWarningMessage(message.message);
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        void vscode_1.window.showInformationMessage(message.message);
                        break;
                    default:
                        void vscode_1.window.showInformationMessage(message.message);
                }
            });
            connection.onRequest(vscode_languageserver_protocol_1.ShowMessageRequest.type, (params) => {
                let messageFunc;
                switch (params.type) {
                    case vscode_languageserver_protocol_1.MessageType.Error:
                        messageFunc = vscode_1.window.showErrorMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Warning:
                        messageFunc = vscode_1.window.showWarningMessage;
                        break;
                    case vscode_languageserver_protocol_1.MessageType.Info:
                        messageFunc = vscode_1.window.showInformationMessage;
                        break;
                    default:
                        messageFunc = vscode_1.window.showInformationMessage;
                }
                let actions = params.actions || [];
                return messageFunc(params.message, ...actions);
            });
            connection.onNotification(vscode_languageserver_protocol_1.TelemetryEventNotification.type, (data) => {
                this._telemetryEmitter.fire(data);
            });
            connection.onRequest(vscode_languageserver_protocol_1.ShowDocumentRequest.type, async (params) => {
                const showDocument = async (params) => {
                    const uri = this.protocol2CodeConverter.asUri(params.uri);
                    try {
                        if (params.external === true) {
                            const success = await vscode_1.env.openExternal(uri);
                            return { success };
                        }
                        else {
                            const options = {};
                            if (params.selection !== undefined) {
                                options.selection = this.protocol2CodeConverter.asRange(params.selection);
                            }
                            if (params.takeFocus === undefined || params.takeFocus === false) {
                                options.preserveFocus = true;
                            }
                            else if (params.takeFocus === true) {
                                options.preserveFocus = false;
                            }
                            await vscode_1.window.showTextDocument(uri, options);
                            return { success: true };
                        }
                    }
                    catch (error) {
                        return { success: true };
                    }
                };
                const middleware = this._clientOptions.middleware.window?.showDocument;
                if (middleware !== undefined) {
                    return middleware(params, showDocument);
                }
                else {
                    return showDocument(params);
                }
            });
            connection.listen();
            await this.initialize(connection);
            resolve();
        }
        catch (error) {
            this.$state = ClientState.StartFailed;
            this.error(`${this._name} client: couldn't create connection to server.`, error, 'force');
            reject(error);
        }
        return this._onStart;
    }
    createOnStartPromise() {
        let resolve;
        let reject;
        const promise = new Promise((_resolve, _reject) => {
            resolve = _resolve;
            reject = _reject;
        });
        return [promise, resolve, reject];
    }
    async initialize(connection) {
        this.refreshTrace(connection, false);
        const initOption = this._clientOptions.initializationOptions;
        // If the client is locked to a workspace folder use it. In this case the workspace folder
        // feature is not registered and we need to initialize the value here.
        const [rootPath, workspaceFolders] = this._clientOptions.workspaceFolder !== undefined
            ? [this._clientOptions.workspaceFolder.uri.fsPath, [{ uri: this._c2p.asUri(this._clientOptions.workspaceFolder.uri), name: this._clientOptions.workspaceFolder.name }]]
            : [this._clientGetRootPath(), null];
        const initParams = {
            processId: null,
            clientInfo: {
                name: vscode_1.env.appName,
                version: vscode_1.version
            },
            locale: this.getLocale(),
            rootPath: rootPath ? rootPath : null,
            rootUri: rootPath ? this._c2p.asUri(vscode_1.Uri.file(rootPath)) : null,
            capabilities: this.computeClientCapabilities(),
            initializationOptions: Is.func(initOption) ? initOption() : initOption,
            trace: vscode_languageserver_protocol_1.Trace.toString(this._trace),
            workspaceFolders: workspaceFolders
        };
        this.fillInitializeParams(initParams);
        if (this._clientOptions.progressOnInitialization) {
            const token = UUID.generateUuid();
            const part = new progressPart_1.ProgressPart(connection, token);
            initParams.workDoneToken = token;
            try {
                const result = await this.doInitialize(connection, initParams);
                part.done();
                return result;
            }
            catch (error) {
                part.cancel();
                throw error;
            }
        }
        else {
            return this.doInitialize(connection, initParams);
        }
    }
    async doInitialize(connection, initParams) {
        try {
            const result = await connection.initialize(initParams);
            if (result.capabilities.positionEncoding !== undefined && result.capabilities.positionEncoding !== vscode_languageserver_protocol_1.PositionEncodingKind.UTF16) {
                throw new Error(`Unsupported position encoding (${result.capabilities.positionEncoding}) received from server ${this.name}`);
            }
            this._initializeResult = result;
            this.$state = ClientState.Running;
            let textDocumentSyncOptions = undefined;
            if (Is.number(result.capabilities.textDocumentSync)) {
                if (result.capabilities.textDocumentSync === vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
                    textDocumentSyncOptions = {
                        openClose: false,
                        change: vscode_languageserver_protocol_1.TextDocumentSyncKind.None,
                        save: undefined
                    };
                }
                else {
                    textDocumentSyncOptions = {
                        openClose: true,
                        change: result.capabilities.textDocumentSync,
                        save: {
                            includeText: false
                        }
                    };
                }
            }
            else if (result.capabilities.textDocumentSync !== undefined && result.capabilities.textDocumentSync !== null) {
                textDocumentSyncOptions = result.capabilities.textDocumentSync;
            }
            this._capabilities = Object.assign({}, result.capabilities, { resolvedTextDocumentSync: textDocumentSyncOptions });
            connection.onNotification(vscode_languageserver_protocol_1.PublishDiagnosticsNotification.type, params => this.handleDiagnostics(params));
            connection.onRequest(vscode_languageserver_protocol_1.RegistrationRequest.type, params => this.handleRegistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/registerFeature', params => this.handleRegistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.UnregistrationRequest.type, params => this.handleUnregistrationRequest(params));
            // See https://github.com/Microsoft/vscode-languageserver-node/issues/199
            connection.onRequest('client/unregisterFeature', params => this.handleUnregistrationRequest(params));
            connection.onRequest(vscode_languageserver_protocol_1.ApplyWorkspaceEditRequest.type, params => this.handleApplyWorkspaceEdit(params));
            // Add pending notification, request and progress handlers.
            for (const [method, handler] of this._pendingNotificationHandlers) {
                this._notificationDisposables.set(method, connection.onNotification(method, handler));
            }
            this._pendingNotificationHandlers.clear();
            for (const [method, handler] of this._pendingRequestHandlers) {
                this._requestDisposables.set(method, connection.onRequest(method, handler));
            }
            this._pendingRequestHandlers.clear();
            for (const [token, data] of this._pendingProgressHandlers) {
                this._progressDisposables.set(token, connection.onProgress(data.type, token, data.handler));
            }
            this._pendingProgressHandlers.clear();
            // if (this._clientOptions.suspend.mode !== SuspendMode.off) {
            // 	this._idleInterval =  RAL().timer.setInterval(() => this.checkSuspend(), this._clientOptions.suspend.interval);
            // }
            await connection.sendNotification(vscode_languageserver_protocol_1.InitializedNotification.type, {});
            this.hookFileEvents(connection);
            this.hookConfigurationChanged(connection);
            this.initializeFeatures(connection);
            return result;
        }
        catch (error) {
            if (this._clientOptions.initializationFailedHandler) {
                if (this._clientOptions.initializationFailedHandler(error)) {
                    void this.initialize(connection);
                }
                else {
                    void this.stop();
                }
            }
            else if (error instanceof vscode_languageserver_protocol_1.ResponseError && error.data && error.data.retry) {
                void vscode_1.window.showErrorMessage(error.message, { title: 'Retry', id: 'retry' }).then(item => {
                    if (item && item.id === 'retry') {
                        void this.initialize(connection);
                    }
                    else {
                        void this.stop();
                    }
                });
            }
            else {
                if (error && error.message) {
                    void vscode_1.window.showErrorMessage(error.message);
                }
                this.error('Server initialization failed.', error);
                void this.stop();
            }
            throw error;
        }
    }
    _clientGetRootPath() {
        let folders = vscode_1.workspace.workspaceFolders;
        if (!folders || folders.length === 0) {
            return undefined;
        }
        let folder = folders[0];
        if (folder.uri.scheme === 'file') {
            return folder.uri.fsPath;
        }
        return undefined;
    }
    stop(timeout = 2000) {
        // Wait 2 seconds on stop
        return this.shutdown('stop', timeout);
    }
    suspend() {
        // Wait 5 seconds on suspend.
        return this.shutdown('suspend', 5000);
    }
    async shutdown(mode, timeout) {
        // If the client is in stopped state simple return.
        // It could also be that the client failed to stop
        // which puts it into the stopped state as well.
        if (this.$state === ClientState.Stopped || this.$state === ClientState.Initial) {
            return;
        }
        if (this.$state === ClientState.Stopping && this._onStop) {
            return this._onStop;
        }
        // To ensure proper shutdown we need a proper created connection.
        const connection = await this.$start();
        this._initializeResult = undefined;
        this.$state = ClientState.Stopping;
        this.cleanUp(mode);
        const tp = new Promise(c => { (0, vscode_languageserver_protocol_1.RAL)().timer.setTimeout(c, timeout); });
        const shutdown = (async (connection) => {
            await connection.shutdown();
            await connection.exit();
            return connection;
        })(connection);
        return this._onStop = Promise.race([tp, shutdown]).then((connection) => {
            // The connection won the race with the timeout.
            if (connection !== undefined) {
                connection.end();
                connection.dispose();
            }
            else {
                this.error(`Stopping server timed out`, undefined, false);
                throw new Error(`Stopping the server timed out`);
            }
        }, (error) => {
            this.error(`Stopping server failed`, error, false);
            throw error;
        }).finally(() => {
            this.$state = ClientState.Stopped;
            mode === 'stop' && this.cleanUpChannel();
            this._onStart = undefined;
            this._onStop = undefined;
            this._connection = undefined;
            this._ignoredRegistrations.clear();
        });
    }
    cleanUp(mode) {
        // purge outstanding file events.
        this._fileEvents = [];
        this._fileEventDelayer.cancel();
        if (this._syncedDocuments) {
            this._syncedDocuments.clear();
        }
        // Dispose features in reverse order;
        for (const feature of Array.from(this._features.entries()).map(entry => entry[1]).reverse()) {
            feature.dispose();
        }
        if (mode === 'stop' && this._diagnostics !== undefined) {
            this._diagnostics.dispose();
            this._diagnostics = undefined;
        }
        if (this._idleInterval !== undefined) {
            this._idleInterval.dispose();
            this._idleInterval = undefined;
        }
        // this._idleStart = undefined;
    }
    cleanUpChannel() {
        if (this._outputChannel !== undefined && this._disposeOutputChannel) {
            this._outputChannel.dispose();
            this._outputChannel = undefined;
        }
    }
    notifyFileEvent(event) {
        const client = this;
        async function didChangeWatchedFile(event) {
            client._fileEvents.push(event);
            return client._fileEventDelayer.trigger(async () => {
                const connection = await client.$start();
                client.forceDocumentSync();
                const result = connection.sendNotification(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type, { changes: client._fileEvents });
                client._fileEvents = [];
                return result;
            });
        }
        const workSpaceMiddleware = this.clientOptions.middleware?.workspace;
        (workSpaceMiddleware?.didChangeWatchedFile ? workSpaceMiddleware.didChangeWatchedFile(event, didChangeWatchedFile) : didChangeWatchedFile(event)).catch((error) => {
            client.error(`Notify file events failed.`, error);
        });
    }
    forceDocumentSync() {
        if (this._didChangeTextDocumentFeature === undefined) {
            this._didChangeTextDocumentFeature = this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type.method);
        }
        this._didChangeTextDocumentFeature.forceDelivery();
    }
    handleDiagnostics(params) {
        if (!this._diagnostics) {
            return;
        }
        const key = params.uri;
        if (this._diagnosticQueueState.state === 'busy' && this._diagnosticQueueState.document === key) {
            // Cancel the active run;
            this._diagnosticQueueState.tokenSource.cancel();
        }
        this._diagnosticQueue.set(params.uri, params.diagnostics);
        this.triggerDiagnosticQueue();
    }
    triggerDiagnosticQueue() {
        (0, vscode_languageserver_protocol_1.RAL)().timer.setImmediate(() => { this.workDiagnosticQueue(); });
    }
    workDiagnosticQueue() {
        if (this._diagnosticQueueState.state === 'busy') {
            return;
        }
        const next = this._diagnosticQueue.entries().next();
        if (next.done === true) {
            // Nothing in the queue
            return;
        }
        const [document, diagnostics] = next.value;
        this._diagnosticQueue.delete(document);
        const tokenSource = new vscode_1.CancellationTokenSource();
        this._diagnosticQueueState = { state: 'busy', document: document, tokenSource };
        this._p2c.asDiagnostics(diagnostics, tokenSource.token).then((converted) => {
            if (!tokenSource.token.isCancellationRequested) {
                const uri = this._p2c.asUri(document);
                const middleware = this.clientOptions.middleware;
                if (middleware.handleDiagnostics) {
                    middleware.handleDiagnostics(uri, converted, (uri, diagnostics) => this.setDiagnostics(uri, diagnostics));
                }
                else {
                    this.setDiagnostics(uri, converted);
                }
            }
        }).finally(() => {
            this._diagnosticQueueState = { state: 'idle' };
            this.triggerDiagnosticQueue();
        });
    }
    setDiagnostics(uri, diagnostics) {
        if (!this._diagnostics) {
            return;
        }
        this._diagnostics.set(uri, diagnostics);
    }
    async $start() {
        if (this.$state === ClientState.StartFailed) {
            throw new Error(`Previous start failed. Can't restart server.`);
        }
        await this.start();
        const connection = this.activeConnection();
        if (connection === undefined) {
            throw new Error(`Starting server failed`);
        }
        return connection;
    }
    async createConnection() {
        let errorHandler = (error, message, count) => {
            this.handleConnectionError(error, message, count);
        };
        let closeHandler = () => {
            this.handleConnectionClosed();
        };
        const transports = await this.createMessageTransports(this._clientOptions.stdioEncoding || 'utf8');
        this._connection = createConnection(transports.reader, transports.writer, errorHandler, closeHandler, this._clientOptions.connectionOptions);
        return this._connection;
    }
    handleConnectionClosed() {
        // Check whether this is a normal shutdown in progress or the client stopped normally.
        if (this.$state === ClientState.Stopped) {
            return;
        }
        try {
            if (this._connection !== undefined) {
                this._connection.dispose();
            }
        }
        catch (error) {
            // Disposing a connection could fail if error cases.
        }
        let handlerResult = { action: CloseAction.DoNotRestart };
        if (this.$state !== ClientState.Stopping) {
            try {
                handlerResult = this._clientOptions.errorHandler.closed();
            }
            catch (error) {
                // Ignore errors coming from the error handler.
            }
        }
        this._connection = undefined;
        if (handlerResult.action === CloseAction.DoNotRestart) {
            this.error(handlerResult.message ?? 'Connection to server got closed. Server will not be restarted.', undefined, 'force');
            this.cleanUp('stop');
            if (this.$state === ClientState.Starting) {
                this.$state = ClientState.StartFailed;
            }
            else {
                this.$state = ClientState.Stopped;
            }
            this._onStop = Promise.resolve();
            this._onStart = undefined;
        }
        else if (handlerResult.action === CloseAction.Restart) {
            this.info(handlerResult.message ?? 'Connection to server got closed. Server will restart.');
            this.cleanUp('restart');
            this.$state = ClientState.Initial;
            this._onStop = Promise.resolve();
            this._onStart = undefined;
            this.start().catch((error) => this.error(`Restarting server failed`, error, 'force'));
        }
    }
    handleConnectionError(error, message, count) {
        const handlerResult = this._clientOptions.errorHandler.error(error, message, count);
        if (handlerResult.action === ErrorAction.Shutdown) {
            this.error(handlerResult.message ?? `Client ${this._name}: connection to server is erroring. Shutting down server.`, undefined, 'force');
            this.stop().catch((error) => {
                this.error(`Stopping server failed`, error, false);
            });
        }
    }
    hookConfigurationChanged(connection) {
        vscode_1.workspace.onDidChangeConfiguration(() => {
            this.refreshTrace(connection, true);
        });
    }
    refreshTrace(connection, sendNotification = false) {
        const config = vscode_1.workspace.getConfiguration(this._id);
        let trace = vscode_languageserver_protocol_1.Trace.Off;
        let traceFormat = vscode_languageserver_protocol_1.TraceFormat.Text;
        if (config) {
            const traceConfig = config.get('trace.server', 'off');
            if (typeof traceConfig === 'string') {
                trace = vscode_languageserver_protocol_1.Trace.fromString(traceConfig);
            }
            else {
                trace = vscode_languageserver_protocol_1.Trace.fromString(config.get('trace.server.verbosity', 'off'));
                traceFormat = vscode_languageserver_protocol_1.TraceFormat.fromString(config.get('trace.server.format', 'text'));
            }
        }
        this._trace = trace;
        this._traceFormat = traceFormat;
        connection.trace(this._trace, this._tracer, {
            sendNotification,
            traceFormat: this._traceFormat
        }).catch((error) => { this.error(`Updating trace failed with error`, error, false); });
    }
    hookFileEvents(_connection) {
        let fileEvents = this._clientOptions.synchronize.fileEvents;
        if (!fileEvents) {
            return;
        }
        let watchers;
        if (Is.array(fileEvents)) {
            watchers = fileEvents;
        }
        else {
            watchers = [fileEvents];
        }
        if (!watchers) {
            return;
        }
        this._dynamicFeatures.get(vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type.method).registerRaw(UUID.generateUuid(), watchers);
    }
    registerFeatures(features) {
        for (let feature of features) {
            this.registerFeature(feature);
        }
    }
    registerFeature(feature) {
        this._features.push(feature);
        if (features_1.DynamicFeature.is(feature)) {
            const registrationType = feature.registrationType;
            this._dynamicFeatures.set(registrationType.method, feature);
        }
    }
    getFeature(request) {
        return this._dynamicFeatures.get(request);
    }
    hasDedicatedTextSynchronizationFeature(textDocument) {
        const feature = this.getFeature(vscode_languageserver_protocol_1.NotebookDocumentSyncRegistrationType.method);
        if (feature === undefined || !(feature instanceof notebook_1.NotebookDocumentSyncFeature)) {
            return false;
        }
        return feature.handles(textDocument);
    }
    registerBuiltinFeatures() {
        this.registerFeature(new configuration_1.ConfigurationFeature(this));
        this.registerFeature(new textSynchronization_1.DidOpenTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new textSynchronization_1.DidChangeTextDocumentFeature(this));
        this.registerFeature(new textSynchronization_1.WillSaveFeature(this));
        this.registerFeature(new textSynchronization_1.WillSaveWaitUntilFeature(this));
        this.registerFeature(new textSynchronization_1.DidSaveTextDocumentFeature(this));
        this.registerFeature(new textSynchronization_1.DidCloseTextDocumentFeature(this, this._syncedDocuments));
        this.registerFeature(new fileSystemWatcher_1.FileSystemWatcherFeature(this, (event) => this.notifyFileEvent(event)));
        this.registerFeature(new completion_1.CompletionItemFeature(this));
        this.registerFeature(new hover_1.HoverFeature(this));
        this.registerFeature(new signatureHelp_1.SignatureHelpFeature(this));
        this.registerFeature(new definition_1.DefinitionFeature(this));
        this.registerFeature(new reference_1.ReferencesFeature(this));
        this.registerFeature(new documentHighlight_1.DocumentHighlightFeature(this));
        this.registerFeature(new documentSymbol_1.DocumentSymbolFeature(this));
        this.registerFeature(new workspaceSymbol_1.WorkspaceSymbolFeature(this));
        this.registerFeature(new codeAction_1.CodeActionFeature(this));
        this.registerFeature(new codeLens_1.CodeLensFeature(this));
        this.registerFeature(new formatting_1.DocumentFormattingFeature(this));
        this.registerFeature(new formatting_1.DocumentRangeFormattingFeature(this));
        this.registerFeature(new formatting_1.DocumentOnTypeFormattingFeature(this));
        this.registerFeature(new rename_1.RenameFeature(this));
        this.registerFeature(new documentLink_1.DocumentLinkFeature(this));
        this.registerFeature(new executeCommand_1.ExecuteCommandFeature(this));
        this.registerFeature(new configuration_1.SyncConfigurationFeature(this));
        this.registerFeature(new configuration_2.ConfigurationFeature(this));
        this.registerFeature(new typeDefinition_1.TypeDefinitionFeature(this));
        this.registerFeature(new implementation_1.ImplementationFeature(this));
        this.registerFeature(new colorProvider_1.ColorProviderFeature(this));
        // We only register the workspace folder feature if the client is not locked
        // to a specific workspace folder.
        if (this.clientOptions.workspaceFolder === undefined) {
            this.registerFeature(new workspaceFolder_1.WorkspaceFoldersFeature(this));
        }
        this.registerFeature(new foldingRange_1.FoldingRangeFeature(this));
        this.registerFeature(new declaration_1.DeclarationFeature(this));
        this.registerFeature(new selectionRange_1.SelectionRangeFeature(this));
        this.registerFeature(new progress_1.ProgressFeature(this));
        this.registerFeature(new callHierarchy_1.CallHierarchyFeature(this));
        this.registerFeature(new semanticTokens_1.SemanticTokensFeature(this));
        this.registerFeature(new linkedEditingRange_1.LinkedEditingFeature(this));
        this.registerFeature(new fileOperations_1.DidCreateFilesFeature(this));
        this.registerFeature(new fileOperations_1.DidRenameFilesFeature(this));
        this.registerFeature(new fileOperations_1.DidDeleteFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillCreateFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillRenameFilesFeature(this));
        this.registerFeature(new fileOperations_1.WillDeleteFilesFeature(this));
        this.registerFeature(new typeHierarchy_1.TypeHierarchyFeature(this));
        this.registerFeature(new inlineValue_1.InlineValueFeature(this));
        this.registerFeature(new inlayHint_1.InlayHintsFeature(this));
        this.registerFeature(new diagnostic_1.DiagnosticFeature(this));
        this.registerFeature(new notebook_1.NotebookDocumentSyncFeature(this));
    }
    registerProposedFeatures() {
        this.registerFeatures(ProposedFeatures.createAll(this));
    }
    fillInitializeParams(params) {
        for (let feature of this._features) {
            if (Is.func(feature.fillInitializeParams)) {
                feature.fillInitializeParams(params);
            }
        }
    }
    computeClientCapabilities() {
        const result = {};
        (0, features_1.ensure)(result, 'workspace').applyEdit = true;
        const workspaceEdit = (0, features_1.ensure)((0, features_1.ensure)(result, 'workspace'), 'workspaceEdit');
        workspaceEdit.documentChanges = true;
        workspaceEdit.resourceOperations = [vscode_languageserver_protocol_1.ResourceOperationKind.Create, vscode_languageserver_protocol_1.ResourceOperationKind.Rename, vscode_languageserver_protocol_1.ResourceOperationKind.Delete];
        workspaceEdit.failureHandling = vscode_languageserver_protocol_1.FailureHandlingKind.TextOnlyTransactional;
        workspaceEdit.normalizesLineEndings = true;
        workspaceEdit.changeAnnotationSupport = {
            groupsOnLabel: true
        };
        const diagnostics = (0, features_1.ensure)((0, features_1.ensure)(result, 'textDocument'), 'publishDiagnostics');
        diagnostics.relatedInformation = true;
        diagnostics.versionSupport = false;
        diagnostics.tagSupport = { valueSet: [vscode_languageserver_protocol_1.DiagnosticTag.Unnecessary, vscode_languageserver_protocol_1.DiagnosticTag.Deprecated] };
        diagnostics.codeDescriptionSupport = true;
        diagnostics.dataSupport = true;
        const windowCapabilities = (0, features_1.ensure)(result, 'window');
        const showMessage = (0, features_1.ensure)(windowCapabilities, 'showMessage');
        showMessage.messageActionItem = { additionalPropertiesSupport: true };
        const showDocument = (0, features_1.ensure)(windowCapabilities, 'showDocument');
        showDocument.support = true;
        const generalCapabilities = (0, features_1.ensure)(result, 'general');
        generalCapabilities.staleRequestSupport = {
            cancel: true,
            retryOnContentModified: Array.from(BaseLanguageClient.RequestsToCancelOnContentModified)
        };
        generalCapabilities.regularExpressions = { engine: 'ECMAScript', version: 'ES2020' };
        generalCapabilities.markdown = {
            parser: 'marked',
            version: '1.1.0',
        };
        generalCapabilities.positionEncodings = ['utf-16'];
        if (this._clientOptions.markdown.supportHtml) {
            generalCapabilities.markdown.allowedTags = ['ul', 'li', 'p', 'code', 'blockquote', 'ol', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr', 'em', 'pre', 'table', 'thead', 'tbody', 'tr', 'th', 'td', 'div', 'del', 'a', 'strong', 'br', 'img', 'span'];
        }
        for (let feature of this._features) {
            feature.fillClientCapabilities(result);
        }
        return result;
    }
    initializeFeatures(_connection) {
        const documentSelector = this._clientOptions.documentSelector;
        for (const feature of this._features) {
            if (Is.func(feature.preInitialize)) {
                feature.preInitialize(this._capabilities, documentSelector);
            }
        }
        for (const feature of this._features) {
            feature.initialize(this._capabilities, documentSelector);
        }
    }
    async handleRegistrationRequest(params) {
        // We will not receive a registration call before a client is running
        // from a server. However if we stop or shutdown we might which might
        // try to restart the server. So ignore registrations if we are not running
        if (!this.isRunning()) {
            for (const registration of params.registrations) {
                this._ignoredRegistrations.add(registration.id);
            }
            return;
        }
        for (const registration of params.registrations) {
            const feature = this._dynamicFeatures.get(registration.method);
            if (feature === undefined) {
                return Promise.reject(new Error(`No feature implementation for ${registration.method} found. Registration failed.`));
            }
            const options = registration.registerOptions ?? {};
            options.documentSelector = options.documentSelector ?? this._clientOptions.documentSelector;
            const data = {
                id: registration.id,
                registerOptions: options
            };
            try {
                feature.register(data);
            }
            catch (err) {
                return Promise.reject(err);
            }
        }
    }
    async handleUnregistrationRequest(params) {
        for (let unregistration of params.unregisterations) {
            if (this._ignoredRegistrations.has(unregistration.id)) {
                continue;
            }
            const feature = this._dynamicFeatures.get(unregistration.method);
            if (!feature) {
                return Promise.reject(new Error(`No feature implementation for ${unregistration.method} found. Unregistration failed.`));
            }
            feature.unregister(unregistration.id);
        }
    }
    async handleApplyWorkspaceEdit(params) {
        const workspaceEdit = params.edit;
        // Make sure we convert workspace edits one after the other. Otherwise
        // we might execute a workspace edit received first after we received another
        // one since the conversion might race.
        const converted = await this.workspaceEditLock.lock(() => {
            return this._p2c.asWorkspaceEdit(workspaceEdit);
        });
        // This is some sort of workaround since the version check should be done by VS Code in the Workspace.applyEdit.
        // However doing it here adds some safety since the server can lag more behind then an extension.
        const openTextDocuments = new Map();
        vscode_1.workspace.textDocuments.forEach((document) => openTextDocuments.set(document.uri.toString(), document));
        let versionMismatch = false;
        if (workspaceEdit.documentChanges) {
            for (const change of workspaceEdit.documentChanges) {
                if (vscode_languageserver_protocol_1.TextDocumentEdit.is(change) && change.textDocument.version && change.textDocument.version >= 0) {
                    const textDocument = openTextDocuments.get(change.textDocument.uri);
                    if (textDocument && textDocument.version !== change.textDocument.version) {
                        versionMismatch = true;
                        break;
                    }
                }
            }
        }
        if (versionMismatch) {
            return Promise.resolve({ applied: false });
        }
        return Is.asPromise(vscode_1.workspace.applyEdit(converted).then((value) => { return { applied: value }; }));
    }
    handleFailedRequest(type, token, error, defaultValue, showNotification = true) {
        // If we get a request cancel or a content modified don't log anything.
        if (error instanceof vscode_languageserver_protocol_1.ResponseError) {
            // The connection got disposed while we were waiting for a response.
            // Simply return the default value. Is the best we can do.
            if (error.code === vscode_languageserver_protocol_1.ErrorCodes.PendingResponseRejected || error.code === vscode_languageserver_protocol_1.ErrorCodes.ConnectionInactive) {
                return defaultValue;
            }
            if (error.code === vscode_languageserver_protocol_1.LSPErrorCodes.RequestCancelled || error.code === vscode_languageserver_protocol_1.LSPErrorCodes.ServerCancelled) {
                if (token !== undefined && token.isCancellationRequested) {
                    return defaultValue;
                }
                else {
                    if (error.data !== undefined) {
                        throw new features_1.LSPCancellationError(error.data);
                    }
                    else {
                        throw new vscode_1.CancellationError();
                    }
                }
            }
            else if (error.code === vscode_languageserver_protocol_1.LSPErrorCodes.ContentModified) {
                if (BaseLanguageClient.RequestsToCancelOnContentModified.has(type.method)) {
                    throw new vscode_1.CancellationError();
                }
                else {
                    return defaultValue;
                }
            }
        }
        this.error(`Request ${type.method} failed.`, error, showNotification);
        throw error;
    }
}
exports.BaseLanguageClient = BaseLanguageClient;
BaseLanguageClient.RequestsToCancelOnContentModified = new Set([
    vscode_languageserver_protocol_1.SemanticTokensRequest.method,
    vscode_languageserver_protocol_1.SemanticTokensRangeRequest.method,
    vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.method
]);
class ConsoleLogger {
    error(message) {
        (0, vscode_languageserver_protocol_1.RAL)().console.error(message);
    }
    warn(message) {
        (0, vscode_languageserver_protocol_1.RAL)().console.warn(message);
    }
    info(message) {
        (0, vscode_languageserver_protocol_1.RAL)().console.info(message);
    }
    log(message) {
        (0, vscode_languageserver_protocol_1.RAL)().console.log(message);
    }
}
function createConnection(input, output, errorHandler, closeHandler, options) {
    let _lastUsed = -1;
    const logger = new ConsoleLogger();
    const connection = (0, vscode_languageserver_protocol_1.createProtocolConnection)(input, output, logger, options);
    connection.onError((data) => { errorHandler(data[0], data[1], data[2]); });
    connection.onClose(closeHandler);
    const result = {
        get lastUsed() {
            return _lastUsed;
        },
        resetLastUsed: () => {
            _lastUsed = -1;
        },
        listen: () => connection.listen(),
        sendRequest: (type, ...params) => {
            _lastUsed = Date.now();
            return connection.sendRequest(type, ...params);
        },
        onRequest: (type, handler) => connection.onRequest(type, handler),
        hasPendingResponse: () => connection.hasPendingResponse(),
        sendNotification: (type, params) => {
            _lastUsed = Date.now();
            return connection.sendNotification(type, params);
        },
        onNotification: (type, handler) => connection.onNotification(type, handler),
        onProgress: connection.onProgress,
        sendProgress: connection.sendProgress,
        trace: (value, tracer, sendNotificationOrTraceOptions) => {
            const defaultTraceOptions = {
                sendNotification: false,
                traceFormat: vscode_languageserver_protocol_1.TraceFormat.Text
            };
            if (sendNotificationOrTraceOptions === undefined) {
                return connection.trace(value, tracer, defaultTraceOptions);
            }
            else if (Is.boolean(sendNotificationOrTraceOptions)) {
                return connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
            else {
                return connection.trace(value, tracer, sendNotificationOrTraceOptions);
            }
        },
        initialize: (params) => {
            _lastUsed = Date.now();
            return connection.sendRequest(vscode_languageserver_protocol_1.InitializeRequest.type, params);
        },
        shutdown: () => {
            _lastUsed = Date.now();
            return connection.sendRequest(vscode_languageserver_protocol_1.ShutdownRequest.type, undefined);
        },
        exit: () => {
            _lastUsed = Date.now();
            return connection.sendNotification(vscode_languageserver_protocol_1.ExitNotification.type);
        },
        end: () => connection.end(),
        dispose: () => connection.dispose()
    };
    return result;
}
// Exporting proposed protocol.
var ProposedFeatures;
(function (ProposedFeatures) {
    function createAll(_client) {
        let result = [];
        return result;
    }
    ProposedFeatures.createAll = createAll;
})(ProposedFeatures = exports.ProposedFeatures || (exports.ProposedFeatures = {}));
//# sourceMappingURL=client.js.map

/***/ }),
/* 56 */
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createProtocolConnection = void 0;
const node_1 = __webpack_require__(57);
__exportStar(__webpack_require__(57), exports);
__exportStar(__webpack_require__(77), exports);
function createProtocolConnection(input, output, logger, options) {
    return (0, node_1.createMessageConnection)(input, output, logger, options);
}
exports.createProtocolConnection = createProtocolConnection;
//# sourceMappingURL=main.js.map

/***/ }),
/* 57 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(58);

/***/ }),
/* 58 */
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createMessageConnection = exports.createServerSocketTransport = exports.createClientSocketTransport = exports.createServerPipeTransport = exports.createClientPipeTransport = exports.generateRandomPipeName = exports.StreamMessageWriter = exports.StreamMessageReader = exports.SocketMessageWriter = exports.SocketMessageReader = exports.IPCMessageWriter = exports.IPCMessageReader = void 0;
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */
const ril_1 = __webpack_require__(59);
// Install the node runtime abstract.
ril_1.default.install();
const api_1 = __webpack_require__(64);
const path = __webpack_require__(1);
const os = __webpack_require__(74);
const crypto_1 = __webpack_require__(75);
const net_1 = __webpack_require__(76);
__exportStar(__webpack_require__(64), exports);
class IPCMessageReader extends api_1.AbstractMessageReader {
    constructor(process) {
        super();
        this.process = process;
        let eventEmitter = this.process;
        eventEmitter.on('error', (error) => this.fireError(error));
        eventEmitter.on('close', () => this.fireClose());
    }
    listen(callback) {
        this.process.on('message', callback);
        return api_1.Disposable.create(() => this.process.off('message', callback));
    }
}
exports.IPCMessageReader = IPCMessageReader;
class IPCMessageWriter extends api_1.AbstractMessageWriter {
    constructor(process) {
        super();
        this.process = process;
        this.errorCount = 0;
        let eventEmitter = this.process;
        eventEmitter.on('error', (error) => this.fireError(error));
        eventEmitter.on('close', () => this.fireClose);
    }
    write(msg) {
        try {
            if (typeof this.process.send === 'function') {
                this.process.send(msg, undefined, undefined, (error) => {
                    if (error) {
                        this.errorCount++;
                        this.handleError(error, msg);
                    }
                    else {
                        this.errorCount = 0;
                    }
                });
            }
            return Promise.resolve();
        }
        catch (error) {
            this.handleError(error, msg);
            return Promise.reject(error);
        }
    }
    handleError(error, msg) {
        this.errorCount++;
        this.fireError(error, msg, this.errorCount);
    }
    end() {
    }
}
exports.IPCMessageWriter = IPCMessageWriter;
class SocketMessageReader extends api_1.ReadableStreamMessageReader {
    constructor(socket, encoding = 'utf-8') {
        super((0, ril_1.default)().stream.asReadableStream(socket), encoding);
    }
}
exports.SocketMessageReader = SocketMessageReader;
class SocketMessageWriter extends api_1.WriteableStreamMessageWriter {
    constructor(socket, options) {
        super((0, ril_1.default)().stream.asWritableStream(socket), options);
        this.socket = socket;
    }
    dispose() {
        super.dispose();
        this.socket.destroy();
    }
}
exports.SocketMessageWriter = SocketMessageWriter;
class StreamMessageReader extends api_1.ReadableStreamMessageReader {
    constructor(readble, encoding) {
        super((0, ril_1.default)().stream.asReadableStream(readble), encoding);
    }
}
exports.StreamMessageReader = StreamMessageReader;
class StreamMessageWriter extends api_1.WriteableStreamMessageWriter {
    constructor(writable, options) {
        super((0, ril_1.default)().stream.asWritableStream(writable), options);
    }
}
exports.StreamMessageWriter = StreamMessageWriter;
const XDG_RUNTIME_DIR = process.env['XDG_RUNTIME_DIR'];
const safeIpcPathLengths = new Map([
    ['linux', 107],
    ['darwin', 103]
]);
function generateRandomPipeName() {
    const randomSuffix = (0, crypto_1.randomBytes)(21).toString('hex');
    if (process.platform === 'win32') {
        return `\\\\.\\pipe\\vscode-jsonrpc-${randomSuffix}-sock`;
    }
    let result;
    if (XDG_RUNTIME_DIR) {
        result = path.join(XDG_RUNTIME_DIR, `vscode-ipc-${randomSuffix}.sock`);
    }
    else {
        result = path.join(os.tmpdir(), `vscode-${randomSuffix}.sock`);
    }
    const limit = safeIpcPathLengths.get(process.platform);
    if (limit !== undefined && result.length >= limit) {
        (0, ril_1.default)().console.warn(`WARNING: IPC handle "${result}" is longer than ${limit} characters.`);
    }
    return result;
}
exports.generateRandomPipeName = generateRandomPipeName;
function createClientPipeTransport(pipeName, encoding = 'utf-8') {
    let connectResolve;
    const connected = new Promise((resolve, _reject) => {
        connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
        let server = (0, net_1.createServer)((socket) => {
            server.close();
            connectResolve([
                new SocketMessageReader(socket, encoding),
                new SocketMessageWriter(socket, encoding)
            ]);
        });
        server.on('error', reject);
        server.listen(pipeName, () => {
            server.removeListener('error', reject);
            resolve({
                onConnected: () => { return connected; }
            });
        });
    });
}
exports.createClientPipeTransport = createClientPipeTransport;
function createServerPipeTransport(pipeName, encoding = 'utf-8') {
    const socket = (0, net_1.createConnection)(pipeName);
    return [
        new SocketMessageReader(socket, encoding),
        new SocketMessageWriter(socket, encoding)
    ];
}
exports.createServerPipeTransport = createServerPipeTransport;
function createClientSocketTransport(port, encoding = 'utf-8') {
    let connectResolve;
    const connected = new Promise((resolve, _reject) => {
        connectResolve = resolve;
    });
    return new Promise((resolve, reject) => {
        const server = (0, net_1.createServer)((socket) => {
            server.close();
            connectResolve([
                new SocketMessageReader(socket, encoding),
                new SocketMessageWriter(socket, encoding)
            ]);
        });
        server.on('error', reject);
        server.listen(port, '127.0.0.1', () => {
            server.removeListener('error', reject);
            resolve({
                onConnected: () => { return connected; }
            });
        });
    });
}
exports.createClientSocketTransport = createClientSocketTransport;
function createServerSocketTransport(port, encoding = 'utf-8') {
    const socket = (0, net_1.createConnection)(port, '127.0.0.1');
    return [
        new SocketMessageReader(socket, encoding),
        new SocketMessageWriter(socket, encoding)
    ];
}
exports.createServerSocketTransport = createServerSocketTransport;
function isReadableStream(value) {
    const candidate = value;
    return candidate.read !== undefined && candidate.addListener !== undefined;
}
function isWritableStream(value) {
    const candidate = value;
    return candidate.write !== undefined && candidate.addListener !== undefined;
}
function createMessageConnection(input, output, logger, options) {
    if (!logger) {
        logger = api_1.NullLogger;
    }
    const reader = isReadableStream(input) ? new StreamMessageReader(input) : input;
    const writer = isWritableStream(output) ? new StreamMessageWriter(output) : output;
    if (api_1.ConnectionStrategy.is(options)) {
        options = { connectionStrategy: options };
    }
    return (0, api_1.createMessageConnection)(reader, writer, logger, options);
}
exports.createMessageConnection = createMessageConnection;
//# sourceMappingURL=main.js.map

/***/ }),
/* 59 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const ral_1 = __webpack_require__(60);
const util_1 = __webpack_require__(61);
const disposable_1 = __webpack_require__(62);
const messageBuffer_1 = __webpack_require__(63);
class MessageBuffer extends messageBuffer_1.AbstractMessageBuffer {
    constructor(encoding = 'utf-8') {
        super(encoding);
    }
    emptyBuffer() {
        return MessageBuffer.emptyBuffer;
    }
    fromString(value, encoding) {
        return Buffer.from(value, encoding);
    }
    toString(value, encoding) {
        if (value instanceof Buffer) {
            return value.toString(encoding);
        }
        else {
            return new util_1.TextDecoder(encoding).decode(value);
        }
    }
    asNative(buffer, length) {
        if (length === undefined) {
            return buffer instanceof Buffer ? buffer : Buffer.from(buffer);
        }
        else {
            return buffer instanceof Buffer ? buffer.slice(0, length) : Buffer.from(buffer, 0, length);
        }
    }
    allocNative(length) {
        return Buffer.allocUnsafe(length);
    }
}
MessageBuffer.emptyBuffer = Buffer.allocUnsafe(0);
class ReadableStreamWrapper {
    constructor(stream) {
        this.stream = stream;
    }
    onClose(listener) {
        this.stream.on('close', listener);
        return disposable_1.Disposable.create(() => this.stream.off('close', listener));
    }
    onError(listener) {
        this.stream.on('error', listener);
        return disposable_1.Disposable.create(() => this.stream.off('error', listener));
    }
    onEnd(listener) {
        this.stream.on('end', listener);
        return disposable_1.Disposable.create(() => this.stream.off('end', listener));
    }
    onData(listener) {
        this.stream.on('data', listener);
        return disposable_1.Disposable.create(() => this.stream.off('data', listener));
    }
}
class WritableStreamWrapper {
    constructor(stream) {
        this.stream = stream;
    }
    onClose(listener) {
        this.stream.on('close', listener);
        return disposable_1.Disposable.create(() => this.stream.off('close', listener));
    }
    onError(listener) {
        this.stream.on('error', listener);
        return disposable_1.Disposable.create(() => this.stream.off('error', listener));
    }
    onEnd(listener) {
        this.stream.on('end', listener);
        return disposable_1.Disposable.create(() => this.stream.off('end', listener));
    }
    write(data, encoding) {
        return new Promise((resolve, reject) => {
            const callback = (error) => {
                if (error === undefined || error === null) {
                    resolve();
                }
                else {
                    reject(error);
                }
            };
            if (typeof data === 'string') {
                this.stream.write(data, encoding, callback);
            }
            else {
                this.stream.write(data, callback);
            }
        });
    }
    end() {
        this.stream.end();
    }
}
const _ril = Object.freeze({
    messageBuffer: Object.freeze({
        create: (encoding) => new MessageBuffer(encoding)
    }),
    applicationJson: Object.freeze({
        encoder: Object.freeze({
            name: 'application/json',
            encode: (msg, options) => {
                try {
                    return Promise.resolve(Buffer.from(JSON.stringify(msg, undefined, 0), options.charset));
                }
                catch (err) {
                    return Promise.reject(err);
                }
            }
        }),
        decoder: Object.freeze({
            name: 'application/json',
            decode: (buffer, options) => {
                try {
                    if (buffer instanceof Buffer) {
                        return Promise.resolve(JSON.parse(buffer.toString(options.charset)));
                    }
                    else {
                        return Promise.resolve(JSON.parse(new util_1.TextDecoder(options.charset).decode(buffer)));
                    }
                }
                catch (err) {
                    return Promise.reject(err);
                }
            }
        })
    }),
    stream: Object.freeze({
        asReadableStream: (stream) => new ReadableStreamWrapper(stream),
        asWritableStream: (stream) => new WritableStreamWrapper(stream)
    }),
    console: console,
    timer: Object.freeze({
        setTimeout(callback, ms, ...args) {
            const handle = setTimeout(callback, ms, ...args);
            return { dispose: () => clearTimeout(handle) };
        },
        setImmediate(callback, ...args) {
            const handle = setImmediate(callback, ...args);
            return { dispose: () => clearImmediate(handle) };
        },
        setInterval(callback, ms, ...args) {
            const handle = setInterval(callback, ms, ...args);
            return { dispose: () => clearInterval(handle) };
        }
    })
});
function RIL() {
    return _ril;
}
(function (RIL) {
    function install() {
        ral_1.default.install(_ril);
    }
    RIL.install = install;
})(RIL || (RIL = {}));
exports["default"] = RIL;
//# sourceMappingURL=ril.js.map

/***/ }),
/* 60 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
let _ral;
function RAL() {
    if (_ral === undefined) {
        throw new Error(`No runtime abstraction layer installed`);
    }
    return _ral;
}
(function (RAL) {
    function install(ral) {
        if (ral === undefined) {
            throw new Error(`No runtime abstraction layer provided`);
        }
        _ral = ral;
    }
    RAL.install = install;
})(RAL || (RAL = {}));
exports["default"] = RAL;
//# sourceMappingURL=ral.js.map

/***/ }),
/* 61 */
/***/ ((module) => {

"use strict";
module.exports = require("util");

/***/ }),
/* 62 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Disposable = void 0;
var Disposable;
(function (Disposable) {
    function create(func) {
        return {
            dispose: func
        };
    }
    Disposable.create = create;
})(Disposable = exports.Disposable || (exports.Disposable = {}));
//# sourceMappingURL=disposable.js.map

/***/ }),
/* 63 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.AbstractMessageBuffer = void 0;
const CR = 13;
const LF = 10;
const CRLF = '\r\n';
class AbstractMessageBuffer {
    constructor(encoding = 'utf-8') {
        this._encoding = encoding;
        this._chunks = [];
        this._totalLength = 0;
    }
    get encoding() {
        return this._encoding;
    }
    append(chunk) {
        const toAppend = typeof chunk === 'string' ? this.fromString(chunk, this._encoding) : chunk;
        this._chunks.push(toAppend);
        this._totalLength += toAppend.byteLength;
    }
    tryReadHeaders() {
        if (this._chunks.length === 0) {
            return undefined;
        }
        let state = 0;
        let chunkIndex = 0;
        let offset = 0;
        let chunkBytesRead = 0;
        row: while (chunkIndex < this._chunks.length) {
            const chunk = this._chunks[chunkIndex];
            offset = 0;
            column: while (offset < chunk.length) {
                const value = chunk[offset];
                switch (value) {
                    case CR:
                        switch (state) {
                            case 0:
                                state = 1;
                                break;
                            case 2:
                                state = 3;
                                break;
                            default:
                                state = 0;
                        }
                        break;
                    case LF:
                        switch (state) {
                            case 1:
                                state = 2;
                                break;
                            case 3:
                                state = 4;
                                offset++;
                                break row;
                            default:
                                state = 0;
                        }
                        break;
                    default:
                        state = 0;
                }
                offset++;
            }
            chunkBytesRead += chunk.byteLength;
            chunkIndex++;
        }
        if (state !== 4) {
            return undefined;
        }
        // The buffer contains the two CRLF at the end. So we will
        // have two empty lines after the split at the end as well.
        const buffer = this._read(chunkBytesRead + offset);
        const result = new Map();
        const headers = this.toString(buffer, 'ascii').split(CRLF);
        if (headers.length < 2) {
            return result;
        }
        for (let i = 0; i < headers.length - 2; i++) {
            const header = headers[i];
            const index = header.indexOf(':');
            if (index === -1) {
                throw new Error('Message header must separate key and value using :');
            }
            const key = header.substr(0, index);
            const value = header.substr(index + 1).trim();
            result.set(key, value);
        }
        return result;
    }
    tryReadBody(length) {
        if (this._totalLength < length) {
            return undefined;
        }
        return this._read(length);
    }
    get numberOfBytes() {
        return this._totalLength;
    }
    _read(byteCount) {
        if (byteCount === 0) {
            return this.emptyBuffer();
        }
        if (byteCount > this._totalLength) {
            throw new Error(`Cannot read so many bytes!`);
        }
        if (this._chunks[0].byteLength === byteCount) {
            // super fast path, precisely first chunk must be returned
            const chunk = this._chunks[0];
            this._chunks.shift();
            this._totalLength -= byteCount;
            return this.asNative(chunk);
        }
        if (this._chunks[0].byteLength > byteCount) {
            // fast path, the reading is entirely within the first chunk
            const chunk = this._chunks[0];
            const result = this.asNative(chunk, byteCount);
            this._chunks[0] = chunk.slice(byteCount);
            this._totalLength -= byteCount;
            return result;
        }
        const result = this.allocNative(byteCount);
        let resultOffset = 0;
        let chunkIndex = 0;
        while (byteCount > 0) {
            const chunk = this._chunks[chunkIndex];
            if (chunk.byteLength > byteCount) {
                // this chunk will survive
                const chunkPart = chunk.slice(0, byteCount);
                result.set(chunkPart, resultOffset);
                resultOffset += byteCount;
                this._chunks[chunkIndex] = chunk.slice(byteCount);
                this._totalLength -= byteCount;
                byteCount -= byteCount;
            }
            else {
                // this chunk will be entirely read
                result.set(chunk, resultOffset);
                resultOffset += chunk.byteLength;
                this._chunks.shift();
                this._totalLength -= chunk.byteLength;
                byteCount -= chunk.byteLength;
            }
        }
        return result;
    }
}
exports.AbstractMessageBuffer = AbstractMessageBuffer;
//# sourceMappingURL=messageBuffer.js.map

/***/ }),
/* 64 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
/// <reference path="../../typings/thenable.d.ts" />
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SetTraceNotification = exports.TraceFormat = exports.Trace = exports.ProgressType = exports.ProgressToken = exports.createMessageConnection = exports.NullLogger = exports.ConnectionOptions = exports.ConnectionStrategy = exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = exports.CancellationToken = exports.CancellationTokenSource = exports.Emitter = exports.Event = exports.Disposable = exports.LRUCache = exports.Touch = exports.LinkedMap = exports.ParameterStructures = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.ErrorCodes = exports.ResponseError = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType0 = exports.RequestType = exports.Message = exports.RAL = void 0;
exports.CancellationStrategy = exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = void 0;
const messages_1 = __webpack_require__(65);
Object.defineProperty(exports, "Message", ({ enumerable: true, get: function () { return messages_1.Message; } }));
Object.defineProperty(exports, "RequestType", ({ enumerable: true, get: function () { return messages_1.RequestType; } }));
Object.defineProperty(exports, "RequestType0", ({ enumerable: true, get: function () { return messages_1.RequestType0; } }));
Object.defineProperty(exports, "RequestType1", ({ enumerable: true, get: function () { return messages_1.RequestType1; } }));
Object.defineProperty(exports, "RequestType2", ({ enumerable: true, get: function () { return messages_1.RequestType2; } }));
Object.defineProperty(exports, "RequestType3", ({ enumerable: true, get: function () { return messages_1.RequestType3; } }));
Object.defineProperty(exports, "RequestType4", ({ enumerable: true, get: function () { return messages_1.RequestType4; } }));
Object.defineProperty(exports, "RequestType5", ({ enumerable: true, get: function () { return messages_1.RequestType5; } }));
Object.defineProperty(exports, "RequestType6", ({ enumerable: true, get: function () { return messages_1.RequestType6; } }));
Object.defineProperty(exports, "RequestType7", ({ enumerable: true, get: function () { return messages_1.RequestType7; } }));
Object.defineProperty(exports, "RequestType8", ({ enumerable: true, get: function () { return messages_1.RequestType8; } }));
Object.defineProperty(exports, "RequestType9", ({ enumerable: true, get: function () { return messages_1.RequestType9; } }));
Object.defineProperty(exports, "ResponseError", ({ enumerable: true, get: function () { return messages_1.ResponseError; } }));
Object.defineProperty(exports, "ErrorCodes", ({ enumerable: true, get: function () { return messages_1.ErrorCodes; } }));
Object.defineProperty(exports, "NotificationType", ({ enumerable: true, get: function () { return messages_1.NotificationType; } }));
Object.defineProperty(exports, "NotificationType0", ({ enumerable: true, get: function () { return messages_1.NotificationType0; } }));
Object.defineProperty(exports, "NotificationType1", ({ enumerable: true, get: function () { return messages_1.NotificationType1; } }));
Object.defineProperty(exports, "NotificationType2", ({ enumerable: true, get: function () { return messages_1.NotificationType2; } }));
Object.defineProperty(exports, "NotificationType3", ({ enumerable: true, get: function () { return messages_1.NotificationType3; } }));
Object.defineProperty(exports, "NotificationType4", ({ enumerable: true, get: function () { return messages_1.NotificationType4; } }));
Object.defineProperty(exports, "NotificationType5", ({ enumerable: true, get: function () { return messages_1.NotificationType5; } }));
Object.defineProperty(exports, "NotificationType6", ({ enumerable: true, get: function () { return messages_1.NotificationType6; } }));
Object.defineProperty(exports, "NotificationType7", ({ enumerable: true, get: function () { return messages_1.NotificationType7; } }));
Object.defineProperty(exports, "NotificationType8", ({ enumerable: true, get: function () { return messages_1.NotificationType8; } }));
Object.defineProperty(exports, "NotificationType9", ({ enumerable: true, get: function () { return messages_1.NotificationType9; } }));
Object.defineProperty(exports, "ParameterStructures", ({ enumerable: true, get: function () { return messages_1.ParameterStructures; } }));
const linkedMap_1 = __webpack_require__(67);
Object.defineProperty(exports, "LinkedMap", ({ enumerable: true, get: function () { return linkedMap_1.LinkedMap; } }));
Object.defineProperty(exports, "LRUCache", ({ enumerable: true, get: function () { return linkedMap_1.LRUCache; } }));
Object.defineProperty(exports, "Touch", ({ enumerable: true, get: function () { return linkedMap_1.Touch; } }));
const disposable_1 = __webpack_require__(62);
Object.defineProperty(exports, "Disposable", ({ enumerable: true, get: function () { return disposable_1.Disposable; } }));
const events_1 = __webpack_require__(68);
Object.defineProperty(exports, "Event", ({ enumerable: true, get: function () { return events_1.Event; } }));
Object.defineProperty(exports, "Emitter", ({ enumerable: true, get: function () { return events_1.Emitter; } }));
const cancellation_1 = __webpack_require__(69);
Object.defineProperty(exports, "CancellationTokenSource", ({ enumerable: true, get: function () { return cancellation_1.CancellationTokenSource; } }));
Object.defineProperty(exports, "CancellationToken", ({ enumerable: true, get: function () { return cancellation_1.CancellationToken; } }));
const messageReader_1 = __webpack_require__(70);
Object.defineProperty(exports, "MessageReader", ({ enumerable: true, get: function () { return messageReader_1.MessageReader; } }));
Object.defineProperty(exports, "AbstractMessageReader", ({ enumerable: true, get: function () { return messageReader_1.AbstractMessageReader; } }));
Object.defineProperty(exports, "ReadableStreamMessageReader", ({ enumerable: true, get: function () { return messageReader_1.ReadableStreamMessageReader; } }));
const messageWriter_1 = __webpack_require__(71);
Object.defineProperty(exports, "MessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.MessageWriter; } }));
Object.defineProperty(exports, "AbstractMessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.AbstractMessageWriter; } }));
Object.defineProperty(exports, "WriteableStreamMessageWriter", ({ enumerable: true, get: function () { return messageWriter_1.WriteableStreamMessageWriter; } }));
const connection_1 = __webpack_require__(73);
Object.defineProperty(exports, "ConnectionStrategy", ({ enumerable: true, get: function () { return connection_1.ConnectionStrategy; } }));
Object.defineProperty(exports, "ConnectionOptions", ({ enumerable: true, get: function () { return connection_1.ConnectionOptions; } }));
Object.defineProperty(exports, "NullLogger", ({ enumerable: true, get: function () { return connection_1.NullLogger; } }));
Object.defineProperty(exports, "createMessageConnection", ({ enumerable: true, get: function () { return connection_1.createMessageConnection; } }));
Object.defineProperty(exports, "ProgressToken", ({ enumerable: true, get: function () { return connection_1.ProgressToken; } }));
Object.defineProperty(exports, "ProgressType", ({ enumerable: true, get: function () { return connection_1.ProgressType; } }));
Object.defineProperty(exports, "Trace", ({ enumerable: true, get: function () { return connection_1.Trace; } }));
Object.defineProperty(exports, "TraceFormat", ({ enumerable: true, get: function () { return connection_1.TraceFormat; } }));
Object.defineProperty(exports, "SetTraceNotification", ({ enumerable: true, get: function () { return connection_1.SetTraceNotification; } }));
Object.defineProperty(exports, "LogTraceNotification", ({ enumerable: true, get: function () { return connection_1.LogTraceNotification; } }));
Object.defineProperty(exports, "ConnectionErrors", ({ enumerable: true, get: function () { return connection_1.ConnectionErrors; } }));
Object.defineProperty(exports, "ConnectionError", ({ enumerable: true, get: function () { return connection_1.ConnectionError; } }));
Object.defineProperty(exports, "CancellationReceiverStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationReceiverStrategy; } }));
Object.defineProperty(exports, "CancellationSenderStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationSenderStrategy; } }));
Object.defineProperty(exports, "CancellationStrategy", ({ enumerable: true, get: function () { return connection_1.CancellationStrategy; } }));
const ral_1 = __webpack_require__(60);
exports.RAL = ral_1.default;
//# sourceMappingURL=api.js.map

/***/ }),
/* 65 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Message = exports.NotificationType9 = exports.NotificationType8 = exports.NotificationType7 = exports.NotificationType6 = exports.NotificationType5 = exports.NotificationType4 = exports.NotificationType3 = exports.NotificationType2 = exports.NotificationType1 = exports.NotificationType0 = exports.NotificationType = exports.RequestType9 = exports.RequestType8 = exports.RequestType7 = exports.RequestType6 = exports.RequestType5 = exports.RequestType4 = exports.RequestType3 = exports.RequestType2 = exports.RequestType1 = exports.RequestType = exports.RequestType0 = exports.AbstractMessageSignature = exports.ParameterStructures = exports.ResponseError = exports.ErrorCodes = void 0;
const is = __webpack_require__(66);
/**
 * Predefined error codes.
 */
var ErrorCodes;
(function (ErrorCodes) {
    // Defined by JSON RPC
    ErrorCodes.ParseError = -32700;
    ErrorCodes.InvalidRequest = -32600;
    ErrorCodes.MethodNotFound = -32601;
    ErrorCodes.InvalidParams = -32602;
    ErrorCodes.InternalError = -32603;
    /**
     * This is the start range of JSON RPC reserved error codes.
     * It doesn't denote a real error code. No application error codes should
     * be defined between the start and end range. For backwards
     * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
     * are left in the range.
     *
     * @since 3.16.0
    */
    ErrorCodes.jsonrpcReservedErrorRangeStart = -32099;
    /** @deprecated use  jsonrpcReservedErrorRangeStart */
    ErrorCodes.serverErrorStart = -32099;
    /**
     * An error occurred when write a message to the transport layer.
     */
    ErrorCodes.MessageWriteError = -32099;
    /**
     * An error occurred when reading a message from the transport layer.
     */
    ErrorCodes.MessageReadError = -32098;
    /**
     * The connection got disposed or lost and all pending responses got
     * rejected.
     */
    ErrorCodes.PendingResponseRejected = -32097;
    /**
     * The connection is inactive and a use of it failed.
     */
    ErrorCodes.ConnectionInactive = -32096;
    /**
     * Error code indicating that a server received a notification or
     * request before the server has received the `initialize` request.
     */
    ErrorCodes.ServerNotInitialized = -32002;
    ErrorCodes.UnknownErrorCode = -32001;
    /**
     * This is the end range of JSON RPC reserved error codes.
     * It doesn't denote a real error code.
     *
     * @since 3.16.0
    */
    ErrorCodes.jsonrpcReservedErrorRangeEnd = -32000;
    /** @deprecated use  jsonrpcReservedErrorRangeEnd */
    ErrorCodes.serverErrorEnd = -32000;
})(ErrorCodes = exports.ErrorCodes || (exports.ErrorCodes = {}));
/**
 * An error object return in a response in case a request
 * has failed.
 */
class ResponseError extends Error {
    constructor(code, message, data) {
        super(message);
        this.code = is.number(code) ? code : ErrorCodes.UnknownErrorCode;
        this.data = data;
        Object.setPrototypeOf(this, ResponseError.prototype);
    }
    toJson() {
        const result = {
            code: this.code,
            message: this.message
        };
        if (this.data !== undefined) {
            result.data = this.data;
        }
        return result;
    }
}
exports.ResponseError = ResponseError;
class ParameterStructures {
    constructor(kind) {
        this.kind = kind;
    }
    static is(value) {
        return value === ParameterStructures.auto || value === ParameterStructures.byName || value === ParameterStructures.byPosition;
    }
    toString() {
        return this.kind;
    }
}
exports.ParameterStructures = ParameterStructures;
/**
 * The parameter structure is automatically inferred on the number of parameters
 * and the parameter type in case of a single param.
 */
ParameterStructures.auto = new ParameterStructures('auto');
/**
 * Forces `byPosition` parameter structure. This is useful if you have a single
 * parameter which has a literal type.
 */
ParameterStructures.byPosition = new ParameterStructures('byPosition');
/**
 * Forces `byName` parameter structure. This is only useful when having a single
 * parameter. The library will report errors if used with a different number of
 * parameters.
 */
ParameterStructures.byName = new ParameterStructures('byName');
/**
 * An abstract implementation of a MessageType.
 */
class AbstractMessageSignature {
    constructor(method, numberOfParams) {
        this.method = method;
        this.numberOfParams = numberOfParams;
    }
    get parameterStructures() {
        return ParameterStructures.auto;
    }
}
exports.AbstractMessageSignature = AbstractMessageSignature;
/**
 * Classes to type request response pairs
 */
class RequestType0 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 0);
    }
}
exports.RequestType0 = RequestType0;
class RequestType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.RequestType = RequestType;
class RequestType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.RequestType1 = RequestType1;
class RequestType2 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 2);
    }
}
exports.RequestType2 = RequestType2;
class RequestType3 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 3);
    }
}
exports.RequestType3 = RequestType3;
class RequestType4 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 4);
    }
}
exports.RequestType4 = RequestType4;
class RequestType5 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 5);
    }
}
exports.RequestType5 = RequestType5;
class RequestType6 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 6);
    }
}
exports.RequestType6 = RequestType6;
class RequestType7 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 7);
    }
}
exports.RequestType7 = RequestType7;
class RequestType8 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 8);
    }
}
exports.RequestType8 = RequestType8;
class RequestType9 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 9);
    }
}
exports.RequestType9 = RequestType9;
class NotificationType extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.NotificationType = NotificationType;
class NotificationType0 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 0);
    }
}
exports.NotificationType0 = NotificationType0;
class NotificationType1 extends AbstractMessageSignature {
    constructor(method, _parameterStructures = ParameterStructures.auto) {
        super(method, 1);
        this._parameterStructures = _parameterStructures;
    }
    get parameterStructures() {
        return this._parameterStructures;
    }
}
exports.NotificationType1 = NotificationType1;
class NotificationType2 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 2);
    }
}
exports.NotificationType2 = NotificationType2;
class NotificationType3 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 3);
    }
}
exports.NotificationType3 = NotificationType3;
class NotificationType4 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 4);
    }
}
exports.NotificationType4 = NotificationType4;
class NotificationType5 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 5);
    }
}
exports.NotificationType5 = NotificationType5;
class NotificationType6 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 6);
    }
}
exports.NotificationType6 = NotificationType6;
class NotificationType7 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 7);
    }
}
exports.NotificationType7 = NotificationType7;
class NotificationType8 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 8);
    }
}
exports.NotificationType8 = NotificationType8;
class NotificationType9 extends AbstractMessageSignature {
    constructor(method) {
        super(method, 9);
    }
}
exports.NotificationType9 = NotificationType9;
var Message;
(function (Message) {
    /**
     * Tests if the given message is a request message
     */
    function isRequest(message) {
        const candidate = message;
        return candidate && is.string(candidate.method) && (is.string(candidate.id) || is.number(candidate.id));
    }
    Message.isRequest = isRequest;
    /**
     * Tests if the given message is a notification message
     */
    function isNotification(message) {
        const candidate = message;
        return candidate && is.string(candidate.method) && message.id === void 0;
    }
    Message.isNotification = isNotification;
    /**
     * Tests if the given message is a response message
     */
    function isResponse(message) {
        const candidate = message;
        return candidate && (candidate.result !== void 0 || !!candidate.error) && (is.string(candidate.id) || is.number(candidate.id) || candidate.id === null);
    }
    Message.isResponse = isResponse;
})(Message = exports.Message || (exports.Message = {}));
//# sourceMappingURL=messages.js.map

/***/ }),
/* 66 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
//# sourceMappingURL=is.js.map

/***/ }),
/* 67 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
var _a;
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LRUCache = exports.LinkedMap = exports.Touch = void 0;
var Touch;
(function (Touch) {
    Touch.None = 0;
    Touch.First = 1;
    Touch.AsOld = Touch.First;
    Touch.Last = 2;
    Touch.AsNew = Touch.Last;
})(Touch = exports.Touch || (exports.Touch = {}));
class LinkedMap {
    constructor() {
        this[_a] = 'LinkedMap';
        this._map = new Map();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
        this._state = 0;
    }
    clear() {
        this._map.clear();
        this._head = undefined;
        this._tail = undefined;
        this._size = 0;
        this._state++;
    }
    isEmpty() {
        return !this._head && !this._tail;
    }
    get size() {
        return this._size;
    }
    get first() {
        return this._head?.value;
    }
    get last() {
        return this._tail?.value;
    }
    has(key) {
        return this._map.has(key);
    }
    get(key, touch = Touch.None) {
        const item = this._map.get(key);
        if (!item) {
            return undefined;
        }
        if (touch !== Touch.None) {
            this.touch(item, touch);
        }
        return item.value;
    }
    set(key, value, touch = Touch.None) {
        let item = this._map.get(key);
        if (item) {
            item.value = value;
            if (touch !== Touch.None) {
                this.touch(item, touch);
            }
        }
        else {
            item = { key, value, next: undefined, previous: undefined };
            switch (touch) {
                case Touch.None:
                    this.addItemLast(item);
                    break;
                case Touch.First:
                    this.addItemFirst(item);
                    break;
                case Touch.Last:
                    this.addItemLast(item);
                    break;
                default:
                    this.addItemLast(item);
                    break;
            }
            this._map.set(key, item);
            this._size++;
        }
        return this;
    }
    delete(key) {
        return !!this.remove(key);
    }
    remove(key) {
        const item = this._map.get(key);
        if (!item) {
            return undefined;
        }
        this._map.delete(key);
        this.removeItem(item);
        this._size--;
        return item.value;
    }
    shift() {
        if (!this._head && !this._tail) {
            return undefined;
        }
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        const item = this._head;
        this._map.delete(item.key);
        this.removeItem(item);
        this._size--;
        return item.value;
    }
    forEach(callbackfn, thisArg) {
        const state = this._state;
        let current = this._head;
        while (current) {
            if (thisArg) {
                callbackfn.bind(thisArg)(current.value, current.key, this);
            }
            else {
                callbackfn(current.value, current.key, this);
            }
            if (this._state !== state) {
                throw new Error(`LinkedMap got modified during iteration.`);
            }
            current = current.next;
        }
    }
    keys() {
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]: () => {
                return iterator;
            },
            next: () => {
                if (this._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: current.key, done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    values() {
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]: () => {
                return iterator;
            },
            next: () => {
                if (this._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: current.value, done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    entries() {
        const state = this._state;
        let current = this._head;
        const iterator = {
            [Symbol.iterator]: () => {
                return iterator;
            },
            next: () => {
                if (this._state !== state) {
                    throw new Error(`LinkedMap got modified during iteration.`);
                }
                if (current) {
                    const result = { value: [current.key, current.value], done: false };
                    current = current.next;
                    return result;
                }
                else {
                    return { value: undefined, done: true };
                }
            }
        };
        return iterator;
    }
    [(_a = Symbol.toStringTag, Symbol.iterator)]() {
        return this.entries();
    }
    trimOld(newSize) {
        if (newSize >= this.size) {
            return;
        }
        if (newSize === 0) {
            this.clear();
            return;
        }
        let current = this._head;
        let currentSize = this.size;
        while (current && currentSize > newSize) {
            this._map.delete(current.key);
            current = current.next;
            currentSize--;
        }
        this._head = current;
        this._size = currentSize;
        if (current) {
            current.previous = undefined;
        }
        this._state++;
    }
    addItemFirst(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._tail = item;
        }
        else if (!this._head) {
            throw new Error('Invalid list');
        }
        else {
            item.next = this._head;
            this._head.previous = item;
        }
        this._head = item;
        this._state++;
    }
    addItemLast(item) {
        // First time Insert
        if (!this._head && !this._tail) {
            this._head = item;
        }
        else if (!this._tail) {
            throw new Error('Invalid list');
        }
        else {
            item.previous = this._tail;
            this._tail.next = item;
        }
        this._tail = item;
        this._state++;
    }
    removeItem(item) {
        if (item === this._head && item === this._tail) {
            this._head = undefined;
            this._tail = undefined;
        }
        else if (item === this._head) {
            // This can only happened if size === 1 which is handle
            // by the case above.
            if (!item.next) {
                throw new Error('Invalid list');
            }
            item.next.previous = undefined;
            this._head = item.next;
        }
        else if (item === this._tail) {
            // This can only happened if size === 1 which is handle
            // by the case above.
            if (!item.previous) {
                throw new Error('Invalid list');
            }
            item.previous.next = undefined;
            this._tail = item.previous;
        }
        else {
            const next = item.next;
            const previous = item.previous;
            if (!next || !previous) {
                throw new Error('Invalid list');
            }
            next.previous = previous;
            previous.next = next;
        }
        item.next = undefined;
        item.previous = undefined;
        this._state++;
    }
    touch(item, touch) {
        if (!this._head || !this._tail) {
            throw new Error('Invalid list');
        }
        if ((touch !== Touch.First && touch !== Touch.Last)) {
            return;
        }
        if (touch === Touch.First) {
            if (item === this._head) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item
            if (item === this._tail) {
                // previous must be defined since item was not head but is tail
                // So there are more than on item in the map
                previous.next = undefined;
                this._tail = previous;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            // Insert the node at head
            item.previous = undefined;
            item.next = this._head;
            this._head.previous = item;
            this._head = item;
            this._state++;
        }
        else if (touch === Touch.Last) {
            if (item === this._tail) {
                return;
            }
            const next = item.next;
            const previous = item.previous;
            // Unlink the item.
            if (item === this._head) {
                // next must be defined since item was not tail but is head
                // So there are more than on item in the map
                next.previous = undefined;
                this._head = next;
            }
            else {
                // Both next and previous are not undefined since item was neither head nor tail.
                next.previous = previous;
                previous.next = next;
            }
            item.next = undefined;
            item.previous = this._tail;
            this._tail.next = item;
            this._tail = item;
            this._state++;
        }
    }
    toJSON() {
        const data = [];
        this.forEach((value, key) => {
            data.push([key, value]);
        });
        return data;
    }
    fromJSON(data) {
        this.clear();
        for (const [key, value] of data) {
            this.set(key, value);
        }
    }
}
exports.LinkedMap = LinkedMap;
class LRUCache extends LinkedMap {
    constructor(limit, ratio = 1) {
        super();
        this._limit = limit;
        this._ratio = Math.min(Math.max(0, ratio), 1);
    }
    get limit() {
        return this._limit;
    }
    set limit(limit) {
        this._limit = limit;
        this.checkTrim();
    }
    get ratio() {
        return this._ratio;
    }
    set ratio(ratio) {
        this._ratio = Math.min(Math.max(0, ratio), 1);
        this.checkTrim();
    }
    get(key, touch = Touch.AsNew) {
        return super.get(key, touch);
    }
    peek(key) {
        return super.get(key, Touch.None);
    }
    set(key, value) {
        super.set(key, value, Touch.Last);
        this.checkTrim();
        return this;
    }
    checkTrim() {
        if (this.size > this._limit) {
            this.trimOld(Math.round(this._limit * this._ratio));
        }
    }
}
exports.LRUCache = LRUCache;
//# sourceMappingURL=linkedMap.js.map

/***/ }),
/* 68 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Emitter = exports.Event = void 0;
const ral_1 = __webpack_require__(60);
var Event;
(function (Event) {
    const _disposable = { dispose() { } };
    Event.None = function () { return _disposable; };
})(Event = exports.Event || (exports.Event = {}));
class CallbackList {
    add(callback, context = null, bucket) {
        if (!this._callbacks) {
            this._callbacks = [];
            this._contexts = [];
        }
        this._callbacks.push(callback);
        this._contexts.push(context);
        if (Array.isArray(bucket)) {
            bucket.push({ dispose: () => this.remove(callback, context) });
        }
    }
    remove(callback, context = null) {
        if (!this._callbacks) {
            return;
        }
        let foundCallbackWithDifferentContext = false;
        for (let i = 0, len = this._callbacks.length; i < len; i++) {
            if (this._callbacks[i] === callback) {
                if (this._contexts[i] === context) {
                    // callback & context match => remove it
                    this._callbacks.splice(i, 1);
                    this._contexts.splice(i, 1);
                    return;
                }
                else {
                    foundCallbackWithDifferentContext = true;
                }
            }
        }
        if (foundCallbackWithDifferentContext) {
            throw new Error('When adding a listener with a context, you should remove it with the same context');
        }
    }
    invoke(...args) {
        if (!this._callbacks) {
            return [];
        }
        const ret = [], callbacks = this._callbacks.slice(0), contexts = this._contexts.slice(0);
        for (let i = 0, len = callbacks.length; i < len; i++) {
            try {
                ret.push(callbacks[i].apply(contexts[i], args));
            }
            catch (e) {
                // eslint-disable-next-line no-console
                (0, ral_1.default)().console.error(e);
            }
        }
        return ret;
    }
    isEmpty() {
        return !this._callbacks || this._callbacks.length === 0;
    }
    dispose() {
        this._callbacks = undefined;
        this._contexts = undefined;
    }
}
class Emitter {
    constructor(_options) {
        this._options = _options;
    }
    /**
     * For the public to allow to subscribe
     * to events from this Emitter
     */
    get event() {
        if (!this._event) {
            this._event = (listener, thisArgs, disposables) => {
                if (!this._callbacks) {
                    this._callbacks = new CallbackList();
                }
                if (this._options && this._options.onFirstListenerAdd && this._callbacks.isEmpty()) {
                    this._options.onFirstListenerAdd(this);
                }
                this._callbacks.add(listener, thisArgs);
                const result = {
                    dispose: () => {
                        if (!this._callbacks) {
                            // disposable is disposed after emitter is disposed.
                            return;
                        }
                        this._callbacks.remove(listener, thisArgs);
                        result.dispose = Emitter._noop;
                        if (this._options && this._options.onLastListenerRemove && this._callbacks.isEmpty()) {
                            this._options.onLastListenerRemove(this);
                        }
                    }
                };
                if (Array.isArray(disposables)) {
                    disposables.push(result);
                }
                return result;
            };
        }
        return this._event;
    }
    /**
     * To be kept private to fire an event to
     * subscribers
     */
    fire(event) {
        if (this._callbacks) {
            this._callbacks.invoke.call(this._callbacks, event);
        }
    }
    dispose() {
        if (this._callbacks) {
            this._callbacks.dispose();
            this._callbacks = undefined;
        }
    }
}
exports.Emitter = Emitter;
Emitter._noop = function () { };
//# sourceMappingURL=events.js.map

/***/ }),
/* 69 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CancellationTokenSource = exports.CancellationToken = void 0;
const ral_1 = __webpack_require__(60);
const Is = __webpack_require__(66);
const events_1 = __webpack_require__(68);
var CancellationToken;
(function (CancellationToken) {
    CancellationToken.None = Object.freeze({
        isCancellationRequested: false,
        onCancellationRequested: events_1.Event.None
    });
    CancellationToken.Cancelled = Object.freeze({
        isCancellationRequested: true,
        onCancellationRequested: events_1.Event.None
    });
    function is(value) {
        const candidate = value;
        return candidate && (candidate === CancellationToken.None
            || candidate === CancellationToken.Cancelled
            || (Is.boolean(candidate.isCancellationRequested) && !!candidate.onCancellationRequested));
    }
    CancellationToken.is = is;
})(CancellationToken = exports.CancellationToken || (exports.CancellationToken = {}));
const shortcutEvent = Object.freeze(function (callback, context) {
    const handle = (0, ral_1.default)().timer.setTimeout(callback.bind(context), 0);
    return { dispose() { handle.dispose(); } };
});
class MutableToken {
    constructor() {
        this._isCancelled = false;
    }
    cancel() {
        if (!this._isCancelled) {
            this._isCancelled = true;
            if (this._emitter) {
                this._emitter.fire(undefined);
                this.dispose();
            }
        }
    }
    get isCancellationRequested() {
        return this._isCancelled;
    }
    get onCancellationRequested() {
        if (this._isCancelled) {
            return shortcutEvent;
        }
        if (!this._emitter) {
            this._emitter = new events_1.Emitter();
        }
        return this._emitter.event;
    }
    dispose() {
        if (this._emitter) {
            this._emitter.dispose();
            this._emitter = undefined;
        }
    }
}
class CancellationTokenSource {
    get token() {
        if (!this._token) {
            // be lazy and create the token only when
            // actually needed
            this._token = new MutableToken();
        }
        return this._token;
    }
    cancel() {
        if (!this._token) {
            // save an object by returning the default
            // cancelled token when cancellation happens
            // before someone asks for the token
            this._token = CancellationToken.Cancelled;
        }
        else {
            this._token.cancel();
        }
    }
    dispose() {
        if (!this._token) {
            // ensure to initialize with an empty token if we had none
            this._token = CancellationToken.None;
        }
        else if (this._token instanceof MutableToken) {
            // actually dispose
            this._token.dispose();
        }
    }
}
exports.CancellationTokenSource = CancellationTokenSource;
//# sourceMappingURL=cancellation.js.map

/***/ }),
/* 70 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ReadableStreamMessageReader = exports.AbstractMessageReader = exports.MessageReader = void 0;
const ral_1 = __webpack_require__(60);
const Is = __webpack_require__(66);
const events_1 = __webpack_require__(68);
var MessageReader;
(function (MessageReader) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.listen) && Is.func(candidate.dispose) &&
            Is.func(candidate.onError) && Is.func(candidate.onClose) && Is.func(candidate.onPartialMessage);
    }
    MessageReader.is = is;
})(MessageReader = exports.MessageReader || (exports.MessageReader = {}));
class AbstractMessageReader {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
        this.partialMessageEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error) {
        this.errorEmitter.fire(this.asError(error));
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    get onPartialMessage() {
        return this.partialMessageEmitter.event;
    }
    firePartialMessage(info) {
        this.partialMessageEmitter.fire(info);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Reader received error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageReader = AbstractMessageReader;
var ResolvedMessageReaderOptions;
(function (ResolvedMessageReaderOptions) {
    function fromOptions(options) {
        let charset;
        let result;
        let contentDecoder;
        const contentDecoders = new Map();
        let contentTypeDecoder;
        const contentTypeDecoders = new Map();
        if (options === undefined || typeof options === 'string') {
            charset = options ?? 'utf-8';
        }
        else {
            charset = options.charset ?? 'utf-8';
            if (options.contentDecoder !== undefined) {
                contentDecoder = options.contentDecoder;
                contentDecoders.set(contentDecoder.name, contentDecoder);
            }
            if (options.contentDecoders !== undefined) {
                for (const decoder of options.contentDecoders) {
                    contentDecoders.set(decoder.name, decoder);
                }
            }
            if (options.contentTypeDecoder !== undefined) {
                contentTypeDecoder = options.contentTypeDecoder;
                contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
            }
            if (options.contentTypeDecoders !== undefined) {
                for (const decoder of options.contentTypeDecoders) {
                    contentTypeDecoders.set(decoder.name, decoder);
                }
            }
        }
        if (contentTypeDecoder === undefined) {
            contentTypeDecoder = (0, ral_1.default)().applicationJson.decoder;
            contentTypeDecoders.set(contentTypeDecoder.name, contentTypeDecoder);
        }
        return { charset, contentDecoder, contentDecoders, contentTypeDecoder, contentTypeDecoders };
    }
    ResolvedMessageReaderOptions.fromOptions = fromOptions;
})(ResolvedMessageReaderOptions || (ResolvedMessageReaderOptions = {}));
class ReadableStreamMessageReader extends AbstractMessageReader {
    constructor(readable, options) {
        super();
        this.readable = readable;
        this.options = ResolvedMessageReaderOptions.fromOptions(options);
        this.buffer = (0, ral_1.default)().messageBuffer.create(this.options.charset);
        this._partialMessageTimeout = 10000;
        this.nextMessageLength = -1;
        this.messageToken = 0;
    }
    set partialMessageTimeout(timeout) {
        this._partialMessageTimeout = timeout;
    }
    get partialMessageTimeout() {
        return this._partialMessageTimeout;
    }
    listen(callback) {
        this.nextMessageLength = -1;
        this.messageToken = 0;
        this.partialMessageTimer = undefined;
        this.callback = callback;
        const result = this.readable.onData((data) => {
            this.onData(data);
        });
        this.readable.onError((error) => this.fireError(error));
        this.readable.onClose(() => this.fireClose());
        return result;
    }
    onData(data) {
        this.buffer.append(data);
        while (true) {
            if (this.nextMessageLength === -1) {
                const headers = this.buffer.tryReadHeaders();
                if (!headers) {
                    return;
                }
                const contentLength = headers.get('Content-Length');
                if (!contentLength) {
                    throw new Error('Header must provide a Content-Length property.');
                }
                const length = parseInt(contentLength);
                if (isNaN(length)) {
                    throw new Error('Content-Length value must be a number.');
                }
                this.nextMessageLength = length;
            }
            const body = this.buffer.tryReadBody(this.nextMessageLength);
            if (body === undefined) {
                /** We haven't received the full message yet. */
                this.setPartialMessageTimer();
                return;
            }
            this.clearPartialMessageTimer();
            this.nextMessageLength = -1;
            let p;
            if (this.options.contentDecoder !== undefined) {
                p = this.options.contentDecoder.decode(body);
            }
            else {
                p = Promise.resolve(body);
            }
            p.then((value) => {
                this.options.contentTypeDecoder.decode(value, this.options).then((msg) => {
                    this.callback(msg);
                }, (error) => {
                    this.fireError(error);
                });
            }, (error) => {
                this.fireError(error);
            });
        }
    }
    clearPartialMessageTimer() {
        if (this.partialMessageTimer) {
            this.partialMessageTimer.dispose();
            this.partialMessageTimer = undefined;
        }
    }
    setPartialMessageTimer() {
        this.clearPartialMessageTimer();
        if (this._partialMessageTimeout <= 0) {
            return;
        }
        this.partialMessageTimer = (0, ral_1.default)().timer.setTimeout((token, timeout) => {
            this.partialMessageTimer = undefined;
            if (token === this.messageToken) {
                this.firePartialMessage({ messageToken: token, waitingTime: timeout });
                this.setPartialMessageTimer();
            }
        }, this._partialMessageTimeout, this.messageToken, this._partialMessageTimeout);
    }
}
exports.ReadableStreamMessageReader = ReadableStreamMessageReader;
//# sourceMappingURL=messageReader.js.map

/***/ }),
/* 71 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WriteableStreamMessageWriter = exports.AbstractMessageWriter = exports.MessageWriter = void 0;
const ral_1 = __webpack_require__(60);
const Is = __webpack_require__(66);
const semaphore_1 = __webpack_require__(72);
const events_1 = __webpack_require__(68);
const ContentLength = 'Content-Length: ';
const CRLF = '\r\n';
var MessageWriter;
(function (MessageWriter) {
    function is(value) {
        let candidate = value;
        return candidate && Is.func(candidate.dispose) && Is.func(candidate.onClose) &&
            Is.func(candidate.onError) && Is.func(candidate.write);
    }
    MessageWriter.is = is;
})(MessageWriter = exports.MessageWriter || (exports.MessageWriter = {}));
class AbstractMessageWriter {
    constructor() {
        this.errorEmitter = new events_1.Emitter();
        this.closeEmitter = new events_1.Emitter();
    }
    dispose() {
        this.errorEmitter.dispose();
        this.closeEmitter.dispose();
    }
    get onError() {
        return this.errorEmitter.event;
    }
    fireError(error, message, count) {
        this.errorEmitter.fire([this.asError(error), message, count]);
    }
    get onClose() {
        return this.closeEmitter.event;
    }
    fireClose() {
        this.closeEmitter.fire(undefined);
    }
    asError(error) {
        if (error instanceof Error) {
            return error;
        }
        else {
            return new Error(`Writer received error. Reason: ${Is.string(error.message) ? error.message : 'unknown'}`);
        }
    }
}
exports.AbstractMessageWriter = AbstractMessageWriter;
var ResolvedMessageWriterOptions;
(function (ResolvedMessageWriterOptions) {
    function fromOptions(options) {
        if (options === undefined || typeof options === 'string') {
            return { charset: options ?? 'utf-8', contentTypeEncoder: (0, ral_1.default)().applicationJson.encoder };
        }
        else {
            return { charset: options.charset ?? 'utf-8', contentEncoder: options.contentEncoder, contentTypeEncoder: options.contentTypeEncoder ?? (0, ral_1.default)().applicationJson.encoder };
        }
    }
    ResolvedMessageWriterOptions.fromOptions = fromOptions;
})(ResolvedMessageWriterOptions || (ResolvedMessageWriterOptions = {}));
class WriteableStreamMessageWriter extends AbstractMessageWriter {
    constructor(writable, options) {
        super();
        this.writable = writable;
        this.options = ResolvedMessageWriterOptions.fromOptions(options);
        this.errorCount = 0;
        this.writeSemaphore = new semaphore_1.Semaphore(1);
        this.writable.onError((error) => this.fireError(error));
        this.writable.onClose(() => this.fireClose());
    }
    async write(msg) {
        return this.writeSemaphore.lock(async () => {
            const payload = this.options.contentTypeEncoder.encode(msg, this.options).then((buffer) => {
                if (this.options.contentEncoder !== undefined) {
                    return this.options.contentEncoder.encode(buffer);
                }
                else {
                    return buffer;
                }
            });
            return payload.then((buffer) => {
                const headers = [];
                headers.push(ContentLength, buffer.byteLength.toString(), CRLF);
                headers.push(CRLF);
                return this.doWrite(msg, headers, buffer);
            }, (error) => {
                this.fireError(error);
                throw error;
            });
        });
    }
    async doWrite(msg, headers, data) {
        try {
            await this.writable.write(headers.join(''), 'ascii');
            return this.writable.write(data);
        }
        catch (error) {
            this.handleError(error, msg);
            return Promise.reject(error);
        }
    }
    handleError(error, msg) {
        this.errorCount++;
        this.fireError(error, msg, this.errorCount);
    }
    end() {
        this.writable.end();
    }
}
exports.WriteableStreamMessageWriter = WriteableStreamMessageWriter;
//# sourceMappingURL=messageWriter.js.map

/***/ }),
/* 72 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.Semaphore = void 0;
const ral_1 = __webpack_require__(60);
class Semaphore {
    constructor(capacity = 1) {
        if (capacity <= 0) {
            throw new Error('Capacity must be greater than 0');
        }
        this._capacity = capacity;
        this._active = 0;
        this._waiting = [];
    }
    lock(thunk) {
        return new Promise((resolve, reject) => {
            this._waiting.push({ thunk, resolve, reject });
            this.runNext();
        });
    }
    get active() {
        return this._active;
    }
    runNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        (0, ral_1.default)().timer.setImmediate(() => this.doRunNext());
    }
    doRunNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        const next = this._waiting.shift();
        this._active++;
        if (this._active > this._capacity) {
            throw new Error(`To many thunks active`);
        }
        try {
            const result = next.thunk();
            if (result instanceof Promise) {
                result.then((value) => {
                    this._active--;
                    next.resolve(value);
                    this.runNext();
                }, (err) => {
                    this._active--;
                    next.reject(err);
                    this.runNext();
                });
            }
            else {
                this._active--;
                next.resolve(result);
                this.runNext();
            }
        }
        catch (err) {
            this._active--;
            next.reject(err);
            this.runNext();
        }
    }
}
exports.Semaphore = Semaphore;
//# sourceMappingURL=semaphore.js.map

/***/ }),
/* 73 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createMessageConnection = exports.ConnectionOptions = exports.CancellationStrategy = exports.CancellationSenderStrategy = exports.CancellationReceiverStrategy = exports.ConnectionStrategy = exports.ConnectionError = exports.ConnectionErrors = exports.LogTraceNotification = exports.SetTraceNotification = exports.TraceFormat = exports.Trace = exports.NullLogger = exports.ProgressType = exports.ProgressToken = void 0;
const ral_1 = __webpack_require__(60);
const Is = __webpack_require__(66);
const messages_1 = __webpack_require__(65);
const linkedMap_1 = __webpack_require__(67);
const events_1 = __webpack_require__(68);
const cancellation_1 = __webpack_require__(69);
var CancelNotification;
(function (CancelNotification) {
    CancelNotification.type = new messages_1.NotificationType('$/cancelRequest');
})(CancelNotification || (CancelNotification = {}));
var ProgressToken;
(function (ProgressToken) {
    function is(value) {
        return typeof value === 'string' || typeof value === 'number';
    }
    ProgressToken.is = is;
})(ProgressToken = exports.ProgressToken || (exports.ProgressToken = {}));
var ProgressNotification;
(function (ProgressNotification) {
    ProgressNotification.type = new messages_1.NotificationType('$/progress');
})(ProgressNotification || (ProgressNotification = {}));
class ProgressType {
    constructor() {
    }
}
exports.ProgressType = ProgressType;
var StarRequestHandler;
(function (StarRequestHandler) {
    function is(value) {
        return Is.func(value);
    }
    StarRequestHandler.is = is;
})(StarRequestHandler || (StarRequestHandler = {}));
exports.NullLogger = Object.freeze({
    error: () => { },
    warn: () => { },
    info: () => { },
    log: () => { }
});
var Trace;
(function (Trace) {
    Trace[Trace["Off"] = 0] = "Off";
    Trace[Trace["Messages"] = 1] = "Messages";
    Trace[Trace["Compact"] = 2] = "Compact";
    Trace[Trace["Verbose"] = 3] = "Verbose";
})(Trace = exports.Trace || (exports.Trace = {}));
(function (Trace) {
    function fromString(value) {
        if (!Is.string(value)) {
            return Trace.Off;
        }
        value = value.toLowerCase();
        switch (value) {
            case 'off':
                return Trace.Off;
            case 'messages':
                return Trace.Messages;
            case 'compact':
                return Trace.Compact;
            case 'verbose':
                return Trace.Verbose;
            default:
                return Trace.Off;
        }
    }
    Trace.fromString = fromString;
    function toString(value) {
        switch (value) {
            case Trace.Off:
                return 'off';
            case Trace.Messages:
                return 'messages';
            case Trace.Compact:
                return 'compact';
            case Trace.Verbose:
                return 'verbose';
            default:
                return 'off';
        }
    }
    Trace.toString = toString;
})(Trace = exports.Trace || (exports.Trace = {}));
var TraceFormat;
(function (TraceFormat) {
    TraceFormat["Text"] = "text";
    TraceFormat["JSON"] = "json";
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
(function (TraceFormat) {
    function fromString(value) {
        if (!Is.string(value)) {
            return TraceFormat.Text;
        }
        value = value.toLowerCase();
        if (value === 'json') {
            return TraceFormat.JSON;
        }
        else {
            return TraceFormat.Text;
        }
    }
    TraceFormat.fromString = fromString;
})(TraceFormat = exports.TraceFormat || (exports.TraceFormat = {}));
var SetTraceNotification;
(function (SetTraceNotification) {
    SetTraceNotification.type = new messages_1.NotificationType('$/setTrace');
})(SetTraceNotification = exports.SetTraceNotification || (exports.SetTraceNotification = {}));
var LogTraceNotification;
(function (LogTraceNotification) {
    LogTraceNotification.type = new messages_1.NotificationType('$/logTrace');
})(LogTraceNotification = exports.LogTraceNotification || (exports.LogTraceNotification = {}));
var ConnectionErrors;
(function (ConnectionErrors) {
    /**
     * The connection is closed.
     */
    ConnectionErrors[ConnectionErrors["Closed"] = 1] = "Closed";
    /**
     * The connection got disposed.
     */
    ConnectionErrors[ConnectionErrors["Disposed"] = 2] = "Disposed";
    /**
     * The connection is already in listening mode.
     */
    ConnectionErrors[ConnectionErrors["AlreadyListening"] = 3] = "AlreadyListening";
})(ConnectionErrors = exports.ConnectionErrors || (exports.ConnectionErrors = {}));
class ConnectionError extends Error {
    constructor(code, message) {
        super(message);
        this.code = code;
        Object.setPrototypeOf(this, ConnectionError.prototype);
    }
}
exports.ConnectionError = ConnectionError;
var ConnectionStrategy;
(function (ConnectionStrategy) {
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.cancelUndispatched);
    }
    ConnectionStrategy.is = is;
})(ConnectionStrategy = exports.ConnectionStrategy || (exports.ConnectionStrategy = {}));
var CancellationReceiverStrategy;
(function (CancellationReceiverStrategy) {
    CancellationReceiverStrategy.Message = Object.freeze({
        createCancellationTokenSource(_) {
            return new cancellation_1.CancellationTokenSource();
        }
    });
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.createCancellationTokenSource);
    }
    CancellationReceiverStrategy.is = is;
})(CancellationReceiverStrategy = exports.CancellationReceiverStrategy || (exports.CancellationReceiverStrategy = {}));
var CancellationSenderStrategy;
(function (CancellationSenderStrategy) {
    CancellationSenderStrategy.Message = Object.freeze({
        sendCancellation(conn, id) {
            return conn.sendNotification(CancelNotification.type, { id });
        },
        cleanup(_) { }
    });
    function is(value) {
        const candidate = value;
        return candidate && Is.func(candidate.sendCancellation) && Is.func(candidate.cleanup);
    }
    CancellationSenderStrategy.is = is;
})(CancellationSenderStrategy = exports.CancellationSenderStrategy || (exports.CancellationSenderStrategy = {}));
var CancellationStrategy;
(function (CancellationStrategy) {
    CancellationStrategy.Message = Object.freeze({
        receiver: CancellationReceiverStrategy.Message,
        sender: CancellationSenderStrategy.Message
    });
    function is(value) {
        const candidate = value;
        return candidate && CancellationReceiverStrategy.is(candidate.receiver) && CancellationSenderStrategy.is(candidate.sender);
    }
    CancellationStrategy.is = is;
})(CancellationStrategy = exports.CancellationStrategy || (exports.CancellationStrategy = {}));
var ConnectionOptions;
(function (ConnectionOptions) {
    function is(value) {
        const candidate = value;
        return candidate && (CancellationStrategy.is(candidate.cancellationStrategy) || ConnectionStrategy.is(candidate.connectionStrategy));
    }
    ConnectionOptions.is = is;
})(ConnectionOptions = exports.ConnectionOptions || (exports.ConnectionOptions = {}));
var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["New"] = 1] = "New";
    ConnectionState[ConnectionState["Listening"] = 2] = "Listening";
    ConnectionState[ConnectionState["Closed"] = 3] = "Closed";
    ConnectionState[ConnectionState["Disposed"] = 4] = "Disposed";
})(ConnectionState || (ConnectionState = {}));
function createMessageConnection(messageReader, messageWriter, _logger, options) {
    const logger = _logger !== undefined ? _logger : exports.NullLogger;
    let sequenceNumber = 0;
    let notificationSequenceNumber = 0;
    let unknownResponseSequenceNumber = 0;
    const version = '2.0';
    let starRequestHandler = undefined;
    const requestHandlers = new Map();
    let starNotificationHandler = undefined;
    const notificationHandlers = new Map();
    const progressHandlers = new Map();
    let timer;
    let messageQueue = new linkedMap_1.LinkedMap();
    let responsePromises = new Map();
    let knownCanceledRequests = new Set();
    let requestTokens = new Map();
    let trace = Trace.Off;
    let traceFormat = TraceFormat.Text;
    let tracer;
    let state = ConnectionState.New;
    const errorEmitter = new events_1.Emitter();
    const closeEmitter = new events_1.Emitter();
    const unhandledNotificationEmitter = new events_1.Emitter();
    const unhandledProgressEmitter = new events_1.Emitter();
    const disposeEmitter = new events_1.Emitter();
    const cancellationStrategy = (options && options.cancellationStrategy) ? options.cancellationStrategy : CancellationStrategy.Message;
    function createRequestQueueKey(id) {
        if (id === null) {
            throw new Error(`Can't send requests with id null since the response can't be correlated.`);
        }
        return 'req-' + id.toString();
    }
    function createResponseQueueKey(id) {
        if (id === null) {
            return 'res-unknown-' + (++unknownResponseSequenceNumber).toString();
        }
        else {
            return 'res-' + id.toString();
        }
    }
    function createNotificationQueueKey() {
        return 'not-' + (++notificationSequenceNumber).toString();
    }
    function addMessageToQueue(queue, message) {
        if (messages_1.Message.isRequest(message)) {
            queue.set(createRequestQueueKey(message.id), message);
        }
        else if (messages_1.Message.isResponse(message)) {
            queue.set(createResponseQueueKey(message.id), message);
        }
        else {
            queue.set(createNotificationQueueKey(), message);
        }
    }
    function cancelUndispatched(_message) {
        return undefined;
    }
    function isListening() {
        return state === ConnectionState.Listening;
    }
    function isClosed() {
        return state === ConnectionState.Closed;
    }
    function isDisposed() {
        return state === ConnectionState.Disposed;
    }
    function closeHandler() {
        if (state === ConnectionState.New || state === ConnectionState.Listening) {
            state = ConnectionState.Closed;
            closeEmitter.fire(undefined);
        }
        // If the connection is disposed don't sent close events.
    }
    function readErrorHandler(error) {
        errorEmitter.fire([error, undefined, undefined]);
    }
    function writeErrorHandler(data) {
        errorEmitter.fire(data);
    }
    messageReader.onClose(closeHandler);
    messageReader.onError(readErrorHandler);
    messageWriter.onClose(closeHandler);
    messageWriter.onError(writeErrorHandler);
    function triggerMessageQueue() {
        if (timer || messageQueue.size === 0) {
            return;
        }
        timer = (0, ral_1.default)().timer.setImmediate(() => {
            timer = undefined;
            processMessageQueue();
        });
    }
    function processMessageQueue() {
        if (messageQueue.size === 0) {
            return;
        }
        const message = messageQueue.shift();
        try {
            if (messages_1.Message.isRequest(message)) {
                handleRequest(message);
            }
            else if (messages_1.Message.isNotification(message)) {
                handleNotification(message);
            }
            else if (messages_1.Message.isResponse(message)) {
                handleResponse(message);
            }
            else {
                handleInvalidMessage(message);
            }
        }
        finally {
            triggerMessageQueue();
        }
    }
    const callback = (message) => {
        try {
            // We have received a cancellation message. Check if the message is still in the queue
            // and cancel it if allowed to do so.
            if (messages_1.Message.isNotification(message) && message.method === CancelNotification.type.method) {
                const cancelId = message.params.id;
                const key = createRequestQueueKey(cancelId);
                const toCancel = messageQueue.get(key);
                if (messages_1.Message.isRequest(toCancel)) {
                    const strategy = options?.connectionStrategy;
                    const response = (strategy && strategy.cancelUndispatched) ? strategy.cancelUndispatched(toCancel, cancelUndispatched) : cancelUndispatched(toCancel);
                    if (response && (response.error !== undefined || response.result !== undefined)) {
                        messageQueue.delete(key);
                        requestTokens.delete(cancelId);
                        response.id = toCancel.id;
                        traceSendingResponse(response, message.method, Date.now());
                        messageWriter.write(response).catch(() => logger.error(`Sending response for canceled message failed.`));
                        return;
                    }
                }
                const cancellationToken = requestTokens.get(cancelId);
                // The request is already running. Cancel the token
                if (cancellationToken !== undefined) {
                    cancellationToken.cancel();
                    traceReceivedNotification(message);
                    return;
                }
                else {
                    // Remember the cancel but still queue the message to
                    // clean up state in process message.
                    knownCanceledRequests.add(cancelId);
                }
            }
            addMessageToQueue(messageQueue, message);
        }
        finally {
            triggerMessageQueue();
        }
    };
    function handleRequest(requestMessage) {
        if (isDisposed()) {
            // we return here silently since we fired an event when the
            // connection got disposed.
            return;
        }
        function reply(resultOrError, method, startTime) {
            const message = {
                jsonrpc: version,
                id: requestMessage.id
            };
            if (resultOrError instanceof messages_1.ResponseError) {
                message.error = resultOrError.toJson();
            }
            else {
                message.result = resultOrError === undefined ? null : resultOrError;
            }
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
        }
        function replyError(error, method, startTime) {
            const message = {
                jsonrpc: version,
                id: requestMessage.id,
                error: error.toJson()
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
        }
        function replySuccess(result, method, startTime) {
            // The JSON RPC defines that a response must either have a result or an error
            // So we can't treat undefined as a valid response result.
            if (result === undefined) {
                result = null;
            }
            const message = {
                jsonrpc: version,
                id: requestMessage.id,
                result: result
            };
            traceSendingResponse(message, method, startTime);
            messageWriter.write(message).catch(() => logger.error(`Sending response failed.`));
        }
        traceReceivedRequest(requestMessage);
        const element = requestHandlers.get(requestMessage.method);
        let type;
        let requestHandler;
        if (element) {
            type = element.type;
            requestHandler = element.handler;
        }
        const startTime = Date.now();
        if (requestHandler || starRequestHandler) {
            const tokenKey = requestMessage.id ?? String(Date.now()); //
            const cancellationSource = cancellationStrategy.receiver.createCancellationTokenSource(tokenKey);
            if (requestMessage.id !== null && knownCanceledRequests.has(requestMessage.id)) {
                cancellationSource.cancel();
            }
            if (requestMessage.id !== null) {
                requestTokens.set(tokenKey, cancellationSource);
            }
            try {
                let handlerResult;
                if (requestHandler) {
                    if (requestMessage.params === undefined) {
                        if (type !== undefined && type.numberOfParams !== 0) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines ${type.numberOfParams} params but received none.`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(cancellationSource.token);
                    }
                    else if (Array.isArray(requestMessage.params)) {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byName) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by name but received parameters by position`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(...requestMessage.params, cancellationSource.token);
                    }
                    else {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InvalidParams, `Request ${requestMessage.method} defines parameters by position but received parameters by name`), requestMessage.method, startTime);
                            return;
                        }
                        handlerResult = requestHandler(requestMessage.params, cancellationSource.token);
                    }
                }
                else if (starRequestHandler) {
                    handlerResult = starRequestHandler(requestMessage.method, requestMessage.params, cancellationSource.token);
                }
                const promise = handlerResult;
                if (!handlerResult) {
                    requestTokens.delete(tokenKey);
                    replySuccess(handlerResult, requestMessage.method, startTime);
                }
                else if (promise.then) {
                    promise.then((resultOrError) => {
                        requestTokens.delete(tokenKey);
                        reply(resultOrError, requestMessage.method, startTime);
                    }, error => {
                        requestTokens.delete(tokenKey);
                        if (error instanceof messages_1.ResponseError) {
                            replyError(error, requestMessage.method, startTime);
                        }
                        else if (error && Is.string(error.message)) {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                        }
                        else {
                            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                        }
                    });
                }
                else {
                    requestTokens.delete(tokenKey);
                    reply(handlerResult, requestMessage.method, startTime);
                }
            }
            catch (error) {
                requestTokens.delete(tokenKey);
                if (error instanceof messages_1.ResponseError) {
                    reply(error, requestMessage.method, startTime);
                }
                else if (error && Is.string(error.message)) {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed with message: ${error.message}`), requestMessage.method, startTime);
                }
                else {
                    replyError(new messages_1.ResponseError(messages_1.ErrorCodes.InternalError, `Request ${requestMessage.method} failed unexpectedly without providing any details.`), requestMessage.method, startTime);
                }
            }
        }
        else {
            replyError(new messages_1.ResponseError(messages_1.ErrorCodes.MethodNotFound, `Unhandled method ${requestMessage.method}`), requestMessage.method, startTime);
        }
    }
    function handleResponse(responseMessage) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        if (responseMessage.id === null) {
            if (responseMessage.error) {
                logger.error(`Received response message without id: Error is: \n${JSON.stringify(responseMessage.error, undefined, 4)}`);
            }
            else {
                logger.error(`Received response message without id. No further error information provided.`);
            }
        }
        else {
            const key = responseMessage.id;
            const responsePromise = responsePromises.get(key);
            traceReceivedResponse(responseMessage, responsePromise);
            if (responsePromise !== undefined) {
                responsePromises.delete(key);
                try {
                    if (responseMessage.error) {
                        const error = responseMessage.error;
                        responsePromise.reject(new messages_1.ResponseError(error.code, error.message, error.data));
                    }
                    else if (responseMessage.result !== undefined) {
                        responsePromise.resolve(responseMessage.result);
                    }
                    else {
                        throw new Error('Should never happen.');
                    }
                }
                catch (error) {
                    if (error.message) {
                        logger.error(`Response handler '${responsePromise.method}' failed with message: ${error.message}`);
                    }
                    else {
                        logger.error(`Response handler '${responsePromise.method}' failed unexpectedly.`);
                    }
                }
            }
        }
    }
    function handleNotification(message) {
        if (isDisposed()) {
            // See handle request.
            return;
        }
        let type = undefined;
        let notificationHandler;
        if (message.method === CancelNotification.type.method) {
            const cancelId = message.params.id;
            knownCanceledRequests.delete(cancelId);
            traceReceivedNotification(message);
            return;
        }
        else {
            const element = notificationHandlers.get(message.method);
            if (element) {
                notificationHandler = element.handler;
                type = element.type;
            }
        }
        if (notificationHandler || starNotificationHandler) {
            try {
                traceReceivedNotification(message);
                if (notificationHandler) {
                    if (message.params === undefined) {
                        if (type !== undefined) {
                            if (type.numberOfParams !== 0 && type.parameterStructures !== messages_1.ParameterStructures.byName) {
                                logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but received none.`);
                            }
                        }
                        notificationHandler();
                    }
                    else if (Array.isArray(message.params)) {
                        // There are JSON-RPC libraries that send progress message as positional params although
                        // specified as named. So convert them if this is the case.
                        const params = message.params;
                        if (message.method === ProgressNotification.type.method && params.length === 2 && ProgressToken.is(params[0])) {
                            notificationHandler({ token: params[0], value: params[1] });
                        }
                        else {
                            if (type !== undefined) {
                                if (type.parameterStructures === messages_1.ParameterStructures.byName) {
                                    logger.error(`Notification ${message.method} defines parameters by name but received parameters by position`);
                                }
                                if (type.numberOfParams !== message.params.length) {
                                    logger.error(`Notification ${message.method} defines ${type.numberOfParams} params but received ${params.length} arguments`);
                                }
                            }
                            notificationHandler(...params);
                        }
                    }
                    else {
                        if (type !== undefined && type.parameterStructures === messages_1.ParameterStructures.byPosition) {
                            logger.error(`Notification ${message.method} defines parameters by position but received parameters by name`);
                        }
                        notificationHandler(message.params);
                    }
                }
                else if (starNotificationHandler) {
                    starNotificationHandler(message.method, message.params);
                }
            }
            catch (error) {
                if (error.message) {
                    logger.error(`Notification handler '${message.method}' failed with message: ${error.message}`);
                }
                else {
                    logger.error(`Notification handler '${message.method}' failed unexpectedly.`);
                }
            }
        }
        else {
            unhandledNotificationEmitter.fire(message);
        }
    }
    function handleInvalidMessage(message) {
        if (!message) {
            logger.error('Received empty message.');
            return;
        }
        logger.error(`Received message which is neither a response nor a notification message:\n${JSON.stringify(message, null, 4)}`);
        // Test whether we find an id to reject the promise
        const responseMessage = message;
        if (Is.string(responseMessage.id) || Is.number(responseMessage.id)) {
            const key = responseMessage.id;
            const responseHandler = responsePromises.get(key);
            if (responseHandler) {
                responseHandler.reject(new Error('The received response has neither a result nor an error property.'));
            }
        }
    }
    function stringifyTrace(params) {
        if (params === undefined || params === null) {
            return undefined;
        }
        switch (trace) {
            case Trace.Verbose:
                return JSON.stringify(params, null, 4);
            case Trace.Compact:
                return JSON.stringify(params);
            default:
                return undefined;
        }
    }
    function traceSendingRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if ((trace === Trace.Verbose || trace === Trace.Compact) && message.params) {
                data = `Params: ${stringifyTrace(message.params)}\n\n`;
            }
            tracer.log(`Sending request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('send-request', message);
        }
    }
    function traceSendingNotification(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose || trace === Trace.Compact) {
                if (message.params) {
                    data = `Params: ${stringifyTrace(message.params)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Sending notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('send-notification', message);
        }
    }
    function traceSendingResponse(message, method, startTime) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose || trace === Trace.Compact) {
                if (message.error && message.error.data) {
                    data = `Error data: ${stringifyTrace(message.error.data)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${stringifyTrace(message.result)}\n\n`;
                    }
                    else if (message.error === undefined) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            tracer.log(`Sending response '${method} - (${message.id})'. Processing request took ${Date.now() - startTime}ms`, data);
        }
        else {
            logLSPMessage('send-response', message);
        }
    }
    function traceReceivedRequest(message) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if ((trace === Trace.Verbose || trace === Trace.Compact) && message.params) {
                data = `Params: ${stringifyTrace(message.params)}\n\n`;
            }
            tracer.log(`Received request '${message.method} - (${message.id})'.`, data);
        }
        else {
            logLSPMessage('receive-request', message);
        }
    }
    function traceReceivedNotification(message) {
        if (trace === Trace.Off || !tracer || message.method === LogTraceNotification.type.method) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose || trace === Trace.Compact) {
                if (message.params) {
                    data = `Params: ${stringifyTrace(message.params)}\n\n`;
                }
                else {
                    data = 'No parameters provided.\n\n';
                }
            }
            tracer.log(`Received notification '${message.method}'.`, data);
        }
        else {
            logLSPMessage('receive-notification', message);
        }
    }
    function traceReceivedResponse(message, responsePromise) {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        if (traceFormat === TraceFormat.Text) {
            let data = undefined;
            if (trace === Trace.Verbose || trace === Trace.Compact) {
                if (message.error && message.error.data) {
                    data = `Error data: ${stringifyTrace(message.error.data)}\n\n`;
                }
                else {
                    if (message.result) {
                        data = `Result: ${stringifyTrace(message.result)}\n\n`;
                    }
                    else if (message.error === undefined) {
                        data = 'No result returned.\n\n';
                    }
                }
            }
            if (responsePromise) {
                const error = message.error ? ` Request failed: ${message.error.message} (${message.error.code}).` : '';
                tracer.log(`Received response '${responsePromise.method} - (${message.id})' in ${Date.now() - responsePromise.timerStart}ms.${error}`, data);
            }
            else {
                tracer.log(`Received response ${message.id} without active response promise.`, data);
            }
        }
        else {
            logLSPMessage('receive-response', message);
        }
    }
    function logLSPMessage(type, message) {
        if (!tracer || trace === Trace.Off) {
            return;
        }
        const lspMessage = {
            isLSPMessage: true,
            type,
            message,
            timestamp: Date.now()
        };
        tracer.log(lspMessage);
    }
    function throwIfClosedOrDisposed() {
        if (isClosed()) {
            throw new ConnectionError(ConnectionErrors.Closed, 'Connection is closed.');
        }
        if (isDisposed()) {
            throw new ConnectionError(ConnectionErrors.Disposed, 'Connection is disposed.');
        }
    }
    function throwIfListening() {
        if (isListening()) {
            throw new ConnectionError(ConnectionErrors.AlreadyListening, 'Connection is already listening');
        }
    }
    function throwIfNotListening() {
        if (!isListening()) {
            throw new Error('Call listen() first.');
        }
    }
    function undefinedToNull(param) {
        if (param === undefined) {
            return null;
        }
        else {
            return param;
        }
    }
    function nullToUndefined(param) {
        if (param === null) {
            return undefined;
        }
        else {
            return param;
        }
    }
    function isNamedParam(param) {
        return param !== undefined && param !== null && !Array.isArray(param) && typeof param === 'object';
    }
    function computeSingleParam(parameterStructures, param) {
        switch (parameterStructures) {
            case messages_1.ParameterStructures.auto:
                if (isNamedParam(param)) {
                    return nullToUndefined(param);
                }
                else {
                    return [undefinedToNull(param)];
                }
            case messages_1.ParameterStructures.byName:
                if (!isNamedParam(param)) {
                    throw new Error(`Received parameters by name but param is not an object literal.`);
                }
                return nullToUndefined(param);
            case messages_1.ParameterStructures.byPosition:
                return [undefinedToNull(param)];
            default:
                throw new Error(`Unknown parameter structure ${parameterStructures.toString()}`);
        }
    }
    function computeMessageParams(type, params) {
        let result;
        const numberOfParams = type.numberOfParams;
        switch (numberOfParams) {
            case 0:
                result = undefined;
                break;
            case 1:
                result = computeSingleParam(type.parameterStructures, params[0]);
                break;
            default:
                result = [];
                for (let i = 0; i < params.length && i < numberOfParams; i++) {
                    result.push(undefinedToNull(params[i]));
                }
                if (params.length < numberOfParams) {
                    for (let i = params.length; i < numberOfParams; i++) {
                        result.push(null);
                    }
                }
                break;
        }
        return result;
    }
    const connection = {
        sendNotification: (type, ...args) => {
            throwIfClosedOrDisposed();
            let method;
            let messageParams;
            if (Is.string(type)) {
                method = type;
                const first = args[0];
                let paramStart = 0;
                let parameterStructures = messages_1.ParameterStructures.auto;
                if (messages_1.ParameterStructures.is(first)) {
                    paramStart = 1;
                    parameterStructures = first;
                }
                let paramEnd = args.length;
                const numberOfParams = paramEnd - paramStart;
                switch (numberOfParams) {
                    case 0:
                        messageParams = undefined;
                        break;
                    case 1:
                        messageParams = computeSingleParam(parameterStructures, args[paramStart]);
                        break;
                    default:
                        if (parameterStructures === messages_1.ParameterStructures.byName) {
                            throw new Error(`Received ${numberOfParams} parameters for 'by Name' notification parameter structure.`);
                        }
                        messageParams = args.slice(paramStart, paramEnd).map(value => undefinedToNull(value));
                        break;
                }
            }
            else {
                const params = args;
                method = type.method;
                messageParams = computeMessageParams(type, params);
            }
            const notificationMessage = {
                jsonrpc: version,
                method: method,
                params: messageParams
            };
            traceSendingNotification(notificationMessage);
            return messageWriter.write(notificationMessage).catch(() => logger.error(`Sending notification failed.`));
        },
        onNotification: (type, handler) => {
            throwIfClosedOrDisposed();
            let method;
            if (Is.func(type)) {
                starNotificationHandler = type;
            }
            else if (handler) {
                if (Is.string(type)) {
                    method = type;
                    notificationHandlers.set(type, { type: undefined, handler });
                }
                else {
                    method = type.method;
                    notificationHandlers.set(type.method, { type, handler });
                }
            }
            return {
                dispose: () => {
                    if (method !== undefined) {
                        notificationHandlers.delete(method);
                    }
                    else {
                        starNotificationHandler = undefined;
                    }
                }
            };
        },
        onProgress: (_type, token, handler) => {
            if (progressHandlers.has(token)) {
                throw new Error(`Progress handler for token ${token} already registered`);
            }
            progressHandlers.set(token, handler);
            return {
                dispose: () => {
                    progressHandlers.delete(token);
                }
            };
        },
        sendProgress: (_type, token, value) => {
            return connection.sendNotification(ProgressNotification.type, { token, value });
        },
        onUnhandledProgress: unhandledProgressEmitter.event,
        sendRequest: (type, ...args) => {
            throwIfClosedOrDisposed();
            throwIfNotListening();
            let method;
            let messageParams;
            let token = undefined;
            if (Is.string(type)) {
                method = type;
                const first = args[0];
                const last = args[args.length - 1];
                let paramStart = 0;
                let parameterStructures = messages_1.ParameterStructures.auto;
                if (messages_1.ParameterStructures.is(first)) {
                    paramStart = 1;
                    parameterStructures = first;
                }
                let paramEnd = args.length;
                if (cancellation_1.CancellationToken.is(last)) {
                    paramEnd = paramEnd - 1;
                    token = last;
                }
                const numberOfParams = paramEnd - paramStart;
                switch (numberOfParams) {
                    case 0:
                        messageParams = undefined;
                        break;
                    case 1:
                        messageParams = computeSingleParam(parameterStructures, args[paramStart]);
                        break;
                    default:
                        if (parameterStructures === messages_1.ParameterStructures.byName) {
                            throw new Error(`Received ${numberOfParams} parameters for 'by Name' request parameter structure.`);
                        }
                        messageParams = args.slice(paramStart, paramEnd).map(value => undefinedToNull(value));
                        break;
                }
            }
            else {
                const params = args;
                method = type.method;
                messageParams = computeMessageParams(type, params);
                const numberOfParams = type.numberOfParams;
                token = cancellation_1.CancellationToken.is(params[numberOfParams]) ? params[numberOfParams] : undefined;
            }
            const id = sequenceNumber++;
            let disposable;
            if (token) {
                disposable = token.onCancellationRequested(() => {
                    const p = cancellationStrategy.sender.sendCancellation(connection, id);
                    if (p === undefined) {
                        logger.log(`Received no promise from cancellation strategy when cancelling id ${id}`);
                        return Promise.resolve();
                    }
                    else {
                        return p.catch(() => {
                            logger.log(`Sending cancellation messages for id ${id} failed`);
                        });
                    }
                });
            }
            const result = new Promise((resolve, reject) => {
                const requestMessage = {
                    jsonrpc: version,
                    id: id,
                    method: method,
                    params: messageParams
                };
                const resolveWithCleanup = (r) => {
                    resolve(r);
                    cancellationStrategy.sender.cleanup(id);
                    disposable?.dispose();
                };
                const rejectWithCleanup = (r) => {
                    reject(r);
                    cancellationStrategy.sender.cleanup(id);
                    disposable?.dispose();
                };
                let responsePromise = { method: method, timerStart: Date.now(), resolve: resolveWithCleanup, reject: rejectWithCleanup };
                traceSendingRequest(requestMessage);
                try {
                    messageWriter.write(requestMessage).catch(() => logger.error(`Sending request failed.`));
                }
                catch (e) {
                    // Writing the message failed. So we need to reject the promise.
                    responsePromise.reject(new messages_1.ResponseError(messages_1.ErrorCodes.MessageWriteError, e.message ? e.message : 'Unknown reason'));
                    responsePromise = null;
                }
                if (responsePromise) {
                    responsePromises.set(id, responsePromise);
                }
            });
            return result;
        },
        onRequest: (type, handler) => {
            throwIfClosedOrDisposed();
            let method = null;
            if (StarRequestHandler.is(type)) {
                method = undefined;
                starRequestHandler = type;
            }
            else if (Is.string(type)) {
                method = null;
                if (handler !== undefined) {
                    method = type;
                    requestHandlers.set(type, { handler: handler, type: undefined });
                }
            }
            else {
                if (handler !== undefined) {
                    method = type.method;
                    requestHandlers.set(type.method, { type, handler });
                }
            }
            return {
                dispose: () => {
                    if (method === null) {
                        return;
                    }
                    if (method !== undefined) {
                        requestHandlers.delete(method);
                    }
                    else {
                        starRequestHandler = undefined;
                    }
                }
            };
        },
        hasPendingResponse: () => {
            return responsePromises.size > 0;
        },
        trace: async (_value, _tracer, sendNotificationOrTraceOptions) => {
            let _sendNotification = false;
            let _traceFormat = TraceFormat.Text;
            if (sendNotificationOrTraceOptions !== undefined) {
                if (Is.boolean(sendNotificationOrTraceOptions)) {
                    _sendNotification = sendNotificationOrTraceOptions;
                }
                else {
                    _sendNotification = sendNotificationOrTraceOptions.sendNotification || false;
                    _traceFormat = sendNotificationOrTraceOptions.traceFormat || TraceFormat.Text;
                }
            }
            trace = _value;
            traceFormat = _traceFormat;
            if (trace === Trace.Off) {
                tracer = undefined;
            }
            else {
                tracer = _tracer;
            }
            if (_sendNotification && !isClosed() && !isDisposed()) {
                await connection.sendNotification(SetTraceNotification.type, { value: Trace.toString(_value) });
            }
        },
        onError: errorEmitter.event,
        onClose: closeEmitter.event,
        onUnhandledNotification: unhandledNotificationEmitter.event,
        onDispose: disposeEmitter.event,
        end: () => {
            messageWriter.end();
        },
        dispose: () => {
            if (isDisposed()) {
                return;
            }
            state = ConnectionState.Disposed;
            disposeEmitter.fire(undefined);
            const error = new messages_1.ResponseError(messages_1.ErrorCodes.PendingResponseRejected, 'Pending response rejected since connection got disposed');
            for (const promise of responsePromises.values()) {
                promise.reject(error);
            }
            responsePromises = new Map();
            requestTokens = new Map();
            knownCanceledRequests = new Set();
            messageQueue = new linkedMap_1.LinkedMap();
            // Test for backwards compatibility
            if (Is.func(messageWriter.dispose)) {
                messageWriter.dispose();
            }
            if (Is.func(messageReader.dispose)) {
                messageReader.dispose();
            }
        },
        listen: () => {
            throwIfClosedOrDisposed();
            throwIfListening();
            state = ConnectionState.Listening;
            messageReader.listen(callback);
        },
        inspect: () => {
            // eslint-disable-next-line no-console
            (0, ral_1.default)().console.log('inspect');
        }
    };
    connection.onNotification(LogTraceNotification.type, (params) => {
        if (trace === Trace.Off || !tracer) {
            return;
        }
        const verbose = trace === Trace.Verbose || trace === Trace.Compact;
        tracer.log(params.message, verbose ? params.verbose : undefined);
    });
    connection.onNotification(ProgressNotification.type, (params) => {
        const handler = progressHandlers.get(params.token);
        if (handler) {
            handler(params.value);
        }
        else {
            unhandledProgressEmitter.fire(params);
        }
    });
    return connection;
}
exports.createMessageConnection = createMessageConnection;
//# sourceMappingURL=connection.js.map

/***/ }),
/* 74 */
/***/ ((module) => {

"use strict";
module.exports = require("os");

/***/ }),
/* 75 */
/***/ ((module) => {

"use strict";
module.exports = require("crypto");

/***/ }),
/* 76 */
/***/ ((module) => {

"use strict";
module.exports = require("net");

/***/ }),
/* 77 */
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LSPErrorCodes = exports.createProtocolConnection = void 0;
__exportStar(__webpack_require__(58), exports);
__exportStar(__webpack_require__(78), exports);
__exportStar(__webpack_require__(79), exports);
__exportStar(__webpack_require__(80), exports);
var connection_1 = __webpack_require__(102);
Object.defineProperty(exports, "createProtocolConnection", ({ enumerable: true, get: function () { return connection_1.createProtocolConnection; } }));
var LSPErrorCodes;
(function (LSPErrorCodes) {
    /**
    * This is the start range of LSP reserved error codes.
    * It doesn't denote a real error code.
    *
    * @since 3.16.0
    */
    LSPErrorCodes.lspReservedErrorRangeStart = -32899;
    /**
     * A request failed but it was syntactically correct, e.g the
     * method name was known and the parameters were valid. The error
     * message should contain human readable information about why
     * the request failed.
     *
     * @since 3.17.0
     */
    LSPErrorCodes.RequestFailed = -32803;
    /**
     * The server cancelled the request. This error code should
     * only be used for requests that explicitly support being
     * server cancellable.
     *
     * @since 3.17.0
     */
    LSPErrorCodes.ServerCancelled = -32802;
    /**
     * The server detected that the content of a document got
     * modified outside normal conditions. A server should
     * NOT send this error code if it detects a content change
     * in it unprocessed messages. The result even computed
     * on an older state might still be useful for the client.
     *
     * If a client decides that a result is not of any use anymore
     * the client should cancel the request.
     */
    LSPErrorCodes.ContentModified = -32801;
    /**
     * The client has canceled a request and a server as detected
     * the cancel.
     */
    LSPErrorCodes.RequestCancelled = -32800;
    /**
    * This is the end range of LSP reserved error codes.
    * It doesn't denote a real error code.
    *
    * @since 3.16.0
    */
    LSPErrorCodes.lspReservedErrorRangeEnd = -32800;
})(LSPErrorCodes = exports.LSPErrorCodes || (exports.LSPErrorCodes = {}));
//# sourceMappingURL=api.js.map

/***/ }),
/* 78 */
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "AnnotatedTextEdit": () => (/* binding */ AnnotatedTextEdit),
/* harmony export */   "ChangeAnnotation": () => (/* binding */ ChangeAnnotation),
/* harmony export */   "ChangeAnnotationIdentifier": () => (/* binding */ ChangeAnnotationIdentifier),
/* harmony export */   "CodeAction": () => (/* binding */ CodeAction),
/* harmony export */   "CodeActionContext": () => (/* binding */ CodeActionContext),
/* harmony export */   "CodeActionKind": () => (/* binding */ CodeActionKind),
/* harmony export */   "CodeActionTriggerKind": () => (/* binding */ CodeActionTriggerKind),
/* harmony export */   "CodeDescription": () => (/* binding */ CodeDescription),
/* harmony export */   "CodeLens": () => (/* binding */ CodeLens),
/* harmony export */   "Color": () => (/* binding */ Color),
/* harmony export */   "ColorInformation": () => (/* binding */ ColorInformation),
/* harmony export */   "ColorPresentation": () => (/* binding */ ColorPresentation),
/* harmony export */   "Command": () => (/* binding */ Command),
/* harmony export */   "CompletionItem": () => (/* binding */ CompletionItem),
/* harmony export */   "CompletionItemKind": () => (/* binding */ CompletionItemKind),
/* harmony export */   "CompletionItemLabelDetails": () => (/* binding */ CompletionItemLabelDetails),
/* harmony export */   "CompletionItemTag": () => (/* binding */ CompletionItemTag),
/* harmony export */   "CompletionList": () => (/* binding */ CompletionList),
/* harmony export */   "CreateFile": () => (/* binding */ CreateFile),
/* harmony export */   "DeleteFile": () => (/* binding */ DeleteFile),
/* harmony export */   "Diagnostic": () => (/* binding */ Diagnostic),
/* harmony export */   "DiagnosticRelatedInformation": () => (/* binding */ DiagnosticRelatedInformation),
/* harmony export */   "DiagnosticSeverity": () => (/* binding */ DiagnosticSeverity),
/* harmony export */   "DiagnosticTag": () => (/* binding */ DiagnosticTag),
/* harmony export */   "DocumentHighlight": () => (/* binding */ DocumentHighlight),
/* harmony export */   "DocumentHighlightKind": () => (/* binding */ DocumentHighlightKind),
/* harmony export */   "DocumentLink": () => (/* binding */ DocumentLink),
/* harmony export */   "DocumentSymbol": () => (/* binding */ DocumentSymbol),
/* harmony export */   "DocumentUri": () => (/* binding */ DocumentUri),
/* harmony export */   "EOL": () => (/* binding */ EOL),
/* harmony export */   "FoldingRange": () => (/* binding */ FoldingRange),
/* harmony export */   "FoldingRangeKind": () => (/* binding */ FoldingRangeKind),
/* harmony export */   "FormattingOptions": () => (/* binding */ FormattingOptions),
/* harmony export */   "Hover": () => (/* binding */ Hover),
/* harmony export */   "InlayHint": () => (/* binding */ InlayHint),
/* harmony export */   "InlayHintKind": () => (/* binding */ InlayHintKind),
/* harmony export */   "InlayHintLabelPart": () => (/* binding */ InlayHintLabelPart),
/* harmony export */   "InlineValueContext": () => (/* binding */ InlineValueContext),
/* harmony export */   "InlineValueEvaluatableExpression": () => (/* binding */ InlineValueEvaluatableExpression),
/* harmony export */   "InlineValueText": () => (/* binding */ InlineValueText),
/* harmony export */   "InlineValueVariableLookup": () => (/* binding */ InlineValueVariableLookup),
/* harmony export */   "InsertReplaceEdit": () => (/* binding */ InsertReplaceEdit),
/* harmony export */   "InsertTextFormat": () => (/* binding */ InsertTextFormat),
/* harmony export */   "InsertTextMode": () => (/* binding */ InsertTextMode),
/* harmony export */   "Location": () => (/* binding */ Location),
/* harmony export */   "LocationLink": () => (/* binding */ LocationLink),
/* harmony export */   "MarkedString": () => (/* binding */ MarkedString),
/* harmony export */   "MarkupContent": () => (/* binding */ MarkupContent),
/* harmony export */   "MarkupKind": () => (/* binding */ MarkupKind),
/* harmony export */   "OptionalVersionedTextDocumentIdentifier": () => (/* binding */ OptionalVersionedTextDocumentIdentifier),
/* harmony export */   "ParameterInformation": () => (/* binding */ ParameterInformation),
/* harmony export */   "Position": () => (/* binding */ Position),
/* harmony export */   "Range": () => (/* binding */ Range),
/* harmony export */   "RenameFile": () => (/* binding */ RenameFile),
/* harmony export */   "SelectionRange": () => (/* binding */ SelectionRange),
/* harmony export */   "SemanticTokenModifiers": () => (/* binding */ SemanticTokenModifiers),
/* harmony export */   "SemanticTokenTypes": () => (/* binding */ SemanticTokenTypes),
/* harmony export */   "SemanticTokens": () => (/* binding */ SemanticTokens),
/* harmony export */   "SignatureInformation": () => (/* binding */ SignatureInformation),
/* harmony export */   "SymbolInformation": () => (/* binding */ SymbolInformation),
/* harmony export */   "SymbolKind": () => (/* binding */ SymbolKind),
/* harmony export */   "SymbolTag": () => (/* binding */ SymbolTag),
/* harmony export */   "TextDocument": () => (/* binding */ TextDocument),
/* harmony export */   "TextDocumentEdit": () => (/* binding */ TextDocumentEdit),
/* harmony export */   "TextDocumentIdentifier": () => (/* binding */ TextDocumentIdentifier),
/* harmony export */   "TextDocumentItem": () => (/* binding */ TextDocumentItem),
/* harmony export */   "TextEdit": () => (/* binding */ TextEdit),
/* harmony export */   "URI": () => (/* binding */ URI),
/* harmony export */   "VersionedTextDocumentIdentifier": () => (/* binding */ VersionedTextDocumentIdentifier),
/* harmony export */   "WorkspaceChange": () => (/* binding */ WorkspaceChange),
/* harmony export */   "WorkspaceEdit": () => (/* binding */ WorkspaceEdit),
/* harmony export */   "WorkspaceFolder": () => (/* binding */ WorkspaceFolder),
/* harmony export */   "WorkspaceSymbol": () => (/* binding */ WorkspaceSymbol),
/* harmony export */   "integer": () => (/* binding */ integer),
/* harmony export */   "uinteger": () => (/* binding */ uinteger)
/* harmony export */ });
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

var DocumentUri;
(function (DocumentUri) {
    function is(value) {
        return typeof value === 'string';
    }
    DocumentUri.is = is;
})(DocumentUri || (DocumentUri = {}));
var URI;
(function (URI) {
    function is(value) {
        return typeof value === 'string';
    }
    URI.is = is;
})(URI || (URI = {}));
var integer;
(function (integer) {
    integer.MIN_VALUE = -2147483648;
    integer.MAX_VALUE = 2147483647;
    function is(value) {
        return typeof value === 'number' && integer.MIN_VALUE <= value && value <= integer.MAX_VALUE;
    }
    integer.is = is;
})(integer || (integer = {}));
var uinteger;
(function (uinteger) {
    uinteger.MIN_VALUE = 0;
    uinteger.MAX_VALUE = 2147483647;
    function is(value) {
        return typeof value === 'number' && uinteger.MIN_VALUE <= value && value <= uinteger.MAX_VALUE;
    }
    uinteger.is = is;
})(uinteger || (uinteger = {}));
/**
 * The Position namespace provides helper functions to work with
 * [Position](#Position) literals.
 */
var Position;
(function (Position) {
    /**
     * Creates a new Position literal from the given line and character.
     * @param line The position's line.
     * @param character The position's character.
     */
    function create(line, character) {
        if (line === Number.MAX_VALUE) {
            line = uinteger.MAX_VALUE;
        }
        if (character === Number.MAX_VALUE) {
            character = uinteger.MAX_VALUE;
        }
        return { line: line, character: character };
    }
    Position.create = create;
    /**
     * Checks whether the given literal conforms to the [Position](#Position) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.uinteger(candidate.line) && Is.uinteger(candidate.character);
    }
    Position.is = is;
})(Position || (Position = {}));
/**
 * The Range namespace provides helper functions to work with
 * [Range](#Range) literals.
 */
var Range;
(function (Range) {
    function create(one, two, three, four) {
        if (Is.uinteger(one) && Is.uinteger(two) && Is.uinteger(three) && Is.uinteger(four)) {
            return { start: Position.create(one, two), end: Position.create(three, four) };
        }
        else if (Position.is(one) && Position.is(two)) {
            return { start: one, end: two };
        }
        else {
            throw new Error("Range#create called with invalid arguments[".concat(one, ", ").concat(two, ", ").concat(three, ", ").concat(four, "]"));
        }
    }
    Range.create = create;
    /**
     * Checks whether the given literal conforms to the [Range](#Range) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.start) && Position.is(candidate.end);
    }
    Range.is = is;
})(Range || (Range = {}));
/**
 * The Location namespace provides helper functions to work with
 * [Location](#Location) literals.
 */
var Location;
(function (Location) {
    /**
     * Creates a Location literal.
     * @param uri The location's uri.
     * @param range The location's range.
     */
    function create(uri, range) {
        return { uri: uri, range: range };
    }
    Location.create = create;
    /**
     * Checks whether the given literal conforms to the [Location](#Location) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.string(candidate.uri) || Is.undefined(candidate.uri));
    }
    Location.is = is;
})(Location || (Location = {}));
/**
 * The LocationLink namespace provides helper functions to work with
 * [LocationLink](#LocationLink) literals.
 */
var LocationLink;
(function (LocationLink) {
    /**
     * Creates a LocationLink literal.
     * @param targetUri The definition's uri.
     * @param targetRange The full range of the definition.
     * @param targetSelectionRange The span of the symbol definition at the target.
     * @param originSelectionRange The span of the symbol being defined in the originating source file.
     */
    function create(targetUri, targetRange, targetSelectionRange, originSelectionRange) {
        return { targetUri: targetUri, targetRange: targetRange, targetSelectionRange: targetSelectionRange, originSelectionRange: originSelectionRange };
    }
    LocationLink.create = create;
    /**
     * Checks whether the given literal conforms to the [LocationLink](#LocationLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.targetRange) && Is.string(candidate.targetUri)
            && Range.is(candidate.targetSelectionRange)
            && (Range.is(candidate.originSelectionRange) || Is.undefined(candidate.originSelectionRange));
    }
    LocationLink.is = is;
})(LocationLink || (LocationLink = {}));
/**
 * The Color namespace provides helper functions to work with
 * [Color](#Color) literals.
 */
var Color;
(function (Color) {
    /**
     * Creates a new Color literal.
     */
    function create(red, green, blue, alpha) {
        return {
            red: red,
            green: green,
            blue: blue,
            alpha: alpha,
        };
    }
    Color.create = create;
    /**
     * Checks whether the given literal conforms to the [Color](#Color) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.numberRange(candidate.red, 0, 1)
            && Is.numberRange(candidate.green, 0, 1)
            && Is.numberRange(candidate.blue, 0, 1)
            && Is.numberRange(candidate.alpha, 0, 1);
    }
    Color.is = is;
})(Color || (Color = {}));
/**
 * The ColorInformation namespace provides helper functions to work with
 * [ColorInformation](#ColorInformation) literals.
 */
var ColorInformation;
(function (ColorInformation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(range, color) {
        return {
            range: range,
            color: color,
        };
    }
    ColorInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.range) && Color.is(candidate.color);
    }
    ColorInformation.is = is;
})(ColorInformation || (ColorInformation = {}));
/**
 * The Color namespace provides helper functions to work with
 * [ColorPresentation](#ColorPresentation) literals.
 */
var ColorPresentation;
(function (ColorPresentation) {
    /**
     * Creates a new ColorInformation literal.
     */
    function create(label, textEdit, additionalTextEdits) {
        return {
            label: label,
            textEdit: textEdit,
            additionalTextEdits: additionalTextEdits,
        };
    }
    ColorPresentation.create = create;
    /**
     * Checks whether the given literal conforms to the [ColorInformation](#ColorInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.label)
            && (Is.undefined(candidate.textEdit) || TextEdit.is(candidate))
            && (Is.undefined(candidate.additionalTextEdits) || Is.typedArray(candidate.additionalTextEdits, TextEdit.is));
    }
    ColorPresentation.is = is;
})(ColorPresentation || (ColorPresentation = {}));
/**
 * A set of predefined range kinds.
 */
var FoldingRangeKind;
(function (FoldingRangeKind) {
    /**
     * Folding range for a comment
     */
    FoldingRangeKind.Comment = 'comment';
    /**
     * Folding range for a imports or includes
     */
    FoldingRangeKind.Imports = 'imports';
    /**
     * Folding range for a region (e.g. `#region`)
     */
    FoldingRangeKind.Region = 'region';
})(FoldingRangeKind || (FoldingRangeKind = {}));
/**
 * The folding range namespace provides helper functions to work with
 * [FoldingRange](#FoldingRange) literals.
 */
var FoldingRange;
(function (FoldingRange) {
    /**
     * Creates a new FoldingRange literal.
     */
    function create(startLine, endLine, startCharacter, endCharacter, kind, collapsedText) {
        var result = {
            startLine: startLine,
            endLine: endLine
        };
        if (Is.defined(startCharacter)) {
            result.startCharacter = startCharacter;
        }
        if (Is.defined(endCharacter)) {
            result.endCharacter = endCharacter;
        }
        if (Is.defined(kind)) {
            result.kind = kind;
        }
        if (Is.defined(collapsedText)) {
            result.collapsedText = collapsedText;
        }
        return result;
    }
    FoldingRange.create = create;
    /**
     * Checks whether the given literal conforms to the [FoldingRange](#FoldingRange) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.uinteger(candidate.startLine) && Is.uinteger(candidate.startLine)
            && (Is.undefined(candidate.startCharacter) || Is.uinteger(candidate.startCharacter))
            && (Is.undefined(candidate.endCharacter) || Is.uinteger(candidate.endCharacter))
            && (Is.undefined(candidate.kind) || Is.string(candidate.kind));
    }
    FoldingRange.is = is;
})(FoldingRange || (FoldingRange = {}));
/**
 * The DiagnosticRelatedInformation namespace provides helper functions to work with
 * [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) literals.
 */
var DiagnosticRelatedInformation;
(function (DiagnosticRelatedInformation) {
    /**
     * Creates a new DiagnosticRelatedInformation literal.
     */
    function create(location, message) {
        return {
            location: location,
            message: message
        };
    }
    DiagnosticRelatedInformation.create = create;
    /**
     * Checks whether the given literal conforms to the [DiagnosticRelatedInformation](#DiagnosticRelatedInformation) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Location.is(candidate.location) && Is.string(candidate.message);
    }
    DiagnosticRelatedInformation.is = is;
})(DiagnosticRelatedInformation || (DiagnosticRelatedInformation = {}));
/**
 * The diagnostic's severity.
 */
var DiagnosticSeverity;
(function (DiagnosticSeverity) {
    /**
     * Reports an error.
     */
    DiagnosticSeverity.Error = 1;
    /**
     * Reports a warning.
     */
    DiagnosticSeverity.Warning = 2;
    /**
     * Reports an information.
     */
    DiagnosticSeverity.Information = 3;
    /**
     * Reports a hint.
     */
    DiagnosticSeverity.Hint = 4;
})(DiagnosticSeverity || (DiagnosticSeverity = {}));
/**
 * The diagnostic tags.
 *
 * @since 3.15.0
 */
var DiagnosticTag;
(function (DiagnosticTag) {
    /**
     * Unused or unnecessary code.
     *
     * Clients are allowed to render diagnostics with this tag faded out instead of having
     * an error squiggle.
     */
    DiagnosticTag.Unnecessary = 1;
    /**
     * Deprecated or obsolete code.
     *
     * Clients are allowed to rendered diagnostics with this tag strike through.
     */
    DiagnosticTag.Deprecated = 2;
})(DiagnosticTag || (DiagnosticTag = {}));
/**
 * The CodeDescription namespace provides functions to deal with descriptions for diagnostic codes.
 *
 * @since 3.16.0
 */
var CodeDescription;
(function (CodeDescription) {
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.href);
    }
    CodeDescription.is = is;
})(CodeDescription || (CodeDescription = {}));
/**
 * The Diagnostic namespace provides helper functions to work with
 * [Diagnostic](#Diagnostic) literals.
 */
var Diagnostic;
(function (Diagnostic) {
    /**
     * Creates a new Diagnostic literal.
     */
    function create(range, message, severity, code, source, relatedInformation) {
        var result = { range: range, message: message };
        if (Is.defined(severity)) {
            result.severity = severity;
        }
        if (Is.defined(code)) {
            result.code = code;
        }
        if (Is.defined(source)) {
            result.source = source;
        }
        if (Is.defined(relatedInformation)) {
            result.relatedInformation = relatedInformation;
        }
        return result;
    }
    Diagnostic.create = create;
    /**
     * Checks whether the given literal conforms to the [Diagnostic](#Diagnostic) interface.
     */
    function is(value) {
        var _a;
        var candidate = value;
        return Is.defined(candidate)
            && Range.is(candidate.range)
            && Is.string(candidate.message)
            && (Is.number(candidate.severity) || Is.undefined(candidate.severity))
            && (Is.integer(candidate.code) || Is.string(candidate.code) || Is.undefined(candidate.code))
            && (Is.undefined(candidate.codeDescription) || (Is.string((_a = candidate.codeDescription) === null || _a === void 0 ? void 0 : _a.href)))
            && (Is.string(candidate.source) || Is.undefined(candidate.source))
            && (Is.undefined(candidate.relatedInformation) || Is.typedArray(candidate.relatedInformation, DiagnosticRelatedInformation.is));
    }
    Diagnostic.is = is;
})(Diagnostic || (Diagnostic = {}));
/**
 * The Command namespace provides helper functions to work with
 * [Command](#Command) literals.
 */
var Command;
(function (Command) {
    /**
     * Creates a new Command literal.
     */
    function create(title, command) {
        var args = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            args[_i - 2] = arguments[_i];
        }
        var result = { title: title, command: command };
        if (Is.defined(args) && args.length > 0) {
            result.arguments = args;
        }
        return result;
    }
    Command.create = create;
    /**
     * Checks whether the given literal conforms to the [Command](#Command) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.title) && Is.string(candidate.command);
    }
    Command.is = is;
})(Command || (Command = {}));
/**
 * The TextEdit namespace provides helper function to create replace,
 * insert and delete edits more easily.
 */
var TextEdit;
(function (TextEdit) {
    /**
     * Creates a replace text edit.
     * @param range The range of text to be replaced.
     * @param newText The new text.
     */
    function replace(range, newText) {
        return { range: range, newText: newText };
    }
    TextEdit.replace = replace;
    /**
     * Creates a insert text edit.
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     */
    function insert(position, newText) {
        return { range: { start: position, end: position }, newText: newText };
    }
    TextEdit.insert = insert;
    /**
     * Creates a delete text edit.
     * @param range The range of text to be deleted.
     */
    function del(range) {
        return { range: range, newText: '' };
    }
    TextEdit.del = del;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate)
            && Is.string(candidate.newText)
            && Range.is(candidate.range);
    }
    TextEdit.is = is;
})(TextEdit || (TextEdit = {}));
var ChangeAnnotation;
(function (ChangeAnnotation) {
    function create(label, needsConfirmation, description) {
        var result = { label: label };
        if (needsConfirmation !== undefined) {
            result.needsConfirmation = needsConfirmation;
        }
        if (description !== undefined) {
            result.description = description;
        }
        return result;
    }
    ChangeAnnotation.create = create;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.label) &&
            (Is.boolean(candidate.needsConfirmation) || candidate.needsConfirmation === undefined) &&
            (Is.string(candidate.description) || candidate.description === undefined);
    }
    ChangeAnnotation.is = is;
})(ChangeAnnotation || (ChangeAnnotation = {}));
var ChangeAnnotationIdentifier;
(function (ChangeAnnotationIdentifier) {
    function is(value) {
        var candidate = value;
        return Is.string(candidate);
    }
    ChangeAnnotationIdentifier.is = is;
})(ChangeAnnotationIdentifier || (ChangeAnnotationIdentifier = {}));
var AnnotatedTextEdit;
(function (AnnotatedTextEdit) {
    /**
     * Creates an annotated replace text edit.
     *
     * @param range The range of text to be replaced.
     * @param newText The new text.
     * @param annotation The annotation.
     */
    function replace(range, newText, annotation) {
        return { range: range, newText: newText, annotationId: annotation };
    }
    AnnotatedTextEdit.replace = replace;
    /**
     * Creates an annotated insert text edit.
     *
     * @param position The position to insert the text at.
     * @param newText The text to be inserted.
     * @param annotation The annotation.
     */
    function insert(position, newText, annotation) {
        return { range: { start: position, end: position }, newText: newText, annotationId: annotation };
    }
    AnnotatedTextEdit.insert = insert;
    /**
     * Creates an annotated delete text edit.
     *
     * @param range The range of text to be deleted.
     * @param annotation The annotation.
     */
    function del(range, annotation) {
        return { range: range, newText: '', annotationId: annotation };
    }
    AnnotatedTextEdit.del = del;
    function is(value) {
        var candidate = value;
        return TextEdit.is(candidate) && (ChangeAnnotation.is(candidate.annotationId) || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    AnnotatedTextEdit.is = is;
})(AnnotatedTextEdit || (AnnotatedTextEdit = {}));
/**
 * The TextDocumentEdit namespace provides helper function to create
 * an edit that manipulates a text document.
 */
var TextDocumentEdit;
(function (TextDocumentEdit) {
    /**
     * Creates a new `TextDocumentEdit`
     */
    function create(textDocument, edits) {
        return { textDocument: textDocument, edits: edits };
    }
    TextDocumentEdit.create = create;
    function is(value) {
        var candidate = value;
        return Is.defined(candidate)
            && OptionalVersionedTextDocumentIdentifier.is(candidate.textDocument)
            && Array.isArray(candidate.edits);
    }
    TextDocumentEdit.is = is;
})(TextDocumentEdit || (TextDocumentEdit = {}));
var CreateFile;
(function (CreateFile) {
    function create(uri, options, annotation) {
        var result = {
            kind: 'create',
            uri: uri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    CreateFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'create' && Is.string(candidate.uri) && (candidate.options === undefined ||
            ((candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    CreateFile.is = is;
})(CreateFile || (CreateFile = {}));
var RenameFile;
(function (RenameFile) {
    function create(oldUri, newUri, options, annotation) {
        var result = {
            kind: 'rename',
            oldUri: oldUri,
            newUri: newUri
        };
        if (options !== undefined && (options.overwrite !== undefined || options.ignoreIfExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    RenameFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'rename' && Is.string(candidate.oldUri) && Is.string(candidate.newUri) && (candidate.options === undefined ||
            ((candidate.options.overwrite === undefined || Is.boolean(candidate.options.overwrite)) && (candidate.options.ignoreIfExists === undefined || Is.boolean(candidate.options.ignoreIfExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    RenameFile.is = is;
})(RenameFile || (RenameFile = {}));
var DeleteFile;
(function (DeleteFile) {
    function create(uri, options, annotation) {
        var result = {
            kind: 'delete',
            uri: uri
        };
        if (options !== undefined && (options.recursive !== undefined || options.ignoreIfNotExists !== undefined)) {
            result.options = options;
        }
        if (annotation !== undefined) {
            result.annotationId = annotation;
        }
        return result;
    }
    DeleteFile.create = create;
    function is(value) {
        var candidate = value;
        return candidate && candidate.kind === 'delete' && Is.string(candidate.uri) && (candidate.options === undefined ||
            ((candidate.options.recursive === undefined || Is.boolean(candidate.options.recursive)) && (candidate.options.ignoreIfNotExists === undefined || Is.boolean(candidate.options.ignoreIfNotExists)))) && (candidate.annotationId === undefined || ChangeAnnotationIdentifier.is(candidate.annotationId));
    }
    DeleteFile.is = is;
})(DeleteFile || (DeleteFile = {}));
var WorkspaceEdit;
(function (WorkspaceEdit) {
    function is(value) {
        var candidate = value;
        return candidate &&
            (candidate.changes !== undefined || candidate.documentChanges !== undefined) &&
            (candidate.documentChanges === undefined || candidate.documentChanges.every(function (change) {
                if (Is.string(change.kind)) {
                    return CreateFile.is(change) || RenameFile.is(change) || DeleteFile.is(change);
                }
                else {
                    return TextDocumentEdit.is(change);
                }
            }));
    }
    WorkspaceEdit.is = is;
})(WorkspaceEdit || (WorkspaceEdit = {}));
var TextEditChangeImpl = /** @class */ (function () {
    function TextEditChangeImpl(edits, changeAnnotations) {
        this.edits = edits;
        this.changeAnnotations = changeAnnotations;
    }
    TextEditChangeImpl.prototype.insert = function (position, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.insert(position, newText);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.insert(position, newText, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.insert(position, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.replace = function (range, newText, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.replace(range, newText);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.replace(range, newText, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.replace(range, newText, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.delete = function (range, annotation) {
        var edit;
        var id;
        if (annotation === undefined) {
            edit = TextEdit.del(range);
        }
        else if (ChangeAnnotationIdentifier.is(annotation)) {
            id = annotation;
            edit = AnnotatedTextEdit.del(range, annotation);
        }
        else {
            this.assertChangeAnnotations(this.changeAnnotations);
            id = this.changeAnnotations.manage(annotation);
            edit = AnnotatedTextEdit.del(range, id);
        }
        this.edits.push(edit);
        if (id !== undefined) {
            return id;
        }
    };
    TextEditChangeImpl.prototype.add = function (edit) {
        this.edits.push(edit);
    };
    TextEditChangeImpl.prototype.all = function () {
        return this.edits;
    };
    TextEditChangeImpl.prototype.clear = function () {
        this.edits.splice(0, this.edits.length);
    };
    TextEditChangeImpl.prototype.assertChangeAnnotations = function (value) {
        if (value === undefined) {
            throw new Error("Text edit change is not configured to manage change annotations.");
        }
    };
    return TextEditChangeImpl;
}());
/**
 * A helper class
 */
var ChangeAnnotations = /** @class */ (function () {
    function ChangeAnnotations(annotations) {
        this._annotations = annotations === undefined ? Object.create(null) : annotations;
        this._counter = 0;
        this._size = 0;
    }
    ChangeAnnotations.prototype.all = function () {
        return this._annotations;
    };
    Object.defineProperty(ChangeAnnotations.prototype, "size", {
        get: function () {
            return this._size;
        },
        enumerable: false,
        configurable: true
    });
    ChangeAnnotations.prototype.manage = function (idOrAnnotation, annotation) {
        var id;
        if (ChangeAnnotationIdentifier.is(idOrAnnotation)) {
            id = idOrAnnotation;
        }
        else {
            id = this.nextId();
            annotation = idOrAnnotation;
        }
        if (this._annotations[id] !== undefined) {
            throw new Error("Id ".concat(id, " is already in use."));
        }
        if (annotation === undefined) {
            throw new Error("No annotation provided for id ".concat(id));
        }
        this._annotations[id] = annotation;
        this._size++;
        return id;
    };
    ChangeAnnotations.prototype.nextId = function () {
        this._counter++;
        return this._counter.toString();
    };
    return ChangeAnnotations;
}());
/**
 * A workspace change helps constructing changes to a workspace.
 */
var WorkspaceChange = /** @class */ (function () {
    function WorkspaceChange(workspaceEdit) {
        var _this = this;
        this._textEditChanges = Object.create(null);
        if (workspaceEdit !== undefined) {
            this._workspaceEdit = workspaceEdit;
            if (workspaceEdit.documentChanges) {
                this._changeAnnotations = new ChangeAnnotations(workspaceEdit.changeAnnotations);
                workspaceEdit.changeAnnotations = this._changeAnnotations.all();
                workspaceEdit.documentChanges.forEach(function (change) {
                    if (TextDocumentEdit.is(change)) {
                        var textEditChange = new TextEditChangeImpl(change.edits, _this._changeAnnotations);
                        _this._textEditChanges[change.textDocument.uri] = textEditChange;
                    }
                });
            }
            else if (workspaceEdit.changes) {
                Object.keys(workspaceEdit.changes).forEach(function (key) {
                    var textEditChange = new TextEditChangeImpl(workspaceEdit.changes[key]);
                    _this._textEditChanges[key] = textEditChange;
                });
            }
        }
        else {
            this._workspaceEdit = {};
        }
    }
    Object.defineProperty(WorkspaceChange.prototype, "edit", {
        /**
         * Returns the underlying [WorkspaceEdit](#WorkspaceEdit) literal
         * use to be returned from a workspace edit operation like rename.
         */
        get: function () {
            this.initDocumentChanges();
            if (this._changeAnnotations !== undefined) {
                if (this._changeAnnotations.size === 0) {
                    this._workspaceEdit.changeAnnotations = undefined;
                }
                else {
                    this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
                }
            }
            return this._workspaceEdit;
        },
        enumerable: false,
        configurable: true
    });
    WorkspaceChange.prototype.getTextEditChange = function (key) {
        if (OptionalVersionedTextDocumentIdentifier.is(key)) {
            this.initDocumentChanges();
            if (this._workspaceEdit.documentChanges === undefined) {
                throw new Error('Workspace edit is not configured for document changes.');
            }
            var textDocument = { uri: key.uri, version: key.version };
            var result = this._textEditChanges[textDocument.uri];
            if (!result) {
                var edits = [];
                var textDocumentEdit = {
                    textDocument: textDocument,
                    edits: edits
                };
                this._workspaceEdit.documentChanges.push(textDocumentEdit);
                result = new TextEditChangeImpl(edits, this._changeAnnotations);
                this._textEditChanges[textDocument.uri] = result;
            }
            return result;
        }
        else {
            this.initChanges();
            if (this._workspaceEdit.changes === undefined) {
                throw new Error('Workspace edit is not configured for normal text edit changes.');
            }
            var result = this._textEditChanges[key];
            if (!result) {
                var edits = [];
                this._workspaceEdit.changes[key] = edits;
                result = new TextEditChangeImpl(edits);
                this._textEditChanges[key] = result;
            }
            return result;
        }
    };
    WorkspaceChange.prototype.initDocumentChanges = function () {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
            this._changeAnnotations = new ChangeAnnotations();
            this._workspaceEdit.documentChanges = [];
            this._workspaceEdit.changeAnnotations = this._changeAnnotations.all();
        }
    };
    WorkspaceChange.prototype.initChanges = function () {
        if (this._workspaceEdit.documentChanges === undefined && this._workspaceEdit.changes === undefined) {
            this._workspaceEdit.changes = Object.create(null);
        }
    };
    WorkspaceChange.prototype.createFile = function (uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = CreateFile.create(uri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = CreateFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    WorkspaceChange.prototype.renameFile = function (oldUri, newUri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = RenameFile.create(oldUri, newUri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = RenameFile.create(oldUri, newUri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    WorkspaceChange.prototype.deleteFile = function (uri, optionsOrAnnotation, options) {
        this.initDocumentChanges();
        if (this._workspaceEdit.documentChanges === undefined) {
            throw new Error('Workspace edit is not configured for document changes.');
        }
        var annotation;
        if (ChangeAnnotation.is(optionsOrAnnotation) || ChangeAnnotationIdentifier.is(optionsOrAnnotation)) {
            annotation = optionsOrAnnotation;
        }
        else {
            options = optionsOrAnnotation;
        }
        var operation;
        var id;
        if (annotation === undefined) {
            operation = DeleteFile.create(uri, options);
        }
        else {
            id = ChangeAnnotationIdentifier.is(annotation) ? annotation : this._changeAnnotations.manage(annotation);
            operation = DeleteFile.create(uri, options, id);
        }
        this._workspaceEdit.documentChanges.push(operation);
        if (id !== undefined) {
            return id;
        }
    };
    return WorkspaceChange;
}());

/**
 * The TextDocumentIdentifier namespace provides helper functions to work with
 * [TextDocumentIdentifier](#TextDocumentIdentifier) literals.
 */
var TextDocumentIdentifier;
(function (TextDocumentIdentifier) {
    /**
     * Creates a new TextDocumentIdentifier literal.
     * @param uri The document's uri.
     */
    function create(uri) {
        return { uri: uri };
    }
    TextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentIdentifier](#TextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri);
    }
    TextDocumentIdentifier.is = is;
})(TextDocumentIdentifier || (TextDocumentIdentifier = {}));
/**
 * The VersionedTextDocumentIdentifier namespace provides helper functions to work with
 * [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) literals.
 */
var VersionedTextDocumentIdentifier;
(function (VersionedTextDocumentIdentifier) {
    /**
     * Creates a new VersionedTextDocumentIdentifier literal.
     * @param uri The document's uri.
     * @param version The document's version.
     */
    function create(uri, version) {
        return { uri: uri, version: version };
    }
    VersionedTextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [VersionedTextDocumentIdentifier](#VersionedTextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.integer(candidate.version);
    }
    VersionedTextDocumentIdentifier.is = is;
})(VersionedTextDocumentIdentifier || (VersionedTextDocumentIdentifier = {}));
/**
 * The OptionalVersionedTextDocumentIdentifier namespace provides helper functions to work with
 * [OptionalVersionedTextDocumentIdentifier](#OptionalVersionedTextDocumentIdentifier) literals.
 */
var OptionalVersionedTextDocumentIdentifier;
(function (OptionalVersionedTextDocumentIdentifier) {
    /**
     * Creates a new OptionalVersionedTextDocumentIdentifier literal.
     * @param uri The document's uri.
     * @param version The document's version.
     */
    function create(uri, version) {
        return { uri: uri, version: version };
    }
    OptionalVersionedTextDocumentIdentifier.create = create;
    /**
     * Checks whether the given literal conforms to the [OptionalVersionedTextDocumentIdentifier](#OptionalVersionedTextDocumentIdentifier) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (candidate.version === null || Is.integer(candidate.version));
    }
    OptionalVersionedTextDocumentIdentifier.is = is;
})(OptionalVersionedTextDocumentIdentifier || (OptionalVersionedTextDocumentIdentifier = {}));
/**
 * The TextDocumentItem namespace provides helper functions to work with
 * [TextDocumentItem](#TextDocumentItem) literals.
 */
var TextDocumentItem;
(function (TextDocumentItem) {
    /**
     * Creates a new TextDocumentItem literal.
     * @param uri The document's uri.
     * @param languageId The document's language identifier.
     * @param version The document's version number.
     * @param text The document's text.
     */
    function create(uri, languageId, version, text) {
        return { uri: uri, languageId: languageId, version: version, text: text };
    }
    TextDocumentItem.create = create;
    /**
     * Checks whether the given literal conforms to the [TextDocumentItem](#TextDocumentItem) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && Is.string(candidate.languageId) && Is.integer(candidate.version) && Is.string(candidate.text);
    }
    TextDocumentItem.is = is;
})(TextDocumentItem || (TextDocumentItem = {}));
/**
 * Describes the content type that a client supports in various
 * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
 *
 * Please note that `MarkupKinds` must not start with a `$`. This kinds
 * are reserved for internal usage.
 */
var MarkupKind;
(function (MarkupKind) {
    /**
     * Plain text is supported as a content format
     */
    MarkupKind.PlainText = 'plaintext';
    /**
     * Markdown is supported as a content format
     */
    MarkupKind.Markdown = 'markdown';
    /**
     * Checks whether the given value is a value of the [MarkupKind](#MarkupKind) type.
     */
    function is(value) {
        var candidate = value;
        return candidate === MarkupKind.PlainText || candidate === MarkupKind.Markdown;
    }
    MarkupKind.is = is;
})(MarkupKind || (MarkupKind = {}));
var MarkupContent;
(function (MarkupContent) {
    /**
     * Checks whether the given value conforms to the [MarkupContent](#MarkupContent) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(value) && MarkupKind.is(candidate.kind) && Is.string(candidate.value);
    }
    MarkupContent.is = is;
})(MarkupContent || (MarkupContent = {}));
/**
 * The kind of a completion entry.
 */
var CompletionItemKind;
(function (CompletionItemKind) {
    CompletionItemKind.Text = 1;
    CompletionItemKind.Method = 2;
    CompletionItemKind.Function = 3;
    CompletionItemKind.Constructor = 4;
    CompletionItemKind.Field = 5;
    CompletionItemKind.Variable = 6;
    CompletionItemKind.Class = 7;
    CompletionItemKind.Interface = 8;
    CompletionItemKind.Module = 9;
    CompletionItemKind.Property = 10;
    CompletionItemKind.Unit = 11;
    CompletionItemKind.Value = 12;
    CompletionItemKind.Enum = 13;
    CompletionItemKind.Keyword = 14;
    CompletionItemKind.Snippet = 15;
    CompletionItemKind.Color = 16;
    CompletionItemKind.File = 17;
    CompletionItemKind.Reference = 18;
    CompletionItemKind.Folder = 19;
    CompletionItemKind.EnumMember = 20;
    CompletionItemKind.Constant = 21;
    CompletionItemKind.Struct = 22;
    CompletionItemKind.Event = 23;
    CompletionItemKind.Operator = 24;
    CompletionItemKind.TypeParameter = 25;
})(CompletionItemKind || (CompletionItemKind = {}));
/**
 * Defines whether the insert text in a completion item should be interpreted as
 * plain text or a snippet.
 */
var InsertTextFormat;
(function (InsertTextFormat) {
    /**
     * The primary text to be inserted is treated as a plain string.
     */
    InsertTextFormat.PlainText = 1;
    /**
     * The primary text to be inserted is treated as a snippet.
     *
     * A snippet can define tab stops and placeholders with `$1`, `$2`
     * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
     * the end of the snippet. Placeholders with equal identifiers are linked,
     * that is typing in one will update others too.
     *
     * See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax
     */
    InsertTextFormat.Snippet = 2;
})(InsertTextFormat || (InsertTextFormat = {}));
/**
 * Completion item tags are extra annotations that tweak the rendering of a completion
 * item.
 *
 * @since 3.15.0
 */
var CompletionItemTag;
(function (CompletionItemTag) {
    /**
     * Render a completion as obsolete, usually using a strike-out.
     */
    CompletionItemTag.Deprecated = 1;
})(CompletionItemTag || (CompletionItemTag = {}));
/**
 * The InsertReplaceEdit namespace provides functions to deal with insert / replace edits.
 *
 * @since 3.16.0
 */
var InsertReplaceEdit;
(function (InsertReplaceEdit) {
    /**
     * Creates a new insert / replace edit
     */
    function create(newText, insert, replace) {
        return { newText: newText, insert: insert, replace: replace };
    }
    InsertReplaceEdit.create = create;
    /**
     * Checks whether the given literal conforms to the [InsertReplaceEdit](#InsertReplaceEdit) interface.
     */
    function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.newText) && Range.is(candidate.insert) && Range.is(candidate.replace);
    }
    InsertReplaceEdit.is = is;
})(InsertReplaceEdit || (InsertReplaceEdit = {}));
/**
 * How whitespace and indentation is handled during completion
 * item insertion.
 *
 * @since 3.16.0
 */
var InsertTextMode;
(function (InsertTextMode) {
    /**
     * The insertion or replace strings is taken as it is. If the
     * value is multi line the lines below the cursor will be
     * inserted using the indentation defined in the string value.
     * The client will not apply any kind of adjustments to the
     * string.
     */
    InsertTextMode.asIs = 1;
    /**
     * The editor adjusts leading whitespace of new lines so that
     * they match the indentation up to the cursor of the line for
     * which the item is accepted.
     *
     * Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
     * multi line completion item is indented using 2 tabs and all
     * following lines inserted will be indented using 2 tabs as well.
     */
    InsertTextMode.adjustIndentation = 2;
})(InsertTextMode || (InsertTextMode = {}));
var CompletionItemLabelDetails;
(function (CompletionItemLabelDetails) {
    function is(value) {
        var candidate = value;
        return candidate && (Is.string(candidate.detail) || candidate.detail === undefined) &&
            (Is.string(candidate.description) || candidate.description === undefined);
    }
    CompletionItemLabelDetails.is = is;
})(CompletionItemLabelDetails || (CompletionItemLabelDetails = {}));
/**
 * The CompletionItem namespace provides functions to deal with
 * completion items.
 */
var CompletionItem;
(function (CompletionItem) {
    /**
     * Create a completion item and seed it with a label.
     * @param label The completion item's label
     */
    function create(label) {
        return { label: label };
    }
    CompletionItem.create = create;
})(CompletionItem || (CompletionItem = {}));
/**
 * The CompletionList namespace provides functions to deal with
 * completion lists.
 */
var CompletionList;
(function (CompletionList) {
    /**
     * Creates a new completion list.
     *
     * @param items The completion items.
     * @param isIncomplete The list is not complete.
     */
    function create(items, isIncomplete) {
        return { items: items ? items : [], isIncomplete: !!isIncomplete };
    }
    CompletionList.create = create;
})(CompletionList || (CompletionList = {}));
var MarkedString;
(function (MarkedString) {
    /**
     * Creates a marked string from plain text.
     *
     * @param plainText The plain text.
     */
    function fromPlainText(plainText) {
        return plainText.replace(/[\\`*_{}[\]()#+\-.!]/g, '\\$&'); // escape markdown syntax tokens: http://daringfireball.net/projects/markdown/syntax#backslash
    }
    MarkedString.fromPlainText = fromPlainText;
    /**
     * Checks whether the given value conforms to the [MarkedString](#MarkedString) type.
     */
    function is(value) {
        var candidate = value;
        return Is.string(candidate) || (Is.objectLiteral(candidate) && Is.string(candidate.language) && Is.string(candidate.value));
    }
    MarkedString.is = is;
})(MarkedString || (MarkedString = {}));
var Hover;
(function (Hover) {
    /**
     * Checks whether the given value conforms to the [Hover](#Hover) interface.
     */
    function is(value) {
        var candidate = value;
        return !!candidate && Is.objectLiteral(candidate) && (MarkupContent.is(candidate.contents) ||
            MarkedString.is(candidate.contents) ||
            Is.typedArray(candidate.contents, MarkedString.is)) && (value.range === undefined || Range.is(value.range));
    }
    Hover.is = is;
})(Hover || (Hover = {}));
/**
 * The ParameterInformation namespace provides helper functions to work with
 * [ParameterInformation](#ParameterInformation) literals.
 */
var ParameterInformation;
(function (ParameterInformation) {
    /**
     * Creates a new parameter information literal.
     *
     * @param label A label string.
     * @param documentation A doc string.
     */
    function create(label, documentation) {
        return documentation ? { label: label, documentation: documentation } : { label: label };
    }
    ParameterInformation.create = create;
})(ParameterInformation || (ParameterInformation = {}));
/**
 * The SignatureInformation namespace provides helper functions to work with
 * [SignatureInformation](#SignatureInformation) literals.
 */
var SignatureInformation;
(function (SignatureInformation) {
    function create(label, documentation) {
        var parameters = [];
        for (var _i = 2; _i < arguments.length; _i++) {
            parameters[_i - 2] = arguments[_i];
        }
        var result = { label: label };
        if (Is.defined(documentation)) {
            result.documentation = documentation;
        }
        if (Is.defined(parameters)) {
            result.parameters = parameters;
        }
        else {
            result.parameters = [];
        }
        return result;
    }
    SignatureInformation.create = create;
})(SignatureInformation || (SignatureInformation = {}));
/**
 * A document highlight kind.
 */
var DocumentHighlightKind;
(function (DocumentHighlightKind) {
    /**
     * A textual occurrence.
     */
    DocumentHighlightKind.Text = 1;
    /**
     * Read-access of a symbol, like reading a variable.
     */
    DocumentHighlightKind.Read = 2;
    /**
     * Write-access of a symbol, like writing to a variable.
     */
    DocumentHighlightKind.Write = 3;
})(DocumentHighlightKind || (DocumentHighlightKind = {}));
/**
 * DocumentHighlight namespace to provide helper functions to work with
 * [DocumentHighlight](#DocumentHighlight) literals.
 */
var DocumentHighlight;
(function (DocumentHighlight) {
    /**
     * Create a DocumentHighlight object.
     * @param range The range the highlight applies to.
     * @param kind The highlight kind
     */
    function create(range, kind) {
        var result = { range: range };
        if (Is.number(kind)) {
            result.kind = kind;
        }
        return result;
    }
    DocumentHighlight.create = create;
})(DocumentHighlight || (DocumentHighlight = {}));
/**
 * A symbol kind.
 */
var SymbolKind;
(function (SymbolKind) {
    SymbolKind.File = 1;
    SymbolKind.Module = 2;
    SymbolKind.Namespace = 3;
    SymbolKind.Package = 4;
    SymbolKind.Class = 5;
    SymbolKind.Method = 6;
    SymbolKind.Property = 7;
    SymbolKind.Field = 8;
    SymbolKind.Constructor = 9;
    SymbolKind.Enum = 10;
    SymbolKind.Interface = 11;
    SymbolKind.Function = 12;
    SymbolKind.Variable = 13;
    SymbolKind.Constant = 14;
    SymbolKind.String = 15;
    SymbolKind.Number = 16;
    SymbolKind.Boolean = 17;
    SymbolKind.Array = 18;
    SymbolKind.Object = 19;
    SymbolKind.Key = 20;
    SymbolKind.Null = 21;
    SymbolKind.EnumMember = 22;
    SymbolKind.Struct = 23;
    SymbolKind.Event = 24;
    SymbolKind.Operator = 25;
    SymbolKind.TypeParameter = 26;
})(SymbolKind || (SymbolKind = {}));
/**
 * Symbol tags are extra annotations that tweak the rendering of a symbol.
 * @since 3.16
 */
var SymbolTag;
(function (SymbolTag) {
    /**
     * Render a symbol as obsolete, usually using a strike-out.
     */
    SymbolTag.Deprecated = 1;
})(SymbolTag || (SymbolTag = {}));
var SymbolInformation;
(function (SymbolInformation) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the location of the symbol.
     * @param uri The resource of the location of symbol.
     * @param containerName The name of the symbol containing the symbol.
     */
    function create(name, kind, range, uri, containerName) {
        var result = {
            name: name,
            kind: kind,
            location: { uri: uri, range: range }
        };
        if (containerName) {
            result.containerName = containerName;
        }
        return result;
    }
    SymbolInformation.create = create;
})(SymbolInformation || (SymbolInformation = {}));
var WorkspaceSymbol;
(function (WorkspaceSymbol) {
    /**
     * Create a new workspace symbol.
     *
     * @param name The name of the symbol.
     * @param kind The kind of the symbol.
     * @param uri The resource of the location of the symbol.
     * @param range An options range of the location.
     * @returns A WorkspaceSymbol.
     */
    function create(name, kind, uri, range) {
        return range !== undefined
            ? { name: name, kind: kind, location: { uri: uri, range: range } }
            : { name: name, kind: kind, location: { uri: uri } };
    }
    WorkspaceSymbol.create = create;
})(WorkspaceSymbol || (WorkspaceSymbol = {}));
var DocumentSymbol;
(function (DocumentSymbol) {
    /**
     * Creates a new symbol information literal.
     *
     * @param name The name of the symbol.
     * @param detail The detail of the symbol.
     * @param kind The kind of the symbol.
     * @param range The range of the symbol.
     * @param selectionRange The selectionRange of the symbol.
     * @param children Children of the symbol.
     */
    function create(name, detail, kind, range, selectionRange, children) {
        var result = {
            name: name,
            detail: detail,
            kind: kind,
            range: range,
            selectionRange: selectionRange
        };
        if (children !== undefined) {
            result.children = children;
        }
        return result;
    }
    DocumentSymbol.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentSymbol](#DocumentSymbol) interface.
     */
    function is(value) {
        var candidate = value;
        return candidate &&
            Is.string(candidate.name) && Is.number(candidate.kind) &&
            Range.is(candidate.range) && Range.is(candidate.selectionRange) &&
            (candidate.detail === undefined || Is.string(candidate.detail)) &&
            (candidate.deprecated === undefined || Is.boolean(candidate.deprecated)) &&
            (candidate.children === undefined || Array.isArray(candidate.children)) &&
            (candidate.tags === undefined || Array.isArray(candidate.tags));
    }
    DocumentSymbol.is = is;
})(DocumentSymbol || (DocumentSymbol = {}));
/**
 * A set of predefined code action kinds
 */
var CodeActionKind;
(function (CodeActionKind) {
    /**
     * Empty kind.
     */
    CodeActionKind.Empty = '';
    /**
     * Base kind for quickfix actions: 'quickfix'
     */
    CodeActionKind.QuickFix = 'quickfix';
    /**
     * Base kind for refactoring actions: 'refactor'
     */
    CodeActionKind.Refactor = 'refactor';
    /**
     * Base kind for refactoring extraction actions: 'refactor.extract'
     *
     * Example extract actions:
     *
     * - Extract method
     * - Extract function
     * - Extract variable
     * - Extract interface from class
     * - ...
     */
    CodeActionKind.RefactorExtract = 'refactor.extract';
    /**
     * Base kind for refactoring inline actions: 'refactor.inline'
     *
     * Example inline actions:
     *
     * - Inline function
     * - Inline variable
     * - Inline constant
     * - ...
     */
    CodeActionKind.RefactorInline = 'refactor.inline';
    /**
     * Base kind for refactoring rewrite actions: 'refactor.rewrite'
     *
     * Example rewrite actions:
     *
     * - Convert JavaScript function to class
     * - Add or remove parameter
     * - Encapsulate field
     * - Make method static
     * - Move method to base class
     * - ...
     */
    CodeActionKind.RefactorRewrite = 'refactor.rewrite';
    /**
     * Base kind for source actions: `source`
     *
     * Source code actions apply to the entire file.
     */
    CodeActionKind.Source = 'source';
    /**
     * Base kind for an organize imports source action: `source.organizeImports`
     */
    CodeActionKind.SourceOrganizeImports = 'source.organizeImports';
    /**
     * Base kind for auto-fix source actions: `source.fixAll`.
     *
     * Fix all actions automatically fix errors that have a clear fix that do not require user input.
     * They should not suppress errors or perform unsafe fixes such as generating new types or classes.
     *
     * @since 3.15.0
     */
    CodeActionKind.SourceFixAll = 'source.fixAll';
})(CodeActionKind || (CodeActionKind = {}));
/**
 * The reason why code actions were requested.
 *
 * @since 3.17.0
 */
var CodeActionTriggerKind;
(function (CodeActionTriggerKind) {
    /**
     * Code actions were explicitly requested by the user or by an extension.
     */
    CodeActionTriggerKind.Invoked = 1;
    /**
     * Code actions were requested automatically.
     *
     * This typically happens when current selection in a file changes, but can
     * also be triggered when file content changes.
     */
    CodeActionTriggerKind.Automatic = 2;
})(CodeActionTriggerKind || (CodeActionTriggerKind = {}));
/**
 * The CodeActionContext namespace provides helper functions to work with
 * [CodeActionContext](#CodeActionContext) literals.
 */
var CodeActionContext;
(function (CodeActionContext) {
    /**
     * Creates a new CodeActionContext literal.
     */
    function create(diagnostics, only, triggerKind) {
        var result = { diagnostics: diagnostics };
        if (only !== undefined && only !== null) {
            result.only = only;
        }
        if (triggerKind !== undefined && triggerKind !== null) {
            result.triggerKind = triggerKind;
        }
        return result;
    }
    CodeActionContext.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeActionContext](#CodeActionContext) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.typedArray(candidate.diagnostics, Diagnostic.is)
            && (candidate.only === undefined || Is.typedArray(candidate.only, Is.string))
            && (candidate.triggerKind === undefined || candidate.triggerKind === CodeActionTriggerKind.Invoked || candidate.triggerKind === CodeActionTriggerKind.Automatic);
    }
    CodeActionContext.is = is;
})(CodeActionContext || (CodeActionContext = {}));
var CodeAction;
(function (CodeAction) {
    function create(title, kindOrCommandOrEdit, kind) {
        var result = { title: title };
        var checkKind = true;
        if (typeof kindOrCommandOrEdit === 'string') {
            checkKind = false;
            result.kind = kindOrCommandOrEdit;
        }
        else if (Command.is(kindOrCommandOrEdit)) {
            result.command = kindOrCommandOrEdit;
        }
        else {
            result.edit = kindOrCommandOrEdit;
        }
        if (checkKind && kind !== undefined) {
            result.kind = kind;
        }
        return result;
    }
    CodeAction.create = create;
    function is(value) {
        var candidate = value;
        return candidate && Is.string(candidate.title) &&
            (candidate.diagnostics === undefined || Is.typedArray(candidate.diagnostics, Diagnostic.is)) &&
            (candidate.kind === undefined || Is.string(candidate.kind)) &&
            (candidate.edit !== undefined || candidate.command !== undefined) &&
            (candidate.command === undefined || Command.is(candidate.command)) &&
            (candidate.isPreferred === undefined || Is.boolean(candidate.isPreferred)) &&
            (candidate.edit === undefined || WorkspaceEdit.is(candidate.edit));
    }
    CodeAction.is = is;
})(CodeAction || (CodeAction = {}));
/**
 * The CodeLens namespace provides helper functions to work with
 * [CodeLens](#CodeLens) literals.
 */
var CodeLens;
(function (CodeLens) {
    /**
     * Creates a new CodeLens literal.
     */
    function create(range, data) {
        var result = { range: range };
        if (Is.defined(data)) {
            result.data = data;
        }
        return result;
    }
    CodeLens.create = create;
    /**
     * Checks whether the given literal conforms to the [CodeLens](#CodeLens) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.command) || Command.is(candidate.command));
    }
    CodeLens.is = is;
})(CodeLens || (CodeLens = {}));
/**
 * The FormattingOptions namespace provides helper functions to work with
 * [FormattingOptions](#FormattingOptions) literals.
 */
var FormattingOptions;
(function (FormattingOptions) {
    /**
     * Creates a new FormattingOptions literal.
     */
    function create(tabSize, insertSpaces) {
        return { tabSize: tabSize, insertSpaces: insertSpaces };
    }
    FormattingOptions.create = create;
    /**
     * Checks whether the given literal conforms to the [FormattingOptions](#FormattingOptions) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.uinteger(candidate.tabSize) && Is.boolean(candidate.insertSpaces);
    }
    FormattingOptions.is = is;
})(FormattingOptions || (FormattingOptions = {}));
/**
 * The DocumentLink namespace provides helper functions to work with
 * [DocumentLink](#DocumentLink) literals.
 */
var DocumentLink;
(function (DocumentLink) {
    /**
     * Creates a new DocumentLink literal.
     */
    function create(range, target, data) {
        return { range: range, target: target, data: data };
    }
    DocumentLink.create = create;
    /**
     * Checks whether the given literal conforms to the [DocumentLink](#DocumentLink) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(candidate.range) && (Is.undefined(candidate.target) || Is.string(candidate.target));
    }
    DocumentLink.is = is;
})(DocumentLink || (DocumentLink = {}));
/**
 * The SelectionRange namespace provides helper function to work with
 * SelectionRange literals.
 */
var SelectionRange;
(function (SelectionRange) {
    /**
     * Creates a new SelectionRange
     * @param range the range.
     * @param parent an optional parent.
     */
    function create(range, parent) {
        return { range: range, parent: parent };
    }
    SelectionRange.create = create;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Range.is(candidate.range) && (candidate.parent === undefined || SelectionRange.is(candidate.parent));
    }
    SelectionRange.is = is;
})(SelectionRange || (SelectionRange = {}));
/**
 * A set of predefined token types. This set is not fixed
 * an clients can specify additional token types via the
 * corresponding client capabilities.
 *
 * @since 3.16.0
 */
var SemanticTokenTypes;
(function (SemanticTokenTypes) {
    SemanticTokenTypes["namespace"] = "namespace";
    /**
     * Represents a generic type. Acts as a fallback for types which can't be mapped to
     * a specific type like class or enum.
     */
    SemanticTokenTypes["type"] = "type";
    SemanticTokenTypes["class"] = "class";
    SemanticTokenTypes["enum"] = "enum";
    SemanticTokenTypes["interface"] = "interface";
    SemanticTokenTypes["struct"] = "struct";
    SemanticTokenTypes["typeParameter"] = "typeParameter";
    SemanticTokenTypes["parameter"] = "parameter";
    SemanticTokenTypes["variable"] = "variable";
    SemanticTokenTypes["property"] = "property";
    SemanticTokenTypes["enumMember"] = "enumMember";
    SemanticTokenTypes["event"] = "event";
    SemanticTokenTypes["function"] = "function";
    SemanticTokenTypes["method"] = "method";
    SemanticTokenTypes["macro"] = "macro";
    SemanticTokenTypes["keyword"] = "keyword";
    SemanticTokenTypes["modifier"] = "modifier";
    SemanticTokenTypes["comment"] = "comment";
    SemanticTokenTypes["string"] = "string";
    SemanticTokenTypes["number"] = "number";
    SemanticTokenTypes["regexp"] = "regexp";
    SemanticTokenTypes["operator"] = "operator";
    /**
     * @since 3.17.0
     */
    SemanticTokenTypes["decorator"] = "decorator";
})(SemanticTokenTypes || (SemanticTokenTypes = {}));
/**
 * A set of predefined token modifiers. This set is not fixed
 * an clients can specify additional token types via the
 * corresponding client capabilities.
 *
 * @since 3.16.0
 */
var SemanticTokenModifiers;
(function (SemanticTokenModifiers) {
    SemanticTokenModifiers["declaration"] = "declaration";
    SemanticTokenModifiers["definition"] = "definition";
    SemanticTokenModifiers["readonly"] = "readonly";
    SemanticTokenModifiers["static"] = "static";
    SemanticTokenModifiers["deprecated"] = "deprecated";
    SemanticTokenModifiers["abstract"] = "abstract";
    SemanticTokenModifiers["async"] = "async";
    SemanticTokenModifiers["modification"] = "modification";
    SemanticTokenModifiers["documentation"] = "documentation";
    SemanticTokenModifiers["defaultLibrary"] = "defaultLibrary";
})(SemanticTokenModifiers || (SemanticTokenModifiers = {}));
/**
 * @since 3.16.0
 */
var SemanticTokens;
(function (SemanticTokens) {
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && (candidate.resultId === undefined || typeof candidate.resultId === 'string') &&
            Array.isArray(candidate.data) && (candidate.data.length === 0 || typeof candidate.data[0] === 'number');
    }
    SemanticTokens.is = is;
})(SemanticTokens || (SemanticTokens = {}));
/**
 * The InlineValueText namespace provides functions to deal with InlineValueTexts.
 *
 * @since 3.17.0
 */
var InlineValueText;
(function (InlineValueText) {
    /**
     * Creates a new InlineValueText literal.
     */
    function create(range, text) {
        return { range: range, text: text };
    }
    InlineValueText.create = create;
    function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range) && Is.string(candidate.text);
    }
    InlineValueText.is = is;
})(InlineValueText || (InlineValueText = {}));
/**
 * The InlineValueVariableLookup namespace provides functions to deal with InlineValueVariableLookups.
 *
 * @since 3.17.0
 */
var InlineValueVariableLookup;
(function (InlineValueVariableLookup) {
    /**
     * Creates a new InlineValueText literal.
     */
    function create(range, variableName, caseSensitiveLookup) {
        return { range: range, variableName: variableName, caseSensitiveLookup: caseSensitiveLookup };
    }
    InlineValueVariableLookup.create = create;
    function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range) && Is.boolean(candidate.caseSensitiveLookup)
            && (Is.string(candidate.variableName) || candidate.variableName === undefined);
    }
    InlineValueVariableLookup.is = is;
})(InlineValueVariableLookup || (InlineValueVariableLookup = {}));
/**
 * The InlineValueEvaluatableExpression namespace provides functions to deal with InlineValueEvaluatableExpression.
 *
 * @since 3.17.0
 */
var InlineValueEvaluatableExpression;
(function (InlineValueEvaluatableExpression) {
    /**
     * Creates a new InlineValueEvaluatableExpression literal.
     */
    function create(range, expression) {
        return { range: range, expression: expression };
    }
    InlineValueEvaluatableExpression.create = create;
    function is(value) {
        var candidate = value;
        return candidate !== undefined && candidate !== null && Range.is(candidate.range)
            && (Is.string(candidate.expression) || candidate.expression === undefined);
    }
    InlineValueEvaluatableExpression.is = is;
})(InlineValueEvaluatableExpression || (InlineValueEvaluatableExpression = {}));
/**
 * The InlineValueContext namespace provides helper functions to work with
 * [InlineValueContext](#InlineValueContext) literals.
 *
 * @since 3.17.0
 */
var InlineValueContext;
(function (InlineValueContext) {
    /**
     * Creates a new InlineValueContext literal.
     */
    function create(frameId, stoppedLocation) {
        return { frameId: frameId, stoppedLocation: stoppedLocation };
    }
    InlineValueContext.create = create;
    /**
     * Checks whether the given literal conforms to the [InlineValueContext](#InlineValueContext) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Range.is(value.stoppedLocation);
    }
    InlineValueContext.is = is;
})(InlineValueContext || (InlineValueContext = {}));
/**
 * Inlay hint kinds.
 *
 * @since 3.17.0
 */
var InlayHintKind;
(function (InlayHintKind) {
    /**
     * An inlay hint that for a type annotation.
     */
    InlayHintKind.Type = 1;
    /**
     * An inlay hint that is for a parameter.
     */
    InlayHintKind.Parameter = 2;
    function is(value) {
        return value === 1 || value === 2;
    }
    InlayHintKind.is = is;
})(InlayHintKind || (InlayHintKind = {}));
var InlayHintLabelPart;
(function (InlayHintLabelPart) {
    function create(value) {
        return { value: value };
    }
    InlayHintLabelPart.create = create;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate)
            && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip))
            && (candidate.location === undefined || Location.is(candidate.location))
            && (candidate.command === undefined || Command.is(candidate.command));
    }
    InlayHintLabelPart.is = is;
})(InlayHintLabelPart || (InlayHintLabelPart = {}));
var InlayHint;
(function (InlayHint) {
    function create(position, label, kind) {
        var result = { position: position, label: label };
        if (kind !== undefined) {
            result.kind = kind;
        }
        return result;
    }
    InlayHint.create = create;
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && Position.is(candidate.position)
            && (Is.string(candidate.label) || Is.typedArray(candidate.label, InlayHintLabelPart.is))
            && (candidate.kind === undefined || InlayHintKind.is(candidate.kind))
            && (candidate.textEdits === undefined) || Is.typedArray(candidate.textEdits, TextEdit.is)
            && (candidate.tooltip === undefined || Is.string(candidate.tooltip) || MarkupContent.is(candidate.tooltip))
            && (candidate.paddingLeft === undefined || Is.boolean(candidate.paddingLeft))
            && (candidate.paddingRight === undefined || Is.boolean(candidate.paddingRight));
    }
    InlayHint.is = is;
})(InlayHint || (InlayHint = {}));
var WorkspaceFolder;
(function (WorkspaceFolder) {
    function is(value) {
        var candidate = value;
        return Is.objectLiteral(candidate) && URI.is(candidate.uri) && Is.string(candidate.name);
    }
    WorkspaceFolder.is = is;
})(WorkspaceFolder || (WorkspaceFolder = {}));
var EOL = ['\n', '\r\n', '\r'];
/**
 * @deprecated Use the text document from the new vscode-languageserver-textdocument package.
 */
var TextDocument;
(function (TextDocument) {
    /**
     * Creates a new ITextDocument literal from the given uri and content.
     * @param uri The document's uri.
     * @param languageId The document's language Id.
     * @param version The document's version.
     * @param content The document's content.
     */
    function create(uri, languageId, version, content) {
        return new FullTextDocument(uri, languageId, version, content);
    }
    TextDocument.create = create;
    /**
     * Checks whether the given literal conforms to the [ITextDocument](#ITextDocument) interface.
     */
    function is(value) {
        var candidate = value;
        return Is.defined(candidate) && Is.string(candidate.uri) && (Is.undefined(candidate.languageId) || Is.string(candidate.languageId)) && Is.uinteger(candidate.lineCount)
            && Is.func(candidate.getText) && Is.func(candidate.positionAt) && Is.func(candidate.offsetAt) ? true : false;
    }
    TextDocument.is = is;
    function applyEdits(document, edits) {
        var text = document.getText();
        var sortedEdits = mergeSort(edits, function (a, b) {
            var diff = a.range.start.line - b.range.start.line;
            if (diff === 0) {
                return a.range.start.character - b.range.start.character;
            }
            return diff;
        });
        var lastModifiedOffset = text.length;
        for (var i = sortedEdits.length - 1; i >= 0; i--) {
            var e = sortedEdits[i];
            var startOffset = document.offsetAt(e.range.start);
            var endOffset = document.offsetAt(e.range.end);
            if (endOffset <= lastModifiedOffset) {
                text = text.substring(0, startOffset) + e.newText + text.substring(endOffset, text.length);
            }
            else {
                throw new Error('Overlapping edit');
            }
            lastModifiedOffset = startOffset;
        }
        return text;
    }
    TextDocument.applyEdits = applyEdits;
    function mergeSort(data, compare) {
        if (data.length <= 1) {
            // sorted
            return data;
        }
        var p = (data.length / 2) | 0;
        var left = data.slice(0, p);
        var right = data.slice(p);
        mergeSort(left, compare);
        mergeSort(right, compare);
        var leftIdx = 0;
        var rightIdx = 0;
        var i = 0;
        while (leftIdx < left.length && rightIdx < right.length) {
            var ret = compare(left[leftIdx], right[rightIdx]);
            if (ret <= 0) {
                // smaller_equal -> take left to preserve order
                data[i++] = left[leftIdx++];
            }
            else {
                // greater -> take right
                data[i++] = right[rightIdx++];
            }
        }
        while (leftIdx < left.length) {
            data[i++] = left[leftIdx++];
        }
        while (rightIdx < right.length) {
            data[i++] = right[rightIdx++];
        }
        return data;
    }
})(TextDocument || (TextDocument = {}));
/**
 * @deprecated Use the text document from the new vscode-languageserver-textdocument package.
 */
var FullTextDocument = /** @class */ (function () {
    function FullTextDocument(uri, languageId, version, content) {
        this._uri = uri;
        this._languageId = languageId;
        this._version = version;
        this._content = content;
        this._lineOffsets = undefined;
    }
    Object.defineProperty(FullTextDocument.prototype, "uri", {
        get: function () {
            return this._uri;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "languageId", {
        get: function () {
            return this._languageId;
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(FullTextDocument.prototype, "version", {
        get: function () {
            return this._version;
        },
        enumerable: false,
        configurable: true
    });
    FullTextDocument.prototype.getText = function (range) {
        if (range) {
            var start = this.offsetAt(range.start);
            var end = this.offsetAt(range.end);
            return this._content.substring(start, end);
        }
        return this._content;
    };
    FullTextDocument.prototype.update = function (event, version) {
        this._content = event.text;
        this._version = version;
        this._lineOffsets = undefined;
    };
    FullTextDocument.prototype.getLineOffsets = function () {
        if (this._lineOffsets === undefined) {
            var lineOffsets = [];
            var text = this._content;
            var isLineStart = true;
            for (var i = 0; i < text.length; i++) {
                if (isLineStart) {
                    lineOffsets.push(i);
                    isLineStart = false;
                }
                var ch = text.charAt(i);
                isLineStart = (ch === '\r' || ch === '\n');
                if (ch === '\r' && i + 1 < text.length && text.charAt(i + 1) === '\n') {
                    i++;
                }
            }
            if (isLineStart && text.length > 0) {
                lineOffsets.push(text.length);
            }
            this._lineOffsets = lineOffsets;
        }
        return this._lineOffsets;
    };
    FullTextDocument.prototype.positionAt = function (offset) {
        offset = Math.max(Math.min(offset, this._content.length), 0);
        var lineOffsets = this.getLineOffsets();
        var low = 0, high = lineOffsets.length;
        if (high === 0) {
            return Position.create(0, offset);
        }
        while (low < high) {
            var mid = Math.floor((low + high) / 2);
            if (lineOffsets[mid] > offset) {
                high = mid;
            }
            else {
                low = mid + 1;
            }
        }
        // low is the least x for which the line offset is larger than the current offset
        // or array.length if no line offset is larger than the current offset
        var line = low - 1;
        return Position.create(line, offset - lineOffsets[line]);
    };
    FullTextDocument.prototype.offsetAt = function (position) {
        var lineOffsets = this.getLineOffsets();
        if (position.line >= lineOffsets.length) {
            return this._content.length;
        }
        else if (position.line < 0) {
            return 0;
        }
        var lineOffset = lineOffsets[position.line];
        var nextLineOffset = (position.line + 1 < lineOffsets.length) ? lineOffsets[position.line + 1] : this._content.length;
        return Math.max(Math.min(lineOffset + position.character, nextLineOffset), lineOffset);
    };
    Object.defineProperty(FullTextDocument.prototype, "lineCount", {
        get: function () {
            return this.getLineOffsets().length;
        },
        enumerable: false,
        configurable: true
    });
    return FullTextDocument;
}());
var Is;
(function (Is) {
    var toString = Object.prototype.toString;
    function defined(value) {
        return typeof value !== 'undefined';
    }
    Is.defined = defined;
    function undefined(value) {
        return typeof value === 'undefined';
    }
    Is.undefined = undefined;
    function boolean(value) {
        return value === true || value === false;
    }
    Is.boolean = boolean;
    function string(value) {
        return toString.call(value) === '[object String]';
    }
    Is.string = string;
    function number(value) {
        return toString.call(value) === '[object Number]';
    }
    Is.number = number;
    function numberRange(value, min, max) {
        return toString.call(value) === '[object Number]' && min <= value && value <= max;
    }
    Is.numberRange = numberRange;
    function integer(value) {
        return toString.call(value) === '[object Number]' && -2147483648 <= value && value <= 2147483647;
    }
    Is.integer = integer;
    function uinteger(value) {
        return toString.call(value) === '[object Number]' && 0 <= value && value <= 2147483647;
    }
    Is.uinteger = uinteger;
    function func(value) {
        return toString.call(value) === '[object Function]';
    }
    Is.func = func;
    function objectLiteral(value) {
        // Strictly speaking class instances pass this check as well. Since the LSP
        // doesn't use classes we ignore this for now. If we do we need to add something
        // like this: `Object.getPrototypeOf(Object.getPrototypeOf(x)) === null`
        return value !== null && typeof value === 'object';
    }
    Is.objectLiteral = objectLiteral;
    function typedArray(value, check) {
        return Array.isArray(value) && value.every(check);
    }
    Is.typedArray = typedArray;
})(Is || (Is = {}));


/***/ }),
/* 79 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProtocolNotificationType = exports.ProtocolNotificationType0 = exports.ProtocolRequestType = exports.ProtocolRequestType0 = exports.RegistrationType = void 0;
const vscode_jsonrpc_1 = __webpack_require__(58);
class RegistrationType {
    constructor(method) {
        this.method = method;
    }
}
exports.RegistrationType = RegistrationType;
class ProtocolRequestType0 extends vscode_jsonrpc_1.RequestType0 {
    constructor(method) {
        super(method);
    }
}
exports.ProtocolRequestType0 = ProtocolRequestType0;
class ProtocolRequestType extends vscode_jsonrpc_1.RequestType {
    constructor(method) {
        super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
}
exports.ProtocolRequestType = ProtocolRequestType;
class ProtocolNotificationType0 extends vscode_jsonrpc_1.NotificationType0 {
    constructor(method) {
        super(method);
    }
}
exports.ProtocolNotificationType0 = ProtocolNotificationType0;
class ProtocolNotificationType extends vscode_jsonrpc_1.NotificationType {
    constructor(method) {
        super(method, vscode_jsonrpc_1.ParameterStructures.byName);
    }
}
exports.ProtocolNotificationType = ProtocolNotificationType;
//# sourceMappingURL=messages.js.map

/***/ }),
/* 80 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkspaceSymbolRequest = exports.CodeActionResolveRequest = exports.CodeActionRequest = exports.DocumentSymbolRequest = exports.DocumentHighlightRequest = exports.ReferencesRequest = exports.DefinitionRequest = exports.SignatureHelpRequest = exports.SignatureHelpTriggerKind = exports.HoverRequest = exports.CompletionResolveRequest = exports.CompletionRequest = exports.CompletionTriggerKind = exports.PublishDiagnosticsNotification = exports.WatchKind = exports.RelativePattern = exports.FileChangeType = exports.DidChangeWatchedFilesNotification = exports.WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentNotification = exports.TextDocumentSaveReason = exports.DidSaveTextDocumentNotification = exports.DidCloseTextDocumentNotification = exports.DidChangeTextDocumentNotification = exports.TextDocumentContentChangeEvent = exports.DidOpenTextDocumentNotification = exports.TextDocumentSyncKind = exports.TelemetryEventNotification = exports.LogMessageNotification = exports.ShowMessageRequest = exports.ShowMessageNotification = exports.MessageType = exports.DidChangeConfigurationNotification = exports.ExitNotification = exports.ShutdownRequest = exports.InitializedNotification = exports.InitializeErrorCodes = exports.InitializeRequest = exports.WorkDoneProgressOptions = exports.TextDocumentRegistrationOptions = exports.StaticRegistrationOptions = exports.PositionEncodingKind = exports.FailureHandlingKind = exports.ResourceOperationKind = exports.UnregistrationRequest = exports.RegistrationRequest = exports.DocumentSelector = exports.NotebookCellTextDocumentFilter = exports.NotebookDocumentFilter = exports.TextDocumentFilter = void 0;
exports.TypeHierarchySubtypesRequest = exports.TypeHierarchyPrepareRequest = exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.WillRenameFilesRequest = exports.DidRenameFilesNotification = exports.WillCreateFilesRequest = exports.DidCreateFilesNotification = exports.FileOperationPatternKind = exports.LinkedEditingRangeRequest = exports.ShowDocumentRequest = exports.SemanticTokensRegistrationType = exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.TokenFormat = exports.CallHierarchyPrepareRequest = exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = exports.SelectionRangeRequest = exports.DeclarationRequest = exports.FoldingRangeRequest = exports.ColorPresentationRequest = exports.DocumentColorRequest = exports.ConfigurationRequest = exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = exports.TypeDefinitionRequest = exports.ImplementationRequest = exports.ApplyWorkspaceEditRequest = exports.ExecuteCommandRequest = exports.PrepareRenameRequest = exports.RenameRequest = exports.PrepareSupportDefaultBehavior = exports.DocumentOnTypeFormattingRequest = exports.DocumentRangeFormattingRequest = exports.DocumentFormattingRequest = exports.DocumentLinkResolveRequest = exports.DocumentLinkRequest = exports.CodeLensRefreshRequest = exports.CodeLensResolveRequest = exports.CodeLensRequest = exports.WorkspaceSymbolResolveRequest = void 0;
exports.DidCloseNotebookDocumentNotification = exports.DidSaveNotebookDocumentNotification = exports.DidChangeNotebookDocumentNotification = exports.NotebookCellArrayChange = exports.DidOpenNotebookDocumentNotification = exports.NotebookDocumentSyncRegistrationType = exports.NotebookDocument = exports.NotebookCell = exports.ExecutionSummary = exports.NotebookCellKind = exports.DiagnosticRefreshRequest = exports.WorkspaceDiagnosticRequest = exports.DocumentDiagnosticRequest = exports.DocumentDiagnosticReportKind = exports.DiagnosticServerCancellationData = exports.InlayHintRefreshRequest = exports.InlayHintResolveRequest = exports.InlayHintRequest = exports.InlineValueRefreshRequest = exports.InlineValueRequest = exports.TypeHierarchySupertypesRequest = void 0;
const messages_1 = __webpack_require__(79);
const vscode_languageserver_types_1 = __webpack_require__(78);
const Is = __webpack_require__(81);
const protocol_implementation_1 = __webpack_require__(82);
Object.defineProperty(exports, "ImplementationRequest", ({ enumerable: true, get: function () { return protocol_implementation_1.ImplementationRequest; } }));
const protocol_typeDefinition_1 = __webpack_require__(83);
Object.defineProperty(exports, "TypeDefinitionRequest", ({ enumerable: true, get: function () { return protocol_typeDefinition_1.TypeDefinitionRequest; } }));
const protocol_workspaceFolder_1 = __webpack_require__(84);
Object.defineProperty(exports, "WorkspaceFoldersRequest", ({ enumerable: true, get: function () { return protocol_workspaceFolder_1.WorkspaceFoldersRequest; } }));
Object.defineProperty(exports, "DidChangeWorkspaceFoldersNotification", ({ enumerable: true, get: function () { return protocol_workspaceFolder_1.DidChangeWorkspaceFoldersNotification; } }));
const protocol_configuration_1 = __webpack_require__(85);
Object.defineProperty(exports, "ConfigurationRequest", ({ enumerable: true, get: function () { return protocol_configuration_1.ConfigurationRequest; } }));
const protocol_colorProvider_1 = __webpack_require__(86);
Object.defineProperty(exports, "DocumentColorRequest", ({ enumerable: true, get: function () { return protocol_colorProvider_1.DocumentColorRequest; } }));
Object.defineProperty(exports, "ColorPresentationRequest", ({ enumerable: true, get: function () { return protocol_colorProvider_1.ColorPresentationRequest; } }));
const protocol_foldingRange_1 = __webpack_require__(87);
Object.defineProperty(exports, "FoldingRangeRequest", ({ enumerable: true, get: function () { return protocol_foldingRange_1.FoldingRangeRequest; } }));
const protocol_declaration_1 = __webpack_require__(88);
Object.defineProperty(exports, "DeclarationRequest", ({ enumerable: true, get: function () { return protocol_declaration_1.DeclarationRequest; } }));
const protocol_selectionRange_1 = __webpack_require__(89);
Object.defineProperty(exports, "SelectionRangeRequest", ({ enumerable: true, get: function () { return protocol_selectionRange_1.SelectionRangeRequest; } }));
const protocol_progress_1 = __webpack_require__(90);
Object.defineProperty(exports, "WorkDoneProgress", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgress; } }));
Object.defineProperty(exports, "WorkDoneProgressCreateRequest", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgressCreateRequest; } }));
Object.defineProperty(exports, "WorkDoneProgressCancelNotification", ({ enumerable: true, get: function () { return protocol_progress_1.WorkDoneProgressCancelNotification; } }));
const protocol_callHierarchy_1 = __webpack_require__(91);
Object.defineProperty(exports, "CallHierarchyIncomingCallsRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyIncomingCallsRequest; } }));
Object.defineProperty(exports, "CallHierarchyOutgoingCallsRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyOutgoingCallsRequest; } }));
Object.defineProperty(exports, "CallHierarchyPrepareRequest", ({ enumerable: true, get: function () { return protocol_callHierarchy_1.CallHierarchyPrepareRequest; } }));
const protocol_semanticTokens_1 = __webpack_require__(92);
Object.defineProperty(exports, "TokenFormat", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.TokenFormat; } }));
Object.defineProperty(exports, "SemanticTokensRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRequest; } }));
Object.defineProperty(exports, "SemanticTokensDeltaRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensDeltaRequest; } }));
Object.defineProperty(exports, "SemanticTokensRangeRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRangeRequest; } }));
Object.defineProperty(exports, "SemanticTokensRefreshRequest", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRefreshRequest; } }));
Object.defineProperty(exports, "SemanticTokensRegistrationType", ({ enumerable: true, get: function () { return protocol_semanticTokens_1.SemanticTokensRegistrationType; } }));
const protocol_showDocument_1 = __webpack_require__(93);
Object.defineProperty(exports, "ShowDocumentRequest", ({ enumerable: true, get: function () { return protocol_showDocument_1.ShowDocumentRequest; } }));
const protocol_linkedEditingRange_1 = __webpack_require__(94);
Object.defineProperty(exports, "LinkedEditingRangeRequest", ({ enumerable: true, get: function () { return protocol_linkedEditingRange_1.LinkedEditingRangeRequest; } }));
const protocol_fileOperations_1 = __webpack_require__(95);
Object.defineProperty(exports, "FileOperationPatternKind", ({ enumerable: true, get: function () { return protocol_fileOperations_1.FileOperationPatternKind; } }));
Object.defineProperty(exports, "DidCreateFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidCreateFilesNotification; } }));
Object.defineProperty(exports, "WillCreateFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillCreateFilesRequest; } }));
Object.defineProperty(exports, "DidRenameFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidRenameFilesNotification; } }));
Object.defineProperty(exports, "WillRenameFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillRenameFilesRequest; } }));
Object.defineProperty(exports, "DidDeleteFilesNotification", ({ enumerable: true, get: function () { return protocol_fileOperations_1.DidDeleteFilesNotification; } }));
Object.defineProperty(exports, "WillDeleteFilesRequest", ({ enumerable: true, get: function () { return protocol_fileOperations_1.WillDeleteFilesRequest; } }));
const protocol_moniker_1 = __webpack_require__(96);
Object.defineProperty(exports, "UniquenessLevel", ({ enumerable: true, get: function () { return protocol_moniker_1.UniquenessLevel; } }));
Object.defineProperty(exports, "MonikerKind", ({ enumerable: true, get: function () { return protocol_moniker_1.MonikerKind; } }));
Object.defineProperty(exports, "MonikerRequest", ({ enumerable: true, get: function () { return protocol_moniker_1.MonikerRequest; } }));
const protocol_typeHierarchy_1 = __webpack_require__(97);
Object.defineProperty(exports, "TypeHierarchyPrepareRequest", ({ enumerable: true, get: function () { return protocol_typeHierarchy_1.TypeHierarchyPrepareRequest; } }));
Object.defineProperty(exports, "TypeHierarchySubtypesRequest", ({ enumerable: true, get: function () { return protocol_typeHierarchy_1.TypeHierarchySubtypesRequest; } }));
Object.defineProperty(exports, "TypeHierarchySupertypesRequest", ({ enumerable: true, get: function () { return protocol_typeHierarchy_1.TypeHierarchySupertypesRequest; } }));
const protocol_inlineValue_1 = __webpack_require__(98);
Object.defineProperty(exports, "InlineValueRequest", ({ enumerable: true, get: function () { return protocol_inlineValue_1.InlineValueRequest; } }));
Object.defineProperty(exports, "InlineValueRefreshRequest", ({ enumerable: true, get: function () { return protocol_inlineValue_1.InlineValueRefreshRequest; } }));
const protocol_inlayHint_1 = __webpack_require__(99);
Object.defineProperty(exports, "InlayHintRequest", ({ enumerable: true, get: function () { return protocol_inlayHint_1.InlayHintRequest; } }));
Object.defineProperty(exports, "InlayHintResolveRequest", ({ enumerable: true, get: function () { return protocol_inlayHint_1.InlayHintResolveRequest; } }));
Object.defineProperty(exports, "InlayHintRefreshRequest", ({ enumerable: true, get: function () { return protocol_inlayHint_1.InlayHintRefreshRequest; } }));
const protocol_diagnostic_1 = __webpack_require__(100);
Object.defineProperty(exports, "DiagnosticServerCancellationData", ({ enumerable: true, get: function () { return protocol_diagnostic_1.DiagnosticServerCancellationData; } }));
Object.defineProperty(exports, "DocumentDiagnosticReportKind", ({ enumerable: true, get: function () { return protocol_diagnostic_1.DocumentDiagnosticReportKind; } }));
Object.defineProperty(exports, "DocumentDiagnosticRequest", ({ enumerable: true, get: function () { return protocol_diagnostic_1.DocumentDiagnosticRequest; } }));
Object.defineProperty(exports, "WorkspaceDiagnosticRequest", ({ enumerable: true, get: function () { return protocol_diagnostic_1.WorkspaceDiagnosticRequest; } }));
Object.defineProperty(exports, "DiagnosticRefreshRequest", ({ enumerable: true, get: function () { return protocol_diagnostic_1.DiagnosticRefreshRequest; } }));
const protocol_notebook_1 = __webpack_require__(101);
Object.defineProperty(exports, "NotebookCellKind", ({ enumerable: true, get: function () { return protocol_notebook_1.NotebookCellKind; } }));
Object.defineProperty(exports, "ExecutionSummary", ({ enumerable: true, get: function () { return protocol_notebook_1.ExecutionSummary; } }));
Object.defineProperty(exports, "NotebookCell", ({ enumerable: true, get: function () { return protocol_notebook_1.NotebookCell; } }));
Object.defineProperty(exports, "NotebookDocument", ({ enumerable: true, get: function () { return protocol_notebook_1.NotebookDocument; } }));
Object.defineProperty(exports, "NotebookDocumentSyncRegistrationType", ({ enumerable: true, get: function () { return protocol_notebook_1.NotebookDocumentSyncRegistrationType; } }));
Object.defineProperty(exports, "DidOpenNotebookDocumentNotification", ({ enumerable: true, get: function () { return protocol_notebook_1.DidOpenNotebookDocumentNotification; } }));
Object.defineProperty(exports, "NotebookCellArrayChange", ({ enumerable: true, get: function () { return protocol_notebook_1.NotebookCellArrayChange; } }));
Object.defineProperty(exports, "DidChangeNotebookDocumentNotification", ({ enumerable: true, get: function () { return protocol_notebook_1.DidChangeNotebookDocumentNotification; } }));
Object.defineProperty(exports, "DidSaveNotebookDocumentNotification", ({ enumerable: true, get: function () { return protocol_notebook_1.DidSaveNotebookDocumentNotification; } }));
Object.defineProperty(exports, "DidCloseNotebookDocumentNotification", ({ enumerable: true, get: function () { return protocol_notebook_1.DidCloseNotebookDocumentNotification; } }));
// @ts-ignore: to avoid inlining LocationLink as dynamic import
let __noDynamicImport;
/**
 * The TextDocumentFilter namespace provides helper functions to work with
 * [TextDocumentFilter](#TextDocumentFilter) literals.
 *
 * @since 3.17.0
 */
var TextDocumentFilter;
(function (TextDocumentFilter) {
    function is(value) {
        const candidate = value;
        return Is.string(candidate.language) || Is.string(candidate.scheme) || Is.string(candidate.pattern);
    }
    TextDocumentFilter.is = is;
})(TextDocumentFilter = exports.TextDocumentFilter || (exports.TextDocumentFilter = {}));
/**
 * The NotebookDocumentFilter namespace provides helper functions to work with
 * [NotebookDocumentFilter](#NotebookDocumentFilter) literals.
 *
 * @since 3.17.0
 */
var NotebookDocumentFilter;
(function (NotebookDocumentFilter) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && (Is.string(candidate.notebookType) || Is.string(candidate.scheme) || Is.string(candidate.pattern));
    }
    NotebookDocumentFilter.is = is;
})(NotebookDocumentFilter = exports.NotebookDocumentFilter || (exports.NotebookDocumentFilter = {}));
/**
 * The NotebookCellTextDocumentFilter namespace provides helper functions to work with
 * [NotebookCellTextDocumentFilter](#NotebookCellTextDocumentFilter) literals.
 *
 * @since 3.17.0
 */
var NotebookCellTextDocumentFilter;
(function (NotebookCellTextDocumentFilter) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate)
            && (Is.string(candidate.notebook) || NotebookDocumentFilter.is(candidate.notebook))
            && (candidate.language === undefined || Is.string(candidate.language));
    }
    NotebookCellTextDocumentFilter.is = is;
})(NotebookCellTextDocumentFilter = exports.NotebookCellTextDocumentFilter || (exports.NotebookCellTextDocumentFilter = {}));
/**
 * The DocumentSelector namespace provides helper functions to work with
 * [DocumentSelector](#DocumentSelector)s.
 */
var DocumentSelector;
(function (DocumentSelector) {
    function is(value) {
        if (!Array.isArray(value)) {
            return false;
        }
        for (let elem of value) {
            if (!Is.string(elem) && !TextDocumentFilter.is(elem) && !NotebookCellTextDocumentFilter.is(elem)) {
                return false;
            }
        }
        return true;
    }
    DocumentSelector.is = is;
})(DocumentSelector = exports.DocumentSelector || (exports.DocumentSelector = {}));
/**
 * The `client/registerCapability` request is sent from the server to the client to register a new capability
 * handler on the client side.
 */
var RegistrationRequest;
(function (RegistrationRequest) {
    RegistrationRequest.type = new messages_1.ProtocolRequestType('client/registerCapability');
})(RegistrationRequest = exports.RegistrationRequest || (exports.RegistrationRequest = {}));
/**
 * The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
 * handler on the client side.
 */
var UnregistrationRequest;
(function (UnregistrationRequest) {
    UnregistrationRequest.type = new messages_1.ProtocolRequestType('client/unregisterCapability');
})(UnregistrationRequest = exports.UnregistrationRequest || (exports.UnregistrationRequest = {}));
var ResourceOperationKind;
(function (ResourceOperationKind) {
    /**
     * Supports creating new files and folders.
     */
    ResourceOperationKind.Create = 'create';
    /**
     * Supports renaming existing files and folders.
     */
    ResourceOperationKind.Rename = 'rename';
    /**
     * Supports deleting existing files and folders.
     */
    ResourceOperationKind.Delete = 'delete';
})(ResourceOperationKind = exports.ResourceOperationKind || (exports.ResourceOperationKind = {}));
var FailureHandlingKind;
(function (FailureHandlingKind) {
    /**
     * Applying the workspace change is simply aborted if one of the changes provided
     * fails. All operations executed before the failing operation stay executed.
     */
    FailureHandlingKind.Abort = 'abort';
    /**
     * All operations are executed transactional. That means they either all
     * succeed or no changes at all are applied to the workspace.
     */
    FailureHandlingKind.Transactional = 'transactional';
    /**
     * If the workspace edit contains only textual file changes they are executed transactional.
     * If resource changes (create, rename or delete file) are part of the change the failure
     * handling strategy is abort.
     */
    FailureHandlingKind.TextOnlyTransactional = 'textOnlyTransactional';
    /**
     * The client tries to undo the operations already executed. But there is no
     * guarantee that this is succeeding.
     */
    FailureHandlingKind.Undo = 'undo';
})(FailureHandlingKind = exports.FailureHandlingKind || (exports.FailureHandlingKind = {}));
/**
 * A set of predefined position encoding kinds.
 *
 * @since 3.17.0
 */
var PositionEncodingKind;
(function (PositionEncodingKind) {
    /**
     * Character offsets count UTF-8 code units.
     */
    PositionEncodingKind.UTF8 = 'utf-8';
    /**
     * Character offsets count UTF-16 code units.
     *
     * This is the default and must always be supported
     * by servers
     */
    PositionEncodingKind.UTF16 = 'utf-16';
    /**
     * Character offsets count UTF-32 code units.
     *
     * Implementation note: these are the same as Unicode code points,
     * so this `PositionEncodingKind` may also be used for an
     * encoding-agnostic representation of character offsets.
     */
    PositionEncodingKind.UTF32 = 'utf-32';
})(PositionEncodingKind = exports.PositionEncodingKind || (exports.PositionEncodingKind = {}));
/**
 * The StaticRegistrationOptions namespace provides helper functions to work with
 * [StaticRegistrationOptions](#StaticRegistrationOptions) literals.
 */
var StaticRegistrationOptions;
(function (StaticRegistrationOptions) {
    function hasId(value) {
        const candidate = value;
        return candidate && Is.string(candidate.id) && candidate.id.length > 0;
    }
    StaticRegistrationOptions.hasId = hasId;
})(StaticRegistrationOptions = exports.StaticRegistrationOptions || (exports.StaticRegistrationOptions = {}));
/**
 * The TextDocumentRegistrationOptions namespace provides helper functions to work with
 * [TextDocumentRegistrationOptions](#TextDocumentRegistrationOptions) literals.
 */
var TextDocumentRegistrationOptions;
(function (TextDocumentRegistrationOptions) {
    function is(value) {
        const candidate = value;
        return candidate && (candidate.documentSelector === null || DocumentSelector.is(candidate.documentSelector));
    }
    TextDocumentRegistrationOptions.is = is;
})(TextDocumentRegistrationOptions = exports.TextDocumentRegistrationOptions || (exports.TextDocumentRegistrationOptions = {}));
/**
 * The WorkDoneProgressOptions namespace provides helper functions to work with
 * [WorkDoneProgressOptions](#WorkDoneProgressOptions) literals.
 */
var WorkDoneProgressOptions;
(function (WorkDoneProgressOptions) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && (candidate.workDoneProgress === undefined || Is.boolean(candidate.workDoneProgress));
    }
    WorkDoneProgressOptions.is = is;
    function hasWorkDoneProgress(value) {
        const candidate = value;
        return candidate && Is.boolean(candidate.workDoneProgress);
    }
    WorkDoneProgressOptions.hasWorkDoneProgress = hasWorkDoneProgress;
})(WorkDoneProgressOptions = exports.WorkDoneProgressOptions || (exports.WorkDoneProgressOptions = {}));
/**
 * The initialize request is sent from the client to the server.
 * It is sent once as the request after starting up the server.
 * The requests parameter is of type [InitializeParams](#InitializeParams)
 * the response if of type [InitializeResult](#InitializeResult) of a Thenable that
 * resolves to such.
 */
var InitializeRequest;
(function (InitializeRequest) {
    InitializeRequest.type = new messages_1.ProtocolRequestType('initialize');
})(InitializeRequest = exports.InitializeRequest || (exports.InitializeRequest = {}));
/**
 * Known error codes for an `InitializeErrorCodes`;
 */
var InitializeErrorCodes;
(function (InitializeErrorCodes) {
    /**
     * If the protocol version provided by the client can't be handled by the server.
     *
     * @deprecated This initialize error got replaced by client capabilities. There is
     * no version handshake in version 3.0x
     */
    InitializeErrorCodes.unknownProtocolVersion = 1;
})(InitializeErrorCodes = exports.InitializeErrorCodes || (exports.InitializeErrorCodes = {}));
/**
 * The initialized notification is sent from the client to the
 * server after the client is fully initialized and the server
 * is allowed to send requests from the server to the client.
 */
var InitializedNotification;
(function (InitializedNotification) {
    InitializedNotification.type = new messages_1.ProtocolNotificationType('initialized');
})(InitializedNotification = exports.InitializedNotification || (exports.InitializedNotification = {}));
//---- Shutdown Method ----
/**
 * A shutdown request is sent from the client to the server.
 * It is sent once when the client decides to shutdown the
 * server. The only notification that is sent after a shutdown request
 * is the exit event.
 */
var ShutdownRequest;
(function (ShutdownRequest) {
    ShutdownRequest.type = new messages_1.ProtocolRequestType0('shutdown');
})(ShutdownRequest = exports.ShutdownRequest || (exports.ShutdownRequest = {}));
//---- Exit Notification ----
/**
 * The exit event is sent from the client to the server to
 * ask the server to exit its process.
 */
var ExitNotification;
(function (ExitNotification) {
    ExitNotification.type = new messages_1.ProtocolNotificationType0('exit');
})(ExitNotification = exports.ExitNotification || (exports.ExitNotification = {}));
/**
 * The configuration change notification is sent from the client to the server
 * when the client's configuration has changed. The notification contains
 * the changed configuration as defined by the language client.
 */
var DidChangeConfigurationNotification;
(function (DidChangeConfigurationNotification) {
    DidChangeConfigurationNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeConfiguration');
})(DidChangeConfigurationNotification = exports.DidChangeConfigurationNotification || (exports.DidChangeConfigurationNotification = {}));
//---- Message show and log notifications ----
/**
 * The message type
 */
var MessageType;
(function (MessageType) {
    /**
     * An error message.
     */
    MessageType.Error = 1;
    /**
     * A warning message.
     */
    MessageType.Warning = 2;
    /**
     * An information message.
     */
    MessageType.Info = 3;
    /**
     * A log message.
     */
    MessageType.Log = 4;
})(MessageType = exports.MessageType || (exports.MessageType = {}));
/**
 * The show message notification is sent from a server to a client to ask
 * the client to display a particular message in the user interface.
 */
var ShowMessageNotification;
(function (ShowMessageNotification) {
    ShowMessageNotification.type = new messages_1.ProtocolNotificationType('window/showMessage');
})(ShowMessageNotification = exports.ShowMessageNotification || (exports.ShowMessageNotification = {}));
/**
 * The show message request is sent from the server to the client to show a message
 * and a set of options actions to the user.
 */
var ShowMessageRequest;
(function (ShowMessageRequest) {
    ShowMessageRequest.type = new messages_1.ProtocolRequestType('window/showMessageRequest');
})(ShowMessageRequest = exports.ShowMessageRequest || (exports.ShowMessageRequest = {}));
/**
 * The log message notification is sent from the server to the client to ask
 * the client to log a particular message.
 */
var LogMessageNotification;
(function (LogMessageNotification) {
    LogMessageNotification.type = new messages_1.ProtocolNotificationType('window/logMessage');
})(LogMessageNotification = exports.LogMessageNotification || (exports.LogMessageNotification = {}));
//---- Telemetry notification
/**
 * The telemetry event notification is sent from the server to the client to ask
 * the client to log telemetry data.
 */
var TelemetryEventNotification;
(function (TelemetryEventNotification) {
    TelemetryEventNotification.type = new messages_1.ProtocolNotificationType('telemetry/event');
})(TelemetryEventNotification = exports.TelemetryEventNotification || (exports.TelemetryEventNotification = {}));
/**
 * Defines how the host (editor) should sync
 * document changes to the language server.
 */
var TextDocumentSyncKind;
(function (TextDocumentSyncKind) {
    /**
     * Documents should not be synced at all.
     */
    TextDocumentSyncKind.None = 0;
    /**
     * Documents are synced by always sending the full content
     * of the document.
     */
    TextDocumentSyncKind.Full = 1;
    /**
     * Documents are synced by sending the full content on open.
     * After that only incremental updates to the document are
     * send.
     */
    TextDocumentSyncKind.Incremental = 2;
})(TextDocumentSyncKind = exports.TextDocumentSyncKind || (exports.TextDocumentSyncKind = {}));
/**
 * The document open notification is sent from the client to the server to signal
 * newly opened text documents. The document's truth is now managed by the client
 * and the server must not try to read the document's truth using the document's
 * uri. Open in this sense means it is managed by the client. It doesn't necessarily
 * mean that its content is presented in an editor. An open notification must not
 * be sent more than once without a corresponding close notification send before.
 * This means open and close notification must be balanced and the max open count
 * is one.
 */
var DidOpenTextDocumentNotification;
(function (DidOpenTextDocumentNotification) {
    DidOpenTextDocumentNotification.method = 'textDocument/didOpen';
    DidOpenTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidOpenTextDocumentNotification.method);
})(DidOpenTextDocumentNotification = exports.DidOpenTextDocumentNotification || (exports.DidOpenTextDocumentNotification = {}));
var TextDocumentContentChangeEvent;
(function (TextDocumentContentChangeEvent) {
    /**
     * Checks whether the information describes a delta event.
     */
    function isIncremental(event) {
        let candidate = event;
        return candidate !== undefined && candidate !== null &&
            typeof candidate.text === 'string' && candidate.range !== undefined &&
            (candidate.rangeLength === undefined || typeof candidate.rangeLength === 'number');
    }
    TextDocumentContentChangeEvent.isIncremental = isIncremental;
    /**
     * Checks whether the information describes a full replacement event.
     */
    function isFull(event) {
        let candidate = event;
        return candidate !== undefined && candidate !== null &&
            typeof candidate.text === 'string' && candidate.range === undefined && candidate.rangeLength === undefined;
    }
    TextDocumentContentChangeEvent.isFull = isFull;
})(TextDocumentContentChangeEvent = exports.TextDocumentContentChangeEvent || (exports.TextDocumentContentChangeEvent = {}));
/**
 * The document change notification is sent from the client to the server to signal
 * changes to a text document.
 */
var DidChangeTextDocumentNotification;
(function (DidChangeTextDocumentNotification) {
    DidChangeTextDocumentNotification.method = 'textDocument/didChange';
    DidChangeTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidChangeTextDocumentNotification.method);
})(DidChangeTextDocumentNotification = exports.DidChangeTextDocumentNotification || (exports.DidChangeTextDocumentNotification = {}));
/**
 * The document close notification is sent from the client to the server when
 * the document got closed in the client. The document's truth now exists where
 * the document's uri points to (e.g. if the document's uri is a file uri the
 * truth now exists on disk). As with the open notification the close notification
 * is about managing the document's content. Receiving a close notification
 * doesn't mean that the document was open in an editor before. A close
 * notification requires a previous open notification to be sent.
 */
var DidCloseTextDocumentNotification;
(function (DidCloseTextDocumentNotification) {
    DidCloseTextDocumentNotification.method = 'textDocument/didClose';
    DidCloseTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidCloseTextDocumentNotification.method);
})(DidCloseTextDocumentNotification = exports.DidCloseTextDocumentNotification || (exports.DidCloseTextDocumentNotification = {}));
/**
 * The document save notification is sent from the client to the server when
 * the document got saved in the client.
 */
var DidSaveTextDocumentNotification;
(function (DidSaveTextDocumentNotification) {
    DidSaveTextDocumentNotification.method = 'textDocument/didSave';
    DidSaveTextDocumentNotification.type = new messages_1.ProtocolNotificationType(DidSaveTextDocumentNotification.method);
})(DidSaveTextDocumentNotification = exports.DidSaveTextDocumentNotification || (exports.DidSaveTextDocumentNotification = {}));
/**
 * Represents reasons why a text document is saved.
 */
var TextDocumentSaveReason;
(function (TextDocumentSaveReason) {
    /**
     * Manually triggered, e.g. by the user pressing save, by starting debugging,
     * or by an API call.
     */
    TextDocumentSaveReason.Manual = 1;
    /**
     * Automatic after a delay.
     */
    TextDocumentSaveReason.AfterDelay = 2;
    /**
     * When the editor lost focus.
     */
    TextDocumentSaveReason.FocusOut = 3;
})(TextDocumentSaveReason = exports.TextDocumentSaveReason || (exports.TextDocumentSaveReason = {}));
/**
 * A document will save notification is sent from the client to the server before
 * the document is actually saved.
 */
var WillSaveTextDocumentNotification;
(function (WillSaveTextDocumentNotification) {
    WillSaveTextDocumentNotification.method = 'textDocument/willSave';
    WillSaveTextDocumentNotification.type = new messages_1.ProtocolNotificationType(WillSaveTextDocumentNotification.method);
})(WillSaveTextDocumentNotification = exports.WillSaveTextDocumentNotification || (exports.WillSaveTextDocumentNotification = {}));
/**
 * A document will save request is sent from the client to the server before
 * the document is actually saved. The request can return an array of TextEdits
 * which will be applied to the text document before it is saved. Please note that
 * clients might drop results if computing the text edits took too long or if a
 * server constantly fails on this request. This is done to keep the save fast and
 * reliable.
 */
var WillSaveTextDocumentWaitUntilRequest;
(function (WillSaveTextDocumentWaitUntilRequest) {
    WillSaveTextDocumentWaitUntilRequest.method = 'textDocument/willSaveWaitUntil';
    WillSaveTextDocumentWaitUntilRequest.type = new messages_1.ProtocolRequestType(WillSaveTextDocumentWaitUntilRequest.method);
})(WillSaveTextDocumentWaitUntilRequest = exports.WillSaveTextDocumentWaitUntilRequest || (exports.WillSaveTextDocumentWaitUntilRequest = {}));
/**
 * The watched files notification is sent from the client to the server when
 * the client detects changes to file watched by the language client.
 */
var DidChangeWatchedFilesNotification;
(function (DidChangeWatchedFilesNotification) {
    DidChangeWatchedFilesNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeWatchedFiles');
})(DidChangeWatchedFilesNotification = exports.DidChangeWatchedFilesNotification || (exports.DidChangeWatchedFilesNotification = {}));
/**
 * The file event type
 */
var FileChangeType;
(function (FileChangeType) {
    /**
     * The file got created.
     */
    FileChangeType.Created = 1;
    /**
     * The file got changed.
     */
    FileChangeType.Changed = 2;
    /**
     * The file got deleted.
     */
    FileChangeType.Deleted = 3;
})(FileChangeType = exports.FileChangeType || (exports.FileChangeType = {}));
var RelativePattern;
(function (RelativePattern) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && (vscode_languageserver_types_1.URI.is(candidate.baseUri) || vscode_languageserver_types_1.WorkspaceFolder.is(candidate.baseUri)) && Is.string(candidate.pattern);
    }
    RelativePattern.is = is;
})(RelativePattern = exports.RelativePattern || (exports.RelativePattern = {}));
var WatchKind;
(function (WatchKind) {
    /**
     * Interested in create events.
     */
    WatchKind.Create = 1;
    /**
     * Interested in change events
     */
    WatchKind.Change = 2;
    /**
     * Interested in delete events
     */
    WatchKind.Delete = 4;
})(WatchKind = exports.WatchKind || (exports.WatchKind = {}));
/**
 * Diagnostics notification are sent from the server to the client to signal
 * results of validation runs.
 */
var PublishDiagnosticsNotification;
(function (PublishDiagnosticsNotification) {
    PublishDiagnosticsNotification.type = new messages_1.ProtocolNotificationType('textDocument/publishDiagnostics');
})(PublishDiagnosticsNotification = exports.PublishDiagnosticsNotification || (exports.PublishDiagnosticsNotification = {}));
/**
 * How a completion was triggered
 */
var CompletionTriggerKind;
(function (CompletionTriggerKind) {
    /**
     * Completion was triggered by typing an identifier (24x7 code
     * complete), manual invocation (e.g Ctrl+Space) or via API.
     */
    CompletionTriggerKind.Invoked = 1;
    /**
     * Completion was triggered by a trigger character specified by
     * the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
     */
    CompletionTriggerKind.TriggerCharacter = 2;
    /**
     * Completion was re-triggered as current completion list is incomplete
     */
    CompletionTriggerKind.TriggerForIncompleteCompletions = 3;
})(CompletionTriggerKind = exports.CompletionTriggerKind || (exports.CompletionTriggerKind = {}));
/**
 * Request to request completion at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response
 * is of type [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)
 * or a Thenable that resolves to such.
 *
 * The request can delay the computation of the [`detail`](#CompletionItem.detail)
 * and [`documentation`](#CompletionItem.documentation) properties to the `completionItem/resolve`
 * request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
 * `filterText`, `insertText`, and `textEdit`, must not be changed during resolve.
 */
var CompletionRequest;
(function (CompletionRequest) {
    CompletionRequest.method = 'textDocument/completion';
    CompletionRequest.type = new messages_1.ProtocolRequestType(CompletionRequest.method);
})(CompletionRequest = exports.CompletionRequest || (exports.CompletionRequest = {}));
/**
 * Request to resolve additional information for a given completion item.The request's
 * parameter is of type [CompletionItem](#CompletionItem) the response
 * is of type [CompletionItem](#CompletionItem) or a Thenable that resolves to such.
 */
var CompletionResolveRequest;
(function (CompletionResolveRequest) {
    CompletionResolveRequest.method = 'completionItem/resolve';
    CompletionResolveRequest.type = new messages_1.ProtocolRequestType(CompletionResolveRequest.method);
})(CompletionResolveRequest = exports.CompletionResolveRequest || (exports.CompletionResolveRequest = {}));
/**
 * Request to request hover information at a given text document position. The request's
 * parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response is of
 * type [Hover](#Hover) or a Thenable that resolves to such.
 */
var HoverRequest;
(function (HoverRequest) {
    HoverRequest.method = 'textDocument/hover';
    HoverRequest.type = new messages_1.ProtocolRequestType(HoverRequest.method);
})(HoverRequest = exports.HoverRequest || (exports.HoverRequest = {}));
/**
 * How a signature help was triggered.
 *
 * @since 3.15.0
 */
var SignatureHelpTriggerKind;
(function (SignatureHelpTriggerKind) {
    /**
     * Signature help was invoked manually by the user or by a command.
     */
    SignatureHelpTriggerKind.Invoked = 1;
    /**
     * Signature help was triggered by a trigger character.
     */
    SignatureHelpTriggerKind.TriggerCharacter = 2;
    /**
     * Signature help was triggered by the cursor moving or by the document content changing.
     */
    SignatureHelpTriggerKind.ContentChange = 3;
})(SignatureHelpTriggerKind = exports.SignatureHelpTriggerKind || (exports.SignatureHelpTriggerKind = {}));
var SignatureHelpRequest;
(function (SignatureHelpRequest) {
    SignatureHelpRequest.method = 'textDocument/signatureHelp';
    SignatureHelpRequest.type = new messages_1.ProtocolRequestType(SignatureHelpRequest.method);
})(SignatureHelpRequest = exports.SignatureHelpRequest || (exports.SignatureHelpRequest = {}));
/**
 * A request to resolve the definition location of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the response is of either type [Definition](#Definition)
 * or a typed array of [DefinitionLink](#DefinitionLink) or a Thenable that resolves
 * to such.
 */
var DefinitionRequest;
(function (DefinitionRequest) {
    DefinitionRequest.method = 'textDocument/definition';
    DefinitionRequest.type = new messages_1.ProtocolRequestType(DefinitionRequest.method);
})(DefinitionRequest = exports.DefinitionRequest || (exports.DefinitionRequest = {}));
/**
 * A request to resolve project-wide references for the symbol denoted
 * by the given text document position. The request's parameter is of
 * type [ReferenceParams](#ReferenceParams) the response is of type
 * [Location[]](#Location) or a Thenable that resolves to such.
 */
var ReferencesRequest;
(function (ReferencesRequest) {
    ReferencesRequest.method = 'textDocument/references';
    ReferencesRequest.type = new messages_1.ProtocolRequestType(ReferencesRequest.method);
})(ReferencesRequest = exports.ReferencesRequest || (exports.ReferencesRequest = {}));
/**
 * Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given
 * text document position. The request's parameter is of type [TextDocumentPosition]
 * (#TextDocumentPosition) the request response is of type [DocumentHighlight[]]
 * (#DocumentHighlight) or a Thenable that resolves to such.
 */
var DocumentHighlightRequest;
(function (DocumentHighlightRequest) {
    DocumentHighlightRequest.method = 'textDocument/documentHighlight';
    DocumentHighlightRequest.type = new messages_1.ProtocolRequestType(DocumentHighlightRequest.method);
})(DocumentHighlightRequest = exports.DocumentHighlightRequest || (exports.DocumentHighlightRequest = {}));
/**
 * A request to list all symbols found in a given text document. The request's
 * parameter is of type [TextDocumentIdentifier](#TextDocumentIdentifier) the
 * response is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
 * that resolves to such.
 */
var DocumentSymbolRequest;
(function (DocumentSymbolRequest) {
    DocumentSymbolRequest.method = 'textDocument/documentSymbol';
    DocumentSymbolRequest.type = new messages_1.ProtocolRequestType(DocumentSymbolRequest.method);
})(DocumentSymbolRequest = exports.DocumentSymbolRequest || (exports.DocumentSymbolRequest = {}));
/**
 * A request to provide commands for the given text document and range.
 */
var CodeActionRequest;
(function (CodeActionRequest) {
    CodeActionRequest.method = 'textDocument/codeAction';
    CodeActionRequest.type = new messages_1.ProtocolRequestType(CodeActionRequest.method);
})(CodeActionRequest = exports.CodeActionRequest || (exports.CodeActionRequest = {}));
/**
 * Request to resolve additional information for a given code action.The request's
 * parameter is of type [CodeAction](#CodeAction) the response
 * is of type [CodeAction](#CodeAction) or a Thenable that resolves to such.
 */
var CodeActionResolveRequest;
(function (CodeActionResolveRequest) {
    CodeActionResolveRequest.method = 'codeAction/resolve';
    CodeActionResolveRequest.type = new messages_1.ProtocolRequestType(CodeActionResolveRequest.method);
})(CodeActionResolveRequest = exports.CodeActionResolveRequest || (exports.CodeActionResolveRequest = {}));
/**
 * A request to list project-wide symbols matching the query string given
 * by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is
 * of type [SymbolInformation[]](#SymbolInformation) or a Thenable that
 * resolves to such.
 *
 * @since 3.17.0 - support for WorkspaceSymbol in the returned data. Clients
 *  need to advertise support for WorkspaceSymbols via the client capability
 *  `workspace.symbol.resolveSupport`.
 *
 */
var WorkspaceSymbolRequest;
(function (WorkspaceSymbolRequest) {
    WorkspaceSymbolRequest.method = 'workspace/symbol';
    WorkspaceSymbolRequest.type = new messages_1.ProtocolRequestType(WorkspaceSymbolRequest.method);
})(WorkspaceSymbolRequest = exports.WorkspaceSymbolRequest || (exports.WorkspaceSymbolRequest = {}));
/**
 * A request to resolve the range inside the workspace
 * symbol's location.
 *
 * @since 3.17.0
 */
var WorkspaceSymbolResolveRequest;
(function (WorkspaceSymbolResolveRequest) {
    WorkspaceSymbolResolveRequest.method = 'workspaceSymbol/resolve';
    WorkspaceSymbolResolveRequest.type = new messages_1.ProtocolRequestType(WorkspaceSymbolResolveRequest.method);
})(WorkspaceSymbolResolveRequest = exports.WorkspaceSymbolResolveRequest || (exports.WorkspaceSymbolResolveRequest = {}));
/**
 * A request to provide code lens for the given text document.
 */
var CodeLensRequest;
(function (CodeLensRequest) {
    CodeLensRequest.method = 'textDocument/codeLens';
    CodeLensRequest.type = new messages_1.ProtocolRequestType(CodeLensRequest.method);
})(CodeLensRequest = exports.CodeLensRequest || (exports.CodeLensRequest = {}));
/**
 * A request to resolve a command for a given code lens.
 */
var CodeLensResolveRequest;
(function (CodeLensResolveRequest) {
    CodeLensResolveRequest.method = 'codeLens/resolve';
    CodeLensResolveRequest.type = new messages_1.ProtocolRequestType(CodeLensResolveRequest.method);
})(CodeLensResolveRequest = exports.CodeLensResolveRequest || (exports.CodeLensResolveRequest = {}));
/**
 * A request to refresh all code actions
 *
 * @since 3.16.0
 */
var CodeLensRefreshRequest;
(function (CodeLensRefreshRequest) {
    CodeLensRefreshRequest.method = `workspace/codeLens/refresh`;
    CodeLensRefreshRequest.type = new messages_1.ProtocolRequestType0(CodeLensRefreshRequest.method);
})(CodeLensRefreshRequest = exports.CodeLensRefreshRequest || (exports.CodeLensRefreshRequest = {}));
/**
 * A request to provide document links
 */
var DocumentLinkRequest;
(function (DocumentLinkRequest) {
    DocumentLinkRequest.method = 'textDocument/documentLink';
    DocumentLinkRequest.type = new messages_1.ProtocolRequestType(DocumentLinkRequest.method);
})(DocumentLinkRequest = exports.DocumentLinkRequest || (exports.DocumentLinkRequest = {}));
/**
 * Request to resolve additional information for a given document link. The request's
 * parameter is of type [DocumentLink](#DocumentLink) the response
 * is of type [DocumentLink](#DocumentLink) or a Thenable that resolves to such.
 */
var DocumentLinkResolveRequest;
(function (DocumentLinkResolveRequest) {
    DocumentLinkResolveRequest.method = 'documentLink/resolve';
    DocumentLinkResolveRequest.type = new messages_1.ProtocolRequestType(DocumentLinkResolveRequest.method);
})(DocumentLinkResolveRequest = exports.DocumentLinkResolveRequest || (exports.DocumentLinkResolveRequest = {}));
/**
 * A request to to format a whole document.
 */
var DocumentFormattingRequest;
(function (DocumentFormattingRequest) {
    DocumentFormattingRequest.method = 'textDocument/formatting';
    DocumentFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentFormattingRequest.method);
})(DocumentFormattingRequest = exports.DocumentFormattingRequest || (exports.DocumentFormattingRequest = {}));
/**
 * A request to to format a range in a document.
 */
var DocumentRangeFormattingRequest;
(function (DocumentRangeFormattingRequest) {
    DocumentRangeFormattingRequest.method = 'textDocument/rangeFormatting';
    DocumentRangeFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentRangeFormattingRequest.method);
})(DocumentRangeFormattingRequest = exports.DocumentRangeFormattingRequest || (exports.DocumentRangeFormattingRequest = {}));
/**
 * A request to format a document on type.
 */
var DocumentOnTypeFormattingRequest;
(function (DocumentOnTypeFormattingRequest) {
    DocumentOnTypeFormattingRequest.method = 'textDocument/onTypeFormatting';
    DocumentOnTypeFormattingRequest.type = new messages_1.ProtocolRequestType(DocumentOnTypeFormattingRequest.method);
})(DocumentOnTypeFormattingRequest = exports.DocumentOnTypeFormattingRequest || (exports.DocumentOnTypeFormattingRequest = {}));
//---- Rename ----------------------------------------------
var PrepareSupportDefaultBehavior;
(function (PrepareSupportDefaultBehavior) {
    /**
     * The client's default behavior is to select the identifier
     * according the to language's syntax rule.
     */
    PrepareSupportDefaultBehavior.Identifier = 1;
})(PrepareSupportDefaultBehavior = exports.PrepareSupportDefaultBehavior || (exports.PrepareSupportDefaultBehavior = {}));
/**
 * A request to rename a symbol.
 */
var RenameRequest;
(function (RenameRequest) {
    RenameRequest.method = 'textDocument/rename';
    RenameRequest.type = new messages_1.ProtocolRequestType(RenameRequest.method);
})(RenameRequest = exports.RenameRequest || (exports.RenameRequest = {}));
/**
 * A request to test and perform the setup necessary for a rename.
 *
 * @since 3.16 - support for default behavior
 */
var PrepareRenameRequest;
(function (PrepareRenameRequest) {
    PrepareRenameRequest.method = 'textDocument/prepareRename';
    PrepareRenameRequest.type = new messages_1.ProtocolRequestType(PrepareRenameRequest.method);
})(PrepareRenameRequest = exports.PrepareRenameRequest || (exports.PrepareRenameRequest = {}));
/**
 * A request send from the client to the server to execute a command. The request might return
 * a workspace edit which the client will apply to the workspace.
 */
var ExecuteCommandRequest;
(function (ExecuteCommandRequest) {
    ExecuteCommandRequest.type = new messages_1.ProtocolRequestType('workspace/executeCommand');
})(ExecuteCommandRequest = exports.ExecuteCommandRequest || (exports.ExecuteCommandRequest = {}));
/**
 * A request sent from the server to the client to modified certain resources.
 */
var ApplyWorkspaceEditRequest;
(function (ApplyWorkspaceEditRequest) {
    ApplyWorkspaceEditRequest.type = new messages_1.ProtocolRequestType('workspace/applyEdit');
})(ApplyWorkspaceEditRequest = exports.ApplyWorkspaceEditRequest || (exports.ApplyWorkspaceEditRequest = {}));
//# sourceMappingURL=protocol.js.map

/***/ }),
/* 81 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.objectLiteral = exports.typedArray = exports.stringArray = exports.array = exports.func = exports.error = exports.number = exports.string = exports.boolean = void 0;
function boolean(value) {
    return value === true || value === false;
}
exports.boolean = boolean;
function string(value) {
    return typeof value === 'string' || value instanceof String;
}
exports.string = string;
function number(value) {
    return typeof value === 'number' || value instanceof Number;
}
exports.number = number;
function error(value) {
    return value instanceof Error;
}
exports.error = error;
function func(value) {
    return typeof value === 'function';
}
exports.func = func;
function array(value) {
    return Array.isArray(value);
}
exports.array = array;
function stringArray(value) {
    return array(value) && value.every(elem => string(elem));
}
exports.stringArray = stringArray;
function typedArray(value, check) {
    return Array.isArray(value) && value.every(check);
}
exports.typedArray = typedArray;
function objectLiteral(value) {
    // Strictly speaking class instances pass this check as well. Since the LSP
    // doesn't use classes we ignore this for now. If we do we need to add something
    // like this: `Object.getPrototypeOf(Object.getPrototypeOf(x)) === null`
    return value !== null && typeof value === 'object';
}
exports.objectLiteral = objectLiteral;
//# sourceMappingURL=is.js.map

/***/ }),
/* 82 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ImplementationRequest = void 0;
const messages_1 = __webpack_require__(79);
// @ts-ignore: to avoid inlining LocationLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the implementation locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositionParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var ImplementationRequest;
(function (ImplementationRequest) {
    ImplementationRequest.method = 'textDocument/implementation';
    ImplementationRequest.type = new messages_1.ProtocolRequestType(ImplementationRequest.method);
})(ImplementationRequest = exports.ImplementationRequest || (exports.ImplementationRequest = {}));
//# sourceMappingURL=protocol.implementation.js.map

/***/ }),
/* 83 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeDefinitionRequest = void 0;
const messages_1 = __webpack_require__(79);
// @ts-ignore: to avoid inlining LocatioLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositioParams]
 * (#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
 * Thenable that resolves to such.
 */
var TypeDefinitionRequest;
(function (TypeDefinitionRequest) {
    TypeDefinitionRequest.method = 'textDocument/typeDefinition';
    TypeDefinitionRequest.type = new messages_1.ProtocolRequestType(TypeDefinitionRequest.method);
})(TypeDefinitionRequest = exports.TypeDefinitionRequest || (exports.TypeDefinitionRequest = {}));
//# sourceMappingURL=protocol.typeDefinition.js.map

/***/ }),
/* 84 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DidChangeWorkspaceFoldersNotification = exports.WorkspaceFoldersRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders.
 */
var WorkspaceFoldersRequest;
(function (WorkspaceFoldersRequest) {
    WorkspaceFoldersRequest.type = new messages_1.ProtocolRequestType0('workspace/workspaceFolders');
})(WorkspaceFoldersRequest = exports.WorkspaceFoldersRequest || (exports.WorkspaceFoldersRequest = {}));
/**
 * The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
 * folder configuration changes.
 */
var DidChangeWorkspaceFoldersNotification;
(function (DidChangeWorkspaceFoldersNotification) {
    DidChangeWorkspaceFoldersNotification.type = new messages_1.ProtocolNotificationType('workspace/didChangeWorkspaceFolders');
})(DidChangeWorkspaceFoldersNotification = exports.DidChangeWorkspaceFoldersNotification || (exports.DidChangeWorkspaceFoldersNotification = {}));
//# sourceMappingURL=protocol.workspaceFolder.js.map

/***/ }),
/* 85 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ConfigurationRequest = void 0;
const messages_1 = __webpack_require__(79);
//---- Get Configuration request ----
/**
 * The 'workspace/configuration' request is sent from the server to the client to fetch a certain
 * configuration setting.
 *
 * This pull model replaces the old push model were the client signaled configuration change via an
 * event. If the server still needs to react to configuration changes (since the server caches the
 * result of `workspace/configuration` requests) the server should register for an empty configuration
 * change event and empty the cache if such an event is received.
 */
var ConfigurationRequest;
(function (ConfigurationRequest) {
    ConfigurationRequest.type = new messages_1.ProtocolRequestType('workspace/configuration');
})(ConfigurationRequest = exports.ConfigurationRequest || (exports.ConfigurationRequest = {}));
//# sourceMappingURL=protocol.configuration.js.map

/***/ }),
/* 86 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ColorPresentationRequest = exports.DocumentColorRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to list all color symbols found in a given text document. The request's
 * parameter is of type [DocumentColorParams](#DocumentColorParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var DocumentColorRequest;
(function (DocumentColorRequest) {
    DocumentColorRequest.method = 'textDocument/documentColor';
    DocumentColorRequest.type = new messages_1.ProtocolRequestType(DocumentColorRequest.method);
})(DocumentColorRequest = exports.DocumentColorRequest || (exports.DocumentColorRequest = {}));
/**
 * A request to list all presentation for a color. The request's
 * parameter is of type [ColorPresentationParams](#ColorPresentationParams) the
 * response is of type [ColorInformation[]](#ColorInformation) or a Thenable
 * that resolves to such.
 */
var ColorPresentationRequest;
(function (ColorPresentationRequest) {
    ColorPresentationRequest.type = new messages_1.ProtocolRequestType('textDocument/colorPresentation');
})(ColorPresentationRequest = exports.ColorPresentationRequest || (exports.ColorPresentationRequest = {}));
//# sourceMappingURL=protocol.colorProvider.js.map

/***/ }),
/* 87 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.FoldingRangeRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to provide folding ranges in a document. The request's
 * parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
 * response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
 * that resolves to such.
 */
var FoldingRangeRequest;
(function (FoldingRangeRequest) {
    FoldingRangeRequest.method = 'textDocument/foldingRange';
    FoldingRangeRequest.type = new messages_1.ProtocolRequestType(FoldingRangeRequest.method);
})(FoldingRangeRequest = exports.FoldingRangeRequest || (exports.FoldingRangeRequest = {}));
//# sourceMappingURL=protocol.foldingRange.js.map

/***/ }),
/* 88 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DeclarationRequest = void 0;
const messages_1 = __webpack_require__(79);
// @ts-ignore: to avoid inlining LocationLink as dynamic import
let __noDynamicImport;
/**
 * A request to resolve the type definition locations of a symbol at a given text
 * document position. The request's parameter is of type [TextDocumentPositionParams]
 * (#TextDocumentPositionParams) the response is of type [Declaration](#Declaration)
 * or a typed array of [DeclarationLink](#DeclarationLink) or a Thenable that resolves
 * to such.
 */
var DeclarationRequest;
(function (DeclarationRequest) {
    DeclarationRequest.method = 'textDocument/declaration';
    DeclarationRequest.type = new messages_1.ProtocolRequestType(DeclarationRequest.method);
})(DeclarationRequest = exports.DeclarationRequest || (exports.DeclarationRequest = {}));
//# sourceMappingURL=protocol.declaration.js.map

/***/ }),
/* 89 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SelectionRangeRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to provide selection ranges in a document. The request's
 * parameter is of type [SelectionRangeParams](#SelectionRangeParams), the
 * response is of type [SelectionRange[]](#SelectionRange[]) or a Thenable
 * that resolves to such.
 */
var SelectionRangeRequest;
(function (SelectionRangeRequest) {
    SelectionRangeRequest.method = 'textDocument/selectionRange';
    SelectionRangeRequest.type = new messages_1.ProtocolRequestType(SelectionRangeRequest.method);
})(SelectionRangeRequest = exports.SelectionRangeRequest || (exports.SelectionRangeRequest = {}));
//# sourceMappingURL=protocol.selectionRange.js.map

/***/ }),
/* 90 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkDoneProgressCancelNotification = exports.WorkDoneProgressCreateRequest = exports.WorkDoneProgress = void 0;
const vscode_jsonrpc_1 = __webpack_require__(58);
const messages_1 = __webpack_require__(79);
var WorkDoneProgress;
(function (WorkDoneProgress) {
    WorkDoneProgress.type = new vscode_jsonrpc_1.ProgressType();
    function is(value) {
        return value === WorkDoneProgress.type;
    }
    WorkDoneProgress.is = is;
})(WorkDoneProgress = exports.WorkDoneProgress || (exports.WorkDoneProgress = {}));
/**
 * The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress
 * reporting from the server.
 */
var WorkDoneProgressCreateRequest;
(function (WorkDoneProgressCreateRequest) {
    WorkDoneProgressCreateRequest.method = 'window/workDoneProgress/create';
    WorkDoneProgressCreateRequest.type = new messages_1.ProtocolRequestType(WorkDoneProgressCreateRequest.method);
})(WorkDoneProgressCreateRequest = exports.WorkDoneProgressCreateRequest || (exports.WorkDoneProgressCreateRequest = {}));
/**
 * The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress
 * initiated on the server side.
 */
var WorkDoneProgressCancelNotification;
(function (WorkDoneProgressCancelNotification) {
    WorkDoneProgressCancelNotification.method = 'window/workDoneProgress/cancel';
    WorkDoneProgressCancelNotification.type = new messages_1.ProtocolNotificationType(WorkDoneProgressCancelNotification.method);
})(WorkDoneProgressCancelNotification = exports.WorkDoneProgressCancelNotification || (exports.WorkDoneProgressCancelNotification = {}));
//# sourceMappingURL=protocol.progress.js.map

/***/ }),
/* 91 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) TypeFox, Microsoft and others. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CallHierarchyOutgoingCallsRequest = exports.CallHierarchyIncomingCallsRequest = exports.CallHierarchyPrepareRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to result a `CallHierarchyItem` in a document at a given position.
 * Can be used as an input to a incoming or outgoing call hierarchy.
 *
 * @since 3.16.0
 */
var CallHierarchyPrepareRequest;
(function (CallHierarchyPrepareRequest) {
    CallHierarchyPrepareRequest.method = 'textDocument/prepareCallHierarchy';
    CallHierarchyPrepareRequest.type = new messages_1.ProtocolRequestType(CallHierarchyPrepareRequest.method);
})(CallHierarchyPrepareRequest = exports.CallHierarchyPrepareRequest || (exports.CallHierarchyPrepareRequest = {}));
/**
 * A request to resolve the incoming calls for a given `CallHierarchyItem`.
 *
 * @since 3.16.0
 */
var CallHierarchyIncomingCallsRequest;
(function (CallHierarchyIncomingCallsRequest) {
    CallHierarchyIncomingCallsRequest.method = 'callHierarchy/incomingCalls';
    CallHierarchyIncomingCallsRequest.type = new messages_1.ProtocolRequestType(CallHierarchyIncomingCallsRequest.method);
})(CallHierarchyIncomingCallsRequest = exports.CallHierarchyIncomingCallsRequest || (exports.CallHierarchyIncomingCallsRequest = {}));
/**
 * A request to resolve the outgoing calls for a given `CallHierarchyItem`.
 *
 * @since 3.16.0
 */
var CallHierarchyOutgoingCallsRequest;
(function (CallHierarchyOutgoingCallsRequest) {
    CallHierarchyOutgoingCallsRequest.method = 'callHierarchy/outgoingCalls';
    CallHierarchyOutgoingCallsRequest.type = new messages_1.ProtocolRequestType(CallHierarchyOutgoingCallsRequest.method);
})(CallHierarchyOutgoingCallsRequest = exports.CallHierarchyOutgoingCallsRequest || (exports.CallHierarchyOutgoingCallsRequest = {}));
//# sourceMappingURL=protocol.callHierarchy.js.map

/***/ }),
/* 92 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SemanticTokensRefreshRequest = exports.SemanticTokensRangeRequest = exports.SemanticTokensDeltaRequest = exports.SemanticTokensRequest = exports.SemanticTokensRegistrationType = exports.TokenFormat = void 0;
const messages_1 = __webpack_require__(79);
//------- 'textDocument/semanticTokens' -----
var TokenFormat;
(function (TokenFormat) {
    TokenFormat.Relative = 'relative';
})(TokenFormat = exports.TokenFormat || (exports.TokenFormat = {}));
var SemanticTokensRegistrationType;
(function (SemanticTokensRegistrationType) {
    SemanticTokensRegistrationType.method = 'textDocument/semanticTokens';
    SemanticTokensRegistrationType.type = new messages_1.RegistrationType(SemanticTokensRegistrationType.method);
})(SemanticTokensRegistrationType = exports.SemanticTokensRegistrationType || (exports.SemanticTokensRegistrationType = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRequest;
(function (SemanticTokensRequest) {
    SemanticTokensRequest.method = 'textDocument/semanticTokens/full';
    SemanticTokensRequest.type = new messages_1.ProtocolRequestType(SemanticTokensRequest.method);
})(SemanticTokensRequest = exports.SemanticTokensRequest || (exports.SemanticTokensRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensDeltaRequest;
(function (SemanticTokensDeltaRequest) {
    SemanticTokensDeltaRequest.method = 'textDocument/semanticTokens/full/delta';
    SemanticTokensDeltaRequest.type = new messages_1.ProtocolRequestType(SemanticTokensDeltaRequest.method);
})(SemanticTokensDeltaRequest = exports.SemanticTokensDeltaRequest || (exports.SemanticTokensDeltaRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRangeRequest;
(function (SemanticTokensRangeRequest) {
    SemanticTokensRangeRequest.method = 'textDocument/semanticTokens/range';
    SemanticTokensRangeRequest.type = new messages_1.ProtocolRequestType(SemanticTokensRangeRequest.method);
})(SemanticTokensRangeRequest = exports.SemanticTokensRangeRequest || (exports.SemanticTokensRangeRequest = {}));
/**
 * @since 3.16.0
 */
var SemanticTokensRefreshRequest;
(function (SemanticTokensRefreshRequest) {
    SemanticTokensRefreshRequest.method = `workspace/semanticTokens/refresh`;
    SemanticTokensRefreshRequest.type = new messages_1.ProtocolRequestType0(SemanticTokensRefreshRequest.method);
})(SemanticTokensRefreshRequest = exports.SemanticTokensRefreshRequest || (exports.SemanticTokensRefreshRequest = {}));
//# sourceMappingURL=protocol.semanticTokens.js.map

/***/ }),
/* 93 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ShowDocumentRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to show a document. This request might open an
 * external program depending on the value of the URI to open.
 * For example a request to open `https://code.visualstudio.com/`
 * will very likely open the URI in a WEB browser.
 *
 * @since 3.16.0
*/
var ShowDocumentRequest;
(function (ShowDocumentRequest) {
    ShowDocumentRequest.method = 'window/showDocument';
    ShowDocumentRequest.type = new messages_1.ProtocolRequestType(ShowDocumentRequest.method);
})(ShowDocumentRequest = exports.ShowDocumentRequest || (exports.ShowDocumentRequest = {}));
//# sourceMappingURL=protocol.showDocument.js.map

/***/ }),
/* 94 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LinkedEditingRangeRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to provide ranges that can be edited together.
 *
 * @since 3.16.0
 */
var LinkedEditingRangeRequest;
(function (LinkedEditingRangeRequest) {
    LinkedEditingRangeRequest.method = 'textDocument/linkedEditingRange';
    LinkedEditingRangeRequest.type = new messages_1.ProtocolRequestType(LinkedEditingRangeRequest.method);
})(LinkedEditingRangeRequest = exports.LinkedEditingRangeRequest || (exports.LinkedEditingRangeRequest = {}));
//# sourceMappingURL=protocol.linkedEditingRange.js.map

/***/ }),
/* 95 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WillDeleteFilesRequest = exports.DidDeleteFilesNotification = exports.DidRenameFilesNotification = exports.WillRenameFilesRequest = exports.DidCreateFilesNotification = exports.WillCreateFilesRequest = exports.FileOperationPatternKind = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A pattern kind describing if a glob pattern matches a file a folder or
 * both.
 *
 * @since 3.16.0
 */
var FileOperationPatternKind;
(function (FileOperationPatternKind) {
    /**
     * The pattern matches a file only.
     */
    FileOperationPatternKind.file = 'file';
    /**
     * The pattern matches a folder only.
     */
    FileOperationPatternKind.folder = 'folder';
})(FileOperationPatternKind = exports.FileOperationPatternKind || (exports.FileOperationPatternKind = {}));
/**
 * The will create files request is sent from the client to the server before files are actually
 * created as long as the creation is triggered from within the client.
 *
 * @since 3.16.0
 */
var WillCreateFilesRequest;
(function (WillCreateFilesRequest) {
    WillCreateFilesRequest.method = 'workspace/willCreateFiles';
    WillCreateFilesRequest.type = new messages_1.ProtocolRequestType(WillCreateFilesRequest.method);
})(WillCreateFilesRequest = exports.WillCreateFilesRequest || (exports.WillCreateFilesRequest = {}));
/**
 * The did create files notification is sent from the client to the server when
 * files were created from within the client.
 *
 * @since 3.16.0
 */
var DidCreateFilesNotification;
(function (DidCreateFilesNotification) {
    DidCreateFilesNotification.method = 'workspace/didCreateFiles';
    DidCreateFilesNotification.type = new messages_1.ProtocolNotificationType(DidCreateFilesNotification.method);
})(DidCreateFilesNotification = exports.DidCreateFilesNotification || (exports.DidCreateFilesNotification = {}));
/**
 * The will rename files request is sent from the client to the server before files are actually
 * renamed as long as the rename is triggered from within the client.
 *
 * @since 3.16.0
 */
var WillRenameFilesRequest;
(function (WillRenameFilesRequest) {
    WillRenameFilesRequest.method = 'workspace/willRenameFiles';
    WillRenameFilesRequest.type = new messages_1.ProtocolRequestType(WillRenameFilesRequest.method);
})(WillRenameFilesRequest = exports.WillRenameFilesRequest || (exports.WillRenameFilesRequest = {}));
/**
 * The did rename files notification is sent from the client to the server when
 * files were renamed from within the client.
 *
 * @since 3.16.0
 */
var DidRenameFilesNotification;
(function (DidRenameFilesNotification) {
    DidRenameFilesNotification.method = 'workspace/didRenameFiles';
    DidRenameFilesNotification.type = new messages_1.ProtocolNotificationType(DidRenameFilesNotification.method);
})(DidRenameFilesNotification = exports.DidRenameFilesNotification || (exports.DidRenameFilesNotification = {}));
/**
 * The will delete files request is sent from the client to the server before files are actually
 * deleted as long as the deletion is triggered from within the client.
 *
 * @since 3.16.0
 */
var DidDeleteFilesNotification;
(function (DidDeleteFilesNotification) {
    DidDeleteFilesNotification.method = 'workspace/didDeleteFiles';
    DidDeleteFilesNotification.type = new messages_1.ProtocolNotificationType(DidDeleteFilesNotification.method);
})(DidDeleteFilesNotification = exports.DidDeleteFilesNotification || (exports.DidDeleteFilesNotification = {}));
/**
 * The did delete files notification is sent from the client to the server when
 * files were deleted from within the client.
 *
 * @since 3.16.0
 */
var WillDeleteFilesRequest;
(function (WillDeleteFilesRequest) {
    WillDeleteFilesRequest.method = 'workspace/willDeleteFiles';
    WillDeleteFilesRequest.type = new messages_1.ProtocolRequestType(WillDeleteFilesRequest.method);
})(WillDeleteFilesRequest = exports.WillDeleteFilesRequest || (exports.WillDeleteFilesRequest = {}));
//# sourceMappingURL=protocol.fileOperations.js.map

/***/ }),
/* 96 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.MonikerRequest = exports.MonikerKind = exports.UniquenessLevel = void 0;
const messages_1 = __webpack_require__(79);
/**
 * Moniker uniqueness level to define scope of the moniker.
 *
 * @since 3.16.0
 */
var UniquenessLevel;
(function (UniquenessLevel) {
    /**
     * The moniker is only unique inside a document
     */
    UniquenessLevel.document = 'document';
    /**
     * The moniker is unique inside a project for which a dump got created
     */
    UniquenessLevel.project = 'project';
    /**
     * The moniker is unique inside the group to which a project belongs
     */
    UniquenessLevel.group = 'group';
    /**
     * The moniker is unique inside the moniker scheme.
     */
    UniquenessLevel.scheme = 'scheme';
    /**
     * The moniker is globally unique
     */
    UniquenessLevel.global = 'global';
})(UniquenessLevel = exports.UniquenessLevel || (exports.UniquenessLevel = {}));
/**
 * The moniker kind.
 *
 * @since 3.16.0
 */
var MonikerKind;
(function (MonikerKind) {
    /**
     * The moniker represent a symbol that is imported into a project
     */
    MonikerKind.$import = 'import';
    /**
     * The moniker represents a symbol that is exported from a project
     */
    MonikerKind.$export = 'export';
    /**
     * The moniker represents a symbol that is local to a project (e.g. a local
     * variable of a function, a class not visible outside the project, ...)
     */
    MonikerKind.local = 'local';
})(MonikerKind = exports.MonikerKind || (exports.MonikerKind = {}));
/**
 * A request to get the moniker of a symbol at a given text document position.
 * The request parameter is of type [TextDocumentPositionParams](#TextDocumentPositionParams).
 * The response is of type [Moniker[]](#Moniker[]) or `null`.
 */
var MonikerRequest;
(function (MonikerRequest) {
    MonikerRequest.method = 'textDocument/moniker';
    MonikerRequest.type = new messages_1.ProtocolRequestType(MonikerRequest.method);
})(MonikerRequest = exports.MonikerRequest || (exports.MonikerRequest = {}));
//# sourceMappingURL=protocol.moniker.js.map

/***/ }),
/* 97 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) TypeFox, Microsoft and others. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeHierarchySubtypesRequest = exports.TypeHierarchySupertypesRequest = exports.TypeHierarchyPrepareRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to result a `TypeHierarchyItem` in a document at a given position.
 * Can be used as an input to a subtypes or supertypes type hierarchy.
 *
 * @since 3.17.0
 */
var TypeHierarchyPrepareRequest;
(function (TypeHierarchyPrepareRequest) {
    TypeHierarchyPrepareRequest.method = 'textDocument/prepareTypeHierarchy';
    TypeHierarchyPrepareRequest.type = new messages_1.ProtocolRequestType(TypeHierarchyPrepareRequest.method);
})(TypeHierarchyPrepareRequest = exports.TypeHierarchyPrepareRequest || (exports.TypeHierarchyPrepareRequest = {}));
/**
 * A request to resolve the supertypes for a given `TypeHierarchyItem`.
 *
 * @since 3.17.0
 */
var TypeHierarchySupertypesRequest;
(function (TypeHierarchySupertypesRequest) {
    TypeHierarchySupertypesRequest.method = 'typeHierarchy/supertypes';
    TypeHierarchySupertypesRequest.type = new messages_1.ProtocolRequestType(TypeHierarchySupertypesRequest.method);
})(TypeHierarchySupertypesRequest = exports.TypeHierarchySupertypesRequest || (exports.TypeHierarchySupertypesRequest = {}));
/**
 * A request to resolve the subtypes for a given `TypeHierarchyItem`.
 *
 * @since 3.17.0
 */
var TypeHierarchySubtypesRequest;
(function (TypeHierarchySubtypesRequest) {
    TypeHierarchySubtypesRequest.method = 'typeHierarchy/subtypes';
    TypeHierarchySubtypesRequest.type = new messages_1.ProtocolRequestType(TypeHierarchySubtypesRequest.method);
})(TypeHierarchySubtypesRequest = exports.TypeHierarchySubtypesRequest || (exports.TypeHierarchySubtypesRequest = {}));
//# sourceMappingURL=protocol.typeHierarchy.js.map

/***/ }),
/* 98 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.InlineValueRefreshRequest = exports.InlineValueRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to provide inline values in a document. The request's parameter is of
 * type [InlineValueParams](#InlineValueParams), the response is of type
 * [InlineValue[]](#InlineValue[]) or a Thenable that resolves to such.
 *
 * @since 3.17.0
 */
var InlineValueRequest;
(function (InlineValueRequest) {
    InlineValueRequest.method = 'textDocument/inlineValue';
    InlineValueRequest.type = new messages_1.ProtocolRequestType(InlineValueRequest.method);
})(InlineValueRequest = exports.InlineValueRequest || (exports.InlineValueRequest = {}));
/**
 * @since 3.17.0
 */
var InlineValueRefreshRequest;
(function (InlineValueRefreshRequest) {
    InlineValueRefreshRequest.method = `workspace/inlineValue/refresh`;
    InlineValueRefreshRequest.type = new messages_1.ProtocolRequestType0(InlineValueRefreshRequest.method);
})(InlineValueRefreshRequest = exports.InlineValueRefreshRequest || (exports.InlineValueRefreshRequest = {}));
//# sourceMappingURL=protocol.inlineValue.js.map

/***/ }),
/* 99 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.InlayHintRefreshRequest = exports.InlayHintResolveRequest = exports.InlayHintRequest = void 0;
const messages_1 = __webpack_require__(79);
/**
 * A request to provide inlay hints in a document. The request's parameter is of
 * type [InlayHintsParams](#InlayHintsParams), the response is of type
 * [InlayHint[]](#InlayHint[]) or a Thenable that resolves to such.
 *
 * @since 3.17.0
 */
var InlayHintRequest;
(function (InlayHintRequest) {
    InlayHintRequest.method = 'textDocument/inlayHint';
    InlayHintRequest.type = new messages_1.ProtocolRequestType(InlayHintRequest.method);
})(InlayHintRequest = exports.InlayHintRequest || (exports.InlayHintRequest = {}));
/**
 * A request to resolve additional properties for a inlay hint.
 * The request's parameter is of type [InlayHint](#InlayHint), the response is
 * of type [InlayHint](#InlayHint) or a Thenable that resolves to such.
 *
 * @since 3.17.0
 */
var InlayHintResolveRequest;
(function (InlayHintResolveRequest) {
    InlayHintResolveRequest.method = 'inlayHint/resolve';
    InlayHintResolveRequest.type = new messages_1.ProtocolRequestType(InlayHintResolveRequest.method);
})(InlayHintResolveRequest = exports.InlayHintResolveRequest || (exports.InlayHintResolveRequest = {}));
/**
 * @since 3.17.0
 */
var InlayHintRefreshRequest;
(function (InlayHintRefreshRequest) {
    InlayHintRefreshRequest.method = `workspace/inlayHint/refresh`;
    InlayHintRefreshRequest.type = new messages_1.ProtocolRequestType0(InlayHintRefreshRequest.method);
})(InlayHintRefreshRequest = exports.InlayHintRefreshRequest || (exports.InlayHintRefreshRequest = {}));
//# sourceMappingURL=protocol.inlayHint.js.map

/***/ }),
/* 100 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DiagnosticRefreshRequest = exports.WorkspaceDiagnosticRequest = exports.DocumentDiagnosticRequest = exports.DocumentDiagnosticReportKind = exports.DiagnosticServerCancellationData = void 0;
const vscode_jsonrpc_1 = __webpack_require__(58);
const Is = __webpack_require__(81);
const messages_1 = __webpack_require__(79);
/**
 * @since 3.17.0
 */
var DiagnosticServerCancellationData;
(function (DiagnosticServerCancellationData) {
    function is(value) {
        const candidate = value;
        return candidate && Is.boolean(candidate.retriggerRequest);
    }
    DiagnosticServerCancellationData.is = is;
})(DiagnosticServerCancellationData = exports.DiagnosticServerCancellationData || (exports.DiagnosticServerCancellationData = {}));
/**
 * The document diagnostic report kinds.
 *
 * @since 3.17.0
 */
var DocumentDiagnosticReportKind;
(function (DocumentDiagnosticReportKind) {
    /**
     * A diagnostic report with a full
     * set of problems.
     */
    DocumentDiagnosticReportKind.Full = 'full';
    /**
     * A report indicating that the last
     * returned report is still accurate.
     */
    DocumentDiagnosticReportKind.Unchanged = 'unchanged';
})(DocumentDiagnosticReportKind = exports.DocumentDiagnosticReportKind || (exports.DocumentDiagnosticReportKind = {}));
/**
 * The document diagnostic request definition.
 *
 * @since 3.17.0
 */
var DocumentDiagnosticRequest;
(function (DocumentDiagnosticRequest) {
    DocumentDiagnosticRequest.method = 'textDocument/diagnostic';
    DocumentDiagnosticRequest.type = new messages_1.ProtocolRequestType(DocumentDiagnosticRequest.method);
    DocumentDiagnosticRequest.partialResult = new vscode_jsonrpc_1.ProgressType();
})(DocumentDiagnosticRequest = exports.DocumentDiagnosticRequest || (exports.DocumentDiagnosticRequest = {}));
/**
 * The workspace diagnostic request definition.
 *
 * @since 3.17.0
 */
var WorkspaceDiagnosticRequest;
(function (WorkspaceDiagnosticRequest) {
    WorkspaceDiagnosticRequest.method = 'workspace/diagnostic';
    WorkspaceDiagnosticRequest.type = new messages_1.ProtocolRequestType(WorkspaceDiagnosticRequest.method);
    WorkspaceDiagnosticRequest.partialResult = new vscode_jsonrpc_1.ProgressType();
})(WorkspaceDiagnosticRequest = exports.WorkspaceDiagnosticRequest || (exports.WorkspaceDiagnosticRequest = {}));
/**
 * The diagnostic refresh request definition.
 *
 * @since 3.17.0
 */
var DiagnosticRefreshRequest;
(function (DiagnosticRefreshRequest) {
    DiagnosticRefreshRequest.method = `workspace/diagnostic/refresh`;
    DiagnosticRefreshRequest.type = new messages_1.ProtocolRequestType0(DiagnosticRefreshRequest.method);
})(DiagnosticRefreshRequest = exports.DiagnosticRefreshRequest || (exports.DiagnosticRefreshRequest = {}));
//# sourceMappingURL=protocol.diagnostic.js.map

/***/ }),
/* 101 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DidCloseNotebookDocumentNotification = exports.DidSaveNotebookDocumentNotification = exports.DidChangeNotebookDocumentNotification = exports.NotebookCellArrayChange = exports.DidOpenNotebookDocumentNotification = exports.NotebookDocumentSyncRegistrationType = exports.NotebookDocument = exports.NotebookCell = exports.ExecutionSummary = exports.NotebookCellKind = void 0;
const vscode_languageserver_types_1 = __webpack_require__(78);
const Is = __webpack_require__(81);
const messages_1 = __webpack_require__(79);
/**
 * A notebook cell kind.
 *
 * @since 3.17.0
 */
var NotebookCellKind;
(function (NotebookCellKind) {
    /**
     * A markup-cell is formatted source that is used for display.
     */
    NotebookCellKind.Markup = 1;
    /**
     * A code-cell is source code.
     */
    NotebookCellKind.Code = 2;
    function is(value) {
        return value === 1 || value === 2;
    }
    NotebookCellKind.is = is;
})(NotebookCellKind = exports.NotebookCellKind || (exports.NotebookCellKind = {}));
var ExecutionSummary;
(function (ExecutionSummary) {
    function create(executionOrder, success) {
        const result = { executionOrder };
        if (success === true || success === false) {
            result.success = success;
        }
        return result;
    }
    ExecutionSummary.create = create;
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && vscode_languageserver_types_1.uinteger.is(candidate.executionOrder) && (candidate.success === undefined || Is.boolean(candidate.success));
    }
    ExecutionSummary.is = is;
    function equals(one, other) {
        if (one === other) {
            return true;
        }
        if (one === null || one === undefined || other === null || other === undefined) {
            return false;
        }
        return one.executionOrder === other.executionOrder && one.success === other.success;
    }
    ExecutionSummary.equals = equals;
})(ExecutionSummary = exports.ExecutionSummary || (exports.ExecutionSummary = {}));
var NotebookCell;
(function (NotebookCell) {
    function create(kind, document) {
        return { kind, document };
    }
    NotebookCell.create = create;
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && NotebookCellKind.is(candidate.kind) && vscode_languageserver_types_1.DocumentUri.is(candidate.document) &&
            (candidate.metadata === undefined || Is.objectLiteral(candidate.metadata));
    }
    NotebookCell.is = is;
    function diff(one, two) {
        const result = new Set();
        if (one.document !== two.document) {
            result.add('document');
        }
        if (one.kind !== two.kind) {
            result.add('kind');
        }
        if (one.executionSummary !== two.executionSummary) {
            result.add('executionSummary');
        }
        if ((one.metadata !== undefined || two.metadata !== undefined) && !equalsMetadata(one.metadata, two.metadata)) {
            result.add('metadata');
        }
        if ((one.executionSummary !== undefined || two.executionSummary !== undefined) && !ExecutionSummary.equals(one.executionSummary, two.executionSummary)) {
            result.add('executionSummary');
        }
        return result;
    }
    NotebookCell.diff = diff;
    function equalsMetadata(one, other) {
        if (one === other) {
            return true;
        }
        if (one === null || one === undefined || other === null || other === undefined) {
            return false;
        }
        if (typeof one !== typeof other) {
            return false;
        }
        if (typeof one !== 'object') {
            return false;
        }
        const oneArray = Array.isArray(one);
        const otherArray = Array.isArray(other);
        if (oneArray !== otherArray) {
            return false;
        }
        if (oneArray && otherArray) {
            if (one.length !== other.length) {
                return false;
            }
            for (let i = 0; i < one.length; i++) {
                if (!equalsMetadata(one[i], other[i])) {
                    return false;
                }
            }
        }
        if (Is.objectLiteral(one) && Is.objectLiteral(other)) {
            const oneKeys = Object.keys(one);
            const otherKeys = Object.keys(other);
            if (oneKeys.length !== otherKeys.length) {
                return false;
            }
            oneKeys.sort();
            otherKeys.sort();
            if (!equalsMetadata(oneKeys, otherKeys)) {
                return false;
            }
            for (let i = 0; i < oneKeys.length; i++) {
                const prop = oneKeys[i];
                if (!equalsMetadata(one[prop], other[prop])) {
                    return false;
                }
            }
        }
        return true;
    }
})(NotebookCell = exports.NotebookCell || (exports.NotebookCell = {}));
var NotebookDocument;
(function (NotebookDocument) {
    function create(uri, notebookType, version, cells) {
        return { uri, notebookType, version, cells };
    }
    NotebookDocument.create = create;
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && Is.string(candidate.uri) && vscode_languageserver_types_1.integer.is(candidate.version) && Is.typedArray(candidate.cells, NotebookCell.is);
    }
    NotebookDocument.is = is;
})(NotebookDocument = exports.NotebookDocument || (exports.NotebookDocument = {}));
var NotebookDocumentSyncRegistrationType;
(function (NotebookDocumentSyncRegistrationType) {
    NotebookDocumentSyncRegistrationType.method = 'notebookDocument/sync';
    NotebookDocumentSyncRegistrationType.type = new messages_1.RegistrationType(NotebookDocumentSyncRegistrationType.method);
})(NotebookDocumentSyncRegistrationType = exports.NotebookDocumentSyncRegistrationType || (exports.NotebookDocumentSyncRegistrationType = {}));
/**
 * A notification sent when a notebook opens.
 *
 * @since 3.17.0
 */
var DidOpenNotebookDocumentNotification;
(function (DidOpenNotebookDocumentNotification) {
    DidOpenNotebookDocumentNotification.method = 'notebookDocument/didOpen';
    DidOpenNotebookDocumentNotification.type = new messages_1.ProtocolNotificationType(DidOpenNotebookDocumentNotification.method);
})(DidOpenNotebookDocumentNotification = exports.DidOpenNotebookDocumentNotification || (exports.DidOpenNotebookDocumentNotification = {}));
var NotebookCellArrayChange;
(function (NotebookCellArrayChange) {
    function is(value) {
        const candidate = value;
        return Is.objectLiteral(candidate) && vscode_languageserver_types_1.uinteger.is(candidate.start) && vscode_languageserver_types_1.uinteger.is(candidate.deleteCount) && (candidate.cells === undefined || Is.typedArray(candidate.cells, NotebookCell.is));
    }
    NotebookCellArrayChange.is = is;
    function create(start, deleteCount, cells) {
        const result = { start, deleteCount };
        if (cells !== undefined) {
            result.cells = cells;
        }
        return result;
    }
    NotebookCellArrayChange.create = create;
})(NotebookCellArrayChange = exports.NotebookCellArrayChange || (exports.NotebookCellArrayChange = {}));
var DidChangeNotebookDocumentNotification;
(function (DidChangeNotebookDocumentNotification) {
    DidChangeNotebookDocumentNotification.method = 'notebookDocument/didChange';
    DidChangeNotebookDocumentNotification.type = new messages_1.ProtocolNotificationType(DidChangeNotebookDocumentNotification.method);
})(DidChangeNotebookDocumentNotification = exports.DidChangeNotebookDocumentNotification || (exports.DidChangeNotebookDocumentNotification = {}));
/**
 * A notification sent when a notebook document is saved.
 *
 * @since 3.17.0
 */
var DidSaveNotebookDocumentNotification;
(function (DidSaveNotebookDocumentNotification) {
    DidSaveNotebookDocumentNotification.method = 'notebookDocument/didSave';
    DidSaveNotebookDocumentNotification.type = new messages_1.ProtocolNotificationType(DidSaveNotebookDocumentNotification.method);
})(DidSaveNotebookDocumentNotification = exports.DidSaveNotebookDocumentNotification || (exports.DidSaveNotebookDocumentNotification = {}));
/**
 * A notification sent when a notebook closes.
 *
 * @since 3.17.0
 */
var DidCloseNotebookDocumentNotification;
(function (DidCloseNotebookDocumentNotification) {
    DidCloseNotebookDocumentNotification.method = 'notebookDocument/didClose';
    DidCloseNotebookDocumentNotification.type = new messages_1.ProtocolNotificationType(DidCloseNotebookDocumentNotification.method);
})(DidCloseNotebookDocumentNotification = exports.DidCloseNotebookDocumentNotification || (exports.DidCloseNotebookDocumentNotification = {}));
//# sourceMappingURL=protocol.notebook.js.map

/***/ }),
/* 102 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createProtocolConnection = void 0;
const vscode_jsonrpc_1 = __webpack_require__(58);
function createProtocolConnection(input, output, logger, options) {
    if (vscode_jsonrpc_1.ConnectionStrategy.is(options)) {
        options = { connectionStrategy: options };
    }
    return (0, vscode_jsonrpc_1.createMessageConnection)(input, output, logger, options);
}
exports.createProtocolConnection = createProtocolConnection;
//# sourceMappingURL=connection.js.map

/***/ }),
/* 103 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createConverter = void 0;
const code = __webpack_require__(53);
const proto = __webpack_require__(56);
const Is = __webpack_require__(54);
const async = __webpack_require__(104);
const protocolCompletionItem_1 = __webpack_require__(105);
const protocolCodeLens_1 = __webpack_require__(106);
const protocolDocumentLink_1 = __webpack_require__(107);
const protocolCodeAction_1 = __webpack_require__(108);
const protocolDiagnostic_1 = __webpack_require__(109);
const protocolCallHierarchyItem_1 = __webpack_require__(110);
const protocolTypeHierarchyItem_1 = __webpack_require__(111);
const protocolWorkspaceSymbol_1 = __webpack_require__(112);
const protocolInlayHint_1 = __webpack_require__(113);
var InsertReplaceRange;
(function (InsertReplaceRange) {
    function is(value) {
        const candidate = value;
        return candidate && !!candidate.inserting && !!candidate.replacing;
    }
    InsertReplaceRange.is = is;
})(InsertReplaceRange || (InsertReplaceRange = {}));
function createConverter(uriConverter) {
    const nullConverter = (value) => value.toString();
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri)
        };
    }
    function asTextDocumentItem(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri),
            languageId: textDocument.languageId,
            version: textDocument.version,
            text: textDocument.getText()
        };
    }
    function asVersionedTextDocumentIdentifier(textDocument) {
        return {
            uri: _uriConverter(textDocument.uri),
            version: textDocument.version
        };
    }
    function asOpenTextDocumentParams(textDocument) {
        return {
            textDocument: asTextDocumentItem(textDocument)
        };
    }
    function isTextDocumentChangeEvent(value) {
        let candidate = value;
        return !!candidate.document && !!candidate.contentChanges;
    }
    function isTextDocument(value) {
        let candidate = value;
        return !!candidate.uri && !!candidate.version;
    }
    function asChangeTextDocumentParams(arg) {
        if (isTextDocument(arg)) {
            let result = {
                textDocument: {
                    uri: _uriConverter(arg.uri),
                    version: arg.version
                },
                contentChanges: [{ text: arg.getText() }]
            };
            return result;
        }
        else if (isTextDocumentChangeEvent(arg)) {
            let document = arg.document;
            let result = {
                textDocument: {
                    uri: _uriConverter(document.uri),
                    version: document.version
                },
                contentChanges: arg.contentChanges.map((change) => {
                    let range = change.range;
                    return {
                        range: {
                            start: { line: range.start.line, character: range.start.character },
                            end: { line: range.end.line, character: range.end.character }
                        },
                        rangeLength: change.rangeLength,
                        text: change.text
                    };
                })
            };
            return result;
        }
        else {
            throw Error('Unsupported text document change parameter');
        }
    }
    function asCloseTextDocumentParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asSaveTextDocumentParams(textDocument, includeContent = false) {
        let result = {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
        if (includeContent) {
            result.text = textDocument.getText();
        }
        return result;
    }
    function asTextDocumentSaveReason(reason) {
        switch (reason) {
            case code.TextDocumentSaveReason.Manual:
                return proto.TextDocumentSaveReason.Manual;
            case code.TextDocumentSaveReason.AfterDelay:
                return proto.TextDocumentSaveReason.AfterDelay;
            case code.TextDocumentSaveReason.FocusOut:
                return proto.TextDocumentSaveReason.FocusOut;
        }
        return proto.TextDocumentSaveReason.Manual;
    }
    function asWillSaveTextDocumentParams(event) {
        return {
            textDocument: asTextDocumentIdentifier(event.document),
            reason: asTextDocumentSaveReason(event.reason)
        };
    }
    function asDidCreateFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asDidRenameFilesParams(event) {
        return {
            files: event.files.map((file) => ({
                oldUri: _uriConverter(file.oldUri),
                newUri: _uriConverter(file.newUri),
            })),
        };
    }
    function asDidDeleteFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asWillCreateFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asWillRenameFilesParams(event) {
        return {
            files: event.files.map((file) => ({
                oldUri: _uriConverter(file.oldUri),
                newUri: _uriConverter(file.newUri),
            })),
        };
    }
    function asWillDeleteFilesParams(event) {
        return {
            files: event.files.map((fileUri) => ({
                uri: _uriConverter(fileUri),
            })),
        };
    }
    function asTextDocumentPositionParams(textDocument, position) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position)
        };
    }
    function asCompletionTriggerKind(triggerKind) {
        switch (triggerKind) {
            case code.CompletionTriggerKind.TriggerCharacter:
                return proto.CompletionTriggerKind.TriggerCharacter;
            case code.CompletionTriggerKind.TriggerForIncompleteCompletions:
                return proto.CompletionTriggerKind.TriggerForIncompleteCompletions;
            default:
                return proto.CompletionTriggerKind.Invoked;
        }
    }
    function asCompletionParams(textDocument, position, context) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: {
                triggerKind: asCompletionTriggerKind(context.triggerKind),
                triggerCharacter: context.triggerCharacter
            }
        };
    }
    function asSignatureHelpTriggerKind(triggerKind) {
        switch (triggerKind) {
            case code.SignatureHelpTriggerKind.Invoke:
                return proto.SignatureHelpTriggerKind.Invoked;
            case code.SignatureHelpTriggerKind.TriggerCharacter:
                return proto.SignatureHelpTriggerKind.TriggerCharacter;
            case code.SignatureHelpTriggerKind.ContentChange:
                return proto.SignatureHelpTriggerKind.ContentChange;
        }
    }
    function asParameterInformation(value) {
        // We leave the documentation out on purpose since it usually adds no
        // value for the server.
        return {
            label: value.label
        };
    }
    function asParameterInformations(values) {
        return values.map(asParameterInformation);
    }
    function asSignatureInformation(value) {
        // We leave the documentation out on purpose since it usually adds no
        // value for the server.
        return {
            label: value.label,
            parameters: asParameterInformations(value.parameters)
        };
    }
    function asSignatureInformations(values) {
        return values.map(asSignatureInformation);
    }
    function asSignatureHelp(value) {
        if (value === undefined) {
            return value;
        }
        return {
            signatures: asSignatureInformations(value.signatures),
            activeSignature: value.activeSignature,
            activeParameter: value.activeParameter
        };
    }
    function asSignatureHelpParams(textDocument, position, context) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: {
                isRetrigger: context.isRetrigger,
                triggerCharacter: context.triggerCharacter,
                triggerKind: asSignatureHelpTriggerKind(context.triggerKind),
                activeSignatureHelp: asSignatureHelp(context.activeSignatureHelp)
            }
        };
    }
    function asWorkerPosition(position) {
        return { line: position.line, character: position.character };
    }
    function asPosition(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return { line: value.line > proto.uinteger.MAX_VALUE ? proto.uinteger.MAX_VALUE : value.line, character: value.character > proto.uinteger.MAX_VALUE ? proto.uinteger.MAX_VALUE : value.character };
    }
    function asPositions(value, token) {
        return async.map(value, asPosition, token);
    }
    function asRange(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return { start: asPosition(value.start), end: asPosition(value.end) };
    }
    function asLocation(value) {
        if (value === undefined || value === null) {
            return value;
        }
        return proto.Location.create(asUri(value.uri), asRange(value.range));
    }
    function asDiagnosticSeverity(value) {
        switch (value) {
            case code.DiagnosticSeverity.Error:
                return proto.DiagnosticSeverity.Error;
            case code.DiagnosticSeverity.Warning:
                return proto.DiagnosticSeverity.Warning;
            case code.DiagnosticSeverity.Information:
                return proto.DiagnosticSeverity.Information;
            case code.DiagnosticSeverity.Hint:
                return proto.DiagnosticSeverity.Hint;
        }
    }
    function asDiagnosticTags(tags) {
        if (!tags) {
            return undefined;
        }
        let result = [];
        for (let tag of tags) {
            let converted = asDiagnosticTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length > 0 ? result : undefined;
    }
    function asDiagnosticTag(tag) {
        switch (tag) {
            case code.DiagnosticTag.Unnecessary:
                return proto.DiagnosticTag.Unnecessary;
            case code.DiagnosticTag.Deprecated:
                return proto.DiagnosticTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asRelatedInformation(item) {
        return {
            message: item.message,
            location: asLocation(item.location)
        };
    }
    function asRelatedInformations(items) {
        return items.map(asRelatedInformation);
    }
    function asDiagnosticCode(value) {
        if (value === undefined || value === null) {
            return undefined;
        }
        if (Is.number(value) || Is.string(value)) {
            return value;
        }
        return { value: value.value, target: asUri(value.target) };
    }
    function asDiagnostic(item) {
        const result = proto.Diagnostic.create(asRange(item.range), item.message);
        const protocolDiagnostic = item instanceof protocolDiagnostic_1.ProtocolDiagnostic ? item : undefined;
        if (protocolDiagnostic !== undefined && protocolDiagnostic.data !== undefined) {
            result.data = protocolDiagnostic.data;
        }
        const code = asDiagnosticCode(item.code);
        if (protocolDiagnostic_1.DiagnosticCode.is(code)) {
            if (protocolDiagnostic !== undefined && protocolDiagnostic.hasDiagnosticCode) {
                result.code = code;
            }
            else {
                result.code = code.value;
                result.codeDescription = { href: code.target };
            }
        }
        else {
            result.code = code;
        }
        if (Is.number(item.severity)) {
            result.severity = asDiagnosticSeverity(item.severity);
        }
        if (Array.isArray(item.tags)) {
            result.tags = asDiagnosticTags(item.tags);
        }
        if (item.relatedInformation) {
            result.relatedInformation = asRelatedInformations(item.relatedInformation);
        }
        if (item.source) {
            result.source = item.source;
        }
        return result;
    }
    function asDiagnostics(items, token) {
        if (items === undefined || items === null) {
            return items;
        }
        return async.map(items, asDiagnostic, token);
    }
    function asDocumentation(format, documentation) {
        switch (format) {
            case '$string':
                return documentation;
            case proto.MarkupKind.PlainText:
                return { kind: format, value: documentation };
            case proto.MarkupKind.Markdown:
                return { kind: format, value: documentation.value };
            default:
                return `Unsupported Markup content received. Kind is: ${format}`;
        }
    }
    function asCompletionItemTag(tag) {
        switch (tag) {
            case code.CompletionItemTag.Deprecated:
                return proto.CompletionItemTag.Deprecated;
        }
        return undefined;
    }
    function asCompletionItemTags(tags) {
        if (tags === undefined) {
            return tags;
        }
        const result = [];
        for (let tag of tags) {
            const converted = asCompletionItemTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result;
    }
    function asCompletionItemKind(value, original) {
        if (original !== undefined) {
            return original;
        }
        return value + 1;
    }
    function asCompletionItem(item, labelDetailsSupport = false) {
        let label;
        let labelDetails;
        if (Is.string(item.label)) {
            label = item.label;
        }
        else {
            label = item.label.label;
            if (labelDetailsSupport && (item.label.detail !== undefined || item.label.description !== undefined)) {
                labelDetails = { detail: item.label.detail, description: item.label.description };
            }
        }
        let result = { label: label };
        if (labelDetails !== undefined) {
            result.labelDetails = labelDetails;
        }
        let protocolItem = item instanceof protocolCompletionItem_1.default ? item : undefined;
        if (item.detail) {
            result.detail = item.detail;
        }
        // We only send items back we created. So this can't be something else than
        // a string right now.
        if (item.documentation) {
            if (!protocolItem || protocolItem.documentationFormat === '$string') {
                result.documentation = item.documentation;
            }
            else {
                result.documentation = asDocumentation(protocolItem.documentationFormat, item.documentation);
            }
        }
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        fillPrimaryInsertText(result, item);
        if (Is.number(item.kind)) {
            result.kind = asCompletionItemKind(item.kind, protocolItem && protocolItem.originalItemKind);
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEdits(item.additionalTextEdits);
        }
        if (item.commitCharacters) {
            result.commitCharacters = item.commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        const tags = asCompletionItemTags(item.tags);
        if (protocolItem) {
            if (protocolItem.data !== undefined) {
                result.data = protocolItem.data;
            }
            if (protocolItem.deprecated === true || protocolItem.deprecated === false) {
                if (protocolItem.deprecated === true && tags !== undefined && tags.length > 0) {
                    const index = tags.indexOf(code.CompletionItemTag.Deprecated);
                    if (index !== -1) {
                        tags.splice(index, 1);
                    }
                }
                result.deprecated = protocolItem.deprecated;
            }
            if (protocolItem.insertTextMode !== undefined) {
                result.insertTextMode = protocolItem.insertTextMode;
            }
        }
        if (tags !== undefined && tags.length > 0) {
            result.tags = tags;
        }
        if (result.insertTextMode === undefined && item.keepWhitespace === true) {
            result.insertTextMode = proto.InsertTextMode.adjustIndentation;
        }
        return result;
    }
    function fillPrimaryInsertText(target, source) {
        let format = proto.InsertTextFormat.PlainText;
        let text = undefined;
        let range = undefined;
        if (source.textEdit) {
            text = source.textEdit.newText;
            range = source.textEdit.range;
        }
        else if (source.insertText instanceof code.SnippetString) {
            format = proto.InsertTextFormat.Snippet;
            text = source.insertText.value;
        }
        else {
            text = source.insertText;
        }
        if (source.range) {
            range = source.range;
        }
        target.insertTextFormat = format;
        if (source.fromEdit && text !== undefined && range !== undefined) {
            target.textEdit = asCompletionTextEdit(text, range);
        }
        else {
            target.insertText = text;
        }
    }
    function asCompletionTextEdit(newText, range) {
        if (InsertReplaceRange.is(range)) {
            return proto.InsertReplaceEdit.create(newText, asRange(range.inserting), asRange(range.replacing));
        }
        else {
            return { newText, range: asRange(range) };
        }
    }
    function asTextEdit(edit) {
        return { range: asRange(edit.range), newText: edit.newText };
    }
    function asTextEdits(edits) {
        if (edits === undefined || edits === null) {
            return edits;
        }
        return edits.map(asTextEdit);
    }
    function asSymbolKind(item) {
        if (item <= code.SymbolKind.TypeParameter) {
            // Symbol kind is one based in the protocol and zero based in code.
            return (item + 1);
        }
        return proto.SymbolKind.Property;
    }
    function asSymbolTag(item) {
        return item;
    }
    function asSymbolTags(items) {
        return items.map(asSymbolTag);
    }
    function asReferenceParams(textDocument, position, options) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument),
            position: asWorkerPosition(position),
            context: { includeDeclaration: options.includeDeclaration }
        };
    }
    async function asCodeAction(item, token) {
        let result = proto.CodeAction.create(item.title);
        if (item instanceof protocolCodeAction_1.default && item.data !== undefined) {
            result.data = item.data;
        }
        if (item.kind !== undefined) {
            result.kind = asCodeActionKind(item.kind);
        }
        if (item.diagnostics !== undefined) {
            result.diagnostics = await asDiagnostics(item.diagnostics, token);
        }
        if (item.edit !== undefined) {
            throw new Error(`VS Code code actions can only be converted to a protocol code action without an edit.`);
        }
        if (item.command !== undefined) {
            result.command = asCommand(item.command);
        }
        if (item.isPreferred !== undefined) {
            result.isPreferred = item.isPreferred;
        }
        if (item.disabled !== undefined) {
            result.disabled = { reason: item.disabled.reason };
        }
        return result;
    }
    async function asCodeActionContext(context, token) {
        if (context === undefined || context === null) {
            return context;
        }
        let only;
        if (context.only && Is.string(context.only.value)) {
            only = [context.only.value];
        }
        return proto.CodeActionContext.create(await asDiagnostics(context.diagnostics, token), only, asCodeActionTriggerKind(context.triggerKind));
    }
    function asCodeActionTriggerKind(kind) {
        switch (kind) {
            case code.CodeActionTriggerKind.Invoke:
                return proto.CodeActionTriggerKind.Invoked;
            case code.CodeActionTriggerKind.Automatic:
                return proto.CodeActionTriggerKind.Automatic;
            default:
                return undefined;
        }
    }
    function asCodeActionKind(item) {
        if (item === undefined || item === null) {
            return undefined;
        }
        return item.value;
    }
    function asInlineValueContext(context) {
        if (context === undefined || context === null) {
            return context;
        }
        return proto.InlineValueContext.create(context.frameId, asRange(context.stoppedLocation));
    }
    function asCommand(item) {
        let result = proto.Command.create(item.title, item.command);
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    function asCodeLens(item) {
        let result = proto.CodeLens.create(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item instanceof protocolCodeLens_1.default) {
            if (item.data) {
                result.data = item.data;
            }
        }
        return result;
    }
    function asFormattingOptions(options, fileOptions) {
        const result = { tabSize: options.tabSize, insertSpaces: options.insertSpaces };
        if (fileOptions.trimTrailingWhitespace) {
            result.trimTrailingWhitespace = true;
        }
        if (fileOptions.trimFinalNewlines) {
            result.trimFinalNewlines = true;
        }
        if (fileOptions.insertFinalNewline) {
            result.insertFinalNewline = true;
        }
        return result;
    }
    function asDocumentSymbolParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asCodeLensParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asDocumentLink(item) {
        let result = proto.DocumentLink.create(asRange(item.range));
        if (item.target) {
            result.target = asUri(item.target);
        }
        if (item.tooltip !== undefined) {
            result.tooltip = item.tooltip;
        }
        let protocolItem = item instanceof protocolDocumentLink_1.default ? item : undefined;
        if (protocolItem && protocolItem.data) {
            result.data = protocolItem.data;
        }
        return result;
    }
    function asDocumentLinkParams(textDocument) {
        return {
            textDocument: asTextDocumentIdentifier(textDocument)
        };
    }
    function asCallHierarchyItem(value) {
        const result = {
            name: value.name,
            kind: asSymbolKind(value.kind),
            uri: asUri(value.uri),
            range: asRange(value.range),
            selectionRange: asRange(value.selectionRange)
        };
        if (value.detail !== undefined && value.detail.length > 0) {
            result.detail = value.detail;
        }
        if (value.tags !== undefined) {
            result.tags = asSymbolTags(value.tags);
        }
        if (value instanceof protocolCallHierarchyItem_1.default && value.data !== undefined) {
            result.data = value.data;
        }
        return result;
    }
    function asTypeHierarchyItem(value) {
        const result = {
            name: value.name,
            kind: asSymbolKind(value.kind),
            uri: asUri(value.uri),
            range: asRange(value.range),
            selectionRange: asRange(value.selectionRange),
        };
        if (value.detail !== undefined && value.detail.length > 0) {
            result.detail = value.detail;
        }
        if (value.tags !== undefined) {
            result.tags = asSymbolTags(value.tags);
        }
        if (value instanceof protocolTypeHierarchyItem_1.default && value.data !== undefined) {
            result.data = value.data;
        }
        return result;
    }
    function asWorkspaceSymbol(item) {
        const result = item instanceof protocolWorkspaceSymbol_1.default
            ? { name: item.name, kind: asSymbolKind(item.kind), location: item.hasRange ? asLocation(item.location) : { uri: _uriConverter(item.location.uri) }, data: item.data }
            : { name: item.name, kind: asSymbolKind(item.kind), location: asLocation(item.location) };
        if (item.tags !== undefined) {
            result.tags = asSymbolTags(item.tags);
        }
        if (item.containerName !== '') {
            result.containerName = item.containerName;
        }
        return result;
    }
    function asInlayHint(item) {
        const label = typeof item.label === 'string'
            ? item.label
            : item.label.map(asInlayHintLabelPart);
        const result = proto.InlayHint.create(asPosition(item.position), label);
        if (item.kind !== undefined) {
            result.kind = item.kind;
        }
        if (item.textEdits !== undefined) {
            result.textEdits = asTextEdits(item.textEdits);
        }
        if (item.tooltip !== undefined) {
            result.tooltip = asTooltip(item.tooltip);
        }
        if (item.paddingLeft !== undefined) {
            result.paddingLeft = item.paddingLeft;
        }
        if (item.paddingRight !== undefined) {
            result.paddingRight = item.paddingRight;
        }
        if (item instanceof protocolInlayHint_1.default && item.data !== undefined) {
            result.data = item.data;
        }
        return result;
    }
    function asInlayHintLabelPart(item) {
        const result = proto.InlayHintLabelPart.create(item.value);
        if (item.location !== undefined) {
            result.location = asLocation(item.location);
        }
        if (item.command !== undefined) {
            result.command = asCommand(item.command);
        }
        if (item.tooltip !== undefined) {
            result.tooltip = asTooltip(item.tooltip);
        }
        return result;
    }
    function asTooltip(value) {
        if (typeof value === 'string') {
            return value;
        }
        const result = {
            kind: proto.MarkupKind.Markdown,
            value: value.value
        };
        return result;
    }
    return {
        asUri,
        asTextDocumentIdentifier,
        asTextDocumentItem,
        asVersionedTextDocumentIdentifier,
        asOpenTextDocumentParams,
        asChangeTextDocumentParams,
        asCloseTextDocumentParams,
        asSaveTextDocumentParams,
        asWillSaveTextDocumentParams,
        asDidCreateFilesParams,
        asDidRenameFilesParams,
        asDidDeleteFilesParams,
        asWillCreateFilesParams,
        asWillRenameFilesParams,
        asWillDeleteFilesParams,
        asTextDocumentPositionParams,
        asCompletionParams,
        asSignatureHelpParams,
        asWorkerPosition,
        asRange,
        asPosition,
        asPositions,
        asLocation,
        asDiagnosticSeverity,
        asDiagnosticTag,
        asDiagnostic,
        asDiagnostics,
        asCompletionItem,
        asTextEdit,
        asSymbolKind,
        asSymbolTag,
        asSymbolTags,
        asReferenceParams,
        asCodeAction,
        asCodeActionContext,
        asInlineValueContext,
        asCommand,
        asCodeLens,
        asFormattingOptions,
        asDocumentSymbolParams,
        asCodeLensParams,
        asDocumentLink,
        asDocumentLinkParams,
        asCallHierarchyItem,
        asTypeHierarchyItem,
        asInlayHint,
        asWorkspaceSymbol
    };
}
exports.createConverter = createConverter;
//# sourceMappingURL=codeConverter.js.map

/***/ }),
/* 104 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.forEach = exports.mapAsync = exports.map = exports.Semaphore = exports.Delayer = void 0;
const vscode_languageserver_protocol_1 = __webpack_require__(56);
class Delayer {
    constructor(defaultDelay) {
        this.defaultDelay = defaultDelay;
        this.timeout = undefined;
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
    }
    trigger(task, delay = this.defaultDelay) {
        this.task = task;
        if (delay >= 0) {
            this.cancelTimeout();
        }
        if (!this.completionPromise) {
            this.completionPromise = new Promise((resolve) => {
                this.onSuccess = resolve;
            }).then(() => {
                this.completionPromise = undefined;
                this.onSuccess = undefined;
                var result = this.task();
                this.task = undefined;
                return result;
            });
        }
        if (delay >= 0 || this.timeout === void 0) {
            this.timeout = (0, vscode_languageserver_protocol_1.RAL)().timer.setTimeout(() => {
                this.timeout = undefined;
                this.onSuccess(undefined);
            }, delay >= 0 ? delay : this.defaultDelay);
        }
        return this.completionPromise;
    }
    forceDelivery() {
        if (!this.completionPromise) {
            return undefined;
        }
        this.cancelTimeout();
        let result = this.task();
        this.completionPromise = undefined;
        this.onSuccess = undefined;
        this.task = undefined;
        return result;
    }
    isTriggered() {
        return this.timeout !== undefined;
    }
    cancel() {
        this.cancelTimeout();
        this.completionPromise = undefined;
    }
    cancelTimeout() {
        if (this.timeout !== undefined) {
            this.timeout.dispose();
            this.timeout = undefined;
        }
    }
}
exports.Delayer = Delayer;
class Semaphore {
    constructor(capacity = 1) {
        if (capacity <= 0) {
            throw new Error('Capacity must be greater than 0');
        }
        this._capacity = capacity;
        this._active = 0;
        this._waiting = [];
    }
    lock(thunk) {
        return new Promise((resolve, reject) => {
            this._waiting.push({ thunk, resolve, reject });
            this.runNext();
        });
    }
    get active() {
        return this._active;
    }
    runNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        (0, vscode_languageserver_protocol_1.RAL)().timer.setImmediate(() => this.doRunNext());
    }
    doRunNext() {
        if (this._waiting.length === 0 || this._active === this._capacity) {
            return;
        }
        const next = this._waiting.shift();
        this._active++;
        if (this._active > this._capacity) {
            throw new Error(`To many thunks active`);
        }
        try {
            const result = next.thunk();
            if (result instanceof Promise) {
                result.then((value) => {
                    this._active--;
                    next.resolve(value);
                    this.runNext();
                }, (err) => {
                    this._active--;
                    next.reject(err);
                    this.runNext();
                });
            }
            else {
                this._active--;
                next.resolve(result);
                this.runNext();
            }
        }
        catch (err) {
            this._active--;
            next.reject(err);
            this.runNext();
        }
    }
}
exports.Semaphore = Semaphore;
const defaultYieldTimeout = 15 /*ms*/;
class Timer {
    constructor(yieldAfter = defaultYieldTimeout) {
        this.yieldAfter = Math.max(yieldAfter, defaultYieldTimeout);
        this.startTime = Date.now();
        this.counter = 0;
        this.total = 0;
        // start with a counter interval of 1.
        this.counterInterval = 1;
    }
    start() {
        this.startTime = Date.now();
    }
    shouldYield() {
        if (++this.counter >= this.counterInterval) {
            const timeTaken = Date.now() - this.startTime;
            const timeLeft = Math.max(0, this.yieldAfter - timeTaken);
            this.total += this.counter;
            this.counter = 0;
            if (timeTaken >= this.yieldAfter || timeLeft <= 1) {
                // Yield also if time left <= 1 since we compute the counter
                // for max < 2 ms.
                // Start with interval 1 again. We could do some calculation
                // with using 80% of the last counter however other things (GC)
                // affect the timing heavily since we have small timings (1 - 15ms).
                this.counterInterval = 1;
                this.total = 0;
                return true;
            }
            else {
                // Only increase the counter until we have spent <= 2 ms. Increasing
                // the counter further is very fragile since timing is influenced
                // by other things and can increase the counter too much. This will result
                // that we yield in average after [14 - 16]ms.
                switch (timeTaken) {
                    case 0:
                    case 1:
                        this.counterInterval = this.total * 2;
                        break;
                }
            }
        }
        return false;
    }
}
async function map(items, func, token, options) {
    if (items.length === 0) {
        return [];
    }
    const result = new Array(items.length);
    const timer = new Timer(options?.yieldAfter);
    function convertBatch(start) {
        timer.start();
        for (let i = start; i < items.length; i++) {
            result[i] = func(items[i]);
            if (timer.shouldYield()) {
                options?.yieldCallback && options.yieldCallback();
                return i + 1;
            }
        }
        return -1;
    }
    // Convert the first batch sync on the same frame.
    let index = convertBatch(0);
    while (index !== -1) {
        if (token !== undefined && token.isCancellationRequested) {
            break;
        }
        index = await new Promise((resolve) => {
            (0, vscode_languageserver_protocol_1.RAL)().timer.setImmediate(() => {
                resolve(convertBatch(index));
            });
        });
    }
    return result;
}
exports.map = map;
async function mapAsync(items, func, token, options) {
    if (items.length === 0) {
        return [];
    }
    const result = new Array(items.length);
    const timer = new Timer(options?.yieldAfter);
    async function convertBatch(start) {
        timer.start();
        for (let i = start; i < items.length; i++) {
            result[i] = await func(items[i], token);
            if (timer.shouldYield()) {
                options?.yieldCallback && options.yieldCallback();
                return i + 1;
            }
        }
        return -1;
    }
    let index = await convertBatch(0);
    while (index !== -1) {
        if (token !== undefined && token.isCancellationRequested) {
            break;
        }
        index = await new Promise((resolve) => {
            (0, vscode_languageserver_protocol_1.RAL)().timer.setImmediate(() => {
                resolve(convertBatch(index));
            });
        });
    }
    return result;
}
exports.mapAsync = mapAsync;
async function forEach(items, func, token, options) {
    if (items.length === 0) {
        return;
    }
    const timer = new Timer(options?.yieldAfter);
    function runBatch(start) {
        timer.start();
        for (let i = start; i < items.length; i++) {
            func(items[i]);
            if (timer.shouldYield()) {
                options?.yieldCallback && options.yieldCallback();
                return i + 1;
            }
        }
        return -1;
    }
    // Convert the first batch sync on the same frame.
    let index = runBatch(0);
    while (index !== -1) {
        if (token !== undefined && token.isCancellationRequested) {
            break;
        }
        index = await new Promise((resolve) => {
            (0, vscode_languageserver_protocol_1.RAL)().timer.setImmediate(() => {
                resolve(runBatch(index));
            });
        });
    }
}
exports.forEach = forEach;
//# sourceMappingURL=async.js.map

/***/ }),
/* 105 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolCompletionItem extends code.CompletionItem {
    constructor(label) {
        super(label);
    }
}
exports["default"] = ProtocolCompletionItem;
//# sourceMappingURL=protocolCompletionItem.js.map

/***/ }),
/* 106 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolCodeLens extends code.CodeLens {
    constructor(range) {
        super(range);
    }
}
exports["default"] = ProtocolCodeLens;
//# sourceMappingURL=protocolCodeLens.js.map

/***/ }),
/* 107 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolDocumentLink extends code.DocumentLink {
    constructor(range, target) {
        super(range, target);
    }
}
exports["default"] = ProtocolDocumentLink;
//# sourceMappingURL=protocolDocumentLink.js.map

/***/ }),
/* 108 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const vscode = __webpack_require__(53);
class ProtocolCodeAction extends vscode.CodeAction {
    constructor(title, data) {
        super(title);
        this.data = data;
    }
}
exports["default"] = ProtocolCodeAction;
//# sourceMappingURL=protocolCodeAction.js.map

/***/ }),
/* 109 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProtocolDiagnostic = exports.DiagnosticCode = void 0;
const vscode = __webpack_require__(53);
const Is = __webpack_require__(54);
var DiagnosticCode;
(function (DiagnosticCode) {
    function is(value) {
        const candidate = value;
        return candidate !== undefined && candidate !== null && (Is.number(candidate.value) || Is.string(candidate.value)) && Is.string(candidate.target);
    }
    DiagnosticCode.is = is;
})(DiagnosticCode = exports.DiagnosticCode || (exports.DiagnosticCode = {}));
class ProtocolDiagnostic extends vscode.Diagnostic {
    constructor(range, message, severity, data) {
        super(range, message, severity);
        this.data = data;
        this.hasDiagnosticCode = false;
    }
}
exports.ProtocolDiagnostic = ProtocolDiagnostic;
//# sourceMappingURL=protocolDiagnostic.js.map

/***/ }),
/* 110 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolCallHierarchyItem extends code.CallHierarchyItem {
    constructor(kind, name, detail, uri, range, selectionRange, data) {
        super(kind, name, detail, uri, range, selectionRange);
        if (data !== undefined) {
            this.data = data;
        }
    }
}
exports["default"] = ProtocolCallHierarchyItem;
//# sourceMappingURL=protocolCallHierarchyItem.js.map

/***/ }),
/* 111 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolTypeHierarchyItem extends code.TypeHierarchyItem {
    constructor(kind, name, detail, uri, range, selectionRange, data) {
        super(kind, name, detail, uri, range, selectionRange);
        if (data !== undefined) {
            this.data = data;
        }
    }
}
exports["default"] = ProtocolTypeHierarchyItem;
//# sourceMappingURL=protocolTypeHierarchyItem.js.map

/***/ }),
/* 112 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class WorkspaceSymbol extends code.SymbolInformation {
    constructor(name, kind, containerName, locationOrUri, data) {
        const hasRange = !(locationOrUri instanceof code.Uri);
        super(name, kind, containerName, hasRange ? locationOrUri : new code.Location(locationOrUri, new code.Range(0, 0, 0, 0)));
        this.hasRange = hasRange;
        if (data !== undefined) {
            this.data = data;
        }
    }
}
exports["default"] = WorkspaceSymbol;
//# sourceMappingURL=protocolWorkspaceSymbol.js.map

/***/ }),
/* 113 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
const code = __webpack_require__(53);
class ProtocolInlayHint extends code.InlayHint {
    constructor(position, label, kind) {
        super(position, label, kind);
    }
}
exports["default"] = ProtocolInlayHint;
//# sourceMappingURL=protocolInlayHint.js.map

/***/ }),
/* 114 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.createConverter = void 0;
const code = __webpack_require__(53);
const ls = __webpack_require__(56);
const Is = __webpack_require__(54);
const async = __webpack_require__(104);
const protocolCompletionItem_1 = __webpack_require__(105);
const protocolCodeLens_1 = __webpack_require__(106);
const protocolDocumentLink_1 = __webpack_require__(107);
const protocolCodeAction_1 = __webpack_require__(108);
const protocolDiagnostic_1 = __webpack_require__(109);
const protocolCallHierarchyItem_1 = __webpack_require__(110);
const protocolTypeHierarchyItem_1 = __webpack_require__(111);
const protocolWorkspaceSymbol_1 = __webpack_require__(112);
const protocolInlayHint_1 = __webpack_require__(113);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
var CodeBlock;
(function (CodeBlock) {
    function is(value) {
        let candidate = value;
        return candidate && Is.string(candidate.language) && Is.string(candidate.value);
    }
    CodeBlock.is = is;
})(CodeBlock || (CodeBlock = {}));
function createConverter(uriConverter, trustMarkdown, supportHtml) {
    const nullConverter = (value) => code.Uri.parse(value);
    const _uriConverter = uriConverter || nullConverter;
    function asUri(value) {
        return _uriConverter(value);
    }
    function asDocumentSelector(selector) {
        const result = [];
        for (const filter of selector) {
            if (typeof filter === 'string') {
                result.push(filter);
            }
            else if (vscode_languageserver_protocol_1.NotebookCellTextDocumentFilter.is(filter)) {
                // We first need to check for the notebook cell filter since a TextDocumentFilter would
                // match both (e.g. the notebook is optional).
                if (typeof filter.notebook === 'string') {
                    result.push({ notebookType: filter.notebook, language: filter.language });
                }
                else {
                    const notebookType = filter.notebook.notebookType ?? '*';
                    result.push({ notebookType: notebookType, scheme: filter.notebook.scheme, pattern: filter.notebook.pattern, language: filter.language });
                }
            }
            else if (vscode_languageserver_protocol_1.TextDocumentFilter.is(filter)) {
                result.push({ language: filter.language, scheme: filter.scheme, pattern: filter.pattern });
            }
        }
        return result;
    }
    async function asDiagnostics(diagnostics, token) {
        return async.map(diagnostics, asDiagnostic, token);
    }
    function asDiagnosticsSync(diagnostics) {
        const result = new Array(diagnostics.length);
        for (let i = 0; i < diagnostics.length; i++) {
            result[i] = asDiagnostic(diagnostics[i]);
        }
        return result;
    }
    function asDiagnostic(diagnostic) {
        let result = new protocolDiagnostic_1.ProtocolDiagnostic(asRange(diagnostic.range), diagnostic.message, asDiagnosticSeverity(diagnostic.severity), diagnostic.data);
        if (diagnostic.code !== undefined) {
            if (typeof diagnostic.code === 'string' || typeof diagnostic.code === 'number') {
                if (ls.CodeDescription.is(diagnostic.codeDescription)) {
                    result.code = {
                        value: diagnostic.code,
                        target: asUri(diagnostic.codeDescription.href)
                    };
                }
                else {
                    result.code = diagnostic.code;
                }
            }
            else if (protocolDiagnostic_1.DiagnosticCode.is(diagnostic.code)) {
                // This is for backwards compatibility of a proposed API.
                // We should remove this at some point.
                result.hasDiagnosticCode = true;
                const diagnosticCode = diagnostic.code;
                result.code = {
                    value: diagnosticCode.value,
                    target: asUri(diagnosticCode.target)
                };
            }
        }
        if (diagnostic.source) {
            result.source = diagnostic.source;
        }
        if (diagnostic.relatedInformation) {
            result.relatedInformation = asRelatedInformation(diagnostic.relatedInformation);
        }
        if (Array.isArray(diagnostic.tags)) {
            result.tags = asDiagnosticTags(diagnostic.tags);
        }
        return result;
    }
    function asRelatedInformation(relatedInformation) {
        const result = new Array(relatedInformation.length);
        for (let i = 0; i < relatedInformation.length; i++) {
            const info = relatedInformation[i];
            result[i] = new code.DiagnosticRelatedInformation(asLocation(info.location), info.message);
        }
        return result;
    }
    function asDiagnosticTags(tags) {
        if (!tags) {
            return undefined;
        }
        let result = [];
        for (let tag of tags) {
            let converted = asDiagnosticTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length > 0 ? result : undefined;
    }
    function asDiagnosticTag(tag) {
        switch (tag) {
            case ls.DiagnosticTag.Unnecessary:
                return code.DiagnosticTag.Unnecessary;
            case ls.DiagnosticTag.Deprecated:
                return code.DiagnosticTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asPosition(value) {
        return value ? new code.Position(value.line, value.character) : undefined;
    }
    function asRange(value) {
        return value ? new code.Range(value.start.line, value.start.character, value.end.line, value.end.character) : undefined;
    }
    async function asRanges(items, token) {
        return async.map(items, (range) => {
            return new code.Range(range.start.line, range.start.character, range.end.line, range.end.character);
        }, token);
    }
    function asDiagnosticSeverity(value) {
        if (value === undefined || value === null) {
            return code.DiagnosticSeverity.Error;
        }
        switch (value) {
            case ls.DiagnosticSeverity.Error:
                return code.DiagnosticSeverity.Error;
            case ls.DiagnosticSeverity.Warning:
                return code.DiagnosticSeverity.Warning;
            case ls.DiagnosticSeverity.Information:
                return code.DiagnosticSeverity.Information;
            case ls.DiagnosticSeverity.Hint:
                return code.DiagnosticSeverity.Hint;
        }
        return code.DiagnosticSeverity.Error;
    }
    function asHoverContent(value) {
        if (Is.string(value)) {
            return asMarkdownString(value);
        }
        else if (CodeBlock.is(value)) {
            let result = asMarkdownString();
            return result.appendCodeblock(value.value, value.language);
        }
        else if (Array.isArray(value)) {
            let result = [];
            for (let element of value) {
                let item = asMarkdownString();
                if (CodeBlock.is(element)) {
                    item.appendCodeblock(element.value, element.language);
                }
                else {
                    item.appendMarkdown(element);
                }
                result.push(item);
            }
            return result;
        }
        else {
            return asMarkdownString(value);
        }
    }
    function asDocumentation(value) {
        if (Is.string(value)) {
            return value;
        }
        else {
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    return asMarkdownString(value.value);
                case ls.MarkupKind.PlainText:
                    return value.value;
                default:
                    return `Unsupported Markup content received. Kind is: ${value.kind}`;
            }
        }
    }
    function asMarkdownString(value) {
        let result;
        if (value === undefined || typeof value === 'string') {
            result = new code.MarkdownString(value);
        }
        else {
            switch (value.kind) {
                case ls.MarkupKind.Markdown:
                    result = new code.MarkdownString(value.value);
                    break;
                case ls.MarkupKind.PlainText:
                    result = new code.MarkdownString();
                    result.appendText(value.value);
                    break;
                default:
                    result = new code.MarkdownString();
                    result.appendText(`Unsupported Markup content received. Kind is: ${value.kind}`);
                    break;
            }
        }
        result.isTrusted = trustMarkdown;
        result.supportHtml = supportHtml;
        return result;
    }
    function asHover(hover) {
        if (!hover) {
            return undefined;
        }
        return new code.Hover(asHoverContent(hover.contents), asRange(hover.range));
    }
    async function asCompletionResult(value, allCommitCharacters, token) {
        if (!value) {
            return undefined;
        }
        if (Array.isArray(value)) {
            return async.map(value, (item) => asCompletionItem(item, allCommitCharacters), token);
        }
        const list = value;
        const { defaultRange, commitCharacters } = getCompletionItemDefaults(list, allCommitCharacters);
        const converted = await async.map(list.items, (item) => {
            return asCompletionItem(item, commitCharacters, defaultRange, list.itemDefaults?.insertTextMode, list.itemDefaults?.insertTextFormat, list.itemDefaults?.data);
        }, token);
        return new code.CompletionList(converted, list.isIncomplete);
    }
    function getCompletionItemDefaults(list, allCommitCharacters) {
        const rangeDefaults = list.itemDefaults?.editRange;
        const commitCharacters = list.itemDefaults?.commitCharacters ?? allCommitCharacters;
        return ls.Range.is(rangeDefaults)
            ? { defaultRange: asRange(rangeDefaults), commitCharacters }
            : rangeDefaults !== undefined
                ? { defaultRange: { inserting: asRange(rangeDefaults.insert), replacing: asRange(rangeDefaults.replace) }, commitCharacters }
                : { defaultRange: undefined, commitCharacters };
    }
    function asCompletionItemKind(value) {
        // Protocol item kind is 1 based, codes item kind is zero based.
        if (ls.CompletionItemKind.Text <= value && value <= ls.CompletionItemKind.TypeParameter) {
            return [value - 1, undefined];
        }
        return [code.CompletionItemKind.Text, value];
    }
    function asCompletionItemTag(tag) {
        switch (tag) {
            case ls.CompletionItemTag.Deprecated:
                return code.CompletionItemTag.Deprecated;
        }
        return undefined;
    }
    function asCompletionItemTags(tags) {
        if (tags === undefined || tags === null) {
            return [];
        }
        const result = [];
        for (const tag of tags) {
            const converted = asCompletionItemTag(tag);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result;
    }
    function asCompletionItem(item, defaultCommitCharacters, defaultRange, defaultInsertTextMode, defaultInsertTextFormat, defaultData) {
        const tags = asCompletionItemTags(item.tags);
        const label = asCompletionItemLabel(item);
        const result = new protocolCompletionItem_1.default(label);
        if (item.detail) {
            result.detail = item.detail;
        }
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
            result.documentationFormat = Is.string(item.documentation) ? '$string' : item.documentation.kind;
        }
        if (item.filterText) {
            result.filterText = item.filterText;
        }
        const insertText = asCompletionInsertText(item, defaultRange, defaultInsertTextFormat);
        if (insertText) {
            result.insertText = insertText.text;
            result.range = insertText.range;
            result.fromEdit = insertText.fromEdit;
        }
        if (Is.number(item.kind)) {
            let [itemKind, original] = asCompletionItemKind(item.kind);
            result.kind = itemKind;
            if (original) {
                result.originalItemKind = original;
            }
        }
        if (item.sortText) {
            result.sortText = item.sortText;
        }
        if (item.additionalTextEdits) {
            result.additionalTextEdits = asTextEditsSync(item.additionalTextEdits);
        }
        const commitCharacters = item.commitCharacters !== undefined
            ? Is.stringArray(item.commitCharacters) ? item.commitCharacters : undefined
            : defaultCommitCharacters;
        if (commitCharacters) {
            result.commitCharacters = commitCharacters.slice();
        }
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.deprecated === true || item.deprecated === false) {
            result.deprecated = item.deprecated;
            if (item.deprecated === true) {
                tags.push(code.CompletionItemTag.Deprecated);
            }
        }
        if (item.preselect === true || item.preselect === false) {
            result.preselect = item.preselect;
        }
        const data = item.data ?? defaultData;
        if (data !== undefined) {
            result.data = data;
        }
        if (tags.length > 0) {
            result.tags = tags;
        }
        const insertTextMode = item.insertTextMode ?? defaultInsertTextMode;
        if (insertTextMode !== undefined) {
            result.insertTextMode = insertTextMode;
            if (insertTextMode === ls.InsertTextMode.asIs) {
                result.keepWhitespace = true;
            }
        }
        return result;
    }
    function asCompletionItemLabel(item) {
        if (ls.CompletionItemLabelDetails.is(item.labelDetails)) {
            return {
                label: item.label,
                detail: item.labelDetails.detail,
                description: item.labelDetails.description
            };
        }
        else {
            return item.label;
        }
    }
    function asCompletionInsertText(item, defaultRange, defaultInsertTextFormat) {
        const insertTextFormat = item.insertTextFormat ?? defaultInsertTextFormat;
        if (item.textEdit !== undefined || defaultRange !== undefined) {
            const [range, newText] = item.textEdit !== undefined
                ? getCompletionRangeAndText(item.textEdit)
                : [defaultRange, item.textEditText ?? item.label];
            if (insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(newText), range: range, fromEdit: true };
            }
            else {
                return { text: newText, range: range, fromEdit: true };
            }
        }
        else if (item.insertText) {
            if (insertTextFormat === ls.InsertTextFormat.Snippet) {
                return { text: new code.SnippetString(item.insertText), fromEdit: false };
            }
            else {
                return { text: item.insertText, fromEdit: false };
            }
        }
        else {
            return undefined;
        }
    }
    function getCompletionRangeAndText(value) {
        if (ls.InsertReplaceEdit.is(value)) {
            return [{ inserting: asRange(value.insert), replacing: asRange(value.replace) }, value.newText];
        }
        else {
            return [asRange(value.range), value.newText];
        }
    }
    function asTextEdit(edit) {
        if (!edit) {
            return undefined;
        }
        return new code.TextEdit(asRange(edit.range), edit.newText);
    }
    async function asTextEdits(items, token) {
        if (!items) {
            return undefined;
        }
        return async.map(items, asTextEdit, token);
    }
    function asTextEditsSync(items) {
        if (!items) {
            return undefined;
        }
        const result = new Array(items.length);
        for (let i = 0; i < items.length; i++) {
            result[i] = asTextEdit(items[i]);
        }
        return result;
    }
    async function asSignatureHelp(item, token) {
        if (!item) {
            return undefined;
        }
        let result = new code.SignatureHelp();
        if (Is.number(item.activeSignature)) {
            result.activeSignature = item.activeSignature;
        }
        else {
            // activeSignature was optional in the past
            result.activeSignature = 0;
        }
        if (Is.number(item.activeParameter)) {
            result.activeParameter = item.activeParameter;
        }
        else {
            // activeParameter was optional in the past
            result.activeParameter = 0;
        }
        if (item.signatures) {
            result.signatures = await asSignatureInformations(item.signatures, token);
        }
        return result;
    }
    async function asSignatureInformations(items, token) {
        return async.mapAsync(items, asSignatureInformation, token);
    }
    async function asSignatureInformation(item, token) {
        let result = new code.SignatureInformation(item.label);
        if (item.documentation !== undefined) {
            result.documentation = asDocumentation(item.documentation);
        }
        if (item.parameters !== undefined) {
            result.parameters = await asParameterInformations(item.parameters, token);
        }
        if (item.activeParameter !== undefined) {
            result.activeParameter = item.activeParameter;
        }
        {
            return result;
        }
    }
    function asParameterInformations(items, token) {
        return async.map(items, asParameterInformation, token);
    }
    function asParameterInformation(item) {
        let result = new code.ParameterInformation(item.label);
        if (item.documentation) {
            result.documentation = asDocumentation(item.documentation);
        }
        return result;
    }
    function asLocation(item) {
        return item ? new code.Location(_uriConverter(item.uri), asRange(item.range)) : undefined;
    }
    async function asDeclarationResult(item, token) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item, token);
    }
    async function asDefinitionResult(item, token) {
        if (!item) {
            return undefined;
        }
        return asLocationResult(item, token);
    }
    function asLocationLink(item) {
        if (!item) {
            return undefined;
        }
        let result = {
            targetUri: _uriConverter(item.targetUri),
            targetRange: asRange(item.targetRange),
            originSelectionRange: asRange(item.originSelectionRange),
            targetSelectionRange: asRange(item.targetSelectionRange)
        };
        if (!result.targetSelectionRange) {
            throw new Error(`targetSelectionRange must not be undefined or null`);
        }
        return result;
    }
    async function asLocationResult(item, token) {
        if (!item) {
            return undefined;
        }
        if (Is.array(item)) {
            if (item.length === 0) {
                return [];
            }
            else if (ls.LocationLink.is(item[0])) {
                const links = item;
                return async.map(links, asLocationLink, token);
            }
            else {
                const locations = item;
                return async.map(locations, asLocation, token);
            }
        }
        else if (ls.LocationLink.is(item)) {
            return [asLocationLink(item)];
        }
        else {
            return asLocation(item);
        }
    }
    async function asReferences(values, token) {
        if (!values) {
            return undefined;
        }
        return async.map(values, asLocation, token);
    }
    async function asDocumentHighlights(values, token) {
        if (!values) {
            return undefined;
        }
        return async.map(values, asDocumentHighlight, token);
    }
    function asDocumentHighlight(item) {
        let result = new code.DocumentHighlight(asRange(item.range));
        if (Is.number(item.kind)) {
            result.kind = asDocumentHighlightKind(item.kind);
        }
        return result;
    }
    function asDocumentHighlightKind(item) {
        switch (item) {
            case ls.DocumentHighlightKind.Text:
                return code.DocumentHighlightKind.Text;
            case ls.DocumentHighlightKind.Read:
                return code.DocumentHighlightKind.Read;
            case ls.DocumentHighlightKind.Write:
                return code.DocumentHighlightKind.Write;
        }
        return code.DocumentHighlightKind.Text;
    }
    async function asSymbolInformations(values, token) {
        if (!values) {
            return undefined;
        }
        return async.map(values, asSymbolInformation, token);
    }
    function asSymbolKind(item) {
        if (item <= ls.SymbolKind.TypeParameter) {
            // Symbol kind is one based in the protocol and zero based in code.
            return item - 1;
        }
        return code.SymbolKind.Property;
    }
    function asSymbolTag(value) {
        switch (value) {
            case ls.SymbolTag.Deprecated:
                return code.SymbolTag.Deprecated;
            default:
                return undefined;
        }
    }
    function asSymbolTags(items) {
        if (items === undefined || items === null) {
            return undefined;
        }
        const result = [];
        for (const item of items) {
            const converted = asSymbolTag(item);
            if (converted !== undefined) {
                result.push(converted);
            }
        }
        return result.length === 0 ? undefined : result;
    }
    function asSymbolInformation(item) {
        const data = item.data;
        const location = item.location;
        const result = location.range === undefined || data !== undefined
            ? new protocolWorkspaceSymbol_1.default(item.name, asSymbolKind(item.kind), item.containerName ?? '', location.range === undefined ? _uriConverter(location.uri) : new code.Location(_uriConverter(item.location.uri), asRange(location.range)), data)
            : new code.SymbolInformation(item.name, asSymbolKind(item.kind), item.containerName ?? '', new code.Location(_uriConverter(item.location.uri), asRange(location.range)));
        fillTags(result, item);
        return result;
    }
    async function asDocumentSymbols(values, token) {
        if (values === undefined || values === null) {
            return undefined;
        }
        return async.map(values, asDocumentSymbol, token);
    }
    function asDocumentSymbol(value) {
        let result = new code.DocumentSymbol(value.name, value.detail || '', asSymbolKind(value.kind), asRange(value.range), asRange(value.selectionRange));
        fillTags(result, value);
        if (value.children !== undefined && value.children.length > 0) {
            let children = [];
            for (let child of value.children) {
                children.push(asDocumentSymbol(child));
            }
            result.children = children;
        }
        return result;
    }
    function fillTags(result, value) {
        result.tags = asSymbolTags(value.tags);
        if (value.deprecated) {
            if (!result.tags) {
                result.tags = [code.SymbolTag.Deprecated];
            }
            else {
                if (!result.tags.includes(code.SymbolTag.Deprecated)) {
                    result.tags = result.tags.concat(code.SymbolTag.Deprecated);
                }
            }
        }
    }
    function asCommand(item) {
        let result = { title: item.title, command: item.command };
        if (item.arguments) {
            result.arguments = item.arguments;
        }
        return result;
    }
    async function asCommands(items, token) {
        if (!items) {
            return undefined;
        }
        return async.map(items, asCommand, token);
    }
    const kindMapping = new Map();
    kindMapping.set(ls.CodeActionKind.Empty, code.CodeActionKind.Empty);
    kindMapping.set(ls.CodeActionKind.QuickFix, code.CodeActionKind.QuickFix);
    kindMapping.set(ls.CodeActionKind.Refactor, code.CodeActionKind.Refactor);
    kindMapping.set(ls.CodeActionKind.RefactorExtract, code.CodeActionKind.RefactorExtract);
    kindMapping.set(ls.CodeActionKind.RefactorInline, code.CodeActionKind.RefactorInline);
    kindMapping.set(ls.CodeActionKind.RefactorRewrite, code.CodeActionKind.RefactorRewrite);
    kindMapping.set(ls.CodeActionKind.Source, code.CodeActionKind.Source);
    kindMapping.set(ls.CodeActionKind.SourceOrganizeImports, code.CodeActionKind.SourceOrganizeImports);
    function asCodeActionKind(item) {
        if (item === undefined || item === null) {
            return undefined;
        }
        let result = kindMapping.get(item);
        if (result) {
            return result;
        }
        let parts = item.split('.');
        result = code.CodeActionKind.Empty;
        for (let part of parts) {
            result = result.append(part);
        }
        return result;
    }
    function asCodeActionKinds(items) {
        if (items === undefined || items === null) {
            return undefined;
        }
        return items.map(kind => asCodeActionKind(kind));
    }
    async function asCodeAction(item, token) {
        if (item === undefined || item === null) {
            return undefined;
        }
        let result = new protocolCodeAction_1.default(item.title, item.data);
        if (item.kind !== undefined) {
            result.kind = asCodeActionKind(item.kind);
        }
        if (item.diagnostics !== undefined) {
            result.diagnostics = asDiagnosticsSync(item.diagnostics);
        }
        if (item.edit !== undefined) {
            result.edit = await asWorkspaceEdit(item.edit, token);
        }
        if (item.command !== undefined) {
            result.command = asCommand(item.command);
        }
        if (item.isPreferred !== undefined) {
            result.isPreferred = item.isPreferred;
        }
        if (item.disabled !== undefined) {
            result.disabled = { reason: item.disabled.reason };
        }
        return result;
    }
    function asCodeActionResult(items, token) {
        return async.mapAsync(items, async (item) => {
            if (ls.Command.is(item)) {
                return asCommand(item);
            }
            else {
                return asCodeAction(item, token);
            }
        }, token);
    }
    function asCodeLens(item) {
        if (!item) {
            return undefined;
        }
        let result = new protocolCodeLens_1.default(asRange(item.range));
        if (item.command) {
            result.command = asCommand(item.command);
        }
        if (item.data !== undefined && item.data !== null) {
            result.data = item.data;
        }
        return result;
    }
    async function asCodeLenses(items, token) {
        if (!items) {
            return undefined;
        }
        return async.map(items, asCodeLens, token);
    }
    async function asWorkspaceEdit(item, token) {
        if (!item) {
            return undefined;
        }
        const sharedMetadata = new Map();
        if (item.changeAnnotations !== undefined) {
            const changeAnnotations = item.changeAnnotations;
            await async.forEach(Object.keys(changeAnnotations), (key) => {
                const metaData = asWorkspaceEditEntryMetadata(changeAnnotations[key]);
                sharedMetadata.set(key, metaData);
            }, token);
        }
        const asMetadata = (annotation) => {
            if (annotation === undefined) {
                return undefined;
            }
            else {
                return sharedMetadata.get(annotation);
            }
        };
        const result = new code.WorkspaceEdit();
        if (item.documentChanges) {
            const documentChanges = item.documentChanges;
            await async.forEach(documentChanges, (change) => {
                if (ls.CreateFile.is(change)) {
                    result.createFile(_uriConverter(change.uri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.RenameFile.is(change)) {
                    result.renameFile(_uriConverter(change.oldUri), _uriConverter(change.newUri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.DeleteFile.is(change)) {
                    result.deleteFile(_uriConverter(change.uri), change.options, asMetadata(change.annotationId));
                }
                else if (ls.TextDocumentEdit.is(change)) {
                    const uri = _uriConverter(change.textDocument.uri);
                    for (const edit of change.edits) {
                        if (ls.AnnotatedTextEdit.is(edit)) {
                            result.replace(uri, asRange(edit.range), edit.newText, asMetadata(edit.annotationId));
                        }
                        else {
                            result.replace(uri, asRange(edit.range), edit.newText);
                        }
                    }
                }
                else {
                    throw new Error(`Unknown workspace edit change received:\n${JSON.stringify(change, undefined, 4)}`);
                }
            }, token);
        }
        else if (item.changes) {
            const changes = item.changes;
            await async.forEach(Object.keys(changes), (key) => {
                result.set(_uriConverter(key), asTextEditsSync(changes[key]));
            }, token);
        }
        return result;
    }
    function asWorkspaceEditEntryMetadata(annotation) {
        if (annotation === undefined) {
            return undefined;
        }
        return { label: annotation.label, needsConfirmation: !!annotation.needsConfirmation, description: annotation.description };
    }
    function asDocumentLink(item) {
        let range = asRange(item.range);
        let target = item.target ? asUri(item.target) : undefined;
        // target must be optional in DocumentLink
        let link = new protocolDocumentLink_1.default(range, target);
        if (item.tooltip !== undefined) {
            link.tooltip = item.tooltip;
        }
        if (item.data !== undefined && item.data !== null) {
            link.data = item.data;
        }
        return link;
    }
    async function asDocumentLinks(items, token) {
        if (!items) {
            return undefined;
        }
        return async.map(items, asDocumentLink, token);
    }
    function asColor(color) {
        return new code.Color(color.red, color.green, color.blue, color.alpha);
    }
    function asColorInformation(ci) {
        return new code.ColorInformation(asRange(ci.range), asColor(ci.color));
    }
    async function asColorInformations(colorInformation, token) {
        if (!colorInformation) {
            return undefined;
        }
        return async.map(colorInformation, asColorInformation, token);
    }
    function asColorPresentation(cp) {
        let presentation = new code.ColorPresentation(cp.label);
        presentation.additionalTextEdits = asTextEditsSync(cp.additionalTextEdits);
        if (cp.textEdit) {
            presentation.textEdit = asTextEdit(cp.textEdit);
        }
        return presentation;
    }
    async function asColorPresentations(colorPresentations, token) {
        if (!colorPresentations) {
            return undefined;
        }
        return async.map(colorPresentations, asColorPresentation, token);
    }
    function asFoldingRangeKind(kind) {
        if (kind) {
            switch (kind) {
                case ls.FoldingRangeKind.Comment:
                    return code.FoldingRangeKind.Comment;
                case ls.FoldingRangeKind.Imports:
                    return code.FoldingRangeKind.Imports;
                case ls.FoldingRangeKind.Region:
                    return code.FoldingRangeKind.Region;
            }
        }
        return undefined;
    }
    function asFoldingRange(r) {
        return new code.FoldingRange(r.startLine, r.endLine, asFoldingRangeKind(r.kind));
    }
    async function asFoldingRanges(foldingRanges, token) {
        if (!foldingRanges) {
            return undefined;
        }
        return async.map(foldingRanges, asFoldingRange, token);
    }
    function asSelectionRange(selectionRange) {
        return new code.SelectionRange(asRange(selectionRange.range), selectionRange.parent ? asSelectionRange(selectionRange.parent) : undefined);
    }
    async function asSelectionRanges(selectionRanges, token) {
        if (!Array.isArray(selectionRanges)) {
            return [];
        }
        return async.map(selectionRanges, asSelectionRange, token);
    }
    function asInlineValue(inlineValue) {
        if (ls.InlineValueText.is(inlineValue)) {
            return new code.InlineValueText(asRange(inlineValue.range), inlineValue.text);
        }
        else if (ls.InlineValueVariableLookup.is(inlineValue)) {
            return new code.InlineValueVariableLookup(asRange(inlineValue.range), inlineValue.variableName, inlineValue.caseSensitiveLookup);
        }
        else {
            return new code.InlineValueEvaluatableExpression(asRange(inlineValue.range), inlineValue.expression);
        }
    }
    async function asInlineValues(inlineValues, token) {
        if (!Array.isArray(inlineValues)) {
            return [];
        }
        return async.map(inlineValues, asInlineValue, token);
    }
    async function asInlayHint(value, token) {
        const label = typeof value.label === 'string'
            ? value.label
            : await async.map(value.label, asInlayHintLabelPart, token);
        const result = new protocolInlayHint_1.default(asPosition(value.position), label);
        if (value.kind !== undefined) {
            result.kind = value.kind;
        }
        if (value.textEdits !== undefined) {
            result.textEdits = await asTextEdits(value.textEdits, token);
        }
        if (value.tooltip !== undefined) {
            result.tooltip = asTooltip(value.tooltip);
        }
        if (value.paddingLeft !== undefined) {
            result.paddingLeft = value.paddingLeft;
        }
        if (value.paddingRight !== undefined) {
            result.paddingRight = value.paddingRight;
        }
        if (value.data !== undefined) {
            result.data = value.data;
        }
        return result;
    }
    function asInlayHintLabelPart(part) {
        const result = new code.InlayHintLabelPart(part.value);
        if (part.location !== undefined) {
            result.location = asLocation(part.location);
        }
        if (part.tooltip !== undefined) {
            result.tooltip = asTooltip(part.tooltip);
        }
        if (part.command !== undefined) {
            result.command = asCommand(part.command);
        }
        return result;
    }
    function asTooltip(value) {
        if (typeof value === 'string') {
            return value;
        }
        return asMarkdownString(value);
    }
    async function asInlayHints(values, token) {
        if (!Array.isArray(values)) {
            return undefined;
        }
        return async.mapAsync(values, asInlayHint, token);
    }
    function asCallHierarchyItem(item) {
        if (item === null) {
            return undefined;
        }
        const result = new protocolCallHierarchyItem_1.default(asSymbolKind(item.kind), item.name, item.detail || '', asUri(item.uri), asRange(item.range), asRange(item.selectionRange), item.data);
        if (item.tags !== undefined) {
            result.tags = asSymbolTags(item.tags);
        }
        return result;
    }
    async function asCallHierarchyItems(items, token) {
        if (items === null) {
            return undefined;
        }
        return async.map(items, asCallHierarchyItem, token);
    }
    async function asCallHierarchyIncomingCall(item, token) {
        return new code.CallHierarchyIncomingCall(asCallHierarchyItem(item.from), await asRanges(item.fromRanges, token));
    }
    async function asCallHierarchyIncomingCalls(items, token) {
        if (items === null) {
            return undefined;
        }
        return async.mapAsync(items, asCallHierarchyIncomingCall, token);
    }
    async function asCallHierarchyOutgoingCall(item, token) {
        return new code.CallHierarchyOutgoingCall(asCallHierarchyItem(item.to), await asRanges(item.fromRanges, token));
    }
    async function asCallHierarchyOutgoingCalls(items, token) {
        if (items === null) {
            return undefined;
        }
        return async.mapAsync(items, asCallHierarchyOutgoingCall, token);
    }
    async function asSemanticTokens(value, _token) {
        if (value === undefined || value === null) {
            return undefined;
        }
        return new code.SemanticTokens(new Uint32Array(value.data), value.resultId);
    }
    function asSemanticTokensEdit(value) {
        return new code.SemanticTokensEdit(value.start, value.deleteCount, value.data !== undefined ? new Uint32Array(value.data) : undefined);
    }
    async function asSemanticTokensEdits(value, _token) {
        if (value === undefined || value === null) {
            return undefined;
        }
        return new code.SemanticTokensEdits(value.edits.map(asSemanticTokensEdit), value.resultId);
    }
    function asSemanticTokensLegend(value) {
        return value;
    }
    async function asLinkedEditingRanges(value, token) {
        if (value === null || value === undefined) {
            return undefined;
        }
        return new code.LinkedEditingRanges(await asRanges(value.ranges, token), asRegularExpression(value.wordPattern));
    }
    function asRegularExpression(value) {
        if (value === null || value === undefined) {
            return undefined;
        }
        return new RegExp(value);
    }
    function asTypeHierarchyItem(item) {
        if (item === null) {
            return undefined;
        }
        let result = new protocolTypeHierarchyItem_1.default(asSymbolKind(item.kind), item.name, item.detail || '', asUri(item.uri), asRange(item.range), asRange(item.selectionRange), item.data);
        if (item.tags !== undefined) {
            result.tags = asSymbolTags(item.tags);
        }
        return result;
    }
    async function asTypeHierarchyItems(items, token) {
        if (items === null) {
            return undefined;
        }
        return async.map(items, asTypeHierarchyItem, token);
    }
    function asGlobPattern(pattern) {
        if (Is.string(pattern)) {
            return pattern;
        }
        if (ls.RelativePattern.is(pattern)) {
            if (ls.URI.is(pattern.baseUri)) {
                return new code.RelativePattern(asUri(pattern.baseUri), pattern.pattern);
            }
            else if (ls.WorkspaceFolder.is(pattern.baseUri)) {
                const workspaceFolder = code.workspace.getWorkspaceFolder(asUri(pattern.baseUri.uri));
                return workspaceFolder !== undefined ? new code.RelativePattern(workspaceFolder, pattern.pattern) : undefined;
            }
        }
        return undefined;
    }
    return {
        asUri,
        asDocumentSelector,
        asDiagnostics,
        asDiagnostic,
        asRange,
        asRanges,
        asPosition,
        asDiagnosticSeverity,
        asDiagnosticTag,
        asHover,
        asCompletionResult,
        asCompletionItem,
        asTextEdit,
        asTextEdits,
        asSignatureHelp,
        asSignatureInformations,
        asSignatureInformation,
        asParameterInformations,
        asParameterInformation,
        asDeclarationResult,
        asDefinitionResult,
        asLocation,
        asReferences,
        asDocumentHighlights,
        asDocumentHighlight,
        asDocumentHighlightKind,
        asSymbolKind,
        asSymbolTag,
        asSymbolTags,
        asSymbolInformations,
        asSymbolInformation,
        asDocumentSymbols,
        asDocumentSymbol,
        asCommand,
        asCommands,
        asCodeAction,
        asCodeActionKind,
        asCodeActionKinds,
        asCodeActionResult,
        asCodeLens,
        asCodeLenses,
        asWorkspaceEdit,
        asDocumentLink,
        asDocumentLinks,
        asFoldingRangeKind,
        asFoldingRange,
        asFoldingRanges,
        asColor,
        asColorInformation,
        asColorInformations,
        asColorPresentation,
        asColorPresentations,
        asSelectionRange,
        asSelectionRanges,
        asInlineValue,
        asInlineValues,
        asInlayHint,
        asInlayHints,
        asSemanticTokensLegend,
        asSemanticTokens,
        asSemanticTokensEdit,
        asSemanticTokensEdits,
        asCallHierarchyItem,
        asCallHierarchyItems,
        asCallHierarchyIncomingCall,
        asCallHierarchyIncomingCalls,
        asCallHierarchyOutgoingCall,
        asCallHierarchyOutgoingCalls,
        asLinkedEditingRanges: asLinkedEditingRanges,
        asTypeHierarchyItem,
        asTypeHierarchyItems,
        asGlobPattern
    };
}
exports.createConverter = createConverter;
//# sourceMappingURL=protocolConverter.js.map

/***/ }),
/* 115 */
/***/ ((__unused_webpack_module, exports) => {

"use strict";

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.generateUuid = exports.parse = exports.isUUID = exports.v4 = exports.empty = void 0;
class ValueUUID {
    constructor(_value) {
        this._value = _value;
        // empty
    }
    asHex() {
        return this._value;
    }
    equals(other) {
        return this.asHex() === other.asHex();
    }
}
class V4UUID extends ValueUUID {
    constructor() {
        super([
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            '4',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._oneOf(V4UUID._timeHighBits),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            '-',
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
            V4UUID._randomHex(),
        ].join(''));
    }
    static _oneOf(array) {
        return array[Math.floor(array.length * Math.random())];
    }
    static _randomHex() {
        return V4UUID._oneOf(V4UUID._chars);
    }
}
V4UUID._chars = ['0', '1', '2', '3', '4', '5', '6', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
V4UUID._timeHighBits = ['8', '9', 'a', 'b'];
/**
 * An empty UUID that contains only zeros.
 */
exports.empty = new ValueUUID('00000000-0000-0000-0000-000000000000');
function v4() {
    return new V4UUID();
}
exports.v4 = v4;
const _UUIDPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
function isUUID(value) {
    return _UUIDPattern.test(value);
}
exports.isUUID = isUUID;
/**
 * Parses a UUID that is of the format xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.
 * @param value A uuid string.
 */
function parse(value) {
    if (!isUUID(value)) {
        throw new Error('invalid uuid');
    }
    return new ValueUUID(value);
}
exports.parse = parse;
function generateUuid() {
    return v4().asHex();
}
exports.generateUuid = generateUuid;
//# sourceMappingURL=uuid.js.map

/***/ }),
/* 116 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProgressPart = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const Is = __webpack_require__(54);
class ProgressPart {
    constructor(_client, _token, done) {
        this._client = _client;
        this._token = _token;
        this._reported = 0;
        this._infinite = false;
        this._lspProgressDisposable = this._client.onProgress(vscode_languageserver_protocol_1.WorkDoneProgress.type, this._token, (value) => {
            switch (value.kind) {
                case 'begin':
                    this.begin(value);
                    break;
                case 'report':
                    this.report(value);
                    break;
                case 'end':
                    this.done();
                    done && done(this);
                    break;
            }
        });
    }
    begin(params) {
        this._infinite = params.percentage === undefined;
        // the progress as already been marked as done / canceled. Ignore begin call
        if (this._lspProgressDisposable === undefined) {
            return;
        }
        // Since we don't use commands this will be a silent window progress with a hidden notification.
        void vscode_1.window.withProgress({ location: vscode_1.ProgressLocation.Window, cancellable: params.cancellable, title: params.title }, async (progress, cancellationToken) => {
            // the progress as already been marked as done / canceled. Ignore begin call
            if (this._lspProgressDisposable === undefined) {
                return;
            }
            this._progress = progress;
            this._cancellationToken = cancellationToken;
            this._tokenDisposable = this._cancellationToken.onCancellationRequested(() => {
                this._client.sendNotification(vscode_languageserver_protocol_1.WorkDoneProgressCancelNotification.type, { token: this._token });
            });
            this.report(params);
            return new Promise((resolve, reject) => {
                this._resolve = resolve;
                this._reject = reject;
            });
        });
    }
    report(params) {
        if (this._infinite && Is.string(params.message)) {
            this._progress !== undefined && this._progress.report({ message: params.message });
        }
        else if (Is.number(params.percentage)) {
            const percentage = Math.max(0, Math.min(params.percentage, 100));
            const delta = Math.max(0, percentage - this._reported);
            this._reported += delta;
            this._progress !== undefined && this._progress.report({ message: params.message, increment: delta });
        }
    }
    cancel() {
        this.cleanup();
        if (this._reject !== undefined) {
            this._reject();
            this._resolve = undefined;
            this._reject = undefined;
        }
    }
    done() {
        this.cleanup();
        if (this._resolve !== undefined) {
            this._resolve();
            this._resolve = undefined;
            this._reject = undefined;
        }
    }
    cleanup() {
        if (this._lspProgressDisposable !== undefined) {
            this._lspProgressDisposable.dispose();
            this._lspProgressDisposable = undefined;
        }
        if (this._tokenDisposable !== undefined) {
            this._tokenDisposable.dispose();
            this._tokenDisposable = undefined;
        }
        this._progress = undefined;
        this._cancellationToken = undefined;
    }
}
exports.ProgressPart = ProgressPart;
//# sourceMappingURL=progressPart.js.map

/***/ }),
/* 117 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkspaceFeature = exports.TextDocumentLanguageFeature = exports.TextDocumentEventFeature = exports.DynamicDocumentFeature = exports.DynamicFeature = exports.StaticFeature = exports.ensure = exports.LSPCancellationError = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const Is = __webpack_require__(54);
const UUID = __webpack_require__(115);
class LSPCancellationError extends vscode_1.CancellationError {
    constructor(data) {
        super();
        this.data = data;
    }
}
exports.LSPCancellationError = LSPCancellationError;
function ensure(target, key) {
    if (target[key] === undefined) {
        target[key] = {};
    }
    return target[key];
}
exports.ensure = ensure;
var StaticFeature;
(function (StaticFeature) {
    function is(value) {
        const candidate = value;
        return candidate !== undefined && candidate !== null &&
            Is.func(candidate.fillClientCapabilities) && Is.func(candidate.initialize) && Is.func(candidate.getState) && Is.func(candidate.dispose) &&
            (candidate.fillInitializeParams === undefined || Is.func(candidate.fillInitializeParams));
    }
    StaticFeature.is = is;
})(StaticFeature = exports.StaticFeature || (exports.StaticFeature = {}));
var DynamicFeature;
(function (DynamicFeature) {
    function is(value) {
        const candidate = value;
        return candidate !== undefined && candidate !== null &&
            Is.func(candidate.fillClientCapabilities) && Is.func(candidate.initialize) && Is.func(candidate.getState) && Is.func(candidate.dispose) &&
            (candidate.fillInitializeParams === undefined || Is.func(candidate.fillInitializeParams)) && Is.func(candidate.register) &&
            Is.func(candidate.unregister) && candidate.registrationType !== undefined;
    }
    DynamicFeature.is = is;
})(DynamicFeature = exports.DynamicFeature || (exports.DynamicFeature = {}));
/**
 * An abstract dynamic feature implementation that operates on documents (e.g. text
 * documents or notebooks).
 */
class DynamicDocumentFeature {
    constructor(client) {
        this._client = client;
    }
    /**
     * Returns the state the feature is in.
     */
    getState() {
        const selectors = this.getDocumentSelectors();
        let count = 0;
        for (const selector of selectors) {
            count++;
            for (const document of vscode_1.workspace.textDocuments) {
                if (vscode_1.languages.match(selector, document) > 0) {
                    return { kind: 'document', id: this.registrationType.method, registrations: true, matches: true };
                }
            }
        }
        const registrations = count > 0;
        return { kind: 'document', id: this.registrationType.method, registrations, matches: false };
    }
}
exports.DynamicDocumentFeature = DynamicDocumentFeature;
/**
 * An abstract base class to implement features that react to events
 * emitted from text documents.
 */
class TextDocumentEventFeature extends DynamicDocumentFeature {
    constructor(client, event, type, middleware, createParams, textDocument, selectorFilter) {
        super(client);
        this._event = event;
        this._type = type;
        this._middleware = middleware;
        this._createParams = createParams;
        this._textDocument = textDocument;
        this._selectorFilter = selectorFilter;
        this._selectors = new Map();
        this._onNotificationSent = new vscode_1.EventEmitter();
    }
    static textDocumentFilter(selectors, textDocument) {
        for (const selector of selectors) {
            if (vscode_1.languages.match(selector, textDocument) > 0) {
                return true;
            }
        }
        return false;
    }
    getStateInfo() {
        return [this._selectors.values(), false];
    }
    getDocumentSelectors() {
        return this._selectors.values();
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = this._event((data) => {
                this.callback(data).catch((error) => {
                    this._client.error(`Sending document notification ${this._type.method} failed.`, error);
                });
            });
        }
        this._selectors.set(data.id, this._client.protocol2CodeConverter.asDocumentSelector(data.registerOptions.documentSelector));
    }
    async callback(data) {
        const doSend = async (data) => {
            const params = this._createParams(data);
            await this._client.sendNotification(this._type, params).catch();
            this.notificationSent(data, this._type, params);
        };
        if (this.matches(data)) {
            const middleware = this._middleware();
            return middleware ? middleware(data, (data) => doSend(data)) : doSend(data);
        }
    }
    matches(data) {
        if (this._client.hasDedicatedTextSynchronizationFeature(this._textDocument(data))) {
            return false;
        }
        return !this._selectorFilter || this._selectorFilter(this._selectors.values(), data);
    }
    get onNotificationSent() {
        return this._onNotificationSent.event;
    }
    notificationSent(data, type, params) {
        this._onNotificationSent.fire({ original: data, type, params });
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        this._onNotificationSent.dispose();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    getProvider(document) {
        for (const selector of this._selectors.values()) {
            if (vscode_1.languages.match(selector, document) > 0) {
                return {
                    send: (data) => {
                        return this.callback(data);
                    }
                };
            }
        }
        return undefined;
    }
}
exports.TextDocumentEventFeature = TextDocumentEventFeature;
/**
 * A abstract feature implementation that registers language providers
 * for text documents using a given document selector.
 */
class TextDocumentLanguageFeature extends DynamicDocumentFeature {
    constructor(client, registrationType) {
        super(client);
        this._registrationType = registrationType;
        this._registrations = new Map();
    }
    *getDocumentSelectors() {
        for (const registration of this._registrations.values()) {
            const selector = registration.data.registerOptions.documentSelector;
            if (selector === null) {
                continue;
            }
            yield this._client.protocol2CodeConverter.asDocumentSelector(selector);
        }
    }
    get registrationType() {
        return this._registrationType;
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        let registration = this.registerLanguageProvider(data.registerOptions, data.id);
        this._registrations.set(data.id, { disposable: registration[0], data, provider: registration[1] });
    }
    unregister(id) {
        let registration = this._registrations.get(id);
        if (registration !== undefined) {
            registration.disposable.dispose();
        }
    }
    dispose() {
        this._registrations.forEach((value) => {
            value.disposable.dispose();
        });
        this._registrations.clear();
    }
    getRegistration(documentSelector, capability) {
        if (!capability) {
            return [undefined, undefined];
        }
        else if (vscode_languageserver_protocol_1.TextDocumentRegistrationOptions.is(capability)) {
            const id = vscode_languageserver_protocol_1.StaticRegistrationOptions.hasId(capability) ? capability.id : UUID.generateUuid();
            const selector = capability.documentSelector || documentSelector;
            if (selector) {
                return [id, Object.assign({}, capability, { documentSelector: selector })];
            }
        }
        else if (Is.boolean(capability) && capability === true || vscode_languageserver_protocol_1.WorkDoneProgressOptions.is(capability)) {
            if (!documentSelector) {
                return [undefined, undefined];
            }
            let options = (Is.boolean(capability) && capability === true ? { documentSelector } : Object.assign({}, capability, { documentSelector }));
            return [UUID.generateUuid(), options];
        }
        return [undefined, undefined];
    }
    getRegistrationOptions(documentSelector, capability) {
        if (!documentSelector || !capability) {
            return undefined;
        }
        return (Is.boolean(capability) && capability === true ? { documentSelector } : Object.assign({}, capability, { documentSelector }));
    }
    getProvider(textDocument) {
        for (const registration of this._registrations.values()) {
            let selector = registration.data.registerOptions.documentSelector;
            if (selector !== null && vscode_1.languages.match(this._client.protocol2CodeConverter.asDocumentSelector(selector), textDocument) > 0) {
                return registration.provider;
            }
        }
        return undefined;
    }
    getAllProviders() {
        const result = [];
        for (const item of this._registrations.values()) {
            result.push(item.provider);
        }
        return result;
    }
}
exports.TextDocumentLanguageFeature = TextDocumentLanguageFeature;
class WorkspaceFeature {
    constructor(client, registrationType) {
        this._client = client;
        this._registrationType = registrationType;
        this._registrations = new Map();
    }
    getState() {
        const registrations = this._registrations.size > 0;
        return { kind: 'workspace', id: this._registrationType.method, registrations };
    }
    get registrationType() {
        return this._registrationType;
    }
    register(data) {
        const registration = this.registerLanguageProvider(data.registerOptions);
        this._registrations.set(data.id, { disposable: registration[0], provider: registration[1] });
    }
    unregister(id) {
        let registration = this._registrations.get(id);
        if (registration !== undefined) {
            registration.disposable.dispose();
        }
    }
    dispose() {
        this._registrations.forEach((registration) => {
            registration.disposable.dispose();
        });
        this._registrations.clear();
    }
    getProviders() {
        const result = [];
        for (const registration of this._registrations.values()) {
            result.push(registration.provider);
        }
        return result;
    }
}
exports.WorkspaceFeature = WorkspaceFeature;
//# sourceMappingURL=features.js.map

/***/ }),
/* 118 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DiagnosticFeature = exports.DiagnosticPullMode = exports.vsdiag = void 0;
const minimatch = __webpack_require__(119);
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const uuid_1 = __webpack_require__(115);
const features_1 = __webpack_require__(117);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
var vsdiag;
(function (vsdiag) {
    let DocumentDiagnosticReportKind;
    (function (DocumentDiagnosticReportKind) {
        DocumentDiagnosticReportKind["full"] = "full";
        DocumentDiagnosticReportKind["unChanged"] = "unChanged";
    })(DocumentDiagnosticReportKind = vsdiag.DocumentDiagnosticReportKind || (vsdiag.DocumentDiagnosticReportKind = {}));
})(vsdiag = exports.vsdiag || (exports.vsdiag = {}));
var DiagnosticPullMode;
(function (DiagnosticPullMode) {
    DiagnosticPullMode["onType"] = "onType";
    DiagnosticPullMode["onSave"] = "onSave";
})(DiagnosticPullMode = exports.DiagnosticPullMode || (exports.DiagnosticPullMode = {}));
var RequestStateKind;
(function (RequestStateKind) {
    RequestStateKind["active"] = "open";
    RequestStateKind["reschedule"] = "reschedule";
    RequestStateKind["outDated"] = "drop";
})(RequestStateKind || (RequestStateKind = {}));
class Tabs {
    constructor() {
        this.open = new Set();
        const openTabsHandler = () => {
            this.open.clear();
            for (const group of vscode_1.window.tabGroups.all) {
                for (const tab of group.tabs) {
                    const input = tab.input;
                    if (input instanceof vscode_1.TabInputText) {
                        this.open.add(input.uri.toString());
                    }
                    else if (input instanceof vscode_1.TabInputTextDiff) {
                        this.open.add(input.modified.toString());
                    }
                }
            }
        };
        openTabsHandler();
        if (vscode_1.window.tabGroups.onDidChangeTabGroups !== undefined) {
            this.disposable = vscode_1.window.tabGroups.onDidChangeTabGroups(openTabsHandler);
        }
        else {
            this.disposable = { dispose: () => { } };
        }
    }
    dispose() {
        this.disposable.dispose();
    }
    isActive(document) {
        return document instanceof vscode_1.Uri
            ? vscode_1.window.activeTextEditor?.document.uri === document
            : vscode_1.window.activeTextEditor?.document === document;
    }
    isVisible(document) {
        const uri = document instanceof vscode_1.Uri ? document : document.uri;
        return this.open.has(uri.toString());
    }
    getTabResources() {
        const result = [];
        for (const group of vscode_1.window.tabGroups.all) {
            for (const tab of group.tabs) {
                const input = tab.input;
                if (input instanceof vscode_1.TabInputText) {
                    result.push(input.uri);
                }
                else if (input instanceof vscode_1.TabInputTextDiff) {
                    result.push(input.modified);
                }
            }
        }
        return result;
    }
}
var PullState;
(function (PullState) {
    PullState[PullState["document"] = 1] = "document";
    PullState[PullState["workspace"] = 2] = "workspace";
})(PullState || (PullState = {}));
class DocumentPullStateTracker {
    constructor() {
        this.documentPullStates = new Map();
        this.workspacePullStates = new Map();
    }
    track(kind, document, arg1) {
        const states = kind === PullState.document ? this.documentPullStates : this.workspacePullStates;
        const [key, uri, version] = document instanceof vscode_1.Uri
            ? [document.toString(), document, arg1]
            : [document.uri.toString(), document.uri, document.version];
        let state = states.get(key);
        if (state === undefined) {
            state = { document: uri, pulledVersion: version, resultId: undefined };
            states.set(key, state);
        }
        return state;
    }
    update(kind, document, arg1, arg2) {
        const states = kind === PullState.document ? this.documentPullStates : this.workspacePullStates;
        const [key, uri, version, resultId] = document instanceof vscode_1.Uri
            ? [document.toString(), document, arg1, arg2]
            : [document.uri.toString(), document.uri, document.version, arg1];
        let state = states.get(key);
        if (state === undefined) {
            state = { document: uri, pulledVersion: version, resultId };
            states.set(key, state);
        }
        else {
            state.pulledVersion = version;
            state.resultId = resultId;
        }
    }
    unTrack(kind, document) {
        const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
        const states = kind === PullState.document ? this.documentPullStates : this.workspacePullStates;
        states.delete(key);
    }
    tracks(kind, document) {
        const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
        const states = kind === PullState.document ? this.documentPullStates : this.workspacePullStates;
        return states.has(key);
    }
    getResultId(kind, document) {
        const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
        const states = kind === PullState.document ? this.documentPullStates : this.workspacePullStates;
        return states.get(key)?.resultId;
    }
    getAllResultIds() {
        const result = [];
        for (let [uri, value] of this.workspacePullStates) {
            if (this.documentPullStates.has(uri)) {
                value = this.documentPullStates.get(uri);
            }
            if (value.resultId !== undefined) {
                result.push({ uri, value: value.resultId });
            }
        }
        return result;
    }
}
class DiagnosticRequestor {
    constructor(client, tabs, options) {
        this.client = client;
        this.tabs = tabs;
        this.options = options;
        this.isDisposed = false;
        this.onDidChangeDiagnosticsEmitter = new vscode_1.EventEmitter();
        this.provider = this.createProvider();
        this.diagnostics = vscode_1.languages.createDiagnosticCollection(options.identifier);
        this.openRequests = new Map();
        this.documentStates = new DocumentPullStateTracker();
        this.workspaceErrorCounter = 0;
    }
    knows(kind, textDocument) {
        return this.documentStates.tracks(kind, textDocument);
    }
    pull(document, cb) {
        if (this.isDisposed) {
            return;
        }
        const uri = document instanceof vscode_1.Uri ? document : document.uri;
        this.pullAsync(document).then(() => {
            if (cb) {
                cb();
            }
        }, (error) => {
            this.client.error(`Document pull failed for text document ${uri.toString()}`, error, false);
        });
    }
    async pullAsync(document, version) {
        if (this.isDisposed) {
            return;
        }
        const isUri = document instanceof vscode_1.Uri;
        const uri = isUri ? document : document.uri;
        const key = uri.toString();
        version = isUri ? version : document.version;
        const currentRequestState = this.openRequests.get(key);
        const documentState = isUri
            ? this.documentStates.track(PullState.document, document, version)
            : this.documentStates.track(PullState.document, document);
        if (currentRequestState === undefined) {
            const tokenSource = new vscode_1.CancellationTokenSource();
            this.openRequests.set(key, { state: RequestStateKind.active, document: document, version: version, tokenSource });
            let report;
            let afterState;
            try {
                report = await this.provider.provideDiagnostics(document, documentState.resultId, tokenSource.token) ?? { kind: vsdiag.DocumentDiagnosticReportKind.full, items: [] };
            }
            catch (error) {
                if (error instanceof features_1.LSPCancellationError && vscode_languageserver_protocol_1.DiagnosticServerCancellationData.is(error.data) && error.data.retriggerRequest === false) {
                    afterState = { state: RequestStateKind.outDated, document };
                }
                if (afterState === undefined && error instanceof vscode_1.CancellationError) {
                    afterState = { state: RequestStateKind.reschedule, document };
                }
                else {
                    throw error;
                }
            }
            afterState = afterState ?? this.openRequests.get(key);
            if (afterState === undefined) {
                // This shouldn't happen. Log it
                this.client.error(`Lost request state in diagnostic pull model. Clearing diagnostics for ${key}`);
                this.diagnostics.delete(uri);
                return;
            }
            this.openRequests.delete(key);
            if (!this.tabs.isVisible(document)) {
                this.documentStates.unTrack(PullState.document, document);
                return;
            }
            if (afterState.state === RequestStateKind.outDated) {
                return;
            }
            // report is only undefined if the request has thrown.
            if (report !== undefined) {
                if (report.kind === vsdiag.DocumentDiagnosticReportKind.full) {
                    this.diagnostics.set(uri, report.items);
                }
                documentState.pulledVersion = version;
                documentState.resultId = report.resultId;
            }
            if (afterState.state === RequestStateKind.reschedule) {
                this.pull(document);
            }
        }
        else {
            if (currentRequestState.state === RequestStateKind.active) {
                // Cancel the current request and reschedule a new one when the old one returned.
                currentRequestState.tokenSource.cancel();
                this.openRequests.set(key, { state: RequestStateKind.reschedule, document: currentRequestState.document });
            }
            else if (currentRequestState.state === RequestStateKind.outDated) {
                this.openRequests.set(key, { state: RequestStateKind.reschedule, document: currentRequestState.document });
            }
        }
    }
    cleanupPull(document) {
        const uri = document instanceof vscode_1.Uri ? document : document.uri;
        const key = uri.toString();
        const request = this.openRequests.get(key);
        if (this.options.workspaceDiagnostics || this.options.interFileDependencies) {
            if (request !== undefined) {
                this.openRequests.set(key, { state: RequestStateKind.reschedule, document: document });
            }
            else {
                this.pull(document);
            }
        }
        else {
            if (request !== undefined) {
                if (request.state === RequestStateKind.active) {
                    request.tokenSource.cancel();
                }
                this.openRequests.set(key, { state: RequestStateKind.outDated, document: document });
            }
            this.diagnostics.delete(uri);
        }
    }
    pullWorkspace() {
        if (this.isDisposed) {
            return;
        }
        this.pullWorkspaceAsync().then(() => {
            this.workspaceTimeout = (0, vscode_languageserver_protocol_1.RAL)().timer.setTimeout(() => {
                this.pullWorkspace();
            }, 2000);
        }, (error) => {
            if (!(error instanceof features_1.LSPCancellationError) && !vscode_languageserver_protocol_1.DiagnosticServerCancellationData.is(error.data)) {
                this.client.error(`Workspace diagnostic pull failed.`, error, false);
                this.workspaceErrorCounter++;
            }
            if (this.workspaceErrorCounter <= 5) {
                this.workspaceTimeout = (0, vscode_languageserver_protocol_1.RAL)().timer.setTimeout(() => {
                    this.pullWorkspace();
                }, 2000);
            }
        });
    }
    async pullWorkspaceAsync() {
        if (!this.provider.provideWorkspaceDiagnostics || this.isDisposed) {
            return;
        }
        if (this.workspaceCancellation !== undefined) {
            this.workspaceCancellation.cancel();
            this.workspaceCancellation = undefined;
        }
        this.workspaceCancellation = new vscode_1.CancellationTokenSource();
        const previousResultIds = this.documentStates.getAllResultIds().map((item) => {
            return {
                uri: this.client.protocol2CodeConverter.asUri(item.uri),
                value: item.value
            };
        });
        await this.provider.provideWorkspaceDiagnostics(previousResultIds, this.workspaceCancellation.token, (chunk) => {
            if (!chunk || this.isDisposed) {
                return;
            }
            for (const item of chunk.items) {
                if (item.kind === vsdiag.DocumentDiagnosticReportKind.full) {
                    // Favour document pull result over workspace results. So skip if it is tracked
                    // as a document result.
                    if (!this.documentStates.tracks(PullState.document, item.uri)) {
                        this.diagnostics.set(item.uri, item.items);
                    }
                }
                this.documentStates.update(PullState.workspace, item.uri, item.version ?? undefined, item.resultId);
            }
        });
    }
    createProvider() {
        const result = {
            onDidChangeDiagnostics: this.onDidChangeDiagnosticsEmitter.event,
            provideDiagnostics: (document, previousResultId, token) => {
                const provideDiagnostics = (document, previousResultId, token) => {
                    const params = {
                        identifier: this.options.identifier,
                        textDocument: { uri: this.client.code2ProtocolConverter.asUri(document instanceof vscode_1.Uri ? document : document.uri) },
                        previousResultId: previousResultId
                    };
                    if (this.isDisposed === true || !this.client.isRunning()) {
                        return { kind: vsdiag.DocumentDiagnosticReportKind.full, items: [] };
                    }
                    return this.client.sendRequest(vscode_languageserver_protocol_1.DocumentDiagnosticRequest.type, params, token).then(async (result) => {
                        if (result === undefined || result === null || this.isDisposed || token.isCancellationRequested) {
                            return { kind: vsdiag.DocumentDiagnosticReportKind.full, items: [] };
                        }
                        if (result.kind === vscode_languageserver_protocol_1.DocumentDiagnosticReportKind.Full) {
                            return { kind: vsdiag.DocumentDiagnosticReportKind.full, resultId: result.resultId, items: await this.client.protocol2CodeConverter.asDiagnostics(result.items, token) };
                        }
                        else {
                            return { kind: vsdiag.DocumentDiagnosticReportKind.unChanged, resultId: result.resultId };
                        }
                    }, (error) => {
                        return this.client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentDiagnosticRequest.type, token, error, { kind: vsdiag.DocumentDiagnosticReportKind.full, items: [] });
                    });
                };
                const middleware = this.client.middleware;
                return middleware.provideDiagnostics
                    ? middleware.provideDiagnostics(document, previousResultId, token, provideDiagnostics)
                    : provideDiagnostics(document, previousResultId, token);
            }
        };
        if (this.options.workspaceDiagnostics) {
            result.provideWorkspaceDiagnostics = (resultIds, token, resultReporter) => {
                const convertReport = async (report) => {
                    if (report.kind === vscode_languageserver_protocol_1.DocumentDiagnosticReportKind.Full) {
                        return {
                            kind: vsdiag.DocumentDiagnosticReportKind.full,
                            uri: this.client.protocol2CodeConverter.asUri(report.uri),
                            resultId: report.resultId,
                            version: report.version,
                            items: await this.client.protocol2CodeConverter.asDiagnostics(report.items, token)
                        };
                    }
                    else {
                        return {
                            kind: vsdiag.DocumentDiagnosticReportKind.unChanged,
                            uri: this.client.protocol2CodeConverter.asUri(report.uri),
                            resultId: report.resultId,
                            version: report.version
                        };
                    }
                };
                const convertPreviousResultIds = (resultIds) => {
                    const converted = [];
                    for (const item of resultIds) {
                        converted.push({ uri: this.client.code2ProtocolConverter.asUri(item.uri), value: item.value });
                    }
                    return converted;
                };
                const provideDiagnostics = (resultIds, token) => {
                    const partialResultToken = (0, uuid_1.generateUuid)();
                    const disposable = this.client.onProgress(vscode_languageserver_protocol_1.WorkspaceDiagnosticRequest.partialResult, partialResultToken, async (partialResult) => {
                        if (partialResult === undefined || partialResult === null) {
                            resultReporter(null);
                            return;
                        }
                        const converted = {
                            items: []
                        };
                        for (const item of partialResult.items) {
                            try {
                                converted.items.push(await convertReport(item));
                            }
                            catch (error) {
                                this.client.error(`Converting workspace diagnostics failed.`, error);
                            }
                        }
                        resultReporter(converted);
                    });
                    const params = {
                        identifier: this.options.identifier,
                        previousResultIds: convertPreviousResultIds(resultIds),
                        partialResultToken: partialResultToken
                    };
                    if (this.isDisposed === true || !this.client.isRunning()) {
                        return { items: [] };
                    }
                    return this.client.sendRequest(vscode_languageserver_protocol_1.WorkspaceDiagnosticRequest.type, params, token).then(async (result) => {
                        if (token.isCancellationRequested) {
                            return { items: [] };
                        }
                        const converted = {
                            items: []
                        };
                        for (const item of result.items) {
                            converted.items.push(await convertReport(item));
                        }
                        disposable.dispose();
                        resultReporter(converted);
                        return { items: [] };
                    }, (error) => {
                        disposable.dispose();
                        return this.client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentDiagnosticRequest.type, token, error, { items: [] });
                    });
                };
                const middleware = this.client.middleware;
                return middleware.provideWorkspaceDiagnostics
                    ? middleware.provideWorkspaceDiagnostics(resultIds, token, resultReporter, provideDiagnostics)
                    : provideDiagnostics(resultIds, token, resultReporter);
            };
        }
        return result;
    }
    dispose() {
        this.isDisposed = true;
        // Cancel and clear workspace pull if present.
        this.workspaceCancellation?.cancel();
        this.workspaceTimeout?.dispose();
        // Cancel all request and mark open requests as outdated.
        for (const [key, request] of this.openRequests) {
            if (request.state === RequestStateKind.active) {
                request.tokenSource.cancel();
            }
            this.openRequests.set(key, { state: RequestStateKind.outDated, document: request.document });
        }
    }
}
class BackgroundScheduler {
    constructor(diagnosticRequestor) {
        this.diagnosticRequestor = diagnosticRequestor;
        this.documents = new vscode_languageserver_protocol_1.LinkedMap();
        this.isDisposed = false;
    }
    add(document) {
        if (this.isDisposed === true) {
            return;
        }
        const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
        if (this.documents.has(key)) {
            return;
        }
        this.documents.set(key, document, vscode_languageserver_protocol_1.Touch.Last);
        this.trigger();
    }
    remove(document) {
        const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
        if (this.documents.has(key)) {
            this.documents.delete(key);
            // Do a last pull
            this.diagnosticRequestor.pull(document);
        }
        // No more documents. Stop background activity.
        if (this.documents.size === 0) {
            this.stop();
        }
        else if (document === this.endDocument) {
            // Make sure we have a correct last document. It could have
            this.endDocument = this.documents.last;
        }
    }
    trigger() {
        if (this.isDisposed === true) {
            return;
        }
        // We have a round running. So simply make sure we run up to the
        // last document
        if (this.intervalHandle !== undefined) {
            this.endDocument = this.documents.last;
            return;
        }
        this.endDocument = this.documents.last;
        this.intervalHandle = (0, vscode_languageserver_protocol_1.RAL)().timer.setInterval(() => {
            const document = this.documents.first;
            if (document !== undefined) {
                const key = document instanceof vscode_1.Uri ? document.toString() : document.uri.toString();
                this.diagnosticRequestor.pull(document);
                this.documents.set(key, document, vscode_languageserver_protocol_1.Touch.Last);
                if (document === this.endDocument) {
                    this.stop();
                }
            }
        }, 200);
    }
    dispose() {
        this.isDisposed = true;
        this.stop();
        this.documents.clear();
    }
    stop() {
        this.intervalHandle?.dispose();
        this.intervalHandle = undefined;
        this.endDocument = undefined;
    }
}
class DiagnosticFeatureProviderImpl {
    constructor(client, tabs, options) {
        const diagnosticPullOptions = client.clientOptions.diagnosticPullOptions ?? { onChange: true, onSave: false };
        const documentSelector = client.protocol2CodeConverter.asDocumentSelector(options.documentSelector);
        const disposables = [];
        const matchResource = (resource) => {
            const selector = options.documentSelector;
            if (diagnosticPullOptions.match !== undefined) {
                return diagnosticPullOptions.match(selector, resource);
            }
            for (const filter of selector) {
                if (!vscode_languageserver_protocol_1.TextDocumentFilter.is(filter)) {
                    continue;
                }
                // The filter is a language id. We can't determine if it matches
                // so we return false.
                if (typeof filter === 'string') {
                    return false;
                }
                if (filter.language !== undefined && filter.language !== '*') {
                    return false;
                }
                if (filter.scheme !== undefined && filter.scheme !== '*' && filter.scheme !== resource.scheme) {
                    return false;
                }
                if (filter.pattern !== undefined) {
                    const matcher = new minimatch.Minimatch(filter.pattern, { noext: true });
                    if (!matcher.makeRe()) {
                        return false;
                    }
                    if (!matcher.match(resource.fsPath)) {
                        return false;
                    }
                }
            }
            return true;
        };
        const matches = (document) => {
            return document instanceof vscode_1.Uri
                ? matchResource(document)
                : vscode_1.languages.match(documentSelector, document) > 0 && tabs.isVisible(document);
        };
        const isActiveDocument = (document) => {
            return document instanceof vscode_1.Uri
                ? this.activeTextDocument?.uri.toString() === document.toString()
                : this.activeTextDocument === document;
        };
        this.diagnosticRequestor = new DiagnosticRequestor(client, tabs, options);
        this.backgroundScheduler = new BackgroundScheduler(this.diagnosticRequestor);
        const addToBackgroundIfNeeded = (document) => {
            if (!matches(document) || !options.interFileDependencies || isActiveDocument(document)) {
                return;
            }
            this.backgroundScheduler.add(document);
        };
        this.activeTextDocument = vscode_1.window.activeTextEditor?.document;
        vscode_1.window.onDidChangeActiveTextEditor((editor) => {
            const oldActive = this.activeTextDocument;
            this.activeTextDocument = editor?.document;
            if (oldActive !== undefined) {
                addToBackgroundIfNeeded(oldActive);
            }
            if (this.activeTextDocument !== undefined) {
                this.backgroundScheduler.remove(this.activeTextDocument);
            }
        });
        // We always pull on open.
        const openFeature = client.getFeature(vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.method);
        disposables.push(openFeature.onNotificationSent((event) => {
            const textDocument = event.original;
            if (matches(textDocument)) {
                this.diagnosticRequestor.pull(textDocument, () => { addToBackgroundIfNeeded(textDocument); });
            }
        }));
        // Pull all diagnostics for documents that are already open
        const pullTextDocuments = new Set();
        for (const textDocument of vscode_1.workspace.textDocuments) {
            if (matches(textDocument)) {
                this.diagnosticRequestor.pull(textDocument, () => { addToBackgroundIfNeeded(textDocument); });
                pullTextDocuments.add(textDocument.uri.toString());
            }
        }
        // Pull all tabs if not already pull as text document
        if (diagnosticPullOptions.onTabs === true) {
            for (const resource of tabs.getTabResources()) {
                if (!pullTextDocuments.has(resource.toString()) && matches(resource)) {
                    this.diagnosticRequestor.pull(resource, () => { addToBackgroundIfNeeded(resource); });
                }
            }
        }
        if (diagnosticPullOptions.onChange === true) {
            const changeFeature = client.getFeature(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.method);
            disposables.push(changeFeature.onNotificationSent(async (event) => {
                const textDocument = event.original.document;
                if ((diagnosticPullOptions.filter === undefined || !diagnosticPullOptions.filter(textDocument, DiagnosticPullMode.onType)) && this.diagnosticRequestor.knows(PullState.document, textDocument) && event.original.contentChanges.length > 0) {
                    this.diagnosticRequestor.pull(textDocument, () => { this.backgroundScheduler.trigger(); });
                }
            }));
        }
        if (diagnosticPullOptions.onSave === true) {
            const saveFeature = client.getFeature(vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.method);
            disposables.push(saveFeature.onNotificationSent((event) => {
                const textDocument = event.original;
                if ((diagnosticPullOptions.filter === undefined || !diagnosticPullOptions.filter(textDocument, DiagnosticPullMode.onSave)) && this.diagnosticRequestor.knows(PullState.document, textDocument)) {
                    this.diagnosticRequestor.pull(event.original, () => { this.backgroundScheduler.trigger(); });
                }
            }));
        }
        // When the document closes clear things up
        const closeFeature = client.getFeature(vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.method);
        disposables.push(closeFeature.onNotificationSent((event) => {
            const textDocument = event.original;
            this.diagnosticRequestor.cleanupPull(textDocument);
            this.backgroundScheduler.remove(textDocument);
        }));
        // We received a did change from the server.
        this.diagnosticRequestor.onDidChangeDiagnosticsEmitter.event(() => {
            for (const textDocument of vscode_1.workspace.textDocuments) {
                if (matches(textDocument)) {
                    this.diagnosticRequestor.pull(textDocument);
                }
            }
        });
        // da348dc5-c30a-4515-9d98-31ff3be38d14 is the test UUID to test the middle ware. So don't auto trigger pulls.
        if (options.workspaceDiagnostics === true && options.identifier !== 'da348dc5-c30a-4515-9d98-31ff3be38d14') {
            this.diagnosticRequestor.pullWorkspace();
        }
        this.disposable = vscode_1.Disposable.from(...disposables, this.backgroundScheduler, this.diagnosticRequestor);
    }
    get onDidChangeDiagnosticsEmitter() {
        return this.diagnosticRequestor.onDidChangeDiagnosticsEmitter;
    }
    get diagnostics() {
        return this.diagnosticRequestor.provider;
    }
}
class DiagnosticFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentDiagnosticRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let capability = ensure(ensure(capabilities, 'textDocument'), 'diagnostic');
        capability.dynamicRegistration = true;
        // We first need to decide how a UI will look with related documents.
        // An easy implementation would be to only show related diagnostics for
        // the active editor.
        capability.relatedDocumentSupport = false;
        ensure(ensure(capabilities, 'workspace'), 'diagnostics').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.DiagnosticRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeDiagnosticsEmitter.fire();
            }
        });
        let [id, options] = this.getRegistration(documentSelector, capabilities.diagnosticProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    dispose() {
        if (this.tabs !== undefined) {
            this.tabs.dispose();
            this.tabs = undefined;
        }
        super.dispose();
    }
    registerLanguageProvider(options) {
        if (this.tabs === undefined) {
            this.tabs = new Tabs();
        }
        const provider = new DiagnosticFeatureProviderImpl(this._client, this.tabs, options);
        return [provider.disposable, provider];
    }
}
exports.DiagnosticFeature = DiagnosticFeature;
//# sourceMappingURL=diagnostic.js.map

/***/ }),
/* 119 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

module.exports = minimatch
minimatch.Minimatch = Minimatch

var path = (function () { try { return __webpack_require__(1) } catch (e) {}}()) || {
  sep: '/'
}
minimatch.sep = path.sep

var GLOBSTAR = minimatch.GLOBSTAR = Minimatch.GLOBSTAR = {}
var expand = __webpack_require__(120)

var plTypes = {
  '!': { open: '(?:(?!(?:', close: '))[^/]*?)'},
  '?': { open: '(?:', close: ')?' },
  '+': { open: '(?:', close: ')+' },
  '*': { open: '(?:', close: ')*' },
  '@': { open: '(?:', close: ')' }
}

// any single thing other than /
// don't need to escape / when using new RegExp()
var qmark = '[^/]'

// * => any number of characters
var star = qmark + '*?'

// ** when dots are allowed.  Anything goes, except .. and .
// not (^ or / followed by one or two dots followed by $ or /),
// followed by anything, any number of times.
var twoStarDot = '(?:(?!(?:\\\/|^)(?:\\.{1,2})($|\\\/)).)*?'

// not a ^ or / followed by a dot,
// followed by anything, any number of times.
var twoStarNoDot = '(?:(?!(?:\\\/|^)\\.).)*?'

// characters that need to be escaped in RegExp.
var reSpecials = charSet('().*{}+?[]^$\\!')

// "abc" -> { a:true, b:true, c:true }
function charSet (s) {
  return s.split('').reduce(function (set, c) {
    set[c] = true
    return set
  }, {})
}

// normalizes slashes.
var slashSplit = /\/+/

minimatch.filter = filter
function filter (pattern, options) {
  options = options || {}
  return function (p, i, list) {
    return minimatch(p, pattern, options)
  }
}

function ext (a, b) {
  b = b || {}
  var t = {}
  Object.keys(a).forEach(function (k) {
    t[k] = a[k]
  })
  Object.keys(b).forEach(function (k) {
    t[k] = b[k]
  })
  return t
}

minimatch.defaults = function (def) {
  if (!def || typeof def !== 'object' || !Object.keys(def).length) {
    return minimatch
  }

  var orig = minimatch

  var m = function minimatch (p, pattern, options) {
    return orig(p, pattern, ext(def, options))
  }

  m.Minimatch = function Minimatch (pattern, options) {
    return new orig.Minimatch(pattern, ext(def, options))
  }
  m.Minimatch.defaults = function defaults (options) {
    return orig.defaults(ext(def, options)).Minimatch
  }

  m.filter = function filter (pattern, options) {
    return orig.filter(pattern, ext(def, options))
  }

  m.defaults = function defaults (options) {
    return orig.defaults(ext(def, options))
  }

  m.makeRe = function makeRe (pattern, options) {
    return orig.makeRe(pattern, ext(def, options))
  }

  m.braceExpand = function braceExpand (pattern, options) {
    return orig.braceExpand(pattern, ext(def, options))
  }

  m.match = function (list, pattern, options) {
    return orig.match(list, pattern, ext(def, options))
  }

  return m
}

Minimatch.defaults = function (def) {
  return minimatch.defaults(def).Minimatch
}

function minimatch (p, pattern, options) {
  assertValidPattern(pattern)

  if (!options) options = {}

  // shortcut: comments match nothing.
  if (!options.nocomment && pattern.charAt(0) === '#') {
    return false
  }

  return new Minimatch(pattern, options).match(p)
}

function Minimatch (pattern, options) {
  if (!(this instanceof Minimatch)) {
    return new Minimatch(pattern, options)
  }

  assertValidPattern(pattern)

  if (!options) options = {}

  pattern = pattern.trim()

  // windows support: need to use /, not \
  if (!options.allowWindowsEscape && path.sep !== '/') {
    pattern = pattern.split(path.sep).join('/')
  }

  this.options = options
  this.set = []
  this.pattern = pattern
  this.regexp = null
  this.negate = false
  this.comment = false
  this.empty = false
  this.partial = !!options.partial

  // make the set of regexps etc.
  this.make()
}

Minimatch.prototype.debug = function () {}

Minimatch.prototype.make = make
function make () {
  var pattern = this.pattern
  var options = this.options

  // empty patterns and comments match nothing.
  if (!options.nocomment && pattern.charAt(0) === '#') {
    this.comment = true
    return
  }
  if (!pattern) {
    this.empty = true
    return
  }

  // step 1: figure out negation, etc.
  this.parseNegate()

  // step 2: expand braces
  var set = this.globSet = this.braceExpand()

  if (options.debug) this.debug = function debug() { console.error.apply(console, arguments) }

  this.debug(this.pattern, set)

  // step 3: now we have a set, so turn each one into a series of path-portion
  // matching patterns.
  // These will be regexps, except in the case of "**", which is
  // set to the GLOBSTAR object for globstar behavior,
  // and will not contain any / characters
  set = this.globParts = set.map(function (s) {
    return s.split(slashSplit)
  })

  this.debug(this.pattern, set)

  // glob --> regexps
  set = set.map(function (s, si, set) {
    return s.map(this.parse, this)
  }, this)

  this.debug(this.pattern, set)

  // filter out everything that didn't compile properly.
  set = set.filter(function (s) {
    return s.indexOf(false) === -1
  })

  this.debug(this.pattern, set)

  this.set = set
}

Minimatch.prototype.parseNegate = parseNegate
function parseNegate () {
  var pattern = this.pattern
  var negate = false
  var options = this.options
  var negateOffset = 0

  if (options.nonegate) return

  for (var i = 0, l = pattern.length
    ; i < l && pattern.charAt(i) === '!'
    ; i++) {
    negate = !negate
    negateOffset++
  }

  if (negateOffset) this.pattern = pattern.substr(negateOffset)
  this.negate = negate
}

// Brace expansion:
// a{b,c}d -> abd acd
// a{b,}c -> abc ac
// a{0..3}d -> a0d a1d a2d a3d
// a{b,c{d,e}f}g -> abg acdfg acefg
// a{b,c}d{e,f}g -> abdeg acdeg abdeg abdfg
//
// Invalid sets are not expanded.
// a{2..}b -> a{2..}b
// a{b}c -> a{b}c
minimatch.braceExpand = function (pattern, options) {
  return braceExpand(pattern, options)
}

Minimatch.prototype.braceExpand = braceExpand

function braceExpand (pattern, options) {
  if (!options) {
    if (this instanceof Minimatch) {
      options = this.options
    } else {
      options = {}
    }
  }

  pattern = typeof pattern === 'undefined'
    ? this.pattern : pattern

  assertValidPattern(pattern)

  // Thanks to Yeting Li <https://github.com/yetingli> for
  // improving this regexp to avoid a ReDOS vulnerability.
  if (options.nobrace || !/\{(?:(?!\{).)*\}/.test(pattern)) {
    // shortcut. no need to expand.
    return [pattern]
  }

  return expand(pattern)
}

var MAX_PATTERN_LENGTH = 1024 * 64
var assertValidPattern = function (pattern) {
  if (typeof pattern !== 'string') {
    throw new TypeError('invalid pattern')
  }

  if (pattern.length > MAX_PATTERN_LENGTH) {
    throw new TypeError('pattern is too long')
  }
}

// parse a component of the expanded set.
// At this point, no pattern may contain "/" in it
// so we're going to return a 2d array, where each entry is the full
// pattern, split on '/', and then turned into a regular expression.
// A regexp is made at the end which joins each array with an
// escaped /, and another full one which joins each regexp with |.
//
// Following the lead of Bash 4.1, note that "**" only has special meaning
// when it is the *only* thing in a path portion.  Otherwise, any series
// of * is equivalent to a single *.  Globstar behavior is enabled by
// default, and can be disabled by setting options.noglobstar.
Minimatch.prototype.parse = parse
var SUBPARSE = {}
function parse (pattern, isSub) {
  assertValidPattern(pattern)

  var options = this.options

  // shortcuts
  if (pattern === '**') {
    if (!options.noglobstar)
      return GLOBSTAR
    else
      pattern = '*'
  }
  if (pattern === '') return ''

  var re = ''
  var hasMagic = !!options.nocase
  var escaping = false
  // ? => one single character
  var patternListStack = []
  var negativeLists = []
  var stateChar
  var inClass = false
  var reClassStart = -1
  var classStart = -1
  // . and .. never match anything that doesn't start with .,
  // even when options.dot is set.
  var patternStart = pattern.charAt(0) === '.' ? '' // anything
  // not (start or / followed by . or .. followed by / or end)
  : options.dot ? '(?!(?:^|\\\/)\\.{1,2}(?:$|\\\/))'
  : '(?!\\.)'
  var self = this

  function clearStateChar () {
    if (stateChar) {
      // we had some state-tracking character
      // that wasn't consumed by this pass.
      switch (stateChar) {
        case '*':
          re += star
          hasMagic = true
        break
        case '?':
          re += qmark
          hasMagic = true
        break
        default:
          re += '\\' + stateChar
        break
      }
      self.debug('clearStateChar %j %j', stateChar, re)
      stateChar = false
    }
  }

  for (var i = 0, len = pattern.length, c
    ; (i < len) && (c = pattern.charAt(i))
    ; i++) {
    this.debug('%s\t%s %s %j', pattern, i, re, c)

    // skip over any that are escaped.
    if (escaping && reSpecials[c]) {
      re += '\\' + c
      escaping = false
      continue
    }

    switch (c) {
      /* istanbul ignore next */
      case '/': {
        // completely not allowed, even escaped.
        // Should already be path-split by now.
        return false
      }

      case '\\':
        clearStateChar()
        escaping = true
      continue

      // the various stateChar values
      // for the "extglob" stuff.
      case '?':
      case '*':
      case '+':
      case '@':
      case '!':
        this.debug('%s\t%s %s %j <-- stateChar', pattern, i, re, c)

        // all of those are literals inside a class, except that
        // the glob [!a] means [^a] in regexp
        if (inClass) {
          this.debug('  in class')
          if (c === '!' && i === classStart + 1) c = '^'
          re += c
          continue
        }

        // if we already have a stateChar, then it means
        // that there was something like ** or +? in there.
        // Handle the stateChar, then proceed with this one.
        self.debug('call clearStateChar %j', stateChar)
        clearStateChar()
        stateChar = c
        // if extglob is disabled, then +(asdf|foo) isn't a thing.
        // just clear the statechar *now*, rather than even diving into
        // the patternList stuff.
        if (options.noext) clearStateChar()
      continue

      case '(':
        if (inClass) {
          re += '('
          continue
        }

        if (!stateChar) {
          re += '\\('
          continue
        }

        patternListStack.push({
          type: stateChar,
          start: i - 1,
          reStart: re.length,
          open: plTypes[stateChar].open,
          close: plTypes[stateChar].close
        })
        // negation is (?:(?!js)[^/]*)
        re += stateChar === '!' ? '(?:(?!(?:' : '(?:'
        this.debug('plType %j %j', stateChar, re)
        stateChar = false
      continue

      case ')':
        if (inClass || !patternListStack.length) {
          re += '\\)'
          continue
        }

        clearStateChar()
        hasMagic = true
        var pl = patternListStack.pop()
        // negation is (?:(?!js)[^/]*)
        // The others are (?:<pattern>)<type>
        re += pl.close
        if (pl.type === '!') {
          negativeLists.push(pl)
        }
        pl.reEnd = re.length
      continue

      case '|':
        if (inClass || !patternListStack.length || escaping) {
          re += '\\|'
          escaping = false
          continue
        }

        clearStateChar()
        re += '|'
      continue

      // these are mostly the same in regexp and glob
      case '[':
        // swallow any state-tracking char before the [
        clearStateChar()

        if (inClass) {
          re += '\\' + c
          continue
        }

        inClass = true
        classStart = i
        reClassStart = re.length
        re += c
      continue

      case ']':
        //  a right bracket shall lose its special
        //  meaning and represent itself in
        //  a bracket expression if it occurs
        //  first in the list.  -- POSIX.2 2.8.3.2
        if (i === classStart + 1 || !inClass) {
          re += '\\' + c
          escaping = false
          continue
        }

        // handle the case where we left a class open.
        // "[z-a]" is valid, equivalent to "\[z-a\]"
        // split where the last [ was, make sure we don't have
        // an invalid re. if so, re-walk the contents of the
        // would-be class to re-translate any characters that
        // were passed through as-is
        // TODO: It would probably be faster to determine this
        // without a try/catch and a new RegExp, but it's tricky
        // to do safely.  For now, this is safe and works.
        var cs = pattern.substring(classStart + 1, i)
        try {
          RegExp('[' + cs + ']')
        } catch (er) {
          // not a valid class!
          var sp = this.parse(cs, SUBPARSE)
          re = re.substr(0, reClassStart) + '\\[' + sp[0] + '\\]'
          hasMagic = hasMagic || sp[1]
          inClass = false
          continue
        }

        // finish up the class.
        hasMagic = true
        inClass = false
        re += c
      continue

      default:
        // swallow any state char that wasn't consumed
        clearStateChar()

        if (escaping) {
          // no need
          escaping = false
        } else if (reSpecials[c]
          && !(c === '^' && inClass)) {
          re += '\\'
        }

        re += c

    } // switch
  } // for

  // handle the case where we left a class open.
  // "[abc" is valid, equivalent to "\[abc"
  if (inClass) {
    // split where the last [ was, and escape it
    // this is a huge pita.  We now have to re-walk
    // the contents of the would-be class to re-translate
    // any characters that were passed through as-is
    cs = pattern.substr(classStart + 1)
    sp = this.parse(cs, SUBPARSE)
    re = re.substr(0, reClassStart) + '\\[' + sp[0]
    hasMagic = hasMagic || sp[1]
  }

  // handle the case where we had a +( thing at the *end*
  // of the pattern.
  // each pattern list stack adds 3 chars, and we need to go through
  // and escape any | chars that were passed through as-is for the regexp.
  // Go through and escape them, taking care not to double-escape any
  // | chars that were already escaped.
  for (pl = patternListStack.pop(); pl; pl = patternListStack.pop()) {
    var tail = re.slice(pl.reStart + pl.open.length)
    this.debug('setting tail', re, pl)
    // maybe some even number of \, then maybe 1 \, followed by a |
    tail = tail.replace(/((?:\\{2}){0,64})(\\?)\|/g, function (_, $1, $2) {
      if (!$2) {
        // the | isn't already escaped, so escape it.
        $2 = '\\'
      }

      // need to escape all those slashes *again*, without escaping the
      // one that we need for escaping the | character.  As it works out,
      // escaping an even number of slashes can be done by simply repeating
      // it exactly after itself.  That's why this trick works.
      //
      // I am sorry that you have to see this.
      return $1 + $1 + $2 + '|'
    })

    this.debug('tail=%j\n   %s', tail, tail, pl, re)
    var t = pl.type === '*' ? star
      : pl.type === '?' ? qmark
      : '\\' + pl.type

    hasMagic = true
    re = re.slice(0, pl.reStart) + t + '\\(' + tail
  }

  // handle trailing things that only matter at the very end.
  clearStateChar()
  if (escaping) {
    // trailing \\
    re += '\\\\'
  }

  // only need to apply the nodot start if the re starts with
  // something that could conceivably capture a dot
  var addPatternStart = false
  switch (re.charAt(0)) {
    case '[': case '.': case '(': addPatternStart = true
  }

  // Hack to work around lack of negative lookbehind in JS
  // A pattern like: *.!(x).!(y|z) needs to ensure that a name
  // like 'a.xyz.yz' doesn't match.  So, the first negative
  // lookahead, has to look ALL the way ahead, to the end of
  // the pattern.
  for (var n = negativeLists.length - 1; n > -1; n--) {
    var nl = negativeLists[n]

    var nlBefore = re.slice(0, nl.reStart)
    var nlFirst = re.slice(nl.reStart, nl.reEnd - 8)
    var nlLast = re.slice(nl.reEnd - 8, nl.reEnd)
    var nlAfter = re.slice(nl.reEnd)

    nlLast += nlAfter

    // Handle nested stuff like *(*.js|!(*.json)), where open parens
    // mean that we should *not* include the ) in the bit that is considered
    // "after" the negated section.
    var openParensBefore = nlBefore.split('(').length - 1
    var cleanAfter = nlAfter
    for (i = 0; i < openParensBefore; i++) {
      cleanAfter = cleanAfter.replace(/\)[+*?]?/, '')
    }
    nlAfter = cleanAfter

    var dollar = ''
    if (nlAfter === '' && isSub !== SUBPARSE) {
      dollar = '$'
    }
    var newRe = nlBefore + nlFirst + nlAfter + dollar + nlLast
    re = newRe
  }

  // if the re is not "" at this point, then we need to make sure
  // it doesn't match against an empty path part.
  // Otherwise a/* will match a/, which it should not.
  if (re !== '' && hasMagic) {
    re = '(?=.)' + re
  }

  if (addPatternStart) {
    re = patternStart + re
  }

  // parsing just a piece of a larger pattern.
  if (isSub === SUBPARSE) {
    return [re, hasMagic]
  }

  // skip the regexp for non-magical patterns
  // unescape anything in it, though, so that it'll be
  // an exact match against a file etc.
  if (!hasMagic) {
    return globUnescape(pattern)
  }

  var flags = options.nocase ? 'i' : ''
  try {
    var regExp = new RegExp('^' + re + '$', flags)
  } catch (er) /* istanbul ignore next - should be impossible */ {
    // If it was an invalid regular expression, then it can't match
    // anything.  This trick looks for a character after the end of
    // the string, which is of course impossible, except in multi-line
    // mode, but it's not a /m regex.
    return new RegExp('$.')
  }

  regExp._glob = pattern
  regExp._src = re

  return regExp
}

minimatch.makeRe = function (pattern, options) {
  return new Minimatch(pattern, options || {}).makeRe()
}

Minimatch.prototype.makeRe = makeRe
function makeRe () {
  if (this.regexp || this.regexp === false) return this.regexp

  // at this point, this.set is a 2d array of partial
  // pattern strings, or "**".
  //
  // It's better to use .match().  This function shouldn't
  // be used, really, but it's pretty convenient sometimes,
  // when you just want to work with a regex.
  var set = this.set

  if (!set.length) {
    this.regexp = false
    return this.regexp
  }
  var options = this.options

  var twoStar = options.noglobstar ? star
    : options.dot ? twoStarDot
    : twoStarNoDot
  var flags = options.nocase ? 'i' : ''

  var re = set.map(function (pattern) {
    return pattern.map(function (p) {
      return (p === GLOBSTAR) ? twoStar
      : (typeof p === 'string') ? regExpEscape(p)
      : p._src
    }).join('\\\/')
  }).join('|')

  // must match entire pattern
  // ending in a * or ** will make it less strict.
  re = '^(?:' + re + ')$'

  // can match anything, as long as it's not this.
  if (this.negate) re = '^(?!' + re + ').*$'

  try {
    this.regexp = new RegExp(re, flags)
  } catch (ex) /* istanbul ignore next - should be impossible */ {
    this.regexp = false
  }
  return this.regexp
}

minimatch.match = function (list, pattern, options) {
  options = options || {}
  var mm = new Minimatch(pattern, options)
  list = list.filter(function (f) {
    return mm.match(f)
  })
  if (mm.options.nonull && !list.length) {
    list.push(pattern)
  }
  return list
}

Minimatch.prototype.match = function match (f, partial) {
  if (typeof partial === 'undefined') partial = this.partial
  this.debug('match', f, this.pattern)
  // short-circuit in the case of busted things.
  // comments, etc.
  if (this.comment) return false
  if (this.empty) return f === ''

  if (f === '/' && partial) return true

  var options = this.options

  // windows: need to use /, not \
  if (path.sep !== '/') {
    f = f.split(path.sep).join('/')
  }

  // treat the test path as a set of pathparts.
  f = f.split(slashSplit)
  this.debug(this.pattern, 'split', f)

  // just ONE of the pattern sets in this.set needs to match
  // in order for it to be valid.  If negating, then just one
  // match means that we have failed.
  // Either way, return on the first hit.

  var set = this.set
  this.debug(this.pattern, 'set', set)

  // Find the basename of the path by looking for the last non-empty segment
  var filename
  var i
  for (i = f.length - 1; i >= 0; i--) {
    filename = f[i]
    if (filename) break
  }

  for (i = 0; i < set.length; i++) {
    var pattern = set[i]
    var file = f
    if (options.matchBase && pattern.length === 1) {
      file = [filename]
    }
    var hit = this.matchOne(file, pattern, partial)
    if (hit) {
      if (options.flipNegate) return true
      return !this.negate
    }
  }

  // didn't get any hits.  this is success if it's a negative
  // pattern, failure otherwise.
  if (options.flipNegate) return false
  return this.negate
}

// set partial to true to test if, for example,
// "/a/b" matches the start of "/*/b/*/d"
// Partial means, if you run out of file before you run
// out of pattern, then that's fine, as long as all
// the parts match.
Minimatch.prototype.matchOne = function (file, pattern, partial) {
  var options = this.options

  this.debug('matchOne',
    { 'this': this, file: file, pattern: pattern })

  this.debug('matchOne', file.length, pattern.length)

  for (var fi = 0,
      pi = 0,
      fl = file.length,
      pl = pattern.length
      ; (fi < fl) && (pi < pl)
      ; fi++, pi++) {
    this.debug('matchOne loop')
    var p = pattern[pi]
    var f = file[fi]

    this.debug(pattern, p, f)

    // should be impossible.
    // some invalid regexp stuff in the set.
    /* istanbul ignore if */
    if (p === false) return false

    if (p === GLOBSTAR) {
      this.debug('GLOBSTAR', [pattern, p, f])

      // "**"
      // a/**/b/**/c would match the following:
      // a/b/x/y/z/c
      // a/x/y/z/b/c
      // a/b/x/b/x/c
      // a/b/c
      // To do this, take the rest of the pattern after
      // the **, and see if it would match the file remainder.
      // If so, return success.
      // If not, the ** "swallows" a segment, and try again.
      // This is recursively awful.
      //
      // a/**/b/**/c matching a/b/x/y/z/c
      // - a matches a
      // - doublestar
      //   - matchOne(b/x/y/z/c, b/**/c)
      //     - b matches b
      //     - doublestar
      //       - matchOne(x/y/z/c, c) -> no
      //       - matchOne(y/z/c, c) -> no
      //       - matchOne(z/c, c) -> no
      //       - matchOne(c, c) yes, hit
      var fr = fi
      var pr = pi + 1
      if (pr === pl) {
        this.debug('** at the end')
        // a ** at the end will just swallow the rest.
        // We have found a match.
        // however, it will not swallow /.x, unless
        // options.dot is set.
        // . and .. are *never* matched by **, for explosively
        // exponential reasons.
        for (; fi < fl; fi++) {
          if (file[fi] === '.' || file[fi] === '..' ||
            (!options.dot && file[fi].charAt(0) === '.')) return false
        }
        return true
      }

      // ok, let's see if we can swallow whatever we can.
      while (fr < fl) {
        var swallowee = file[fr]

        this.debug('\nglobstar while', file, fr, pattern, pr, swallowee)

        // XXX remove this slice.  Just pass the start index.
        if (this.matchOne(file.slice(fr), pattern.slice(pr), partial)) {
          this.debug('globstar found match!', fr, fl, swallowee)
          // found a match.
          return true
        } else {
          // can't swallow "." or ".." ever.
          // can only swallow ".foo" when explicitly asked.
          if (swallowee === '.' || swallowee === '..' ||
            (!options.dot && swallowee.charAt(0) === '.')) {
            this.debug('dot detected!', file, fr, pattern, pr)
            break
          }

          // ** swallows a segment, and continue.
          this.debug('globstar swallow a segment, and continue')
          fr++
        }
      }

      // no match was found.
      // However, in partial mode, we can't say this is necessarily over.
      // If there's more *pattern* left, then
      /* istanbul ignore if */
      if (partial) {
        // ran out of file
        this.debug('\n>>> no match, partial?', file, fr, pattern, pr)
        if (fr === fl) return true
      }
      return false
    }

    // something other than **
    // non-magic patterns just have to match exactly
    // patterns with magic have been turned into regexps.
    var hit
    if (typeof p === 'string') {
      hit = f === p
      this.debug('string match', p, f, hit)
    } else {
      hit = f.match(p)
      this.debug('pattern match', p, f, hit)
    }

    if (!hit) return false
  }

  // Note: ending in / means that we'll get a final ""
  // at the end of the pattern.  This can only match a
  // corresponding "" at the end of the file.
  // If the file ends in /, then it can only match a
  // a pattern that ends in /, unless the pattern just
  // doesn't have any more for it. But, a/b/ should *not*
  // match "a/b/*", even though "" matches against the
  // [^/]*? pattern, except in partial mode, where it might
  // simply not be reached yet.
  // However, a/b/ should still satisfy a/*

  // now either we fell off the end of the pattern, or we're done.
  if (fi === fl && pi === pl) {
    // ran out of pattern and filename at the same time.
    // an exact hit!
    return true
  } else if (fi === fl) {
    // ran out of file, but still had pattern left.
    // this is ok if we're doing the match as part of
    // a glob fs traversal.
    return partial
  } else /* istanbul ignore else */ if (pi === pl) {
    // ran out of pattern, still have file left.
    // this is only acceptable if we're on the very last
    // empty segment of a file with a trailing slash.
    // a/* should match a/b/
    return (fi === fl - 1) && (file[fi] === '')
  }

  // should be unreachable.
  /* istanbul ignore next */
  throw new Error('wtf?')
}

// replace stuff like \* with *
function globUnescape (s) {
  return s.replace(/\\(.)/g, '$1')
}

function regExpEscape (s) {
  return s.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&')
}


/***/ }),
/* 120 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

var concatMap = __webpack_require__(121);
var balanced = __webpack_require__(122);

module.exports = expandTop;

var escSlash = '\0SLASH'+Math.random()+'\0';
var escOpen = '\0OPEN'+Math.random()+'\0';
var escClose = '\0CLOSE'+Math.random()+'\0';
var escComma = '\0COMMA'+Math.random()+'\0';
var escPeriod = '\0PERIOD'+Math.random()+'\0';

function numeric(str) {
  return parseInt(str, 10) == str
    ? parseInt(str, 10)
    : str.charCodeAt(0);
}

function escapeBraces(str) {
  return str.split('\\\\').join(escSlash)
            .split('\\{').join(escOpen)
            .split('\\}').join(escClose)
            .split('\\,').join(escComma)
            .split('\\.').join(escPeriod);
}

function unescapeBraces(str) {
  return str.split(escSlash).join('\\')
            .split(escOpen).join('{')
            .split(escClose).join('}')
            .split(escComma).join(',')
            .split(escPeriod).join('.');
}


// Basically just str.split(","), but handling cases
// where we have nested braced sections, which should be
// treated as individual members, like {a,{b,c},d}
function parseCommaParts(str) {
  if (!str)
    return [''];

  var parts = [];
  var m = balanced('{', '}', str);

  if (!m)
    return str.split(',');

  var pre = m.pre;
  var body = m.body;
  var post = m.post;
  var p = pre.split(',');

  p[p.length-1] += '{' + body + '}';
  var postParts = parseCommaParts(post);
  if (post.length) {
    p[p.length-1] += postParts.shift();
    p.push.apply(p, postParts);
  }

  parts.push.apply(parts, p);

  return parts;
}

function expandTop(str) {
  if (!str)
    return [];

  // I don't know why Bash 4.3 does this, but it does.
  // Anything starting with {} will have the first two bytes preserved
  // but *only* at the top level, so {},a}b will not expand to anything,
  // but a{},b}c will be expanded to [a}c,abc].
  // One could argue that this is a bug in Bash, but since the goal of
  // this module is to match Bash's rules, we escape a leading {}
  if (str.substr(0, 2) === '{}') {
    str = '\\{\\}' + str.substr(2);
  }

  return expand(escapeBraces(str), true).map(unescapeBraces);
}

function identity(e) {
  return e;
}

function embrace(str) {
  return '{' + str + '}';
}
function isPadded(el) {
  return /^-?0\d/.test(el);
}

function lte(i, y) {
  return i <= y;
}
function gte(i, y) {
  return i >= y;
}

function expand(str, isTop) {
  var expansions = [];

  var m = balanced('{', '}', str);
  if (!m || /\$$/.test(m.pre)) return [str];

  var isNumericSequence = /^-?\d+\.\.-?\d+(?:\.\.-?\d+)?$/.test(m.body);
  var isAlphaSequence = /^[a-zA-Z]\.\.[a-zA-Z](?:\.\.-?\d+)?$/.test(m.body);
  var isSequence = isNumericSequence || isAlphaSequence;
  var isOptions = m.body.indexOf(',') >= 0;
  if (!isSequence && !isOptions) {
    // {a},b}
    if (m.post.match(/,.*\}/)) {
      str = m.pre + '{' + m.body + escClose + m.post;
      return expand(str);
    }
    return [str];
  }

  var n;
  if (isSequence) {
    n = m.body.split(/\.\./);
  } else {
    n = parseCommaParts(m.body);
    if (n.length === 1) {
      // x{{a,b}}y ==> x{a}y x{b}y
      n = expand(n[0], false).map(embrace);
      if (n.length === 1) {
        var post = m.post.length
          ? expand(m.post, false)
          : [''];
        return post.map(function(p) {
          return m.pre + n[0] + p;
        });
      }
    }
  }

  // at this point, n is the parts, and we know it's not a comma set
  // with a single entry.

  // no need to expand pre, since it is guaranteed to be free of brace-sets
  var pre = m.pre;
  var post = m.post.length
    ? expand(m.post, false)
    : [''];

  var N;

  if (isSequence) {
    var x = numeric(n[0]);
    var y = numeric(n[1]);
    var width = Math.max(n[0].length, n[1].length)
    var incr = n.length == 3
      ? Math.abs(numeric(n[2]))
      : 1;
    var test = lte;
    var reverse = y < x;
    if (reverse) {
      incr *= -1;
      test = gte;
    }
    var pad = n.some(isPadded);

    N = [];

    for (var i = x; test(i, y); i += incr) {
      var c;
      if (isAlphaSequence) {
        c = String.fromCharCode(i);
        if (c === '\\')
          c = '';
      } else {
        c = String(i);
        if (pad) {
          var need = width - c.length;
          if (need > 0) {
            var z = new Array(need + 1).join('0');
            if (i < 0)
              c = '-' + z + c.slice(1);
            else
              c = z + c;
          }
        }
      }
      N.push(c);
    }
  } else {
    N = concatMap(n, function(el) { return expand(el, false) });
  }

  for (var j = 0; j < N.length; j++) {
    for (var k = 0; k < post.length; k++) {
      var expansion = pre + N[j] + post[k];
      if (!isTop || isSequence || expansion)
        expansions.push(expansion);
    }
  }

  return expansions;
}



/***/ }),
/* 121 */
/***/ ((module) => {

module.exports = function (xs, fn) {
    var res = [];
    for (var i = 0; i < xs.length; i++) {
        var x = fn(xs[i], i);
        if (isArray(x)) res.push.apply(res, x);
        else res.push(x);
    }
    return res;
};

var isArray = Array.isArray || function (xs) {
    return Object.prototype.toString.call(xs) === '[object Array]';
};


/***/ }),
/* 122 */
/***/ ((module) => {

"use strict";

module.exports = balanced;
function balanced(a, b, str) {
  if (a instanceof RegExp) a = maybeMatch(a, str);
  if (b instanceof RegExp) b = maybeMatch(b, str);

  var r = range(a, b, str);

  return r && {
    start: r[0],
    end: r[1],
    pre: str.slice(0, r[0]),
    body: str.slice(r[0] + a.length, r[1]),
    post: str.slice(r[1] + b.length)
  };
}

function maybeMatch(reg, str) {
  var m = str.match(reg);
  return m ? m[0] : null;
}

balanced.range = range;
function range(a, b, str) {
  var begs, beg, left, right, result;
  var ai = str.indexOf(a);
  var bi = str.indexOf(b, ai + 1);
  var i = ai;

  if (ai >= 0 && bi > 0) {
    if(a===b) {
      return [ai, bi];
    }
    begs = [];
    left = str.length;

    while (i >= 0 && !result) {
      if (i == ai) {
        begs.push(i);
        ai = str.indexOf(a, i + 1);
      } else if (begs.length == 1) {
        result = [ begs.pop(), bi ];
      } else {
        beg = begs.pop();
        if (beg < left) {
          left = beg;
          right = bi;
        }

        bi = str.indexOf(b, i + 1);
      }

      i = ai < bi && ai >= 0 ? ai : bi;
    }

    if (begs.length) {
      result = [ left, right ];
    }
  }

  return result;
}


/***/ }),
/* 123 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.NotebookDocumentSyncFeature = void 0;
const vscode = __webpack_require__(53);
const minimatch = __webpack_require__(119);
const proto = __webpack_require__(56);
const UUID = __webpack_require__(115);
const Is = __webpack_require__(54);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
var Converter;
(function (Converter) {
    let c2p;
    (function (c2p) {
        function asVersionedNotebookDocumentIdentifier(notebookDocument, base) {
            return {
                version: notebookDocument.version,
                uri: base.asUri(notebookDocument.uri)
            };
        }
        c2p.asVersionedNotebookDocumentIdentifier = asVersionedNotebookDocumentIdentifier;
        function asNotebookDocument(notebookDocument, cells, base) {
            const result = proto.NotebookDocument.create(base.asUri(notebookDocument.uri), notebookDocument.notebookType, notebookDocument.version, asNotebookCells(cells, base));
            if (Object.keys(notebookDocument.metadata).length > 0) {
                result.metadata = asMetadata(notebookDocument.metadata);
            }
            return result;
        }
        c2p.asNotebookDocument = asNotebookDocument;
        function asNotebookCells(cells, base) {
            return cells.map(cell => asNotebookCell(cell, base));
        }
        c2p.asNotebookCells = asNotebookCells;
        function asMetadata(metadata) {
            const seen = new Set();
            return deepCopy(seen, metadata);
        }
        c2p.asMetadata = asMetadata;
        function asNotebookCell(cell, base) {
            const result = proto.NotebookCell.create(asNotebookCellKind(cell.kind), base.asUri(cell.document.uri));
            if (Object.keys(cell.metadata).length > 0) {
                result.metadata = asMetadata(cell.metadata);
            }
            if (cell.executionSummary !== undefined && (Is.number(cell.executionSummary.executionOrder) && Is.boolean(cell.executionSummary.success))) {
                result.executionSummary = {
                    executionOrder: cell.executionSummary.executionOrder,
                    success: cell.executionSummary.success
                };
            }
            return result;
        }
        c2p.asNotebookCell = asNotebookCell;
        function asNotebookCellKind(kind) {
            switch (kind) {
                case vscode.NotebookCellKind.Markup:
                    return proto.NotebookCellKind.Markup;
                case vscode.NotebookCellKind.Code:
                    return proto.NotebookCellKind.Code;
            }
        }
        function deepCopy(seen, value) {
            if (seen.has(value)) {
                throw new Error(`Can't deep copy cyclic structures.`);
            }
            if (Array.isArray(value)) {
                const result = [];
                for (const elem of value) {
                    if (elem !== null && typeof elem === 'object' || Array.isArray(elem)) {
                        result.push(deepCopy(seen, elem));
                    }
                    else {
                        if (elem instanceof RegExp) {
                            throw new Error(`Can't transfer regular expressions to the server`);
                        }
                        result.push(elem);
                    }
                }
                return result;
            }
            else {
                const props = Object.keys(value);
                const result = Object.create(null);
                for (const prop of props) {
                    const elem = value[prop];
                    if (elem !== null && typeof elem === 'object' || Array.isArray(elem)) {
                        result[prop] = deepCopy(seen, elem);
                    }
                    else {
                        if (elem instanceof RegExp) {
                            throw new Error(`Can't transfer regular expressions to the server`);
                        }
                        result[prop] = elem;
                    }
                }
                return result;
            }
        }
        function asTextContentChange(event, base) {
            const params = base.asChangeTextDocumentParams(event);
            return { document: params.textDocument, changes: params.contentChanges };
        }
        c2p.asTextContentChange = asTextContentChange;
        function asNotebookDocumentChangeEvent(event, base) {
            const result = Object.create(null);
            if (event.metadata) {
                result.metadata = Converter.c2p.asMetadata(event.metadata);
            }
            if (event.cells !== undefined) {
                const cells = Object.create(null);
                const changedCells = event.cells;
                if (changedCells.structure) {
                    cells.structure = {
                        array: {
                            start: changedCells.structure.array.start,
                            deleteCount: changedCells.structure.array.deleteCount,
                            cells: changedCells.structure.array.cells !== undefined ? changedCells.structure.array.cells.map(cell => Converter.c2p.asNotebookCell(cell, base)) : undefined
                        },
                        didOpen: changedCells.structure.didOpen !== undefined
                            ? changedCells.structure.didOpen.map(cell => base.asOpenTextDocumentParams(cell.document).textDocument)
                            : undefined,
                        didClose: changedCells.structure.didClose !== undefined
                            ? changedCells.structure.didClose.map(cell => base.asCloseTextDocumentParams(cell.document).textDocument)
                            : undefined
                    };
                }
                if (changedCells.data !== undefined) {
                    cells.data = changedCells.data.map(cell => Converter.c2p.asNotebookCell(cell, base));
                }
                if (changedCells.textContent !== undefined) {
                    cells.textContent = changedCells.textContent.map(event => Converter.c2p.asTextContentChange(event, base));
                }
                if (Object.keys(cells).length > 0) {
                    result.cells = cells;
                }
            }
            return result;
        }
        c2p.asNotebookDocumentChangeEvent = asNotebookDocumentChangeEvent;
    })(c2p = Converter.c2p || (Converter.c2p = {}));
})(Converter || (Converter = {}));
var $NotebookCell;
(function ($NotebookCell) {
    function computeDiff(originalCells, modifiedCells, compareMetadata) {
        const originalLength = originalCells.length;
        const modifiedLength = modifiedCells.length;
        let startIndex = 0;
        while (startIndex < modifiedLength && startIndex < originalLength && equals(originalCells[startIndex], modifiedCells[startIndex], compareMetadata)) {
            startIndex++;
        }
        if (startIndex < modifiedLength && startIndex < originalLength) {
            let originalEndIndex = originalLength - 1;
            let modifiedEndIndex = modifiedLength - 1;
            while (originalEndIndex >= 0 && modifiedEndIndex >= 0 && equals(originalCells[originalEndIndex], modifiedCells[modifiedEndIndex], compareMetadata)) {
                originalEndIndex--;
                modifiedEndIndex--;
            }
            const deleteCount = (originalEndIndex + 1) - startIndex;
            const newCells = startIndex === modifiedEndIndex + 1 ? undefined : modifiedCells.slice(startIndex, modifiedEndIndex + 1);
            return newCells !== undefined ? { start: startIndex, deleteCount, cells: newCells } : { start: startIndex, deleteCount };
        }
        else if (startIndex < modifiedLength) {
            return { start: startIndex, deleteCount: 0, cells: modifiedCells.slice(startIndex) };
        }
        else if (startIndex < originalLength) {
            return { start: startIndex, deleteCount: originalLength - startIndex };
        }
        else {
            // The two arrays are the same.
            return undefined;
        }
    }
    $NotebookCell.computeDiff = computeDiff;
    /**
     * We only sync kind, document, execution and metadata to the server. So we only need to compare those.
     */
    function equals(one, other, compareMetaData = true) {
        if (one.kind !== other.kind || one.document.uri.toString() !== other.document.uri.toString() || one.document.languageId !== other.document.languageId ||
            !equalsExecution(one.executionSummary, other.executionSummary)) {
            return false;
        }
        return !compareMetaData || (compareMetaData && equalsMetadata(one.metadata, other.metadata));
    }
    function equalsExecution(one, other) {
        if (one === other) {
            return true;
        }
        if (one === undefined || other === undefined) {
            return false;
        }
        return one.executionOrder === other.executionOrder && one.success === other.success && equalsTiming(one.timing, other.timing);
    }
    function equalsTiming(one, other) {
        if (one === other) {
            return true;
        }
        if (one === undefined || other === undefined) {
            return false;
        }
        return one.startTime === other.startTime && one.endTime === other.endTime;
    }
    function equalsMetadata(one, other) {
        if (one === other) {
            return true;
        }
        if (one === null || one === undefined || other === null || other === undefined) {
            return false;
        }
        if (typeof one !== typeof other) {
            return false;
        }
        if (typeof one !== 'object') {
            return false;
        }
        const oneArray = Array.isArray(one);
        const otherArray = Array.isArray(other);
        if (oneArray !== otherArray) {
            return false;
        }
        if (oneArray && otherArray) {
            if (one.length !== other.length) {
                return false;
            }
            for (let i = 0; i < one.length; i++) {
                if (!equalsMetadata(one[i], other[i])) {
                    return false;
                }
            }
        }
        if (isObjectLiteral(one) && isObjectLiteral(other)) {
            const oneKeys = Object.keys(one);
            const otherKeys = Object.keys(other);
            if (oneKeys.length !== otherKeys.length) {
                return false;
            }
            oneKeys.sort();
            otherKeys.sort();
            if (!equalsMetadata(oneKeys, otherKeys)) {
                return false;
            }
            for (let i = 0; i < oneKeys.length; i++) {
                const prop = oneKeys[i];
                if (!equalsMetadata(one[prop], other[prop])) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }
    function isObjectLiteral(value) {
        return value !== null && typeof value === 'object';
    }
    $NotebookCell.isObjectLiteral = isObjectLiteral;
})($NotebookCell || ($NotebookCell = {}));
var $NotebookDocumentFilter;
(function ($NotebookDocumentFilter) {
    function matchNotebook(filter, notebookDocument) {
        if (typeof filter === 'string') {
            return filter === '*' || notebookDocument.notebookType === filter;
        }
        if (filter.notebookType !== undefined && filter.notebookType !== '*' && notebookDocument.notebookType !== filter.notebookType) {
            return false;
        }
        const uri = notebookDocument.uri;
        if (filter.scheme !== undefined && filter.scheme !== '*' && uri.scheme !== filter.scheme) {
            return false;
        }
        if (filter.pattern !== undefined) {
            const matcher = new minimatch.Minimatch(filter.pattern, { noext: true });
            if (!matcher.makeRe()) {
                return false;
            }
            if (!matcher.match(uri.fsPath)) {
                return false;
            }
        }
        return true;
    }
    $NotebookDocumentFilter.matchNotebook = matchNotebook;
})($NotebookDocumentFilter || ($NotebookDocumentFilter = {}));
var $NotebookDocumentSyncOptions;
(function ($NotebookDocumentSyncOptions) {
    function asDocumentSelector(options) {
        const selector = options.notebookSelector;
        const result = [];
        for (const element of selector) {
            const notebookType = (typeof element.notebook === 'string' ? element.notebook : element.notebook?.notebookType) ?? '*';
            const scheme = (typeof element.notebook === 'string') ? undefined : element.notebook?.scheme;
            const pattern = (typeof element.notebook === 'string') ? undefined : element.notebook?.pattern;
            if (element.cells !== undefined) {
                for (const cell of element.cells) {
                    result.push(asDocumentFilter(notebookType, scheme, pattern, cell.language));
                }
            }
            else {
                result.push(asDocumentFilter(notebookType, scheme, pattern, undefined));
            }
        }
        return result;
    }
    $NotebookDocumentSyncOptions.asDocumentSelector = asDocumentSelector;
    function asDocumentFilter(notebookType, scheme, pattern, language) {
        return scheme === undefined && pattern === undefined
            ? { notebook: notebookType, language }
            : { notebook: { notebookType, scheme, pattern }, language };
    }
})($NotebookDocumentSyncOptions || ($NotebookDocumentSyncOptions = {}));
var SyncInfo;
(function (SyncInfo) {
    function create(cells) {
        return {
            cells,
            uris: new Set(cells.map(cell => cell.document.uri.toString()))
        };
    }
    SyncInfo.create = create;
})(SyncInfo || (SyncInfo = {}));
class NotebookDocumentSyncFeatureProvider {
    constructor(client, options) {
        this.client = client;
        this.options = options;
        this.notebookSyncInfo = new Map();
        this.notebookDidOpen = new Set();
        this.disposables = [];
        this.selector = client.protocol2CodeConverter.asDocumentSelector($NotebookDocumentSyncOptions.asDocumentSelector(options));
        // open
        vscode.workspace.onDidOpenNotebookDocument((notebookDocument) => {
            this.notebookDidOpen.add(notebookDocument.uri.toString());
            this.didOpen(notebookDocument);
        }, undefined, this.disposables);
        for (const notebookDocument of vscode.workspace.notebookDocuments) {
            this.notebookDidOpen.add(notebookDocument.uri.toString());
            this.didOpen(notebookDocument);
        }
        // Notebook document changed.
        vscode.workspace.onDidChangeNotebookDocument(event => this.didChangeNotebookDocument(event), undefined, this.disposables);
        //save
        if (this.options.save === true) {
            vscode.workspace.onDidSaveNotebookDocument(notebookDocument => this.didSave(notebookDocument), undefined, this.disposables);
        }
        // close
        vscode.workspace.onDidCloseNotebookDocument((notebookDocument) => {
            this.didClose(notebookDocument);
            this.notebookDidOpen.delete(notebookDocument.uri.toString());
        }, undefined, this.disposables);
    }
    getState() {
        for (const notebook of vscode.workspace.notebookDocuments) {
            const matchingCells = this.getMatchingCells(notebook);
            if (matchingCells !== undefined) {
                return { kind: 'document', id: '$internal', registrations: true, matches: true };
            }
        }
        return { kind: 'document', id: '$internal', registrations: true, matches: false };
    }
    get mode() {
        return 'notebook';
    }
    handles(textDocument) {
        return vscode.languages.match(this.selector, textDocument) > 0;
    }
    didOpenNotebookCellTextDocument(notebookDocument, cell) {
        if (vscode.languages.match(this.selector, cell.document) === 0) {
            return;
        }
        if (!this.notebookDidOpen.has(notebookDocument.uri.toString())) {
            // We have never received an open notification for the notebook document.
            // VS Code guarantees that we first get cell document open and then
            // notebook open. So simply wait for the notebook open.
            return;
        }
        const syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString());
        // In VS Code we receive a notebook open before a cell document open.
        // The document and the cell is synced.
        const cellMatches = this.cellMatches(notebookDocument, cell);
        if (syncInfo !== undefined) {
            const cellIsSynced = syncInfo.uris.has(cell.document.uri.toString());
            if ((cellMatches && cellIsSynced) || (!cellMatches && !cellIsSynced)) {
                // The cell doesn't match and was not synced or it matches and is synced.
                // In both cases nothing to do.
                //
                // Note that if the language mode of a document changes we remove the
                // cell and add it back to update the language mode on the server side.
                return;
            }
            if (cellMatches) {
                // don't use cells from above since there might be more matching cells in the notebook
                // Since we had a matching cell above we will have matching cells now.
                const matchingCells = this.getMatchingCells(notebookDocument);
                if (matchingCells !== undefined) {
                    const event = this.asNotebookDocumentChangeEvent(notebookDocument, undefined, syncInfo, matchingCells);
                    if (event !== undefined) {
                        this.doSendChange(event, matchingCells).catch(() => { });
                    }
                }
            }
        }
        else {
            // No sync info. But we have a open event for the notebook document
            // itself. If the cell matches then we need to send an open with
            // exactly that cell.
            if (cellMatches) {
                this.doSendOpen(notebookDocument, [cell]).catch(() => { });
            }
        }
    }
    didChangeNotebookCellTextDocument(notebookDocument, event) {
        // No match with the selector
        if (vscode.languages.match(this.selector, event.document) === 0) {
            return;
        }
        this.doSendChange({
            notebook: notebookDocument,
            cells: { textContent: [event] }
        }, undefined).catch(() => { });
    }
    didCloseNotebookCellTextDocument(notebookDocument, cell) {
        const syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString());
        if (syncInfo === undefined) {
            // The notebook document got never synced. So it doesn't matter if a cell
            // document closes.
            return;
        }
        const cellUri = cell.document.uri;
        const index = syncInfo.cells.findIndex((item) => item.document.uri.toString() === cellUri.toString());
        if (index === -1) {
            // The cell never got synced or it got deleted and we now received the document
            // close event.
            return;
        }
        if (index === 0 && syncInfo.cells.length === 1) {
            // The last cell. Close the notebook document in the server.
            this.doSendClose(notebookDocument, syncInfo.cells).catch(() => { });
        }
        else {
            const newCells = syncInfo.cells.slice();
            const deleted = newCells.splice(index, 1);
            this.doSendChange({
                notebook: notebookDocument,
                cells: {
                    structure: {
                        array: { start: index, deleteCount: 1 },
                        didClose: deleted
                    }
                }
            }, newCells).catch(() => { });
        }
    }
    dispose() {
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
    }
    didOpen(notebookDocument, matchingCells = this.getMatchingCells(notebookDocument), syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString())) {
        if (syncInfo !== undefined) {
            if (matchingCells !== undefined) {
                const event = this.asNotebookDocumentChangeEvent(notebookDocument, undefined, syncInfo, matchingCells);
                if (event !== undefined) {
                    this.doSendChange(event, matchingCells).catch(() => { });
                }
            }
            else {
                this.doSendClose(notebookDocument, []).catch(() => { });
            }
        }
        else {
            // Check if we need to sync the notebook document.
            if (matchingCells === undefined) {
                return;
            }
            this.doSendOpen(notebookDocument, matchingCells).catch(() => { });
        }
    }
    didChangeNotebookDocument(event) {
        const notebookDocument = event.notebook;
        const syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString());
        if (syncInfo === undefined) {
            // We have no changes to the cells. Since the notebook wasn't synced
            // it will not be synced now.
            if (event.contentChanges.length === 0) {
                return;
            }
            // Check if we have new matching cells.
            const cells = this.getMatchingCells(notebookDocument);
            // No matching cells and the notebook never synced. So still no need
            // to sync it.
            if (cells === undefined) {
                return;
            }
            // Open the notebook document and ignore the rest of the changes
            // this the notebooks will be synced with the correct settings.
            this.didOpen(notebookDocument, cells, syncInfo);
        }
        else {
            // The notebook is synced. First check if we have no matching
            // cells anymore and if so close the notebook
            const cells = this.getMatchingCells(notebookDocument);
            if (cells === undefined) {
                this.didClose(notebookDocument, syncInfo);
                return;
            }
            const newEvent = this.asNotebookDocumentChangeEvent(event.notebook, event, syncInfo, cells);
            if (newEvent !== undefined) {
                this.doSendChange(newEvent, cells).catch(() => { });
            }
        }
    }
    didSave(notebookDocument) {
        const syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString());
        if (syncInfo === undefined) {
            return;
        }
        this.doSendSave(notebookDocument).catch(() => { });
    }
    didClose(notebookDocument, syncInfo = this.notebookSyncInfo.get(notebookDocument.uri.toString())) {
        if (syncInfo === undefined) {
            return;
        }
        const syncedCells = notebookDocument.getCells().filter(cell => syncInfo.uris.has(cell.document.uri.toString()));
        this.doSendClose(notebookDocument, syncedCells).catch(() => { });
    }
    async sendDidOpenNotebookDocument(notebookDocument) {
        const cells = this.getMatchingCells(notebookDocument);
        if (cells === undefined) {
            return;
        }
        return this.doSendOpen(notebookDocument, cells);
    }
    async doSendOpen(notebookDocument, cells) {
        const send = async (notebookDocument, cells) => {
            const nb = Converter.c2p.asNotebookDocument(notebookDocument, cells, this.client.code2ProtocolConverter);
            const cellDocuments = cells.map(cell => this.client.code2ProtocolConverter.asTextDocumentItem(cell.document));
            try {
                await this.client.sendNotification(proto.DidOpenNotebookDocumentNotification.type, {
                    notebookDocument: nb,
                    cellTextDocuments: cellDocuments
                });
            }
            catch (error) {
                this.client.error('Sending DidOpenNotebookDocumentNotification failed', error);
                throw error;
            }
        };
        const middleware = this.client.middleware?.notebooks;
        this.notebookSyncInfo.set(notebookDocument.uri.toString(), SyncInfo.create(cells));
        return middleware?.didOpen !== undefined ? middleware.didOpen(notebookDocument, cells, send) : send(notebookDocument, cells);
    }
    async sendDidChangeNotebookDocument(event) {
        return this.doSendChange(event, undefined);
    }
    async doSendChange(event, cells = this.getMatchingCells(event.notebook)) {
        const send = async (event) => {
            try {
                await this.client.sendNotification(proto.DidChangeNotebookDocumentNotification.type, {
                    notebookDocument: Converter.c2p.asVersionedNotebookDocumentIdentifier(event.notebook, this.client.code2ProtocolConverter),
                    change: Converter.c2p.asNotebookDocumentChangeEvent(event, this.client.code2ProtocolConverter)
                });
            }
            catch (error) {
                this.client.error('Sending DidChangeNotebookDocumentNotification failed', error);
                throw error;
            }
        };
        const middleware = this.client.middleware?.notebooks;
        if (event.cells?.structure !== undefined) {
            this.notebookSyncInfo.set(event.notebook.uri.toString(), SyncInfo.create(cells ?? []));
        }
        return middleware?.didChange !== undefined ? middleware?.didChange(event, send) : send(event);
    }
    async sendDidSaveNotebookDocument(notebookDocument) {
        return this.doSendSave(notebookDocument);
    }
    async doSendSave(notebookDocument) {
        const send = async (notebookDocument) => {
            try {
                await this.client.sendNotification(proto.DidSaveNotebookDocumentNotification.type, {
                    notebookDocument: { uri: this.client.code2ProtocolConverter.asUri(notebookDocument.uri) }
                });
            }
            catch (error) {
                this.client.error('Sending DidSaveNotebookDocumentNotification failed', error);
                throw error;
            }
        };
        const middleware = this.client.middleware?.notebooks;
        return middleware?.didSave !== undefined ? middleware.didSave(notebookDocument, send) : send(notebookDocument);
    }
    async sendDidCloseNotebookDocument(notebookDocument) {
        return this.doSendClose(notebookDocument, this.getMatchingCells(notebookDocument) ?? []);
    }
    async doSendClose(notebookDocument, cells) {
        const send = async (notebookDocument, cells) => {
            try {
                await this.client.sendNotification(proto.DidCloseNotebookDocumentNotification.type, {
                    notebookDocument: { uri: this.client.code2ProtocolConverter.asUri(notebookDocument.uri) },
                    cellTextDocuments: cells.map(cell => this.client.code2ProtocolConverter.asTextDocumentIdentifier(cell.document))
                });
            }
            catch (error) {
                this.client.error('Sending DidCloseNotebookDocumentNotification failed', error);
                throw error;
            }
        };
        const middleware = this.client.middleware?.notebooks;
        this.notebookSyncInfo.delete(notebookDocument.uri.toString());
        return middleware?.didClose !== undefined ? middleware.didClose(notebookDocument, cells, send) : send(notebookDocument, cells);
    }
    asNotebookDocumentChangeEvent(notebook, event, syncInfo, matchingCells) {
        if (event !== undefined && event.notebook !== notebook) {
            throw new Error('Notebook must be identical');
        }
        const result = {
            notebook: notebook
        };
        if (event?.metadata !== undefined) {
            result.metadata = Converter.c2p.asMetadata(event.metadata);
        }
        let matchingCellsSet;
        if (event?.cellChanges !== undefined && event.cellChanges.length > 0) {
            const data = [];
            // Only consider the new matching cells.
            matchingCellsSet = new Set(matchingCells.map(cell => cell.document.uri.toString()));
            for (const cellChange of event.cellChanges) {
                if (matchingCellsSet.has(cellChange.cell.document.uri.toString()) && (cellChange.executionSummary !== undefined || cellChange.metadata !== undefined)) {
                    data.push(cellChange.cell);
                }
            }
            if (data.length > 0) {
                result.cells = result.cells ?? {};
                result.cells.data = data;
            }
        }
        if (((event?.contentChanges !== undefined && event.contentChanges.length > 0) || event === undefined) && syncInfo !== undefined && matchingCells !== undefined) {
            // We still have matching cells. Check if the cell changes
            // affect the notebook on the server side.
            const oldCells = syncInfo.cells;
            const newCells = matchingCells;
            // meta data changes are reported using on the cell itself. So we can ignore comparing
            // it which has a positive effect on performance.
            const diff = $NotebookCell.computeDiff(oldCells, newCells, false);
            let addedCells;
            let removedCells;
            if (diff !== undefined) {
                addedCells = diff.cells === undefined
                    ? new Map()
                    : new Map(diff.cells.map(cell => [cell.document.uri.toString(), cell]));
                removedCells = diff.deleteCount === 0
                    ? new Map()
                    : new Map(oldCells.slice(diff.start, diff.start + diff.deleteCount).map(cell => [cell.document.uri.toString(), cell]));
                // Remove the onces that got deleted and inserted again.
                for (const key of Array.from(removedCells.keys())) {
                    if (addedCells.has(key)) {
                        removedCells.delete(key);
                        addedCells.delete(key);
                    }
                }
                result.cells = result.cells ?? {};
                const didOpen = [];
                const didClose = [];
                if (addedCells.size > 0 || removedCells.size > 0) {
                    for (const cell of addedCells.values()) {
                        didOpen.push(cell);
                    }
                    for (const cell of removedCells.values()) {
                        didClose.push(cell);
                    }
                }
                result.cells.structure = {
                    array: diff,
                    didOpen,
                    didClose
                };
            }
        }
        // The notebook is a property as well.
        return Object.keys(result).length > 1 ? result : undefined;
    }
    getMatchingCells(notebookDocument, cells = notebookDocument.getCells()) {
        if (this.options.notebookSelector === undefined) {
            return undefined;
        }
        for (const item of this.options.notebookSelector) {
            if (item.notebook === undefined) {
                if (item.cells === undefined) {
                    return undefined;
                }
                const filtered = this.filterCells(notebookDocument, cells, item.cells);
                return filtered.length === 0 ? undefined : filtered;
            }
            else if ($NotebookDocumentFilter.matchNotebook(item.notebook, notebookDocument)) {
                return item.cells === undefined ? cells : this.filterCells(notebookDocument, cells, item.cells);
            }
        }
        return undefined;
    }
    cellMatches(notebookDocument, cell) {
        const cells = this.getMatchingCells(notebookDocument, [cell]);
        return cells !== undefined && cells[0] === cell;
    }
    filterCells(notebookDocument, cells, cellSelector) {
        const result = cells.filter((cell) => {
            const cellLanguage = cell.document.languageId;
            return cellSelector.some((filter => (filter.language === '*' || cellLanguage === filter.language)));
        });
        return typeof this.client.clientOptions.notebookDocumentOptions?.filterCells === 'function'
            ? this.client.clientOptions.notebookDocumentOptions.filterCells(notebookDocument, cells)
            : result;
    }
}
class NotebookDocumentSyncFeature {
    constructor(client) {
        this.client = client;
        this.registrations = new Map();
        this.registrationType = proto.NotebookDocumentSyncRegistrationType.type;
        // We don't receive an event for cells where the document changes its language mode
        // Since we allow servers to filter on the language mode we fire such an event ourselves.
        vscode.workspace.onDidOpenTextDocument((textDocument) => {
            if (textDocument.uri.scheme !== NotebookDocumentSyncFeature.CellScheme) {
                return;
            }
            const [notebookDocument, notebookCell] = this.findNotebookDocumentAndCell(textDocument);
            if (notebookDocument === undefined || notebookCell === undefined) {
                return;
            }
            for (const provider of this.registrations.values()) {
                if (provider instanceof NotebookDocumentSyncFeatureProvider) {
                    provider.didOpenNotebookCellTextDocument(notebookDocument, notebookCell);
                }
            }
        });
        vscode.workspace.onDidChangeTextDocument((event) => {
            if (event.contentChanges.length === 0) {
                return;
            }
            const textDocument = event.document;
            if (textDocument.uri.scheme !== NotebookDocumentSyncFeature.CellScheme) {
                return;
            }
            const [notebookDocument,] = this.findNotebookDocumentAndCell(textDocument);
            if (notebookDocument === undefined) {
                return;
            }
            for (const provider of this.registrations.values()) {
                if (provider instanceof NotebookDocumentSyncFeatureProvider) {
                    provider.didChangeNotebookCellTextDocument(notebookDocument, event);
                }
            }
        });
        vscode.workspace.onDidCloseTextDocument((textDocument) => {
            if (textDocument.uri.scheme !== NotebookDocumentSyncFeature.CellScheme) {
                return;
            }
            // There are two cases when we receive a close for a text document
            // 1: the cell got removed. This is handled in `onDidChangeNotebookCells`
            // 2: the language mode of a cell changed. This keeps the URI stable so
            //    we will still find the cell and the notebook document.
            const [notebookDocument, notebookCell] = this.findNotebookDocumentAndCell(textDocument);
            if (notebookDocument === undefined || notebookCell === undefined) {
                return;
            }
            for (const provider of this.registrations.values()) {
                if (provider instanceof NotebookDocumentSyncFeatureProvider) {
                    provider.didCloseNotebookCellTextDocument(notebookDocument, notebookCell);
                }
            }
        });
    }
    getState() {
        if (this.registrations.size === 0) {
            return { kind: 'document', id: this.registrationType.method, registrations: false, matches: false };
        }
        for (const provider of this.registrations.values()) {
            const state = provider.getState();
            if (state.kind === 'document' && state.registrations === true && state.matches === true) {
                return { kind: 'document', id: this.registrationType.method, registrations: true, matches: true };
            }
        }
        return { kind: 'document', id: this.registrationType.method, registrations: true, matches: false };
    }
    fillClientCapabilities(capabilities) {
        const synchronization = ensure(ensure(capabilities, 'notebookDocument'), 'synchronization');
        synchronization.dynamicRegistration = true;
        synchronization.executionSummarySupport = true;
    }
    preInitialize(capabilities) {
        const options = capabilities.notebookDocumentSync;
        if (options === undefined) {
            return;
        }
        this.dedicatedChannel = this.client.protocol2CodeConverter.asDocumentSelector($NotebookDocumentSyncOptions.asDocumentSelector(options));
    }
    initialize(capabilities) {
        const options = capabilities.notebookDocumentSync;
        if (options === undefined) {
            return;
        }
        const id = options.id ?? UUID.generateUuid();
        this.register({ id, registerOptions: options });
    }
    register(data) {
        const provider = new NotebookDocumentSyncFeatureProvider(this.client, data.registerOptions);
        this.registrations.set(data.id, provider);
    }
    unregister(id) {
        const provider = this.registrations.get(id);
        provider && provider.dispose();
    }
    dispose() {
        for (const provider of this.registrations.values()) {
            provider.dispose();
        }
        this.registrations.clear();
    }
    handles(textDocument) {
        if (textDocument.uri.scheme !== NotebookDocumentSyncFeature.CellScheme) {
            return false;
        }
        if (this.dedicatedChannel !== undefined && vscode.languages.match(this.dedicatedChannel, textDocument) > 0) {
            return true;
        }
        for (const provider of this.registrations.values()) {
            if (provider.handles(textDocument)) {
                return true;
            }
        }
        return false;
    }
    getProvider(notebookCell) {
        for (const provider of this.registrations.values()) {
            if (provider.handles(notebookCell.document)) {
                return provider;
            }
        }
        return undefined;
    }
    findNotebookDocumentAndCell(textDocument) {
        const uri = textDocument.uri.toString();
        for (const notebookDocument of vscode.workspace.notebookDocuments) {
            for (const cell of notebookDocument.getCells()) {
                if (cell.document.uri.toString() === uri) {
                    return [notebookDocument, cell];
                }
            }
        }
        return [undefined, undefined];
    }
}
exports.NotebookDocumentSyncFeature = NotebookDocumentSyncFeature;
NotebookDocumentSyncFeature.CellScheme = 'vscode-notebook-cell';
//# sourceMappingURL=notebook.js.map

/***/ }),
/* 124 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SyncConfigurationFeature = exports.toJSONObject = exports.ConfigurationFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const Is = __webpack_require__(54);
const UUID = __webpack_require__(115);
const features_1 = __webpack_require__(117);
/**
 * Configuration pull model. Form server to client.
 */
class ConfigurationFeature {
    constructor(client) {
        this._client = client;
    }
    getState() {
        return { kind: 'static' };
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.configuration = true;
    }
    initialize() {
        let client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.ConfigurationRequest.type, (params, token) => {
            let configuration = (params) => {
                let result = [];
                for (let item of params.items) {
                    let resource = item.scopeUri !== void 0 && item.scopeUri !== null ? this._client.protocol2CodeConverter.asUri(item.scopeUri) : undefined;
                    result.push(this.getConfiguration(resource, item.section !== null ? item.section : undefined));
                }
                return result;
            };
            let middleware = client.middleware.workspace;
            return middleware && middleware.configuration
                ? middleware.configuration(params, token, configuration)
                : configuration(params, token);
        });
    }
    getConfiguration(resource, section) {
        let result = null;
        if (section) {
            let index = section.lastIndexOf('.');
            if (index === -1) {
                result = toJSONObject(vscode_1.workspace.getConfiguration(undefined, resource).get(section));
            }
            else {
                let config = vscode_1.workspace.getConfiguration(section.substr(0, index), resource);
                if (config) {
                    result = toJSONObject(config.get(section.substr(index + 1)));
                }
            }
        }
        else {
            let config = vscode_1.workspace.getConfiguration(undefined, resource);
            result = {};
            for (let key of Object.keys(config)) {
                if (config.has(key)) {
                    result[key] = toJSONObject(config.get(key));
                }
            }
        }
        if (result === undefined) {
            result = null;
        }
        return result;
    }
    dispose() {
    }
}
exports.ConfigurationFeature = ConfigurationFeature;
function toJSONObject(obj) {
    if (obj) {
        if (Array.isArray(obj)) {
            return obj.map(toJSONObject);
        }
        else if (typeof obj === 'object') {
            const res = Object.create(null);
            for (const key in obj) {
                if (Object.prototype.hasOwnProperty.call(obj, key)) {
                    res[key] = toJSONObject(obj[key]);
                }
            }
            return res;
        }
    }
    return obj;
}
exports.toJSONObject = toJSONObject;
class SyncConfigurationFeature {
    constructor(_client) {
        this._client = _client;
        this._listeners = new Map();
    }
    getState() {
        return { kind: 'workspace', id: this.registrationType.method, registrations: this._listeners.size > 0 };
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'didChangeConfiguration').dynamicRegistration = true;
    }
    initialize() {
        let section = this._client.clientOptions.synchronize?.configurationSection;
        if (section !== undefined) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: {
                    section: section
                }
            });
        }
    }
    register(data) {
        let disposable = vscode_1.workspace.onDidChangeConfiguration((event) => {
            this.onDidChangeConfiguration(data.registerOptions.section, event);
        });
        this._listeners.set(data.id, disposable);
        if (data.registerOptions.section !== undefined) {
            this.onDidChangeConfiguration(data.registerOptions.section, undefined);
        }
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable) {
            this._listeners.delete(id);
            disposable.dispose();
        }
    }
    dispose() {
        for (const disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    onDidChangeConfiguration(configurationSection, event) {
        let sections;
        if (Is.string(configurationSection)) {
            sections = [configurationSection];
        }
        else {
            sections = configurationSection;
        }
        if (sections !== undefined && event !== undefined) {
            let affected = sections.some((section) => event.affectsConfiguration(section));
            if (!affected) {
                return;
            }
        }
        const didChangeConfiguration = async (sections) => {
            if (sections === undefined) {
                return this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: null });
            }
            else {
                return this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type, { settings: this.extractSettingsInformation(sections) });
            }
        };
        let middleware = this._client.middleware.workspace?.didChangeConfiguration;
        (middleware ? middleware(sections, didChangeConfiguration) : didChangeConfiguration(sections)).catch((error) => {
            this._client.error(`Sending notification ${vscode_languageserver_protocol_1.DidChangeConfigurationNotification.type.method} failed`, error);
        });
    }
    extractSettingsInformation(keys) {
        function ensurePath(config, path) {
            let current = config;
            for (let i = 0; i < path.length - 1; i++) {
                let obj = current[path[i]];
                if (!obj) {
                    obj = Object.create(null);
                    current[path[i]] = obj;
                }
                current = obj;
            }
            return current;
        }
        let resource = this._client.clientOptions.workspaceFolder
            ? this._client.clientOptions.workspaceFolder.uri
            : undefined;
        let result = Object.create(null);
        for (let i = 0; i < keys.length; i++) {
            let key = keys[i];
            let index = key.indexOf('.');
            let config = null;
            if (index >= 0) {
                config = vscode_1.workspace.getConfiguration(key.substr(0, index), resource).get(key.substr(index + 1));
            }
            else {
                config = vscode_1.workspace.getConfiguration(undefined, resource).get(key);
            }
            if (config) {
                let path = keys[i].split('.');
                ensurePath(result, path)[path[path.length - 1]] = toJSONObject(config);
            }
        }
        return result;
    }
}
exports.SyncConfigurationFeature = SyncConfigurationFeature;
//# sourceMappingURL=configuration.js.map

/***/ }),
/* 125 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DidSaveTextDocumentFeature = exports.WillSaveWaitUntilFeature = exports.WillSaveFeature = exports.DidChangeTextDocumentFeature = exports.DidCloseTextDocumentFeature = exports.DidOpenTextDocumentFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const async_1 = __webpack_require__(104);
const UUID = __webpack_require__(115);
class DidOpenTextDocumentFeature extends features_1.TextDocumentEventFeature {
    constructor(client, syncedDocuments) {
        super(client, vscode_1.workspace.onDidOpenTextDocument, vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type, () => client.middleware.didOpen, (textDocument) => client.code2ProtocolConverter.asOpenTextDocumentParams(textDocument), (data) => data, features_1.TextDocumentEventFeature.textDocumentFilter);
        this._syncedDocuments = syncedDocuments;
    }
    get openDocuments() {
        return this._syncedDocuments.values();
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register({ id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidOpenTextDocumentNotification.type;
    }
    register(data) {
        super.register(data);
        if (!data.registerOptions.documentSelector) {
            return;
        }
        const documentSelector = this._client.protocol2CodeConverter.asDocumentSelector(data.registerOptions.documentSelector);
        vscode_1.workspace.textDocuments.forEach((textDocument) => {
            const uri = textDocument.uri.toString();
            if (this._syncedDocuments.has(uri)) {
                return;
            }
            if (vscode_1.languages.match(documentSelector, textDocument) > 0 && !this._client.hasDedicatedTextSynchronizationFeature(textDocument)) {
                const middleware = this._client.middleware;
                const didOpen = (textDocument) => {
                    return this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                (middleware.didOpen ? middleware.didOpen(textDocument, didOpen) : didOpen(textDocument)).catch((error) => {
                    this._client.error(`Sending document notification ${this._type.method} failed`, error);
                });
                this._syncedDocuments.set(uri, textDocument);
            }
        });
    }
    notificationSent(textDocument, type, params) {
        super.notificationSent(textDocument, type, params);
        this._syncedDocuments.set(textDocument.uri.toString(), textDocument);
    }
}
exports.DidOpenTextDocumentFeature = DidOpenTextDocumentFeature;
class DidCloseTextDocumentFeature extends features_1.TextDocumentEventFeature {
    constructor(client, syncedDocuments) {
        super(client, vscode_1.workspace.onDidCloseTextDocument, vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type, () => client.middleware.didClose, (textDocument) => client.code2ProtocolConverter.asCloseTextDocumentParams(textDocument), (data) => data, features_1.TextDocumentEventFeature.textDocumentFilter);
        this._syncedDocuments = syncedDocuments;
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidCloseTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.openClose) {
            this.register({ id: UUID.generateUuid(), registerOptions: { documentSelector: documentSelector } });
        }
    }
    notificationSent(textDocument, type, params) {
        super.notificationSent(textDocument, type, params);
        this._syncedDocuments.delete(textDocument.uri.toString());
    }
    unregister(id) {
        const selector = this._selectors.get(id);
        // The super call removed the selector from the map
        // of selectors.
        super.unregister(id);
        const selectors = this._selectors.values();
        this._syncedDocuments.forEach((textDocument) => {
            if (vscode_1.languages.match(selector, textDocument) > 0 && !this._selectorFilter(selectors, textDocument) && !this._client.hasDedicatedTextSynchronizationFeature(textDocument)) {
                let middleware = this._client.middleware;
                let didClose = (textDocument) => {
                    return this._client.sendNotification(this._type, this._createParams(textDocument));
                };
                this._syncedDocuments.delete(textDocument.uri.toString());
                (middleware.didClose ? middleware.didClose(textDocument, didClose) : didClose(textDocument)).catch((error) => {
                    this._client.error(`Sending document notification ${this._type.method} failed`, error);
                });
            }
        });
    }
}
exports.DidCloseTextDocumentFeature = DidCloseTextDocumentFeature;
class DidChangeTextDocumentFeature extends features_1.DynamicDocumentFeature {
    constructor(client) {
        super(client);
        this._forcingDelivery = false;
        this._changeData = new Map();
        this._onNotificationSent = new vscode_1.EventEmitter();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.change !== undefined && textDocumentSyncOptions.change !== vscode_languageserver_protocol_1.TextDocumentSyncKind.None) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, { syncKind: textDocumentSyncOptions.change })
            });
        }
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onDidChangeTextDocument(this.callback, this);
        }
        this._changeData.set(data.id, {
            syncKind: data.registerOptions.syncKind,
            documentSelector: this._client.protocol2CodeConverter.asDocumentSelector(data.registerOptions.documentSelector),
        });
    }
    *getDocumentSelectors() {
        for (const data of this._changeData.values()) {
            yield data.documentSelector;
        }
    }
    async callback(event) {
        // Text document changes are send for dirty changes as well. We don't
        // have dirty / un-dirty events in the LSP so we ignore content changes
        // with length zero.
        if (event.contentChanges.length === 0) {
            return;
        }
        const promises = [];
        for (const changeData of this._changeData.values()) {
            if (vscode_1.languages.match(changeData.documentSelector, event.document) > 0 && !this._client.hasDedicatedTextSynchronizationFeature(event.document)) {
                const middleware = this._client.middleware;
                if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Incremental) {
                    const didChange = async (event) => {
                        const params = this._client.code2ProtocolConverter.asChangeTextDocumentParams(event);
                        await this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                        this.notificationSent(event, vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                    };
                    promises.push(middleware.didChange ? middleware.didChange(event, event => didChange(event)) : didChange(event));
                }
                else if (changeData.syncKind === vscode_languageserver_protocol_1.TextDocumentSyncKind.Full) {
                    const didChange = async (event) => {
                        const doSend = async (event) => {
                            const params = this._client.code2ProtocolConverter.asChangeTextDocumentParams(event.document);
                            await this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                            this.notificationSent(event, vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type, params);
                        };
                        if (this._changeDelayer) {
                            if (this._changeDelayer.uri !== event.document.uri.toString()) {
                                // Use this force delivery to track boolean state. Otherwise we might call two times.
                                this.forceDelivery();
                                this._changeDelayer.uri = event.document.uri.toString();
                            }
                            return this._changeDelayer.delayer.trigger(() => doSend(event));
                        }
                        else {
                            this._changeDelayer = {
                                uri: event.document.uri.toString(),
                                delayer: new async_1.Delayer(200)
                            };
                            return this._changeDelayer.delayer.trigger(() => doSend(event), -1);
                        }
                    };
                    promises.push(middleware.didChange ? middleware.didChange(event, event => didChange(event)) : didChange(event));
                }
            }
        }
        return Promise.all(promises).then(undefined, (error) => {
            this._client.error(`Sending document notification ${vscode_languageserver_protocol_1.DidChangeTextDocumentNotification.type.method} failed`, error);
            throw error;
        });
    }
    get onNotificationSent() {
        return this._onNotificationSent.event;
    }
    notificationSent(changeEvent, type, params) {
        this._onNotificationSent.fire({ original: changeEvent, type, params });
    }
    unregister(id) {
        this._changeData.delete(id);
        if (this._changeData.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        if (this._changeDelayer !== undefined) {
            this._changeDelayer.delayer.cancel();
        }
        this._changeDelayer = undefined;
        this._forcingDelivery = false;
        this._changeData.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    forceDelivery() {
        if (this._forcingDelivery || !this._changeDelayer) {
            return;
        }
        try {
            this._forcingDelivery = true;
            this._changeDelayer.delayer.forceDelivery();
        }
        finally {
            this._forcingDelivery = false;
        }
    }
    getProvider(document) {
        for (const changeData of this._changeData.values()) {
            if (vscode_1.languages.match(changeData.documentSelector, document) > 0) {
                return {
                    send: (event) => {
                        return this.callback(event);
                    }
                };
            }
        }
        return undefined;
    }
}
exports.DidChangeTextDocumentFeature = DidChangeTextDocumentFeature;
class WillSaveFeature extends features_1.TextDocumentEventFeature {
    constructor(client) {
        super(client, vscode_1.workspace.onWillSaveTextDocument, vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type, () => client.middleware.willSave, (willSaveEvent) => client.code2ProtocolConverter.asWillSaveTextDocumentParams(willSaveEvent), (event) => event.document, (selectors, willSaveEvent) => features_1.TextDocumentEventFeature.textDocumentFilter(selectors, willSaveEvent.document));
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        let value = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization');
        value.willSave = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSave) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
}
exports.WillSaveFeature = WillSaveFeature;
class WillSaveWaitUntilFeature extends features_1.DynamicDocumentFeature {
    constructor(client) {
        super(client);
        this._selectors = new Map();
    }
    getDocumentSelectors() {
        return this._selectors.values();
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type;
    }
    fillClientCapabilities(capabilities) {
        let value = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization');
        value.willSaveWaitUntil = true;
    }
    initialize(capabilities, documentSelector) {
        let textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.willSaveWaitUntil) {
            this.register({
                id: UUID.generateUuid(),
                registerOptions: { documentSelector: documentSelector }
            });
        }
    }
    register(data) {
        if (!data.registerOptions.documentSelector) {
            return;
        }
        if (!this._listener) {
            this._listener = vscode_1.workspace.onWillSaveTextDocument(this.callback, this);
        }
        this._selectors.set(data.id, this._client.protocol2CodeConverter.asDocumentSelector(data.registerOptions.documentSelector));
    }
    callback(event) {
        if (features_1.TextDocumentEventFeature.textDocumentFilter(this._selectors.values(), event.document) && !this._client.hasDedicatedTextSynchronizationFeature(event.document)) {
            let middleware = this._client.middleware;
            let willSaveWaitUntil = (event) => {
                return this._client.sendRequest(vscode_languageserver_protocol_1.WillSaveTextDocumentWaitUntilRequest.type, this._client.code2ProtocolConverter.asWillSaveTextDocumentParams(event)).then(async (edits) => {
                    let vEdits = await this._client.protocol2CodeConverter.asTextEdits(edits);
                    return vEdits === undefined ? [] : vEdits;
                });
            };
            event.waitUntil(middleware.willSaveWaitUntil
                ? middleware.willSaveWaitUntil(event, willSaveWaitUntil)
                : willSaveWaitUntil(event));
        }
    }
    unregister(id) {
        this._selectors.delete(id);
        if (this._selectors.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._selectors.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
}
exports.WillSaveWaitUntilFeature = WillSaveWaitUntilFeature;
class DidSaveTextDocumentFeature extends features_1.TextDocumentEventFeature {
    constructor(client) {
        super(client, vscode_1.workspace.onDidSaveTextDocument, vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type, () => client.middleware.didSave, (textDocument) => client.code2ProtocolConverter.asSaveTextDocumentParams(textDocument, this._includeText), (data) => data, features_1.TextDocumentEventFeature.textDocumentFilter);
        this._includeText = false;
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidSaveTextDocumentNotification.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'synchronization').didSave = true;
    }
    initialize(capabilities, documentSelector) {
        const textDocumentSyncOptions = capabilities.resolvedTextDocumentSync;
        if (documentSelector && textDocumentSyncOptions && textDocumentSyncOptions.save) {
            const saveOptions = typeof textDocumentSyncOptions.save === 'boolean'
                ? { includeText: false }
                : { includeText: !!textDocumentSyncOptions.save.includeText };
            this.register({
                id: UUID.generateUuid(),
                registerOptions: Object.assign({}, { documentSelector: documentSelector }, saveOptions)
            });
        }
    }
    register(data) {
        this._includeText = !!data.registerOptions.includeText;
        super.register(data);
    }
}
exports.DidSaveTextDocumentFeature = DidSaveTextDocumentFeature;
//# sourceMappingURL=textSynchronization.js.map

/***/ }),
/* 126 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CompletionItemFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
const SupportedCompletionItemKinds = [
    vscode_languageserver_protocol_1.CompletionItemKind.Text,
    vscode_languageserver_protocol_1.CompletionItemKind.Method,
    vscode_languageserver_protocol_1.CompletionItemKind.Function,
    vscode_languageserver_protocol_1.CompletionItemKind.Constructor,
    vscode_languageserver_protocol_1.CompletionItemKind.Field,
    vscode_languageserver_protocol_1.CompletionItemKind.Variable,
    vscode_languageserver_protocol_1.CompletionItemKind.Class,
    vscode_languageserver_protocol_1.CompletionItemKind.Interface,
    vscode_languageserver_protocol_1.CompletionItemKind.Module,
    vscode_languageserver_protocol_1.CompletionItemKind.Property,
    vscode_languageserver_protocol_1.CompletionItemKind.Unit,
    vscode_languageserver_protocol_1.CompletionItemKind.Value,
    vscode_languageserver_protocol_1.CompletionItemKind.Enum,
    vscode_languageserver_protocol_1.CompletionItemKind.Keyword,
    vscode_languageserver_protocol_1.CompletionItemKind.Snippet,
    vscode_languageserver_protocol_1.CompletionItemKind.Color,
    vscode_languageserver_protocol_1.CompletionItemKind.File,
    vscode_languageserver_protocol_1.CompletionItemKind.Reference,
    vscode_languageserver_protocol_1.CompletionItemKind.Folder,
    vscode_languageserver_protocol_1.CompletionItemKind.EnumMember,
    vscode_languageserver_protocol_1.CompletionItemKind.Constant,
    vscode_languageserver_protocol_1.CompletionItemKind.Struct,
    vscode_languageserver_protocol_1.CompletionItemKind.Event,
    vscode_languageserver_protocol_1.CompletionItemKind.Operator,
    vscode_languageserver_protocol_1.CompletionItemKind.TypeParameter
];
class CompletionItemFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CompletionRequest.type);
        this.labelDetailsSupport = new Map();
    }
    fillClientCapabilities(capabilities) {
        let completion = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'completion');
        completion.dynamicRegistration = true;
        completion.contextSupport = true;
        completion.completionItem = {
            snippetSupport: true,
            commitCharactersSupport: true,
            documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText],
            deprecatedSupport: true,
            preselectSupport: true,
            tagSupport: { valueSet: [vscode_languageserver_protocol_1.CompletionItemTag.Deprecated] },
            insertReplaceSupport: true,
            resolveSupport: {
                properties: ['documentation', 'detail', 'additionalTextEdits']
            },
            insertTextModeSupport: { valueSet: [vscode_languageserver_protocol_1.InsertTextMode.asIs, vscode_languageserver_protocol_1.InsertTextMode.adjustIndentation] },
            labelDetailsSupport: true
        };
        completion.insertTextMode = vscode_languageserver_protocol_1.InsertTextMode.adjustIndentation;
        completion.completionItemKind = { valueSet: SupportedCompletionItemKinds };
        completion.completionList = {
            itemDefaults: [
                'commitCharacters', 'editRange', 'insertTextFormat', 'insertTextMode'
            ]
        };
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.completionProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options, id) {
        this.labelDetailsSupport.set(id, !!options.completionItem?.labelDetailsSupport);
        const triggerCharacters = options.triggerCharacters ?? [];
        const defaultCommitCharacters = options.allCommitCharacters;
        const selector = options.documentSelector;
        const provider = {
            provideCompletionItems: (document, position, token, context) => {
                const client = this._client;
                const middleware = this._client.middleware;
                const provideCompletionItems = (document, position, context, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.CompletionRequest.type, client.code2ProtocolConverter.asCompletionParams(document, position, context), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asCompletionResult(result, defaultCommitCharacters, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CompletionRequest.type, token, error, null);
                    });
                };
                return middleware.provideCompletionItem
                    ? middleware.provideCompletionItem(document, position, context, token, provideCompletionItems)
                    : provideCompletionItems(document, position, context, token);
            },
            resolveCompletionItem: options.resolveProvider
                ? (item, token) => {
                    const client = this._client;
                    const middleware = this._client.middleware;
                    const resolveCompletionItem = (item, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, client.code2ProtocolConverter.asCompletionItem(item, !!this.labelDetailsSupport.get(id)), token).then((result) => {
                            if (token.isCancellationRequested) {
                                return null;
                            }
                            return client.protocol2CodeConverter.asCompletionItem(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CompletionResolveRequest.type, token, error, item);
                        });
                    };
                    return middleware.resolveCompletionItem
                        ? middleware.resolveCompletionItem(item, token, resolveCompletionItem)
                        : resolveCompletionItem(item, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCompletionItemProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider, ...triggerCharacters), provider];
    }
}
exports.CompletionItemFeature = CompletionItemFeature;
//# sourceMappingURL=completion.js.map

/***/ }),
/* 127 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.HoverFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class HoverFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.HoverRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const hoverCapability = ((0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'hover'));
        hoverCapability.dynamicRegistration = true;
        hoverCapability.contentFormat = [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText];
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.hoverProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideHover: (document, position, token) => {
                const client = this._client;
                const provideHover = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.HoverRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asHover(result);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.HoverRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideHover
                    ? middleware.provideHover(document, position, token, provideHover)
                    : provideHover(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerHoverProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.HoverFeature = HoverFeature;
//# sourceMappingURL=hover.js.map

/***/ }),
/* 128 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DefinitionFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class DefinitionFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DefinitionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let definitionSupport = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'definition');
        definitionSupport.dynamicRegistration = true;
        definitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.definitionProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDefinition: (document, position, token) => {
                const client = this._client;
                const provideDefinition = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDefinitionResult(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DefinitionRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDefinition
                    ? middleware.provideDefinition(document, position, token, provideDefinition)
                    : provideDefinition(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerDefinitionProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.DefinitionFeature = DefinitionFeature;
//# sourceMappingURL=definition.js.map

/***/ }),
/* 129 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SignatureHelpFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class SignatureHelpFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SignatureHelpRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let config = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'signatureHelp');
        config.dynamicRegistration = true;
        config.signatureInformation = { documentationFormat: [vscode_languageserver_protocol_1.MarkupKind.Markdown, vscode_languageserver_protocol_1.MarkupKind.PlainText] };
        config.signatureInformation.parameterInformation = { labelOffsetSupport: true };
        config.signatureInformation.activeParameterSupport = true;
        config.contextSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.signatureHelpProvider);
        if (!options) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: options
        });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideSignatureHelp: (document, position, token, context) => {
                const client = this._client;
                const providerSignatureHelp = (document, position, context, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, client.code2ProtocolConverter.asSignatureHelpParams(document, position, context), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asSignatureHelp(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.SignatureHelpRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideSignatureHelp
                    ? middleware.provideSignatureHelp(document, position, context, token, providerSignatureHelp)
                    : providerSignatureHelp(document, position, context, token);
            }
        };
        return [this.registerProvider(options, provider), provider];
    }
    registerProvider(options, provider) {
        const selector = this._client.protocol2CodeConverter.asDocumentSelector(options.documentSelector);
        if (options.retriggerCharacters === undefined) {
            const triggerCharacters = options.triggerCharacters || [];
            return vscode_1.languages.registerSignatureHelpProvider(selector, provider, ...triggerCharacters);
        }
        else {
            const metaData = {
                triggerCharacters: options.triggerCharacters || [],
                retriggerCharacters: options.retriggerCharacters || []
            };
            return vscode_1.languages.registerSignatureHelpProvider(selector, provider, metaData);
        }
    }
}
exports.SignatureHelpFeature = SignatureHelpFeature;
//# sourceMappingURL=signatureHelp.js.map

/***/ }),
/* 130 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DocumentHighlightFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class DocumentHighlightFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentHighlightRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'documentHighlight').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentHighlightProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDocumentHighlights: (document, position, token) => {
                const client = this._client;
                const _provideDocumentHighlights = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDocumentHighlights(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentHighlightRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentHighlights
                    ? middleware.provideDocumentHighlights(document, position, token, _provideDocumentHighlights)
                    : _provideDocumentHighlights(document, position, token);
            }
        };
        return [vscode_1.languages.registerDocumentHighlightProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.DocumentHighlightFeature = DocumentHighlightFeature;
//# sourceMappingURL=documentHighlight.js.map

/***/ }),
/* 131 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DocumentSymbolFeature = exports.SupportedSymbolTags = exports.SupportedSymbolKinds = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
exports.SupportedSymbolKinds = [
    vscode_languageserver_protocol_1.SymbolKind.File,
    vscode_languageserver_protocol_1.SymbolKind.Module,
    vscode_languageserver_protocol_1.SymbolKind.Namespace,
    vscode_languageserver_protocol_1.SymbolKind.Package,
    vscode_languageserver_protocol_1.SymbolKind.Class,
    vscode_languageserver_protocol_1.SymbolKind.Method,
    vscode_languageserver_protocol_1.SymbolKind.Property,
    vscode_languageserver_protocol_1.SymbolKind.Field,
    vscode_languageserver_protocol_1.SymbolKind.Constructor,
    vscode_languageserver_protocol_1.SymbolKind.Enum,
    vscode_languageserver_protocol_1.SymbolKind.Interface,
    vscode_languageserver_protocol_1.SymbolKind.Function,
    vscode_languageserver_protocol_1.SymbolKind.Variable,
    vscode_languageserver_protocol_1.SymbolKind.Constant,
    vscode_languageserver_protocol_1.SymbolKind.String,
    vscode_languageserver_protocol_1.SymbolKind.Number,
    vscode_languageserver_protocol_1.SymbolKind.Boolean,
    vscode_languageserver_protocol_1.SymbolKind.Array,
    vscode_languageserver_protocol_1.SymbolKind.Object,
    vscode_languageserver_protocol_1.SymbolKind.Key,
    vscode_languageserver_protocol_1.SymbolKind.Null,
    vscode_languageserver_protocol_1.SymbolKind.EnumMember,
    vscode_languageserver_protocol_1.SymbolKind.Struct,
    vscode_languageserver_protocol_1.SymbolKind.Event,
    vscode_languageserver_protocol_1.SymbolKind.Operator,
    vscode_languageserver_protocol_1.SymbolKind.TypeParameter
];
exports.SupportedSymbolTags = [
    vscode_languageserver_protocol_1.SymbolTag.Deprecated
];
class DocumentSymbolFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentSymbolRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let symbolCapabilities = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'documentSymbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: exports.SupportedSymbolKinds
        };
        symbolCapabilities.hierarchicalDocumentSymbolSupport = true;
        symbolCapabilities.tagSupport = {
            valueSet: exports.SupportedSymbolTags
        };
        symbolCapabilities.labelSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentSymbolProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDocumentSymbols: (document, token) => {
                const client = this._client;
                const _provideDocumentSymbols = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, client.code2ProtocolConverter.asDocumentSymbolParams(document), token).then(async (data) => {
                        if (token.isCancellationRequested || data === undefined || data === null) {
                            return null;
                        }
                        if (data.length === 0) {
                            return [];
                        }
                        else {
                            const first = data[0];
                            if (vscode_languageserver_protocol_1.DocumentSymbol.is(first)) {
                                return await client.protocol2CodeConverter.asDocumentSymbols(data, token);
                            }
                            else {
                                return await client.protocol2CodeConverter.asSymbolInformations(data, token);
                            }
                        }
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentSymbolRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentSymbols
                    ? middleware.provideDocumentSymbols(document, token, _provideDocumentSymbols)
                    : _provideDocumentSymbols(document, token);
            }
        };
        const metaData = options.label !== undefined ? { label: options.label } : undefined;
        return [vscode_1.languages.registerDocumentSymbolProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider, metaData), provider];
    }
}
exports.DocumentSymbolFeature = DocumentSymbolFeature;
//# sourceMappingURL=documentSymbol.js.map

/***/ }),
/* 132 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkspaceSymbolFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const documentSymbol_1 = __webpack_require__(131);
const UUID = __webpack_require__(115);
class WorkspaceSymbolFeature extends features_1.WorkspaceFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let symbolCapabilities = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'symbol');
        symbolCapabilities.dynamicRegistration = true;
        symbolCapabilities.symbolKind = {
            valueSet: documentSymbol_1.SupportedSymbolKinds
        };
        symbolCapabilities.tagSupport = {
            valueSet: documentSymbol_1.SupportedSymbolTags
        };
        symbolCapabilities.resolveSupport = { properties: ['location.range'] };
    }
    initialize(capabilities) {
        if (!capabilities.workspaceSymbolProvider) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: capabilities.workspaceSymbolProvider === true ? { workDoneProgress: false } : capabilities.workspaceSymbolProvider
        });
    }
    registerLanguageProvider(options) {
        const provider = {
            provideWorkspaceSymbols: (query, token) => {
                const client = this._client;
                const provideWorkspaceSymbols = (query, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, { query }, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asSymbolInformations(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.WorkspaceSymbolRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideWorkspaceSymbols
                    ? middleware.provideWorkspaceSymbols(query, token, provideWorkspaceSymbols)
                    : provideWorkspaceSymbols(query, token);
            },
            resolveWorkspaceSymbol: options.resolveProvider === true
                ? (item, token) => {
                    const client = this._client;
                    const resolveWorkspaceSymbol = (item, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.WorkspaceSymbolResolveRequest.type, client.code2ProtocolConverter.asWorkspaceSymbol(item), token).then((result) => {
                            if (token.isCancellationRequested) {
                                return null;
                            }
                            return client.protocol2CodeConverter.asSymbolInformation(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.WorkspaceSymbolResolveRequest.type, token, error, null);
                        });
                    };
                    const middleware = client.middleware;
                    return middleware.resolveWorkspaceSymbol
                        ? middleware.resolveWorkspaceSymbol(item, token, resolveWorkspaceSymbol)
                        : resolveWorkspaceSymbol(item, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerWorkspaceSymbolProvider(provider), provider];
    }
}
exports.WorkspaceSymbolFeature = WorkspaceSymbolFeature;
//# sourceMappingURL=workspaceSymbol.js.map

/***/ }),
/* 133 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ReferencesFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class ReferencesFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ReferencesRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'references').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.referencesProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideReferences: (document, position, options, token) => {
                const client = this._client;
                const _providerReferences = (document, position, options, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, client.code2ProtocolConverter.asReferenceParams(document, position, options), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asReferences(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ReferencesRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideReferences
                    ? middleware.provideReferences(document, position, options, token, _providerReferences)
                    : _providerReferences(document, position, options, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerReferenceProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.ReferencesFeature = ReferencesFeature;
//# sourceMappingURL=reference.js.map

/***/ }),
/* 134 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CodeActionFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const UUID = __webpack_require__(115);
const features_1 = __webpack_require__(117);
class CodeActionFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeActionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const cap = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'codeAction');
        cap.dynamicRegistration = true;
        cap.isPreferredSupport = true;
        cap.disabledSupport = true;
        cap.dataSupport = true;
        // We can only resolve the edit property.
        cap.resolveSupport = {
            properties: ['edit']
        };
        cap.codeActionLiteralSupport = {
            codeActionKind: {
                valueSet: [
                    vscode_languageserver_protocol_1.CodeActionKind.Empty,
                    vscode_languageserver_protocol_1.CodeActionKind.QuickFix,
                    vscode_languageserver_protocol_1.CodeActionKind.Refactor,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorExtract,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorInline,
                    vscode_languageserver_protocol_1.CodeActionKind.RefactorRewrite,
                    vscode_languageserver_protocol_1.CodeActionKind.Source,
                    vscode_languageserver_protocol_1.CodeActionKind.SourceOrganizeImports
                ]
            }
        };
        cap.honorsChangeAnnotations = false;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.codeActionProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideCodeActions: (document, range, context, token) => {
                const client = this._client;
                const _provideCodeActions = async (document, range, context, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(range),
                        context: await client.code2ProtocolConverter.asCodeActionContext(context, token)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, params, token).then((values) => {
                        if (token.isCancellationRequested || values === null || values === undefined) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asCodeActionResult(values, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeActionRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideCodeActions
                    ? middleware.provideCodeActions(document, range, context, token, _provideCodeActions)
                    : _provideCodeActions(document, range, context, token);
            },
            resolveCodeAction: options.resolveProvider
                ? (item, token) => {
                    const client = this._client;
                    const middleware = this._client.middleware;
                    const resolveCodeAction = async (item, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CodeActionResolveRequest.type, await client.code2ProtocolConverter.asCodeAction(item, token), token).then((result) => {
                            if (token.isCancellationRequested) {
                                return item;
                            }
                            return client.protocol2CodeConverter.asCodeAction(result, token);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeActionResolveRequest.type, token, error, item);
                        });
                    };
                    return middleware.resolveCodeAction
                        ? middleware.resolveCodeAction(item, token, resolveCodeAction)
                        : resolveCodeAction(item, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCodeActionsProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider, (options.codeActionKinds
                ? { providedCodeActionKinds: this._client.protocol2CodeConverter.asCodeActionKinds(options.codeActionKinds) }
                : undefined)), provider];
    }
}
exports.CodeActionFeature = CodeActionFeature;
//# sourceMappingURL=codeAction.js.map

/***/ }),
/* 135 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CodeLensFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const UUID = __webpack_require__(115);
const features_1 = __webpack_require__(117);
class CodeLensFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CodeLensRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'codeLens').dynamicRegistration = true;
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'codeLens').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.CodeLensRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeCodeLensEmitter.fire();
            }
        });
        const options = this.getRegistrationOptions(documentSelector, capabilities.codeLensProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const eventEmitter = new vscode_1.EventEmitter();
        const provider = {
            onDidChangeCodeLenses: eventEmitter.event,
            provideCodeLenses: (document, token) => {
                const client = this._client;
                const provideCodeLenses = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, client.code2ProtocolConverter.asCodeLensParams(document), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asCodeLenses(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeLensRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideCodeLenses
                    ? middleware.provideCodeLenses(document, token, provideCodeLenses)
                    : provideCodeLenses(document, token);
            },
            resolveCodeLens: (options.resolveProvider)
                ? (codeLens, token) => {
                    const client = this._client;
                    const resolveCodeLens = (codeLens, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, client.code2ProtocolConverter.asCodeLens(codeLens), token).then((result) => {
                            if (token.isCancellationRequested) {
                                return codeLens;
                            }
                            return client.protocol2CodeConverter.asCodeLens(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.CodeLensResolveRequest.type, token, error, codeLens);
                        });
                    };
                    const middleware = client.middleware;
                    return middleware.resolveCodeLens
                        ? middleware.resolveCodeLens(codeLens, token, resolveCodeLens)
                        : resolveCodeLens(codeLens, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerCodeLensProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), { provider, onDidChangeCodeLensEmitter: eventEmitter }];
    }
}
exports.CodeLensFeature = CodeLensFeature;
//# sourceMappingURL=codeLens.js.map

/***/ }),
/* 136 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DocumentOnTypeFormattingFeature = exports.DocumentRangeFormattingFeature = exports.DocumentFormattingFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const UUID = __webpack_require__(115);
const features_1 = __webpack_require__(117);
var FileFormattingOptions;
(function (FileFormattingOptions) {
    function fromConfiguration(document) {
        const filesConfig = vscode_1.workspace.getConfiguration('files', document);
        return {
            trimTrailingWhitespace: filesConfig.get('trimTrailingWhitespace'),
            trimFinalNewlines: filesConfig.get('trimFinalNewlines'),
            insertFinalNewline: filesConfig.get('insertFinalNewline'),
        };
    }
    FileFormattingOptions.fromConfiguration = fromConfiguration;
})(FileFormattingOptions || (FileFormattingOptions = {}));
class DocumentFormattingFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'formatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDocumentFormattingEdits: (document, options, token) => {
                const client = this._client;
                const provideDocumentFormattingEdits = (document, options, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, params, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asTextEdits(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentFormattingRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentFormattingEdits
                    ? middleware.provideDocumentFormattingEdits(document, options, token, provideDocumentFormattingEdits)
                    : provideDocumentFormattingEdits(document, options, token);
            }
        };
        return [vscode_1.languages.registerDocumentFormattingEditProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.DocumentFormattingFeature = DocumentFormattingFeature;
class DocumentRangeFormattingFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'rangeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentRangeFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDocumentRangeFormattingEdits: (document, range, options, token) => {
                const client = this._client;
                const provideDocumentRangeFormattingEdits = (document, range, options, token) => {
                    const params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(range),
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, params, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asTextEdits(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentRangeFormattingRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentRangeFormattingEdits
                    ? middleware.provideDocumentRangeFormattingEdits(document, range, options, token, provideDocumentRangeFormattingEdits)
                    : provideDocumentRangeFormattingEdits(document, range, options, token);
            }
        };
        return [vscode_1.languages.registerDocumentRangeFormattingEditProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.DocumentRangeFormattingFeature = DocumentRangeFormattingFeature;
class DocumentOnTypeFormattingFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'onTypeFormatting').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentOnTypeFormattingProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideOnTypeFormattingEdits: (document, position, ch, options, token) => {
                const client = this._client;
                const provideOnTypeFormattingEdits = (document, position, ch, options, token) => {
                    let params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        position: client.code2ProtocolConverter.asPosition(position),
                        ch: ch,
                        options: client.code2ProtocolConverter.asFormattingOptions(options, FileFormattingOptions.fromConfiguration(document))
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, params, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asTextEdits(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentOnTypeFormattingRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideOnTypeFormattingEdits
                    ? middleware.provideOnTypeFormattingEdits(document, position, ch, options, token, provideOnTypeFormattingEdits)
                    : provideOnTypeFormattingEdits(document, position, ch, options, token);
            }
        };
        const moreTriggerCharacter = options.moreTriggerCharacter || [];
        return [vscode_1.languages.registerOnTypeFormattingEditProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider, options.firstTriggerCharacter, ...moreTriggerCharacter), provider];
    }
}
exports.DocumentOnTypeFormattingFeature = DocumentOnTypeFormattingFeature;
//# sourceMappingURL=formatting.js.map

/***/ }),
/* 137 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.RenameFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const UUID = __webpack_require__(115);
const Is = __webpack_require__(54);
const features_1 = __webpack_require__(117);
class RenameFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.RenameRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let rename = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'rename');
        rename.dynamicRegistration = true;
        rename.prepareSupport = true;
        rename.prepareSupportDefaultBehavior = vscode_languageserver_protocol_1.PrepareSupportDefaultBehavior.Identifier;
        rename.honorsChangeAnnotations = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.renameProvider);
        if (!options) {
            return;
        }
        if (Is.boolean(capabilities.renameProvider)) {
            options.prepareProvider = false;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideRenameEdits: (document, position, newName, token) => {
                const client = this._client;
                const provideRenameEdits = (document, position, newName, token) => {
                    let params = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        position: client.code2ProtocolConverter.asPosition(position),
                        newName: newName
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.RenameRequest.type, params, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asWorkspaceEdit(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.RenameRequest.type, token, error, null, false);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideRenameEdits
                    ? middleware.provideRenameEdits(document, position, newName, token, provideRenameEdits)
                    : provideRenameEdits(document, position, newName, token);
            },
            prepareRename: options.prepareProvider
                ? (document, position, token) => {
                    const client = this._client;
                    const prepareRename = (document, position, token) => {
                        let params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                            position: client.code2ProtocolConverter.asPosition(position),
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.PrepareRenameRequest.type, params, token).then((result) => {
                            if (token.isCancellationRequested) {
                                return null;
                            }
                            if (vscode_languageserver_protocol_1.Range.is(result)) {
                                return client.protocol2CodeConverter.asRange(result);
                            }
                            else if (this.isDefaultBehavior(result)) {
                                return result.defaultBehavior === true
                                    ? null
                                    : Promise.reject(new Error(`The element can't be renamed.`));
                            }
                            else if (result && vscode_languageserver_protocol_1.Range.is(result.range)) {
                                return {
                                    range: client.protocol2CodeConverter.asRange(result.range),
                                    placeholder: result.placeholder
                                };
                            }
                            // To cancel the rename vscode API expects a rejected promise.
                            return Promise.reject(new Error(`The element can't be renamed.`));
                        }, (error) => {
                            if (typeof error.message === 'string') {
                                throw new Error(error.message);
                            }
                            else {
                                throw new Error(`The element can't be renamed.`);
                            }
                        });
                    };
                    const middleware = client.middleware;
                    return middleware.prepareRename
                        ? middleware.prepareRename(document, position, token, prepareRename)
                        : prepareRename(document, position, token);
                }
                : undefined
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerRenameProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
    isDefaultBehavior(value) {
        const candidate = value;
        return candidate && Is.boolean(candidate.defaultBehavior);
    }
}
exports.RenameFeature = RenameFeature;
//# sourceMappingURL=rename.js.map

/***/ }),
/* 138 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DocumentLinkFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const UUID = __webpack_require__(115);
class DocumentLinkFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentLinkRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const documentLinkCapabilities = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'documentLink');
        documentLinkCapabilities.dynamicRegistration = true;
        documentLinkCapabilities.tooltipSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const options = this.getRegistrationOptions(documentSelector, capabilities.documentLinkProvider);
        if (!options) {
            return;
        }
        this.register({ id: UUID.generateUuid(), registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDocumentLinks: (document, token) => {
                const client = this._client;
                const provideDocumentLinks = (document, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, client.code2ProtocolConverter.asDocumentLinkParams(document), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDocumentLinks(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentLinkRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentLinks
                    ? middleware.provideDocumentLinks(document, token, provideDocumentLinks)
                    : provideDocumentLinks(document, token);
            },
            resolveDocumentLink: options.resolveProvider
                ? (link, token) => {
                    const client = this._client;
                    let resolveDocumentLink = (link, token) => {
                        return client.sendRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, client.code2ProtocolConverter.asDocumentLink(link), token).then((result) => {
                            if (token.isCancellationRequested) {
                                return link;
                            }
                            return client.protocol2CodeConverter.asDocumentLink(result);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentLinkResolveRequest.type, token, error, link);
                        });
                    };
                    const middleware = client.middleware;
                    return middleware.resolveDocumentLink
                        ? middleware.resolveDocumentLink(link, token, resolveDocumentLink)
                        : resolveDocumentLink(link, token);
                }
                : undefined
        };
        return [vscode_1.languages.registerDocumentLinkProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.DocumentLinkFeature = DocumentLinkFeature;
//# sourceMappingURL=documentLink.js.map

/***/ }),
/* 139 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ExecuteCommandFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const UUID = __webpack_require__(115);
const features_1 = __webpack_require__(117);
class ExecuteCommandFeature {
    constructor(client) {
        this._client = client;
        this._commands = new Map();
    }
    getState() {
        return { kind: 'workspace', id: this.registrationType.method, registrations: this._commands.size > 0 };
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.ExecuteCommandRequest.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'executeCommand').dynamicRegistration = true;
    }
    initialize(capabilities) {
        if (!capabilities.executeCommandProvider) {
            return;
        }
        this.register({
            id: UUID.generateUuid(),
            registerOptions: Object.assign({}, capabilities.executeCommandProvider)
        });
    }
    register(data) {
        const client = this._client;
        const middleware = client.middleware;
        const executeCommand = (command, args) => {
            let params = {
                command,
                arguments: args
            };
            return client.sendRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, params).then(undefined, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.ExecuteCommandRequest.type, undefined, error, undefined);
            });
        };
        if (data.registerOptions.commands) {
            const disposables = [];
            for (const command of data.registerOptions.commands) {
                disposables.push(vscode_1.commands.registerCommand(command, (...args) => {
                    return middleware.executeCommand
                        ? middleware.executeCommand(command, args, executeCommand)
                        : executeCommand(command, args);
                }));
            }
            this._commands.set(data.id, disposables);
        }
    }
    unregister(id) {
        let disposables = this._commands.get(id);
        if (disposables) {
            disposables.forEach(disposable => disposable.dispose());
        }
    }
    dispose() {
        this._commands.forEach((value) => {
            value.forEach(disposable => disposable.dispose());
        });
        this._commands.clear();
    }
}
exports.ExecuteCommandFeature = ExecuteCommandFeature;
//# sourceMappingURL=executeCommand.js.map

/***/ }),
/* 140 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.FileSystemWatcherFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class FileSystemWatcherFeature {
    constructor(client, notifyFileEvent) {
        this._client = client;
        this._notifyFileEvent = notifyFileEvent;
        this._watchers = new Map();
    }
    getState() {
        return { kind: 'workspace', id: this.registrationType.method, registrations: this._watchers.size > 0 };
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeWatchedFilesNotification.type;
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'didChangeWatchedFiles').dynamicRegistration = true;
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'didChangeWatchedFiles').relativePatternSupport = true;
    }
    initialize(_capabilities, _documentSelector) {
    }
    register(data) {
        if (!Array.isArray(data.registerOptions.watchers)) {
            return;
        }
        const disposables = [];
        for (const watcher of data.registerOptions.watchers) {
            const globPattern = this._client.protocol2CodeConverter.asGlobPattern(watcher.globPattern);
            if (globPattern === undefined) {
                continue;
            }
            let watchCreate = true, watchChange = true, watchDelete = true;
            if (watcher.kind !== undefined && watcher.kind !== null) {
                watchCreate = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Create) !== 0;
                watchChange = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Change) !== 0;
                watchDelete = (watcher.kind & vscode_languageserver_protocol_1.WatchKind.Delete) !== 0;
            }
            const fileSystemWatcher = vscode_1.workspace.createFileSystemWatcher(globPattern, !watchCreate, !watchChange, !watchDelete);
            this.hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete, disposables);
            disposables.push(fileSystemWatcher);
        }
        this._watchers.set(data.id, disposables);
    }
    registerRaw(id, fileSystemWatchers) {
        let disposables = [];
        for (let fileSystemWatcher of fileSystemWatchers) {
            this.hookListeners(fileSystemWatcher, true, true, true, disposables);
        }
        this._watchers.set(id, disposables);
    }
    hookListeners(fileSystemWatcher, watchCreate, watchChange, watchDelete, listeners) {
        if (watchCreate) {
            fileSystemWatcher.onDidCreate((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Created
            }), null, listeners);
        }
        if (watchChange) {
            fileSystemWatcher.onDidChange((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Changed
            }), null, listeners);
        }
        if (watchDelete) {
            fileSystemWatcher.onDidDelete((resource) => this._notifyFileEvent({
                uri: this._client.code2ProtocolConverter.asUri(resource),
                type: vscode_languageserver_protocol_1.FileChangeType.Deleted
            }), null, listeners);
        }
    }
    unregister(id) {
        let disposables = this._watchers.get(id);
        if (disposables) {
            for (let disposable of disposables) {
                disposable.dispose();
            }
        }
    }
    dispose() {
        this._watchers.forEach((disposables) => {
            for (let disposable of disposables) {
                disposable.dispose();
            }
        });
        this._watchers.clear();
    }
}
exports.FileSystemWatcherFeature = FileSystemWatcherFeature;
//# sourceMappingURL=fileSystemWatcher.js.map

/***/ }),
/* 141 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ColorProviderFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class ColorProviderFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DocumentColorRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'colorProvider').dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.colorProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideColorPresentations: (color, context, token) => {
                const client = this._client;
                const provideColorPresentations = (color, context, token) => {
                    const requestParams = {
                        color,
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(context.document),
                        range: client.code2ProtocolConverter.asRange(context.range)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, requestParams, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return this._client.protocol2CodeConverter.asColorPresentations(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ColorPresentationRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideColorPresentations
                    ? middleware.provideColorPresentations(color, context, token, provideColorPresentations)
                    : provideColorPresentations(color, context, token);
            },
            provideDocumentColors: (document, token) => {
                const client = this._client;
                const provideDocumentColors = (document, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.DocumentColorRequest.type, requestParams, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return this._client.protocol2CodeConverter.asColorInformations(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DocumentColorRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDocumentColors
                    ? middleware.provideDocumentColors(document, token, provideDocumentColors)
                    : provideDocumentColors(document, token);
            }
        };
        return [vscode_1.languages.registerColorProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.ColorProviderFeature = ColorProviderFeature;
//# sourceMappingURL=colorProvider.js.map

/***/ }),
/* 142 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ImplementationFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class ImplementationFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.ImplementationRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let implementationSupport = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'implementation');
        implementationSupport.dynamicRegistration = true;
        implementationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.implementationProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideImplementation: (document, position, token) => {
                const client = this._client;
                const provideImplementation = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDefinitionResult(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.ImplementationRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideImplementation
                    ? middleware.provideImplementation(document, position, token, provideImplementation)
                    : provideImplementation(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerImplementationProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.ImplementationFeature = ImplementationFeature;
//# sourceMappingURL=implementation.js.map

/***/ }),
/* 143 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeDefinitionFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class TypeDefinitionFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.TypeDefinitionRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'typeDefinition').dynamicRegistration = true;
        let typeDefinitionSupport = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'typeDefinition');
        typeDefinitionSupport.dynamicRegistration = true;
        typeDefinitionSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.typeDefinitionProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideTypeDefinition: (document, position, token) => {
                const client = this._client;
                const provideTypeDefinition = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDefinitionResult(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.TypeDefinitionRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideTypeDefinition
                    ? middleware.provideTypeDefinition(document, position, token, provideTypeDefinition)
                    : provideTypeDefinition(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerTypeDefinitionProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.TypeDefinitionFeature = TypeDefinitionFeature;
//# sourceMappingURL=typeDefinition.js.map

/***/ }),
/* 144 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WorkspaceFoldersFeature = exports.arrayDiff = void 0;
const UUID = __webpack_require__(115);
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
function access(target, key) {
    if (target === void 0) {
        return undefined;
    }
    return target[key];
}
function arrayDiff(left, right) {
    return left.filter(element => right.indexOf(element) < 0);
}
exports.arrayDiff = arrayDiff;
class WorkspaceFoldersFeature {
    constructor(client) {
        this._client = client;
        this._listeners = new Map();
    }
    getState() {
        return { kind: 'workspace', id: this.registrationType.method, registrations: this._listeners.size > 0 };
    }
    get registrationType() {
        return vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type;
    }
    fillInitializeParams(params) {
        const folders = vscode_1.workspace.workspaceFolders;
        this.initializeWithFolders(folders);
        if (folders === void 0) {
            params.workspaceFolders = null;
        }
        else {
            params.workspaceFolders = folders.map(folder => this.asProtocol(folder));
        }
    }
    initializeWithFolders(currentWorkspaceFolders) {
        this._initialFolders = currentWorkspaceFolders;
    }
    fillClientCapabilities(capabilities) {
        capabilities.workspace = capabilities.workspace || {};
        capabilities.workspace.workspaceFolders = true;
    }
    initialize(capabilities) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.WorkspaceFoldersRequest.type, (token) => {
            const workspaceFolders = () => {
                const folders = vscode_1.workspace.workspaceFolders;
                if (folders === undefined) {
                    return null;
                }
                const result = folders.map((folder) => {
                    return this.asProtocol(folder);
                });
                return result;
            };
            const middleware = client.middleware.workspace;
            return middleware && middleware.workspaceFolders
                ? middleware.workspaceFolders(token, workspaceFolders)
                : workspaceFolders(token);
        });
        const value = access(access(access(capabilities, 'workspace'), 'workspaceFolders'), 'changeNotifications');
        let id;
        if (typeof value === 'string') {
            id = value;
        }
        else if (value === true) {
            id = UUID.generateUuid();
        }
        if (id) {
            this.register({ id: id, registerOptions: undefined });
        }
    }
    sendInitialEvent(currentWorkspaceFolders) {
        let promise;
        if (this._initialFolders && currentWorkspaceFolders) {
            const removed = arrayDiff(this._initialFolders, currentWorkspaceFolders);
            const added = arrayDiff(currentWorkspaceFolders, this._initialFolders);
            if (added.length > 0 || removed.length > 0) {
                promise = this.doSendEvent(added, removed);
            }
        }
        else if (this._initialFolders) {
            promise = this.doSendEvent([], this._initialFolders);
        }
        else if (currentWorkspaceFolders) {
            promise = this.doSendEvent(currentWorkspaceFolders, []);
        }
        if (promise !== undefined) {
            promise.catch((error) => {
                this._client.error(`Sending notification ${vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type.method} failed`, error);
            });
        }
    }
    doSendEvent(addedFolders, removedFolders) {
        let params = {
            event: {
                added: addedFolders.map(folder => this.asProtocol(folder)),
                removed: removedFolders.map(folder => this.asProtocol(folder))
            }
        };
        return this._client.sendNotification(vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type, params);
    }
    register(data) {
        let id = data.id;
        let client = this._client;
        let disposable = vscode_1.workspace.onDidChangeWorkspaceFolders((event) => {
            let didChangeWorkspaceFolders = (event) => {
                return this.doSendEvent(event.added, event.removed);
            };
            let middleware = client.middleware.workspace;
            const promise = middleware && middleware.didChangeWorkspaceFolders
                ? middleware.didChangeWorkspaceFolders(event, didChangeWorkspaceFolders)
                : didChangeWorkspaceFolders(event);
            promise.catch((error) => {
                this._client.error(`Sending notification ${vscode_languageserver_protocol_1.DidChangeWorkspaceFoldersNotification.type.method} failed`, error);
            });
        });
        this._listeners.set(id, disposable);
        this.sendInitialEvent(vscode_1.workspace.workspaceFolders);
    }
    unregister(id) {
        let disposable = this._listeners.get(id);
        if (disposable === void 0) {
            return;
        }
        this._listeners.delete(id);
        disposable.dispose();
    }
    dispose() {
        for (let disposable of this._listeners.values()) {
            disposable.dispose();
        }
        this._listeners.clear();
    }
    asProtocol(workspaceFolder) {
        if (workspaceFolder === void 0) {
            return null;
        }
        return { uri: this._client.code2ProtocolConverter.asUri(workspaceFolder.uri), name: workspaceFolder.name };
    }
}
exports.WorkspaceFoldersFeature = WorkspaceFoldersFeature;
//# sourceMappingURL=workspaceFolder.js.map

/***/ }),
/* 145 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.FoldingRangeFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class FoldingRangeFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.FoldingRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        let capability = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'foldingRange');
        capability.dynamicRegistration = true;
        capability.rangeLimit = 5000;
        capability.lineFoldingOnly = true;
        capability.foldingRangeKind = { valueSet: [vscode_languageserver_protocol_1.FoldingRangeKind.Comment, vscode_languageserver_protocol_1.FoldingRangeKind.Imports, vscode_languageserver_protocol_1.FoldingRangeKind.Region] };
        capability.foldingRange = { collapsedText: false };
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.foldingRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideFoldingRanges: (document, context, token) => {
                const client = this._client;
                const provideFoldingRanges = (document, _, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, requestParams, token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asFoldingRanges(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.FoldingRangeRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideFoldingRanges
                    ? middleware.provideFoldingRanges(document, context, token, provideFoldingRanges)
                    : provideFoldingRanges(document, context, token);
            }
        };
        return [vscode_1.languages.registerFoldingRangeProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider), provider];
    }
}
exports.FoldingRangeFeature = FoldingRangeFeature;
//# sourceMappingURL=foldingRange.js.map

/***/ }),
/* 146 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DeclarationFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class DeclarationFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.DeclarationRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const declarationSupport = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'declaration');
        declarationSupport.dynamicRegistration = true;
        declarationSupport.linkSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.declarationProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideDeclaration: (document, position, token) => {
                const client = this._client;
                const provideDeclaration = (document, position, token) => {
                    return client.sendRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asDeclarationResult(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.DeclarationRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideDeclaration
                    ? middleware.provideDeclaration(document, position, token, provideDeclaration)
                    : provideDeclaration(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerDeclarationProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.DeclarationFeature = DeclarationFeature;
//# sourceMappingURL=declaration.js.map

/***/ }),
/* 147 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SelectionRangeFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class SelectionRangeFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SelectionRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const capability = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'selectionRange');
        capability.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.selectionRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideSelectionRanges: (document, positions, token) => {
                const client = this._client;
                const provideSelectionRanges = async (document, positions, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        positions: await client.code2ProtocolConverter.asPositions(positions, token)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.SelectionRangeRequest.type, requestParams, token).then((ranges) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asSelectionRanges(ranges, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.SelectionRangeRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideSelectionRanges
                    ? middleware.provideSelectionRanges(document, positions, token, provideSelectionRanges)
                    : provideSelectionRanges(document, positions, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerSelectionRangeProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.SelectionRangeFeature = SelectionRangeFeature;
//# sourceMappingURL=selectionRange.js.map

/***/ }),
/* 148 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.ProgressFeature = void 0;
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const progressPart_1 = __webpack_require__(116);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = Object.create(null);
    }
    return target[key];
}
class ProgressFeature {
    constructor(_client) {
        this._client = _client;
        this.activeParts = new Set();
    }
    getState() {
        return { kind: 'window', id: vscode_languageserver_protocol_1.WorkDoneProgressCreateRequest.method, registrations: this.activeParts.size > 0 };
    }
    fillClientCapabilities(capabilities) {
        ensure(capabilities, 'window').workDoneProgress = true;
    }
    initialize() {
        const client = this._client;
        const deleteHandler = (part) => {
            this.activeParts.delete(part);
        };
        const createHandler = (params) => {
            this.activeParts.add(new progressPart_1.ProgressPart(this._client, params.token, deleteHandler));
        };
        client.onRequest(vscode_languageserver_protocol_1.WorkDoneProgressCreateRequest.type, createHandler);
    }
    dispose() {
        for (const part of this.activeParts) {
            part.done();
        }
        this.activeParts.clear();
    }
}
exports.ProgressFeature = ProgressFeature;
//# sourceMappingURL=progress.js.map

/***/ }),
/* 149 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.CallHierarchyFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class CallHierarchyProvider {
    constructor(client) {
        this.client = client;
        this.middleware = client.middleware;
    }
    prepareCallHierarchy(document, position, token) {
        const client = this.client;
        const middleware = this.middleware;
        const prepareCallHierarchy = (document, position, token) => {
            const params = client.code2ProtocolConverter.asTextDocumentPositionParams(document, position);
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asCallHierarchyItems(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type, token, error, null);
            });
        };
        return middleware.prepareCallHierarchy
            ? middleware.prepareCallHierarchy(document, position, token, prepareCallHierarchy)
            : prepareCallHierarchy(document, position, token);
    }
    provideCallHierarchyIncomingCalls(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideCallHierarchyIncomingCalls = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asCallHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyIncomingCallsRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asCallHierarchyIncomingCalls(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyIncomingCallsRequest.type, token, error, null);
            });
        };
        return middleware.provideCallHierarchyIncomingCalls
            ? middleware.provideCallHierarchyIncomingCalls(item, token, provideCallHierarchyIncomingCalls)
            : provideCallHierarchyIncomingCalls(item, token);
    }
    provideCallHierarchyOutgoingCalls(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideCallHierarchyOutgoingCalls = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asCallHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.CallHierarchyOutgoingCallsRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asCallHierarchyOutgoingCalls(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.CallHierarchyOutgoingCallsRequest.type, token, error, null);
            });
        };
        return middleware.provideCallHierarchyOutgoingCalls
            ? middleware.provideCallHierarchyOutgoingCalls(item, token, provideCallHierarchyOutgoingCalls)
            : provideCallHierarchyOutgoingCalls(item, token);
    }
}
class CallHierarchyFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.CallHierarchyPrepareRequest.type);
    }
    fillClientCapabilities(cap) {
        const capabilities = cap;
        const capability = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'callHierarchy');
        capability.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.callHierarchyProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const client = this._client;
        const provider = new CallHierarchyProvider(client);
        return [vscode_1.languages.registerCallHierarchyProvider(this._client.protocol2CodeConverter.asDocumentSelector(options.documentSelector), provider), provider];
    }
}
exports.CallHierarchyFeature = CallHierarchyFeature;
//# sourceMappingURL=callHierarchy.js.map

/***/ }),
/* 150 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.SemanticTokensFeature = void 0;
const vscode = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
const Is = __webpack_require__(54);
class SemanticTokensFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.SemanticTokensRegistrationType.type);
    }
    fillClientCapabilities(capabilities) {
        const capability = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'semanticTokens');
        capability.dynamicRegistration = true;
        capability.tokenTypes = [
            vscode_languageserver_protocol_1.SemanticTokenTypes.namespace,
            vscode_languageserver_protocol_1.SemanticTokenTypes.type,
            vscode_languageserver_protocol_1.SemanticTokenTypes.class,
            vscode_languageserver_protocol_1.SemanticTokenTypes.enum,
            vscode_languageserver_protocol_1.SemanticTokenTypes.interface,
            vscode_languageserver_protocol_1.SemanticTokenTypes.struct,
            vscode_languageserver_protocol_1.SemanticTokenTypes.typeParameter,
            vscode_languageserver_protocol_1.SemanticTokenTypes.parameter,
            vscode_languageserver_protocol_1.SemanticTokenTypes.variable,
            vscode_languageserver_protocol_1.SemanticTokenTypes.property,
            vscode_languageserver_protocol_1.SemanticTokenTypes.enumMember,
            vscode_languageserver_protocol_1.SemanticTokenTypes.event,
            vscode_languageserver_protocol_1.SemanticTokenTypes.function,
            vscode_languageserver_protocol_1.SemanticTokenTypes.method,
            vscode_languageserver_protocol_1.SemanticTokenTypes.macro,
            vscode_languageserver_protocol_1.SemanticTokenTypes.keyword,
            vscode_languageserver_protocol_1.SemanticTokenTypes.modifier,
            vscode_languageserver_protocol_1.SemanticTokenTypes.comment,
            vscode_languageserver_protocol_1.SemanticTokenTypes.string,
            vscode_languageserver_protocol_1.SemanticTokenTypes.number,
            vscode_languageserver_protocol_1.SemanticTokenTypes.regexp,
            vscode_languageserver_protocol_1.SemanticTokenTypes.operator,
            vscode_languageserver_protocol_1.SemanticTokenTypes.decorator
        ];
        capability.tokenModifiers = [
            vscode_languageserver_protocol_1.SemanticTokenModifiers.declaration,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.definition,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.readonly,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.static,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.deprecated,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.abstract,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.async,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.modification,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.documentation,
            vscode_languageserver_protocol_1.SemanticTokenModifiers.defaultLibrary
        ];
        capability.formats = [vscode_languageserver_protocol_1.TokenFormat.Relative];
        capability.requests = {
            range: true,
            full: {
                delta: true
            }
        };
        capability.multilineTokenSupport = false;
        capability.overlappingTokenSupport = false;
        capability.serverCancelSupport = true;
        capability.augmentsSyntaxTokens = true;
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'semanticTokens').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        const client = this._client;
        client.onRequest(vscode_languageserver_protocol_1.SemanticTokensRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeSemanticTokensEmitter.fire();
            }
        });
        const [id, options] = this.getRegistration(documentSelector, capabilities.semanticTokensProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const fullProvider = Is.boolean(options.full) ? options.full : options.full !== undefined;
        const hasEditProvider = options.full !== undefined && typeof options.full !== 'boolean' && options.full.delta === true;
        const eventEmitter = new vscode.EventEmitter();
        const documentProvider = fullProvider
            ? {
                onDidChangeSemanticTokens: eventEmitter.event,
                provideDocumentSemanticTokens: (document, token) => {
                    const client = this._client;
                    const middleware = client.middleware;
                    const provideDocumentSemanticTokens = (document, token) => {
                        const params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document)
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensRequest.type, params, token).then((result) => {
                            if (token.isCancellationRequested) {
                                return null;
                            }
                            return client.protocol2CodeConverter.asSemanticTokens(result, token);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensRequest.type, token, error, null);
                        });
                    };
                    return middleware.provideDocumentSemanticTokens
                        ? middleware.provideDocumentSemanticTokens(document, token, provideDocumentSemanticTokens)
                        : provideDocumentSemanticTokens(document, token);
                },
                provideDocumentSemanticTokensEdits: hasEditProvider
                    ? (document, previousResultId, token) => {
                        const client = this._client;
                        const middleware = client.middleware;
                        const provideDocumentSemanticTokensEdits = (document, previousResultId, token) => {
                            const params = {
                                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                                previousResultId
                            };
                            return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.type, params, token).then(async (result) => {
                                if (token.isCancellationRequested) {
                                    return null;
                                }
                                if (vscode_languageserver_protocol_1.SemanticTokens.is(result)) {
                                    return await client.protocol2CodeConverter.asSemanticTokens(result, token);
                                }
                                else {
                                    return await client.protocol2CodeConverter.asSemanticTokensEdits(result, token);
                                }
                            }, (error) => {
                                return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensDeltaRequest.type, token, error, null);
                            });
                        };
                        return middleware.provideDocumentSemanticTokensEdits
                            ? middleware.provideDocumentSemanticTokensEdits(document, previousResultId, token, provideDocumentSemanticTokensEdits)
                            : provideDocumentSemanticTokensEdits(document, previousResultId, token);
                    }
                    : undefined
            }
            : undefined;
        const hasRangeProvider = options.range === true;
        const rangeProvider = hasRangeProvider
            ? {
                provideDocumentRangeSemanticTokens: (document, range, token) => {
                    const client = this._client;
                    const middleware = client.middleware;
                    const provideDocumentRangeSemanticTokens = (document, range, token) => {
                        const params = {
                            textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                            range: client.code2ProtocolConverter.asRange(range)
                        };
                        return client.sendRequest(vscode_languageserver_protocol_1.SemanticTokensRangeRequest.type, params, token).then((result) => {
                            if (token.isCancellationRequested) {
                                return null;
                            }
                            return client.protocol2CodeConverter.asSemanticTokens(result, token);
                        }, (error) => {
                            return client.handleFailedRequest(vscode_languageserver_protocol_1.SemanticTokensRangeRequest.type, token, error, null);
                        });
                    };
                    return middleware.provideDocumentRangeSemanticTokens
                        ? middleware.provideDocumentRangeSemanticTokens(document, range, token, provideDocumentRangeSemanticTokens)
                        : provideDocumentRangeSemanticTokens(document, range, token);
                }
            }
            : undefined;
        const disposables = [];
        const client = this._client;
        const legend = client.protocol2CodeConverter.asSemanticTokensLegend(options.legend);
        const documentSelector = client.protocol2CodeConverter.asDocumentSelector(selector);
        if (documentProvider !== undefined) {
            disposables.push(vscode.languages.registerDocumentSemanticTokensProvider(documentSelector, documentProvider, legend));
        }
        if (rangeProvider !== undefined) {
            disposables.push(vscode.languages.registerDocumentRangeSemanticTokensProvider(documentSelector, rangeProvider, legend));
        }
        return [new vscode.Disposable(() => disposables.forEach(item => item.dispose())), { range: rangeProvider, full: documentProvider, onDidChangeSemanticTokensEmitter: eventEmitter }];
    }
}
exports.SemanticTokensFeature = SemanticTokensFeature;
//# sourceMappingURL=semanticTokens.js.map

/***/ }),
/* 151 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.WillDeleteFilesFeature = exports.WillRenameFilesFeature = exports.WillCreateFilesFeature = exports.DidDeleteFilesFeature = exports.DidRenameFilesFeature = exports.DidCreateFilesFeature = void 0;
const code = __webpack_require__(53);
const minimatch = __webpack_require__(119);
const proto = __webpack_require__(56);
const UUID = __webpack_require__(115);
function ensure(target, key) {
    if (target[key] === void 0) {
        target[key] = {};
    }
    return target[key];
}
function access(target, key) {
    return target[key];
}
function assign(target, key, value) {
    target[key] = value;
}
class FileOperationFeature {
    constructor(client, event, registrationType, clientCapability, serverCapability) {
        this._client = client;
        this._event = event;
        this._registrationType = registrationType;
        this._clientCapability = clientCapability;
        this._serverCapability = serverCapability;
        this._filters = new Map();
    }
    getState() {
        return { kind: 'workspace', id: this._registrationType.method, registrations: this._filters.size > 0 };
    }
    filterSize() {
        return this._filters.size;
    }
    get registrationType() {
        return this._registrationType;
    }
    fillClientCapabilities(capabilities) {
        const value = ensure(ensure(capabilities, 'workspace'), 'fileOperations');
        // this happens n times but it is the same value so we tolerate this.
        assign(value, 'dynamicRegistration', true);
        assign(value, this._clientCapability, true);
    }
    initialize(capabilities) {
        const options = capabilities.workspace?.fileOperations;
        const capability = options !== undefined ? access(options, this._serverCapability) : undefined;
        if (capability?.filters !== undefined) {
            try {
                this.register({
                    id: UUID.generateUuid(),
                    registerOptions: { filters: capability.filters }
                });
            }
            catch (e) {
                this._client.warn(`Ignoring invalid glob pattern for ${this._serverCapability} registration: ${e}`);
            }
        }
    }
    register(data) {
        if (!this._listener) {
            this._listener = this._event(this.send, this);
        }
        const minimatchFilter = data.registerOptions.filters.map((filter) => {
            const matcher = new minimatch.Minimatch(filter.pattern.glob, FileOperationFeature.asMinimatchOptions(filter.pattern.options));
            if (!matcher.makeRe()) {
                throw new Error(`Invalid pattern ${filter.pattern.glob}!`);
            }
            return { scheme: filter.scheme, matcher, kind: filter.pattern.matches };
        });
        this._filters.set(data.id, minimatchFilter);
    }
    unregister(id) {
        this._filters.delete(id);
        if (this._filters.size === 0 && this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    dispose() {
        this._filters.clear();
        if (this._listener) {
            this._listener.dispose();
            this._listener = undefined;
        }
    }
    getFileType(uri) {
        return FileOperationFeature.getFileType(uri);
    }
    async filter(event, prop) {
        // (Asynchronously) map each file onto a boolean of whether it matches
        // any of the globs.
        const fileMatches = await Promise.all(event.files.map(async (item) => {
            const uri = prop(item);
            // Use fsPath to make this consistent with file system watchers but help
            // minimatch to use '/' instead of `\\` if present.
            const path = uri.fsPath.replace(/\\/g, '/');
            for (const filters of this._filters.values()) {
                for (const filter of filters) {
                    if (filter.scheme !== undefined && filter.scheme !== uri.scheme) {
                        continue;
                    }
                    if (filter.matcher.match(path)) {
                        // The pattern matches. If kind is undefined then everything is ok
                        if (filter.kind === undefined) {
                            return true;
                        }
                        const fileType = await this.getFileType(uri);
                        // If we can't determine the file type than we treat it as a match.
                        // Dropping it would be another alternative.
                        if (fileType === undefined) {
                            this._client.error(`Failed to determine file type for ${uri.toString()}.`);
                            return true;
                        }
                        if ((fileType === code.FileType.File && filter.kind === proto.FileOperationPatternKind.file) || (fileType === code.FileType.Directory && filter.kind === proto.FileOperationPatternKind.folder)) {
                            return true;
                        }
                    }
                    else if (filter.kind === proto.FileOperationPatternKind.folder) {
                        const fileType = await FileOperationFeature.getFileType(uri);
                        if (fileType === code.FileType.Directory && filter.matcher.match(`${path}/`)) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }));
        // Filter the files to those that matched.
        const files = event.files.filter((_, index) => fileMatches[index]);
        return { ...event, files };
    }
    static async getFileType(uri) {
        try {
            return (await code.workspace.fs.stat(uri)).type;
        }
        catch (e) {
            return undefined;
        }
    }
    static asMinimatchOptions(options) {
        if (options === undefined) {
            return undefined;
        }
        if (options.ignoreCase === true) {
            return { nocase: true };
        }
        return undefined;
    }
}
class NotificationFileOperationFeature extends FileOperationFeature {
    constructor(client, event, notificationType, clientCapability, serverCapability, accessUri, createParams) {
        super(client, event, notificationType, clientCapability, serverCapability);
        this._notificationType = notificationType;
        this._accessUri = accessUri;
        this._createParams = createParams;
    }
    async send(originalEvent) {
        // Create a copy of the event that has the files filtered to match what the
        // server wants.
        const filteredEvent = await this.filter(originalEvent, this._accessUri);
        if (filteredEvent.files.length) {
            const next = async (event) => {
                return this._client.sendNotification(this._notificationType, this._createParams(event));
            };
            return this.doSend(filteredEvent, next);
        }
    }
}
class CachingNotificationFileOperationFeature extends NotificationFileOperationFeature {
    constructor() {
        super(...arguments);
        this._fsPathFileTypes = new Map();
    }
    async getFileType(uri) {
        const fsPath = uri.fsPath;
        if (this._fsPathFileTypes.has(fsPath)) {
            return this._fsPathFileTypes.get(fsPath);
        }
        const type = await FileOperationFeature.getFileType(uri);
        if (type) {
            this._fsPathFileTypes.set(fsPath, type);
        }
        return type;
    }
    async cacheFileTypes(event, prop) {
        // Calling filter will force the matching logic to run. For any item
        // that requires a getFileType lookup, the overriden getFileType will
        // be called that will cache the result so that when onDidRename fires,
        // it can still be checked even though the item no longer exists on disk
        // in its original location.
        await this.filter(event, prop);
    }
    clearFileTypeCache() {
        this._fsPathFileTypes.clear();
    }
    unregister(id) {
        super.unregister(id);
        if (this.filterSize() === 0 && this._willListener) {
            this._willListener.dispose();
            this._willListener = undefined;
        }
    }
    dispose() {
        super.dispose();
        if (this._willListener) {
            this._willListener.dispose();
            this._willListener = undefined;
        }
    }
}
class DidCreateFilesFeature extends NotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidCreateFiles, proto.DidCreateFilesNotification.type, 'didCreate', 'didCreate', (i) => i, client.code2ProtocolConverter.asDidCreateFilesParams);
    }
    doSend(event, next) {
        const middleware = this._client.middleware.workspace;
        return middleware?.didCreateFiles
            ? middleware.didCreateFiles(event, next)
            : next(event);
    }
}
exports.DidCreateFilesFeature = DidCreateFilesFeature;
class DidRenameFilesFeature extends CachingNotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidRenameFiles, proto.DidRenameFilesNotification.type, 'didRename', 'didRename', (i) => i.oldUri, client.code2ProtocolConverter.asDidRenameFilesParams);
    }
    register(data) {
        if (!this._willListener) {
            this._willListener = code.workspace.onWillRenameFiles(this.willRename, this);
        }
        super.register(data);
    }
    willRename(e) {
        e.waitUntil(this.cacheFileTypes(e, (i) => i.oldUri));
    }
    doSend(event, next) {
        this.clearFileTypeCache();
        const middleware = this._client.middleware.workspace;
        return middleware?.didRenameFiles
            ? middleware.didRenameFiles(event, next)
            : next(event);
    }
}
exports.DidRenameFilesFeature = DidRenameFilesFeature;
class DidDeleteFilesFeature extends CachingNotificationFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onDidDeleteFiles, proto.DidDeleteFilesNotification.type, 'didDelete', 'didDelete', (i) => i, client.code2ProtocolConverter.asDidDeleteFilesParams);
    }
    register(data) {
        if (!this._willListener) {
            this._willListener = code.workspace.onWillDeleteFiles(this.willDelete, this);
        }
        super.register(data);
    }
    willDelete(e) {
        e.waitUntil(this.cacheFileTypes(e, (i) => i));
    }
    doSend(event, next) {
        this.clearFileTypeCache();
        const middleware = this._client.middleware.workspace;
        return middleware?.didDeleteFiles
            ? middleware.didDeleteFiles(event, next)
            : next(event);
    }
}
exports.DidDeleteFilesFeature = DidDeleteFilesFeature;
class RequestFileOperationFeature extends FileOperationFeature {
    constructor(client, event, requestType, clientCapability, serverCapability, accessUri, createParams) {
        super(client, event, requestType, clientCapability, serverCapability);
        this._requestType = requestType;
        this._accessUri = accessUri;
        this._createParams = createParams;
    }
    async send(originalEvent) {
        const waitUntil = this.waitUntil(originalEvent);
        originalEvent.waitUntil(waitUntil);
    }
    async waitUntil(originalEvent) {
        // Create a copy of the event that has the files filtered to match what the
        // server wants.
        const filteredEvent = await this.filter(originalEvent, this._accessUri);
        if (filteredEvent.files.length) {
            const next = (event) => {
                return this._client.sendRequest(this._requestType, this._createParams(event), event.token)
                    .then(this._client.protocol2CodeConverter.asWorkspaceEdit);
            };
            return this.doSend(filteredEvent, next);
        }
        else {
            return undefined;
        }
    }
}
class WillCreateFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillCreateFiles, proto.WillCreateFilesRequest.type, 'willCreate', 'willCreate', (i) => i, client.code2ProtocolConverter.asWillCreateFilesParams);
    }
    doSend(event, next) {
        const middleware = this._client.middleware.workspace;
        return middleware?.willCreateFiles
            ? middleware.willCreateFiles(event, next)
            : next(event);
    }
}
exports.WillCreateFilesFeature = WillCreateFilesFeature;
class WillRenameFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillRenameFiles, proto.WillRenameFilesRequest.type, 'willRename', 'willRename', (i) => i.oldUri, client.code2ProtocolConverter.asWillRenameFilesParams);
    }
    doSend(event, next) {
        const middleware = this._client.middleware.workspace;
        return middleware?.willRenameFiles
            ? middleware.willRenameFiles(event, next)
            : next(event);
    }
}
exports.WillRenameFilesFeature = WillRenameFilesFeature;
class WillDeleteFilesFeature extends RequestFileOperationFeature {
    constructor(client) {
        super(client, code.workspace.onWillDeleteFiles, proto.WillDeleteFilesRequest.type, 'willDelete', 'willDelete', (i) => i, client.code2ProtocolConverter.asWillDeleteFilesParams);
    }
    doSend(event, next) {
        const middleware = this._client.middleware.workspace;
        return middleware?.willDeleteFiles
            ? middleware.willDeleteFiles(event, next)
            : next(event);
    }
}
exports.WillDeleteFilesFeature = WillDeleteFilesFeature;
//# sourceMappingURL=fileOperations.js.map

/***/ }),
/* 152 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.LinkedEditingFeature = void 0;
const code = __webpack_require__(53);
const proto = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class LinkedEditingFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, proto.LinkedEditingRangeRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const linkedEditingSupport = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'linkedEditingRange');
        linkedEditingSupport.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        let [id, options] = this.getRegistration(documentSelector, capabilities.linkedEditingRangeProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const provider = {
            provideLinkedEditingRanges: (document, position, token) => {
                const client = this._client;
                const provideLinkedEditing = (document, position, token) => {
                    return client.sendRequest(proto.LinkedEditingRangeRequest.type, client.code2ProtocolConverter.asTextDocumentPositionParams(document, position), token).then((result) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asLinkedEditingRanges(result, token);
                    }, (error) => {
                        return client.handleFailedRequest(proto.LinkedEditingRangeRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideLinkedEditingRange
                    ? middleware.provideLinkedEditingRange(document, position, token, provideLinkedEditing)
                    : provideLinkedEditing(document, position, token);
            }
        };
        return [this.registerProvider(selector, provider), provider];
    }
    registerProvider(selector, provider) {
        return code.languages.registerLinkedEditingRangeProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.LinkedEditingFeature = LinkedEditingFeature;
//# sourceMappingURL=linkedEditingRange.js.map

/***/ }),
/* 153 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.TypeHierarchyFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class TypeHierarchyProvider {
    constructor(client) {
        this.client = client;
        this.middleware = client.middleware;
    }
    prepareTypeHierarchy(document, position, token) {
        const client = this.client;
        const middleware = this.middleware;
        const prepareTypeHierarchy = (document, position, token) => {
            const params = client.code2ProtocolConverter.asTextDocumentPositionParams(document, position);
            return client.sendRequest(vscode_languageserver_protocol_1.TypeHierarchyPrepareRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asTypeHierarchyItems(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.TypeHierarchyPrepareRequest.type, token, error, null);
            });
        };
        return middleware.prepareTypeHierarchy
            ? middleware.prepareTypeHierarchy(document, position, token, prepareTypeHierarchy)
            : prepareTypeHierarchy(document, position, token);
    }
    provideTypeHierarchySupertypes(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideTypeHierarchySupertypes = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asTypeHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.TypeHierarchySupertypesRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asTypeHierarchyItems(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.TypeHierarchySupertypesRequest.type, token, error, null);
            });
        };
        return middleware.provideTypeHierarchySupertypes
            ? middleware.provideTypeHierarchySupertypes(item, token, provideTypeHierarchySupertypes)
            : provideTypeHierarchySupertypes(item, token);
    }
    provideTypeHierarchySubtypes(item, token) {
        const client = this.client;
        const middleware = this.middleware;
        const provideTypeHierarchySubtypes = (item, token) => {
            const params = {
                item: client.code2ProtocolConverter.asTypeHierarchyItem(item)
            };
            return client.sendRequest(vscode_languageserver_protocol_1.TypeHierarchySubtypesRequest.type, params, token).then((result) => {
                if (token.isCancellationRequested) {
                    return null;
                }
                return client.protocol2CodeConverter.asTypeHierarchyItems(result, token);
            }, (error) => {
                return client.handleFailedRequest(vscode_languageserver_protocol_1.TypeHierarchySubtypesRequest.type, token, error, null);
            });
        };
        return middleware.provideTypeHierarchySubtypes
            ? middleware.provideTypeHierarchySubtypes(item, token, provideTypeHierarchySubtypes)
            : provideTypeHierarchySubtypes(item, token);
    }
}
class TypeHierarchyFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.TypeHierarchyPrepareRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const capability = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'typeHierarchy');
        capability.dynamicRegistration = true;
    }
    initialize(capabilities, documentSelector) {
        const [id, options] = this.getRegistration(documentSelector, capabilities.typeHierarchyProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const client = this._client;
        const provider = new TypeHierarchyProvider(client);
        return [vscode_1.languages.registerTypeHierarchyProvider(client.protocol2CodeConverter.asDocumentSelector(options.documentSelector), provider), provider];
    }
}
exports.TypeHierarchyFeature = TypeHierarchyFeature;
//# sourceMappingURL=typeHierarchy.js.map

/***/ }),
/* 154 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.InlineValueFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class InlineValueFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.InlineValueRequest.type);
    }
    fillClientCapabilities(capabilities) {
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'inlineValue').dynamicRegistration = true;
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'inlineValue').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        this._client.onRequest(vscode_languageserver_protocol_1.InlineValueRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeInlineValues.fire();
            }
        });
        const [id, options] = this.getRegistration(documentSelector, capabilities.inlineValueProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const eventEmitter = new vscode_1.EventEmitter();
        const provider = {
            onDidChangeInlineValues: eventEmitter.event,
            provideInlineValues: (document, viewPort, context, token) => {
                const client = this._client;
                const provideInlineValues = (document, viewPort, context, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(viewPort),
                        context: client.code2ProtocolConverter.asInlineValueContext(context)
                    };
                    return client.sendRequest(vscode_languageserver_protocol_1.InlineValueRequest.type, requestParams, token).then((values) => {
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asInlineValues(values, token);
                    }, (error) => {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.InlineValueRequest.type, token, error, null);
                    });
                };
                const middleware = client.middleware;
                return middleware.provideInlineValues
                    ? middleware.provideInlineValues(document, viewPort, context, token, provideInlineValues)
                    : provideInlineValues(document, viewPort, context, token);
            }
        };
        return [this.registerProvider(selector, provider), { provider: provider, onDidChangeInlineValues: eventEmitter }];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerInlineValuesProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.InlineValueFeature = InlineValueFeature;
//# sourceMappingURL=inlineValue.js.map

/***/ }),
/* 155 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.InlayHintsFeature = void 0;
const vscode_1 = __webpack_require__(53);
const vscode_languageserver_protocol_1 = __webpack_require__(56);
const features_1 = __webpack_require__(117);
class InlayHintsFeature extends features_1.TextDocumentLanguageFeature {
    constructor(client) {
        super(client, vscode_languageserver_protocol_1.InlayHintRequest.type);
    }
    fillClientCapabilities(capabilities) {
        const inlayHint = (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'textDocument'), 'inlayHint');
        inlayHint.dynamicRegistration = true;
        inlayHint.resolveSupport = {
            properties: ['tooltip', 'textEdits', 'label.tooltip', 'label.location', 'label.command']
        };
        (0, features_1.ensure)((0, features_1.ensure)(capabilities, 'workspace'), 'inlayHint').refreshSupport = true;
    }
    initialize(capabilities, documentSelector) {
        this._client.onRequest(vscode_languageserver_protocol_1.InlayHintRefreshRequest.type, async () => {
            for (const provider of this.getAllProviders()) {
                provider.onDidChangeInlayHints.fire();
            }
        });
        const [id, options] = this.getRegistration(documentSelector, capabilities.inlayHintProvider);
        if (!id || !options) {
            return;
        }
        this.register({ id: id, registerOptions: options });
    }
    registerLanguageProvider(options) {
        const selector = options.documentSelector;
        const eventEmitter = new vscode_1.EventEmitter();
        const provider = {
            onDidChangeInlayHints: eventEmitter.event,
            provideInlayHints: (document, viewPort, token) => {
                const client = this._client;
                const provideInlayHints = async (document, viewPort, token) => {
                    const requestParams = {
                        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(document),
                        range: client.code2ProtocolConverter.asRange(viewPort)
                    };
                    try {
                        const values = await client.sendRequest(vscode_languageserver_protocol_1.InlayHintRequest.type, requestParams, token);
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        return client.protocol2CodeConverter.asInlayHints(values, token);
                    }
                    catch (error) {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.InlayHintRequest.type, token, error, null);
                    }
                };
                const middleware = client.middleware;
                return middleware.provideInlayHints
                    ? middleware.provideInlayHints(document, viewPort, token, provideInlayHints)
                    : provideInlayHints(document, viewPort, token);
            }
        };
        provider.resolveInlayHint = options.resolveProvider === true
            ? (hint, token) => {
                const client = this._client;
                const resolveInlayHint = async (item, token) => {
                    try {
                        const value = await client.sendRequest(vscode_languageserver_protocol_1.InlayHintResolveRequest.type, client.code2ProtocolConverter.asInlayHint(item), token);
                        if (token.isCancellationRequested) {
                            return null;
                        }
                        const result = client.protocol2CodeConverter.asInlayHint(value, token);
                        return token.isCancellationRequested ? null : result;
                    }
                    catch (error) {
                        return client.handleFailedRequest(vscode_languageserver_protocol_1.InlayHintResolveRequest.type, token, error, null);
                    }
                };
                const middleware = client.middleware;
                return middleware.resolveInlayHint
                    ? middleware.resolveInlayHint(hint, token, resolveInlayHint)
                    : resolveInlayHint(hint, token);
            }
            : undefined;
        return [this.registerProvider(selector, provider), { provider: provider, onDidChangeInlayHints: eventEmitter }];
    }
    registerProvider(selector, provider) {
        return vscode_1.languages.registerInlayHintsProvider(this._client.protocol2CodeConverter.asDocumentSelector(selector), provider);
    }
}
exports.InlayHintsFeature = InlayHintsFeature;
//# sourceMappingURL=inlayHint.js.map

/***/ }),
/* 156 */
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.terminate = void 0;
const cp = __webpack_require__(4);
const path_1 = __webpack_require__(1);
const isWindows = (process.platform === 'win32');
const isMacintosh = (process.platform === 'darwin');
const isLinux = (process.platform === 'linux');
function terminate(process, cwd) {
    if (isWindows) {
        try {
            // This we run in Atom execFileSync is available.
            // Ignore stderr since this is otherwise piped to parent.stderr
            // which might be already closed.
            let options = {
                stdio: ['pipe', 'pipe', 'ignore']
            };
            if (cwd) {
                options.cwd = cwd;
            }
            cp.execFileSync('taskkill', ['/T', '/F', '/PID', process.pid.toString()], options);
            return true;
        }
        catch (err) {
            return false;
        }
    }
    else if (isLinux || isMacintosh) {
        try {
            var cmd = (0, path_1.join)(__dirname, 'terminateProcess.sh');
            var result = cp.spawnSync(cmd, [process.pid.toString()]);
            return result.error ? false : true;
        }
        catch (err) {
            return false;
        }
    }
    else {
        process.kill('SIGKILL');
        return true;
    }
}
exports.terminate = terminate;
//# sourceMappingURL=processes.js.map

/***/ }),
/* 157 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ----------------------------------------------------------------------------------------- */


module.exports = __webpack_require__(56);

/***/ }),
/* 158 */
/***/ (function(__unused_webpack_module, exports, __webpack_require__) {

"use strict";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.DiagnosticPullMode = exports.vsdiag = void 0;
__exportStar(__webpack_require__(56), exports);
__exportStar(__webpack_require__(117), exports);
var diagnostic_1 = __webpack_require__(118);
Object.defineProperty(exports, "vsdiag", ({ enumerable: true, get: function () { return diagnostic_1.vsdiag; } }));
Object.defineProperty(exports, "DiagnosticPullMode", ({ enumerable: true, get: function () { return diagnostic_1.DiagnosticPullMode; } }));
__exportStar(__webpack_require__(55), exports);
//# sourceMappingURL=api.js.map

/***/ }),
/* 159 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

const isWindows = process.platform === 'win32' ||
    process.env.OSTYPE === 'cygwin' ||
    process.env.OSTYPE === 'msys'

const path = __webpack_require__(1)
const COLON = isWindows ? ';' : ':'
const isexe = __webpack_require__(160)

const getNotFoundError = (cmd) =>
  Object.assign(new Error(`not found: ${cmd}`), { code: 'ENOENT' })

const getPathInfo = (cmd, opt) => {
  const colon = opt.colon || COLON

  // If it has a slash, then we don't bother searching the pathenv.
  // just check the file itself, and that's it.
  const pathEnv = cmd.match(/\//) || isWindows && cmd.match(/\\/) ? ['']
    : (
      [
        // windows always checks the cwd first
        ...(isWindows ? [process.cwd()] : []),
        ...(opt.path || process.env.PATH ||
          /* istanbul ignore next: very unusual */ '').split(colon),
      ]
    )
  const pathExtExe = isWindows
    ? opt.pathExt || process.env.PATHEXT || '.EXE;.CMD;.BAT;.COM'
    : ''
  const pathExt = isWindows ? pathExtExe.split(colon) : ['']

  if (isWindows) {
    if (cmd.indexOf('.') !== -1 && pathExt[0] !== '')
      pathExt.unshift('')
  }

  return {
    pathEnv,
    pathExt,
    pathExtExe,
  }
}

const which = (cmd, opt, cb) => {
  if (typeof opt === 'function') {
    cb = opt
    opt = {}
  }
  if (!opt)
    opt = {}

  const { pathEnv, pathExt, pathExtExe } = getPathInfo(cmd, opt)
  const found = []

  const step = i => new Promise((resolve, reject) => {
    if (i === pathEnv.length)
      return opt.all && found.length ? resolve(found)
        : reject(getNotFoundError(cmd))

    const ppRaw = pathEnv[i]
    const pathPart = /^".*"$/.test(ppRaw) ? ppRaw.slice(1, -1) : ppRaw

    const pCmd = path.join(pathPart, cmd)
    const p = !pathPart && /^\.[\\\/]/.test(cmd) ? cmd.slice(0, 2) + pCmd
      : pCmd

    resolve(subStep(p, i, 0))
  })

  const subStep = (p, i, ii) => new Promise((resolve, reject) => {
    if (ii === pathExt.length)
      return resolve(step(i + 1))
    const ext = pathExt[ii]
    isexe(p + ext, { pathExt: pathExtExe }, (er, is) => {
      if (!er && is) {
        if (opt.all)
          found.push(p + ext)
        else
          return resolve(p + ext)
      }
      return resolve(subStep(p, i, ii + 1))
    })
  })

  return cb ? step(0).then(res => cb(null, res), cb) : step(0)
}

const whichSync = (cmd, opt) => {
  opt = opt || {}

  const { pathEnv, pathExt, pathExtExe } = getPathInfo(cmd, opt)
  const found = []

  for (let i = 0; i < pathEnv.length; i ++) {
    const ppRaw = pathEnv[i]
    const pathPart = /^".*"$/.test(ppRaw) ? ppRaw.slice(1, -1) : ppRaw

    const pCmd = path.join(pathPart, cmd)
    const p = !pathPart && /^\.[\\\/]/.test(cmd) ? cmd.slice(0, 2) + pCmd
      : pCmd

    for (let j = 0; j < pathExt.length; j ++) {
      const cur = p + pathExt[j]
      try {
        const is = isexe.sync(cur, { pathExt: pathExtExe })
        if (is) {
          if (opt.all)
            found.push(cur)
          else
            return cur
        }
      } catch (ex) {}
    }
  }

  if (opt.all && found.length)
    return found

  if (opt.nothrow)
    return null

  throw getNotFoundError(cmd)
}

module.exports = which
which.sync = whichSync


/***/ }),
/* 160 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

var fs = __webpack_require__(5)
var core
if (process.platform === 'win32' || global.TESTING_WINDOWS) {
  core = __webpack_require__(161)
} else {
  core = __webpack_require__(162)
}

module.exports = isexe
isexe.sync = sync

function isexe (path, options, cb) {
  if (typeof options === 'function') {
    cb = options
    options = {}
  }

  if (!cb) {
    if (typeof Promise !== 'function') {
      throw new TypeError('callback not provided')
    }

    return new Promise(function (resolve, reject) {
      isexe(path, options || {}, function (er, is) {
        if (er) {
          reject(er)
        } else {
          resolve(is)
        }
      })
    })
  }

  core(path, options || {}, function (er, is) {
    // ignore EACCES because that just means we aren't allowed to run it
    if (er) {
      if (er.code === 'EACCES' || options && options.ignoreErrors) {
        er = null
        is = false
      }
    }
    cb(er, is)
  })
}

function sync (path, options) {
  // my kingdom for a filtered catch
  try {
    return core.sync(path, options || {})
  } catch (er) {
    if (options && options.ignoreErrors || er.code === 'EACCES') {
      return false
    } else {
      throw er
    }
  }
}


/***/ }),
/* 161 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

module.exports = isexe
isexe.sync = sync

var fs = __webpack_require__(5)

function checkPathExt (path, options) {
  var pathext = options.pathExt !== undefined ?
    options.pathExt : process.env.PATHEXT

  if (!pathext) {
    return true
  }

  pathext = pathext.split(';')
  if (pathext.indexOf('') !== -1) {
    return true
  }
  for (var i = 0; i < pathext.length; i++) {
    var p = pathext[i].toLowerCase()
    if (p && path.substr(-p.length).toLowerCase() === p) {
      return true
    }
  }
  return false
}

function checkStat (stat, path, options) {
  if (!stat.isSymbolicLink() && !stat.isFile()) {
    return false
  }
  return checkPathExt(path, options)
}

function isexe (path, options, cb) {
  fs.stat(path, function (er, stat) {
    cb(er, er ? false : checkStat(stat, path, options))
  })
}

function sync (path, options) {
  return checkStat(fs.statSync(path), path, options)
}


/***/ }),
/* 162 */
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

module.exports = isexe
isexe.sync = sync

var fs = __webpack_require__(5)

function isexe (path, options, cb) {
  fs.stat(path, function (er, stat) {
    cb(er, er ? false : checkStat(stat, options))
  })
}

function sync (path, options) {
  return checkStat(fs.statSync(path), options)
}

function checkStat (stat, options) {
  return stat.isFile() && checkMode(stat, options)
}

function checkMode (stat, options) {
  var mod = stat.mode
  var uid = stat.uid
  var gid = stat.gid

  var myUid = options.uid !== undefined ?
    options.uid : process.getuid && process.getuid()
  var myGid = options.gid !== undefined ?
    options.gid : process.getgid && process.getgid()

  var u = parseInt('100', 8)
  var g = parseInt('010', 8)
  var o = parseInt('001', 8)
  var ug = u | g

  var ret = (mod & o) ||
    (mod & g) && gid === myGid ||
    (mod & u) && uid === myUid ||
    (mod & ug) && myUid === 0

  return ret
}


/***/ })
/******/ 	]);
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be in strict mode.
(() => {
"use strict";
var exports = __webpack_exports__;

Object.defineProperty(exports, "__esModule", ({ value: true }));
exports.activate = void 0;
const path = __webpack_require__(1);
const node_1 = __webpack_require__(2);
const which = __webpack_require__(159);
async function activate(context) {
    const args = ["lsp"];
    const serverOptions = {
        run: {
            command: await which("wipple"),
            args,
        },
        debug: {
            command: path.join(__dirname, "../../../target/debug/wipple"),
            args,
        },
    };
    const clientOptions = {
        documentSelector: ["wipple"],
    };
    const client = new node_1.LanguageClient("Wipple", serverOptions, clientOptions);
    await client.start();
    context.subscriptions.push({ dispose: client.stop });
}
exports.activate = activate;

})();

module.exports = __webpack_exports__;
/******/ })()
;
//# sourceMappingURL=extension.js.map