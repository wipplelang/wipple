// Adapted from https://replit.com/@TechPandaPro/turtleyjs

const turtley = {};

turtley.ready = false;

// const libSrc = document.currentScript.src;

// const style = document.createElement("link");
// style.rel = "stylesheet";
// style.href = `${libSrc.slice(0, libSrc.lastIndexOf("/"))}/style.css`;
// document.head.appendChild(style);

// style.addEventListener(
//     "load",
//     () => {
//         turtley.ready = true;
//         if (typeof turtley.onReady === "function") turtley.onReady();
//     },
//     { once: true }
// );

turtley.util = {
    isColor: function isColor(color) {
        return CSS.supports("color", color);
    },
};

turtley.Turtle = class Turtle {
    constructor() {
        this._init = false;

        this._width = null;
        this._height = null;

        this._fullscreen = null;

        this._turtleImage = null;

        this._queue = [];

        this._penDown = null;
        this._showTurtle = null;
        this._speed = null;
        this._currentPath = null;
        this._pathPoints = null;
        this._penColor = null;
        this._penSize = null;
        this._fillColor = null;
        this._backgroundColor = null;

        this._x = null;
        this._y = null;
        this._rotation = null;
    }

    init(options = {}) {
        if (this._init) throw new Error("Turtle already initialized");

        if (typeof options !== "object") throw new Error("Options must be an object");
        if ("width" in options && typeof options.width !== "number")
            throw new Error(`Parameter "width" must be a number`);
        if ("height" in options && typeof options.height !== "number")
            throw new Error(`Parameter "height" must be a number`);
        if ("appendTo" in options && !(options.appendTo instanceof HTMLElement))
            throw new Error(`Parameter "appendTo" must be an HTMLElement`);
        if (
            "turtleImage" in options &&
            !(
                options.turtleImage instanceof HTMLImageElement ||
                typeof options.turtleImage === "string"
            )
        )
            throw new Error(`Parameter "turtleImage" must be an HTMLImageElement or a string`);
        if ("turtleWidth" in options && typeof options.turtleWidth !== "number")
            throw new Error(`Parameter "turtleWidth" must be a number`);
        if ("turtleHeight" in options && typeof options.turtleHeight !== "number")
            throw new Error(`Parameter "turtleHeight" must be a number`);

        this._init = true;

        this._width = options.width ?? null;
        this._height = options.height ?? null;

        this._fullscreen =
            this._width === null || this._height === null || Boolean(options.fullscreen);

        this._turtleImage = options.turtleImage;
        this._turtleWidth = options.turtleWidth;
        this._turtleHeight = options.turtleHeight;

        this._penDown = false;
        this._showTurtle = "showTurtle" in options ? Boolean(options.showTurtle) : true;
        this._speed = 1;
        this._penColor = "black";
        this._penSize = 1;
        this._fillColor = "gray";
        this._backgroundColor = "transparent";

        this._x = 0;
        this._y = 0;
        this._rotation = 0;

        const appendTo = options.appendTo ?? document.body;

        const hiddenCanvas = document.createElement("canvas");
        this._hiddenCanvas = hiddenCanvas;
        this._hiddenCtx = this._hiddenCanvas.getContext("2d");

        const visibleCanvas = document.createElement("canvas");
        this._visibleCanvas = visibleCanvas;
        this._visibleCtx = this._visibleCanvas.getContext("2d");
        this._visibleCanvas.classList.add("turtley-canvas");
        appendTo.appendChild(this._visibleCanvas);

        appendTo.classList.add("turtley-container");

        if (this._fullscreen) {
            if (appendTo === document.body)
                document.documentElement.classList.add("turtley-fullscreen");
            appendTo.classList.add("turtley-fullscreen");
            window.addEventListener("resize", this.resize.bind(this));
        }

        this.resize(); // this will automatically call this._draw() as well

        setInterval(this._update.bind(this), 20);
    }

    resize() {
        if (!this._init) throw new Error("Turtle not yet initialized");

        const containerWidth = this._visibleCanvas.parentElement.clientWidth;
        const containerHeight = this._visibleCanvas.parentElement.clientHeight;

        if (this._fullscreen) {
            if (this._width === null || this._height === null) {
                this._visibleCanvas.width = containerWidth;
                this._visibleCanvas.height = containerHeight;
            } else {
                const xGap = containerWidth - this._width;
                const yGap = containerHeight - this._height;

                if (xGap < yGap) {
                    this._visibleCanvas.width = containerWidth;
                    this._visibleCanvas.height =
                        (this._height / this._width) * this._visibleCanvas.width;
                } else {
                    this._visibleCanvas.height = containerHeight;
                    this._visibleCanvas.width =
                        (this._width / this._height) * this._visibleCanvas.height;
                }
            }
        } else {
            this._visibleCanvas.width = this._width;
            this._visibleCanvas.height = this._height;
        }

        rescaleCanvas(this._visibleCanvas);

        const tempHiddenCanvasDupe = document.createElement("canvas");
        const tempHiddenCtx = tempHiddenCanvasDupe.getContext("2d");
        tempHiddenCanvasDupe.width = this._width;
        tempHiddenCanvasDupe.height = this._height;
        rescaleCanvas(tempHiddenCanvasDupe);
        tempHiddenCtx.drawImage(this._hiddenCanvas, 0, 0, this._width, this._height);

        this._hiddenCanvas.width = Math.max(this._width, this._width * 1);
        this._hiddenCanvas.height = Math.max(this._height, this._height * 1);

        rescaleCanvas(this._hiddenCanvas);

        this._hiddenCtx.drawImage(
            tempHiddenCanvasDupe,
            Math.round(this._width / 2 - this._width / 2),
            Math.round(this._height / 2 - this._height / 2),
            this._width,
            this._height
        );

        this._draw();
    }

    _update() {
        if (this._queue.length < 1) return;

        this._hiddenCtx.save();

        this._hiddenCtx.translate(this._width / 2, this._height / 2);

        const current = this._queue[0];

        const moveBy = Math.min(this._speed * 10, current.count);

        switch (current.type) {
            case "setSpeed":
                this._speed = current.speed;
                break;
            case "penDown":
                this._penDown = true;
                this._currentPath = new Path2D();
                this._pathPoints = [];
                break;
            case "penUp":
                this._penDown = false;
                this._currentPath = null;
                this._pathPoints = null;
                break;
            case "showTurtle":
                this._showTurtle = true;
                break;
            case "hideTurtle":
                this._showTurtle = false;
                break;
            case "penColor":
                this._penColor = current.color;
                break;
            case "penSize":
                this._penSize = current.size;
                break;
            case "fillColor":
                this._fillColor = current.color;
                break;
            case "backgroundColor":
                this._backgroundColor = current.color;
                break;
            case "fill":
                if (!this._currentPath)
                    return current.error("No shape to fill. Try calling penDown() first.");

                this._hiddenCtx.globalCompositeOperation = "destination-out";
                this._hiddenCtx.stroke(this._currentPath);

                this._hiddenCtx.globalCompositeOperation = "source-over";

                this._hiddenCtx.fillStyle = this._fillColor;
                this._hiddenCtx.fill(this._currentPath);

                for (let i = 1; i < this._pathPoints.length; i++) {
                    const previousPoint = this._pathPoints[i - 1];
                    const point = this._pathPoints[i];

                    this._hiddenCtx.beginPath();

                    this._hiddenCtx.moveTo(previousPoint.x, previousPoint.y);
                    this._hiddenCtx.lineTo(point.x, point.y);

                    this._hiddenCtx.strokeStyle = point.color;
                    this._hiddenCtx.lineWidth = point.penSize;
                    this._hiddenCtx.stroke();
                }

                break;
            case "stepForward":
            case "stepBackward":
                this._hiddenCtx.save();

                this._hiddenCtx.translate(-this._width / 2, -this._height / 2);

                if (
                    this._penDown &&
                    !this._hiddenCtx.isPointInPath(this._currentPath, this._x, this._y)
                ) {
                    this._currentPath.moveTo(this._x, this._y);
                    this._pathPoints.push({ x: this._x, y: this._y, color: null, penSize: null });
                }

                this._hiddenCtx.restore();

                this._hiddenCtx.beginPath();
                this._hiddenCtx.moveTo(this._x, this._y);

                const moveX =
                    Math.cos(this._rotation) * moveBy * (current.type === "stepBackward" ? -1 : 1);
                const moveY =
                    Math.sin(this._rotation) * moveBy * (current.type === "stepBackward" ? 1 : -1);

                this._x += moveX;
                this._y += moveY;

                this._hiddenCtx.strokeStyle = this._penColor;
                this._hiddenCtx.lineWidth = this._penSize;

                if (this._penDown) {
                    this._hiddenCtx.lineTo(this._x, this._y);
                    this._currentPath.lineTo(this._x, this._y);
                    this._pathPoints.push({
                        x: this._x,
                        y: this._y,
                        color: this._hiddenCtx.strokeStyle,
                        penSize: this._hiddenCtx.lineWidth,
                    });
                    this._hiddenCtx.stroke();
                } else this._hiddenCtx.moveTo(this._x, this._y);

                break;
            case "moveTo":
                this._x = current.x;
                this._y = current.y;

                this._currentPath = null;
                this._pathPoints = null;

                break;
            case "rotateLeft":
            case "rotateRight": {
                const moveByRadians =
                    ((moveBy * Math.PI) / 180) * (current.type === "rotateLeft" ? 1 : -1);
                this._rotation += moveByRadians;
                break;
            }
            case "rotate": {
                const moveByRadians = (moveBy * Math.PI) / 180;
                this._rotation = moveByRadians;
                break;
            }
            default:
                throw new Error(`Unrecognized action "${current.type}"`);
        }

        current.count -= moveBy;

        if (current.count < 1) this._queue.shift();

        this._draw();

        current.whenDone();

        this._hiddenCtx.restore();
    }

    _draw() {
        this._visibleCtx.save();

        this._visibleCtx.clearRect(0, 0, this._width, this._height);

        this._visibleCtx.drawImage(
            this._hiddenCanvas,
            this._width / 2 - this._width / 2,
            this._height / 2 - this._height / 2,
            this._width,
            this._height
        );

        if (this._showTurtle) {
            this._visibleCtx.save();

            this._visibleCtx.translate(this._width / 2 + this._x, this._height / 2 + this._y);

            this._visibleCtx.rotate(-this._rotation);
            this._visibleCtx.rotate(Math.PI / 2);

            // this._visibleCtx.beginPath();

            // this._visibleCtx.strokeStyle = "#000000";
            // this._visibleCtx.lineWidth = 2;
            // this._visibleCtx.fillStyle = this._penColor;

            // this._visibleCtx.moveTo(-10, -7);
            // this._visibleCtx.lineTo(10, 0);
            // this._visibleCtx.lineTo(-10, 7);
            // this._visibleCtx.lineTo(-5, 0);

            // this._visibleCtx.closePath();

            // this._visibleCtx.stroke();
            // this._visibleCtx.fill();

            if (this._turtleImage instanceof HTMLImageElement) {
                this._visibleCtx.drawImage(
                    this._turtleImage,
                    -(this._turtleWidth / 2),
                    -(this._turtleHeight / 2),
                    this._turtleWidth,
                    this._turtleHeight
                );
            } else {
                this._visibleCtx.font = `${this._turtleWidth}px 'Segoe UI Emoji'`;
                this._visibleCtx.fillText(this._turtleImage, -20, 10);
            }

            this._visibleCtx.restore();
        }

        this._visibleCtx.globalCompositeOperation = "destination-over";
        this._visibleCtx.fillStyle = this._backgroundColor;
        this._visibleCtx.fillRect(0, 0, this._width, this._height);

        this._visibleCtx.restore();
    }

    setSpeed(speed) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof speed !== "number") return reject(`Parameter "speed" must be a number`);
            if (speed < 0) return reject(`Parameter "speed" must be at least 0`);

            this._queue.push({
                type: "setSpeed",
                count: 1,
                speed,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    penDown() {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            this._queue.push({ type: "penDown", count: 1, whenDone: resolve, error: reject });
        });
    }

    penUp() {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            this._queue.push({ type: "penUp", count: 1, whenDone: resolve, error: reject });
        });
    }

    showTurtle() {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            this._queue.push({ type: "showTurtle", count: 1, whenDone: resolve, error: reject });
        });
    }

    hideTurtle() {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            this._queue.push({ type: "hideTurtle", count: 1, whenDone: resolve, error: reject });
        });
    }

    setPenColor(color) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (!turtley.util.isColor(color))
                return current.error(`Invalid pen color: ${JSON.stringify(current.color)}`);

            this._queue.push({
                type: "penColor",
                count: 1,
                color,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    setPenSize(size) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof size !== "number") return reject(`Parameter "size" must be a number`);
            if (size < 0) return reject(`Parameter "size" must be at least 0`);

            this._queue.push({ type: "penSize", count: 1, size, whenDone: resolve, error: reject });
        });
    }

    setFillColor(color) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (!turtley.util.isColor(color))
                return current.error(`Invalid fill color: ${JSON.stringify(current.color)}`);

            this._queue.push({
                type: "fillColor",
                count: 1,
                color,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    setBackgroundColor(color) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (!turtley.util.isColor(color))
                return current.error(`Invalid background color: ${JSON.stringify(current.color)}`);

            this._queue.push({
                type: "backgroundColor",
                count: 1,
                color,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    fillShape() {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            this._queue.push({ type: "fill", count: 1, whenDone: resolve, error: reject });
        });
    }

    forward(stepCount) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof stepCount !== "number")
                return reject(`Parameter "stepCount" must be a number`);
            if (stepCount < 1) return reject(`Parameter "stepCount" must be at least 1`);

            this._queue.push({
                type: "stepForward",
                count: stepCount,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    backward(stepCount) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof stepCount !== "number")
                return reject(`Parameter "stepCount" must be a number`);
            if (stepCount < 1) return reject(`Parameter "stepCount" must be at least 1`);

            this._queue.push({
                type: "stepBackward",
                count: stepCount,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    moveTo(x, y) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof x !== "number") return reject(`Parameter "x" must be a number`);
            if (typeof y !== "number") return reject(`Parameter "y" must be a number`);

            this._queue.push({ type: "moveTo", count: 1, x, y, whenDone: resolve, error: reject });
        });
    }

    rotateLeft(degrees) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof degrees !== "number") return reject(`Parameter "degrees" must be a number`);
            if (degrees < 0) return reject(`Parameter "degrees" must be at least 0`);

            this._queue.push({
                type: "rotateLeft",
                count: degrees,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    rotateRight(degrees) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof degrees !== "number") return reject(`Parameter "degrees" must be a number`);
            if (degrees < 0) return reject(`Parameter "degrees" must be at least 0`);

            this._queue.push({
                type: "rotateRight",
                count: degrees,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    rotate(degrees) {
        return new Promise((resolve, reject) => {
            if (!this._init) return reject("Turtle not yet initialized");

            if (typeof degrees !== "number") return reject(`Parameter "degrees" must be a number`);
            if (degrees < 0) return reject(`Parameter "degrees" must be at least 0`);

            this._queue.push({
                type: "rotate",
                count: degrees,
                whenDone: resolve,
                error: reject,
            });
        });
    }

    get x() {
        return this._x;
    }

    get y() {
        return this._y;
    }

    get rotation() {
        return this._rotation ?? null;
    }

    get speed() {
        return this._speed;
    }

    get penColor() {
        return this._penColor;
    }

    get fillColor() {
        return this._fillColor;
    }

    get backgroundColor() {
        return this._backgroundColor;
    }

    get canvasWidth() {
        return this._visibleCanvas?.width ?? null;
    }

    get canvasHeight() {
        return this._visibleCanvas?.width ?? null;
    }
};

// https://www.keanw.com/2017/02/scaling-html-canvases-for-hidpi-screens.html
function rescaleCanvas(canvas) {
    // finally query the various pixel ratios

    let ctx = canvas.getContext("2d");

    let devicePixelRatio = window.devicePixelRatio || 1;

    let backingStoreRatio =
        ctx.webkitBackingStorePixelRatio ||
        ctx.mozBackingStorePixelRatio ||
        ctx.msBackingStorePixelRatio ||
        ctx.oBackingStorePixelRatio ||
        ctx.backingStorePixelRatio ||
        1;

    let ratio = devicePixelRatio / backingStoreRatio;

    // upscale the canvas if the two ratios don't match
    if (devicePixelRatio !== backingStoreRatio) {
        let oldWidth = canvas.width;
        let oldHeight = canvas.height;

        canvas.width = oldWidth * ratio;
        canvas.height = oldHeight * ratio;

        canvas.style.width = oldWidth + "px";
        canvas.style.height = oldHeight + "px";

        // now scale the context to counter
        // the fact that we've manually scaled
        // our canvas element

        ctx.scale(ratio, ratio);
    }
}

export default turtley;
