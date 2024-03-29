var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _Goban_instances, _Goban_getCanvas, _Goban_getPointerEventCoordinates, _Goban_fillBackground, _Goban_drawLines, _Goban_drawDots, _Goban_drawMouseHighlighter, _Goban_drawStone, _Goban_drawDecoration, _Goban_drawCircle, _Goban_drawSquare;
var Stone;
(function (Stone) {
    Stone[Stone["None"] = 0] = "None";
    Stone[Stone["Black"] = 1] = "Black";
    Stone[Stone["White"] = 2] = "White";
})(Stone || (Stone = {}));
function stoneFromColor(color) {
    switch (color) {
        case "B":
            return Stone.Black;
        case "W":
            return Stone.White;
        case " ":
            return Stone.None;
    }
}
class Goban {
    constructor(selector, size, onClick = () => { }) {
        _Goban_instances.add(this);
        this.isPointerDown = false;
        this.pointerCoordinates = null;
        this.lastMatrix = null;
        this.lastDecorations = null;
        this.canvasSelector = selector;
        this.size = size;
        this.onClick = onClick;
    }
    initialize() {
        let canvas = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getCanvas).call(this);
        if (canvas === null) {
            throw `Cannot locate ${this.canvasSelector}`;
        }
        canvas.width = 100 * this.size;
        canvas.height = 100 * this.size;
        canvas.addEventListener("pointerdown", (event) => {
            this.isPointerDown = true;
            this.pointerCoordinates = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getPointerEventCoordinates).call(this, canvas, event);
            this.draw();
        });
        canvas.addEventListener("pointerup", (event) => {
            let { x, y } = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getPointerEventCoordinates).call(this, canvas, event);
            this.onClick(x, y);
            this.isPointerDown = false;
            this.draw();
        });
        // canvas.addEventListener("touchend", (event: TouchEvent) => {
        //   let { x, y } = this.#getPointerEventCoordinates(
        //     canvas,
        //     event.changedTouches[0],
        //   );
        //   this.onClick(x, y);
        //   this.isPointerDown = false;
        //   this.draw();
        // });
        canvas.addEventListener("pointermove", (event) => {
            if (this.isPointerDown) {
                this.pointerCoordinates = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getPointerEventCoordinates).call(this, canvas, event);
                this.draw();
            }
        });
        canvas.addEventListener("touchmove", (event) => {
            if (this.isPointerDown) {
                this.pointerCoordinates = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getPointerEventCoordinates).call(this, canvas, event.changedTouches[0]);
                this.draw();
            }
        });
    }
    draw(matrix, decorations) {
        if (matrix) {
            this.lastMatrix = matrix;
        }
        else if (this.lastMatrix) {
            matrix = this.lastMatrix;
        }
        if (decorations) {
            this.lastDecorations = decorations;
        }
        else if (this.lastDecorations) {
            decorations = this.lastDecorations;
        }
        let canvas = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getCanvas).call(this);
        let ctx = canvas.getContext("2d");
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_fillBackground).call(this, ctx);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawLines).call(this, ctx);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawDots).call(this, ctx);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawMouseHighlighter).call(this, ctx);
        if (matrix) {
            for (let x = 0; x < this.size; x += 1) {
                for (let y = 0; y < this.size; y += 1) {
                    __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawStone).call(this, ctx, matrix[x][y], x, y);
                }
            }
        }
        if (decorations) {
            for (let x = 0; x < this.size; x += 1) {
                for (let y = 0; y < this.size; y += 1) {
                    __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawDecoration).call(this, ctx, decorations[x][y], x, y);
                }
            }
        }
    }
}
_Goban_instances = new WeakSet(), _Goban_getCanvas = function _Goban_getCanvas() {
    return document.querySelector(this.canvasSelector);
}, _Goban_getPointerEventCoordinates = function _Goban_getPointerEventCoordinates(canvas, event) {
    // Calculate offfsetX instead of getting it from the event because Touch events don't have it >:(
    let { x, y } = canvas.getBoundingClientRect();
    let offsetX = event.clientX - x;
    let offsetY = event.clientY - y;
    return {
        x: Math.floor((this.size * offsetX) / canvas.clientWidth),
        y: Math.floor((this.size * offsetY) / canvas.clientHeight),
    };
}, _Goban_fillBackground = function _Goban_fillBackground(ctx) {
    let canvas = __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_getCanvas).call(this);
    ctx.fillStyle = "#f4e5b8";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}, _Goban_drawLines = function _Goban_drawLines(ctx) {
    ctx.lineWidth = 4;
    ctx.strokeStyle = "#000000";
    // Draw vertical lines
    for (let x = 0; x < this.size; x += 1) {
        ctx.beginPath();
        ctx.moveTo(100 * x + 50, 48);
        ctx.lineTo(100 * x + 50, this.size * 100 - 48);
        ctx.stroke();
    }
    // Draw horizontal lines
    for (let y = 0; y < this.size; y += 1) {
        ctx.beginPath();
        ctx.moveTo(50, 100 * y + 50);
        ctx.lineTo(this.size * 100 - 50, 100 * y + 50);
        ctx.stroke();
    }
}, _Goban_drawDots = function _Goban_drawDots(ctx) {
    if (this.size % 2 == 0) {
        // Even sized board??? no dots
        return;
    }
    // Set up dot fill parameters for all drawCircle calls
    ctx.fillStyle = "#000000";
    const dotSize = 10;
    // Draw central dot
    __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 50, this.size * 50, dotSize);
    if (this.size >= 9 && this.size < 13) {
        // Draw corner dots at 3-3 points
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 250, 250, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 100 - 250, 250, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 250, this.size * 100 - 250, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 100 - 250, this.size * 100 - 250, dotSize);
    }
    if (this.size >= 13) {
        // Draw corner dots at 4-4 points
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 100 - 350, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, this.size * 100 - 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 100 - 350, this.size * 100 - 350, dotSize);
    }
    if (this.size >= 15) {
        // Draw side dots
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, this.size * 50, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 100 - 350, this.size * 50, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 50, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 50, this.size * 100 - 350, dotSize);
    }
}, _Goban_drawMouseHighlighter = function _Goban_drawMouseHighlighter(ctx) {
    if (this.isPointerDown && this.pointerCoordinates) {
        ctx.strokeStyle = "#ff0000";
        let { x, y } = this.pointerCoordinates;
        ctx.beginPath();
        ctx.moveTo(0, y * 100 + 50);
        ctx.lineTo(this.size * 100, y * 100 + 50);
        ctx.moveTo(x * 100 + 50, 0);
        ctx.lineTo(x * 100 + 50, this.size * 100);
        ctx.stroke();
    }
}, _Goban_drawStone = function _Goban_drawStone(ctx, stone, x, y) {
    if (stone == Stone.None) {
        return;
    }
    if (stone == Stone.Black) {
        ctx.fillStyle = "#000000";
    }
    if (stone == Stone.White) {
        ctx.fillStyle = "#ffffff";
    }
    __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 100 * x + 50, 100 * y + 50, 48);
    if (stone == Stone.White) {
        // Draw a ring around the stone to make it pop
        ctx.strokeStyle = "#000000";
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.arc(100 * x + 50, 100 * y + 50, 48, 0, Math.PI * 2);
        ctx.stroke();
    }
}, _Goban_drawDecoration = function _Goban_drawDecoration(ctx, decoration, x, y) {
    if (decoration == Stone.None) {
        return;
    }
    if (decoration == Stone.Black) {
        ctx.fillStyle = "#000000";
    }
    if (decoration == Stone.White) {
        ctx.fillStyle = "#ffffff";
    }
    // this.#drawCircle(ctx, 100 * x + 50, 100 * y + 50, 48);
    __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawSquare).call(this, ctx, 100 * x + 25, 100 * y + 25, 50);
    // if (stone == Stone.White) {
    //   // Draw a ring around the stone to make it pop
    //   ctx.strokeStyle = "#000000";
    //   ctx.lineWidth = 2;
    //   ctx.beginPath();
    //   ctx.arc(100 * x + 50, 100 * y + 50, 48, 0, Math.PI * 2);
    //   ctx.stroke();
    // }
}, _Goban_drawCircle = function _Goban_drawCircle(ctx, x, y, radius) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2);
    ctx.fill();
}, _Goban_drawSquare = function _Goban_drawSquare(ctx, x, y, size) {
    ctx.fillRect(x, y, size, size);
};
export { Goban, Stone, stoneFromColor };
