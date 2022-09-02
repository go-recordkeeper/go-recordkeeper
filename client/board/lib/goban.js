"use strict";
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _Goban_instances, _Goban_fillBackground, _Goban_drawLines, _Goban_drawDots, _Goban_drawStone, _Goban_drawCircle;
Object.defineProperty(exports, "__esModule", { value: true });
exports.Stone = exports.Goban = void 0;
var Stone;
(function (Stone) {
    Stone[Stone["None"] = 0] = "None";
    Stone[Stone["Black"] = 1] = "Black";
    Stone[Stone["White"] = 2] = "White";
})(Stone || (Stone = {}));
exports.Stone = Stone;
class Goban {
    constructor(selector, size, onClick = () => { }) {
        _Goban_instances.add(this);
        let canvas = document.querySelector(selector);
        if (canvas === null) {
            throw `Cannot locate ${selector}`;
        }
        this.canvas = canvas;
        this.canvas.width = 100 * size;
        this.canvas.height = 100 * size;
        this.size = size;
        // Initialize stones played
        this.matrix = [];
        for (let x = 0; x < size; x += 1) {
            let column = [];
            for (let y = 0; y < size; y += 1) {
                column.push(Stone.None);
            }
            this.matrix.push(column);
        }
        this.canvas.addEventListener("click", (event) => {
            let x = Math.floor(size * event.offsetX / this.canvas.clientWidth);
            let y = Math.floor(size * event.offsetY / this.canvas.clientHeight);
            onClick(x, y);
        });
    }
    placeStone(stone, x, y) {
        this.matrix[x][y] = stone;
    }
    draw() {
        let { canvas } = this;
        let ctx = canvas.getContext("2d");
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_fillBackground).call(this, ctx);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawLines).call(this, ctx);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawDots).call(this, ctx);
        for (let x = 0; x < this.size; x += 1) {
            for (let y = 0; y < this.size; y += 1) {
                __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawStone).call(this, ctx, this.matrix[x][y], x, y);
            }
        }
    }
}
exports.Goban = Goban;
_Goban_instances = new WeakSet(), _Goban_fillBackground = function _Goban_fillBackground(ctx) {
    ctx.fillStyle = "#f4e5b8";
    ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
}, _Goban_drawLines = function _Goban_drawLines(ctx) {
    ctx.lineWidth = 4;
    ctx.strokeStyle = "#000000";
    // Draw vertical lines
    for (let x = 0; x < this.size; x += 1) {
        ctx.moveTo(100 * x + 50, 48);
        ctx.lineTo(100 * x + 50, (this.size * 100) - 48);
        ctx.stroke();
    }
    // Draw horizontal lines
    for (let y = 0; y < this.size; y += 1) {
        ctx.moveTo(50, 100 * y + 50);
        ctx.lineTo((this.size * 100) - 50, 100 * y + 50);
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
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, (this.size * 100) - 250, 250, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 250, (this.size * 100) - 250, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, (this.size * 100) - 250, (this.size * 100) - 250, dotSize);
    }
    if (this.size >= 13) {
        // Draw corner dots at 4-4 points
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, (this.size * 100) - 350, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, (this.size * 100) - 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, (this.size * 100) - 350, (this.size * 100) - 350, dotSize);
    }
    if (this.size >= 15) {
        // Draw side dots
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, 350, this.size * 50, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, (this.size * 100) - 350, this.size * 50, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 50, 350, dotSize);
        __classPrivateFieldGet(this, _Goban_instances, "m", _Goban_drawCircle).call(this, ctx, this.size * 50, (this.size * 100) - 350, dotSize);
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
}, _Goban_drawCircle = function _Goban_drawCircle(ctx, x, y, radius) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2);
    ctx.fill();
};
