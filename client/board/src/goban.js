"use strict";
class Goban {
    constructor(canvas) {
        this.canvas = canvas;
    }
    draw() {
        let { canvas } = this;
        let ctx = canvas.getContext("2d");
        ctx.beginPath();
        ctx.arc(40, 40, 20, 0, Math.PI * 2);
        ctx.stroke();
    }
}
