enum Stone {
    None,
    Black,
    White,
}

function stoneFromColor(color: 'B' | 'W'): Stone {
    switch (color) {
        case 'B': return Stone.Black;
        case 'W': return Stone.White;
    }
}

class Goban {
    canvasSelector: string;
    size: number;
    matrix: Stone[][];
    onClick: (x: number, y: number) => void;

    constructor(selector: string, size: number, onClick: (x: number, y: number) => void = () => { }) {
        this.canvasSelector = selector;
        this.size = size;
        this.onClick = onClick;
        // Initialize stones played
        this.matrix = [];
        for (let x = 0; x < size; x += 1) {
            let column = [];
            for (let y = 0; y < size; y += 1) {
                column.push(Stone.None);
            }
            this.matrix.push(column);
        }
    }

    #getCanvas() {
        return document.querySelector(this.canvasSelector) as HTMLCanvasElement;
    }

    initialize() {
        let canvas = this.#getCanvas();
        if (canvas === null) {
            throw `Cannot locate ${this.canvasSelector}`;
        }
        canvas.width = 100 * this.size;
        canvas.height = 100 * this.size;
        canvas.addEventListener("click", (event: MouseEvent) => {
            let x = Math.floor(this.size * event.offsetX / canvas.clientWidth);
            let y = Math.floor(this.size * event.offsetY / canvas.clientHeight);
            this.onClick(x, y);
        });
        this.draw();
    }

    placeStone(stone: Stone, x: number, y: number) {
        this.matrix[x][y] = stone;
    }

    draw() {
        let canvas = this.#getCanvas();
        let ctx = canvas.getContext("2d") as CanvasRenderingContext2D;
        this.#fillBackground(ctx);
        this.#drawLines(ctx);
        this.#drawDots(ctx);

        for (let x = 0; x < this.size; x += 1) {
            for (let y = 0; y < this.size; y += 1) {
                this.#drawStone(ctx, this.matrix[x][y], x, y);
            }
        }
    }

    #fillBackground(ctx: CanvasRenderingContext2D) {
        let canvas = this.#getCanvas();
        ctx.fillStyle = "#f4e5b8";
        ctx.fillRect(0, 0, canvas.width, canvas.height)
    }

    #drawLines(ctx: CanvasRenderingContext2D) {
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
    }

    #drawDots(ctx: CanvasRenderingContext2D) {
        if (this.size % 2 == 0) {
            // Even sized board??? no dots
            return;
        }
        // Set up dot fill parameters for all drawCircle calls
        ctx.fillStyle = "#000000"
        const dotSize = 10;
        // Draw central dot
        this.#drawCircle(ctx, this.size * 50, this.size * 50, dotSize);
        if (this.size >= 9 && this.size < 13) {
            // Draw corner dots at 3-3 points
            this.#drawCircle(ctx, 250, 250, dotSize);
            this.#drawCircle(ctx, (this.size * 100) - 250, 250, dotSize);
            this.#drawCircle(ctx, 250, (this.size * 100) - 250, dotSize);
            this.#drawCircle(ctx, (this.size * 100) - 250, (this.size * 100) - 250, dotSize);
        }
        if (this.size >= 13) {
            // Draw corner dots at 4-4 points
            this.#drawCircle(ctx, 350, 350, dotSize);
            this.#drawCircle(ctx, (this.size * 100) - 350, 350, dotSize);
            this.#drawCircle(ctx, 350, (this.size * 100) - 350, dotSize);
            this.#drawCircle(ctx, (this.size * 100) - 350, (this.size * 100) - 350, dotSize);
        }
        if (this.size >= 15) {
            // Draw side dots
            this.#drawCircle(ctx, 350, this.size * 50, dotSize);
            this.#drawCircle(ctx, (this.size * 100) - 350, this.size * 50, dotSize);
            this.#drawCircle(ctx, this.size * 50, 350, dotSize);
            this.#drawCircle(ctx, this.size * 50, (this.size * 100) - 350, dotSize);
        }
    }

    #drawStone(ctx: CanvasRenderingContext2D, stone: Stone, x: number, y: number) {
        if (stone == Stone.None) {
            return;
        }
        if (stone == Stone.Black) {
            ctx.fillStyle = "#000000";
        }
        if (stone == Stone.White) {
            ctx.fillStyle = "#ffffff";
        }
        this.#drawCircle(ctx, 100 * x + 50, 100 * y + 50, 48);
        if (stone == Stone.White) {
            // Draw a ring around the stone to make it pop
            ctx.strokeStyle = "#000000"
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.arc(100 * x + 50, 100 * y + 50, 48, 0, Math.PI * 2);
            ctx.stroke();
        }
    }

    #drawCircle(ctx: CanvasRenderingContext2D, x: number, y: number, radius: number) {
        ctx.beginPath();
        ctx.arc(x, y, radius, 0, Math.PI * 2);
        ctx.fill();
    }
}

export { Goban, Stone, stoneFromColor };