declare enum Stone {
    None = 0,
    Black = 1,
    White = 2
}
declare function stoneFromColor(color: 'B' | 'W'): Stone;
declare class Goban {
    #private;
    canvasSelector: string;
    size: number;
    matrix: Stone[][];
    onClick: (x: number, y: number) => void;
    constructor(selector: string, size: number, onClick?: (x: number, y: number) => void);
    initialize(): void;
    placeStone(stone: Stone, x: number, y: number): void;
    draw(): void;
}
export { Goban, Stone, stoneFromColor };
