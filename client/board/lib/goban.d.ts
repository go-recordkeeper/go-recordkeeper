declare enum Stone {
    None = 0,
    Black = 1,
    White = 2
}
declare function stoneFromColor(color: 'B' | 'W' | ' '): Stone;
declare type BoardState = Stone[][];
declare class Goban {
    #private;
    canvasSelector: string;
    size: number;
    onClick: (x: number, y: number) => void;
    constructor(selector: string, size: number, onClick?: (x: number, y: number) => void);
    initialize(): void;
    draw(matrix: BoardState): void;
}
export { Goban, Stone, stoneFromColor };
