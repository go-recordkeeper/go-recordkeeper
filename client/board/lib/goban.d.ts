declare enum Stone {
    None = 0,
    Black = 1,
    White = 2
}
declare function stoneFromColor(color: "B" | "W" | " "): Stone;
declare type BoardState = Stone[][];
declare type Decoration = Stone;
declare type Decorations = Decoration[][];
declare class Goban {
    #private;
    canvasSelector: string;
    size: number;
    onClick: (x: number, y: number) => void;
    isPointerDown: boolean;
    pointerCoordinates: {
        x: number;
        y: number;
    } | null;
    lastMatrix: BoardState | null;
    lastDecorations: Decorations | null;
    constructor(selector: string, size: number, onClick?: (x: number, y: number) => void);
    initialize(): void;
    draw(matrix?: BoardState, decorations?: Decorations): void;
}
export { Goban, Stone, stoneFromColor };
