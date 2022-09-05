<script lang="ts">
import { Goban, Stone, stoneFromColor } from 'go-board';
import Client from '@/client';
import { defineComponent, watchEffect } from 'vue';
import type { PropType } from 'vue';

export default defineComponent({
    props: {
        size: {
            type: Number,
            required: true,
        },
        matrix: {
            type: Array as PropType<('B' | 'W' | ' ')[][]>,
            required: true,
        },
        onClick: {
            type: Function as PropType<(x: number, y: number) => {}>,
            required: true,
        },
    },
    setup({ size, matrix, onClick }) {
        const goban = new Goban('#goban', size, onClick);
        return { goban };
    },
    mounted() {
        // this.client.getBoard(this.id).then((board) => {
        //     const goban = new Goban('#goban', board.size, (x, y) => {
        //         this.client.playStone(this.id, x, y).then(({ add, remove }) => {
        //             for (let move of add) {
        //                 let { x, y, color } = move;
        //                 goban.placeStone(stoneFromColor(color), x, y);
        //             }
        //             for (let capture of remove) {
        //                 let { x, y } = capture;
        //                 goban.placeStone(Stone.None, x, y);
        //             }
        //             goban.draw();
        //         })
        //     });
        //     for (let move of board.stones) {
        //         let { x, y, color } = move;
        //         goban.placeStone(stoneFromColor(color), x, y);
        //     }
        //     goban.draw();
        // });
        this.goban.initialize();
        watchEffect(() => {
            let stoneMatrix = this.matrix.map((column) => column.map((color) => stoneFromColor(color)));
            this.goban.draw(stoneMatrix);
        });
    }
});
</script>
    
<template>
    <div style="width:100%; padding-top: 100%; position: relative;">
        <canvas id="goban" style="position:absolute; top: 0; width: 100%;"></canvas>
    </div>
</template>
