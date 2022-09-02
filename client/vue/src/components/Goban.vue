<script lang="ts">
import { Goban, Stone, stoneFromColor } from 'go-board';
import Client from '@/client';
import { defineComponent} from 'vue';

export default defineComponent({
    props: {
        id: {
            type: Number,
            required: true,
        },
    },
    mounted() {
        const client = new Client();
        client.getBoard(this.id).then((board) => {
            const goban = new Goban('#goban', board.size, (x, y) => {
                client.playStone(this.id, x, y).then(({ add, remove }) => {
                    for (let move of add) {
                        let { x, y, color } = move;
                        goban.placeStone(stoneFromColor(color), x, y);
                    }
                    for (let capture of remove) {
                        let { x, y } = capture;
                        goban.placeStone(Stone.None, x, y);
                    }
                    goban.draw();
                })
            });
            for (let move of board.stones) {
                let { x, y, color } = move;
                goban.placeStone(stoneFromColor(color), x, y);
            }
            goban.draw();
        });
    }
});
</script>
    
<template>
    <div style="width:100%; padding-top: 100%; position: relative;">
        <canvas id="goban" style="position:absolute; top: 0; width: 100%;"></canvas>
    </div>
</template>
