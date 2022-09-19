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
            type: Function as PropType<(x: number, y: number) => void>,
            required: true,
        },
    },
    setup({ size, matrix, onClick }) {
        const goban = new Goban('#goban', size, onClick);
        return { goban };
    },
    mounted() {
        this.goban.initialize();
        watchEffect(() => {
            let stoneMatrix = this.matrix.map((column) => column.map((color) => stoneFromColor(color)));
            this.goban.draw(stoneMatrix);
        });
    }
});
</script>
    
<template>
    <!-- <div style="width: 100%; padding-top: 100%;" class="relative">
        <canvas id="goban" style="position:absolute; top: 0; max-height: 100%; aspect-ratio: 1 / 1;"></canvas>
    </div> -->
    <canvas id="goban" class="mx-auto w-full h-full"
        style="aspect-ratio: 1 / 1;"></canvas>
</template>
