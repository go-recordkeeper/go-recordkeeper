<script setup lang="ts">
import Client from '@/client';
import type { Ref } from 'vue';
import { ref } from 'vue';

let client = new Client();
let boards: Ref<any[]> = ref([]);
client.getBoards().then((b) => {
    boards.value = b;
});

async function newBoard() {
    await client.createNewBoard(9);
    boards.value = await client.getBoards();
}
</script>
    
<template>
    <div class="about">
        <h1>This is the board list</h1>
        <div v-for="board of boards" :key="board.id">
            <router-link :to="{ name: 'board', params: { id: board.id }}">
                board {{ board.id }}
            </router-link>
        </div>
        <button @click="newBoard">Make a new board</button>
    </div>
</template>
