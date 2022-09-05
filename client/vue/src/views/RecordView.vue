<script setup lang="ts">
import Client from '@/client';
import Goban from '@/components/Goban.vue';
import { reactive, ref } from 'vue';

const props = defineProps({
  id: {
    type: Number,
    required: true,
  },
})

let { id } = props;

let client = new Client();
let size = ref(0);

// Initialize stones played
let matrix: ('B' | 'W' | ' ')[][] = reactive([]);

client.getRecord(id).then((record) => {
  size.value = record.board_size;
  for (let x = 0; x < size.value; x += 1) {
    let column: ('B' | 'W' | ' ')[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      column.push(' ');
    }
    matrix.push(column);
  }
  for (let {x, y, color} of record.stones) {
    matrix[x][y] = color;
  }
});

async function onClick(x: number, y: number) {
  client.playStone(id, x, y).then(({ add, remove }) => {
    for (let move of add) {
      let { x, y, color } = move;
      matrix[x][y] = color;
    }
    for (let capture of remove) {
      let { x, y } = capture;
      matrix[x][y] = ' ';
    }
  })
}

async function undo() {
  let { add, remove } = await client.undo(id);
  for (let move of add) {
    let { x, y, color } = move;
    matrix[x][y] = color;
  }
  for (let capture of remove) {
    let { x, y } = capture;
    matrix[x][y] = ' ';
  }
}

</script>
    
<template>
  <button @click="undo">Undo</button>
  <Goban v-if="size" :size="size" :matrix="matrix" :onClick="onClick">
  </Goban>
  <div v-else>Loading game...{{size}}</div>
</template>
