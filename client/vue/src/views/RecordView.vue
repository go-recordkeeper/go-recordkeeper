<script setup lang="ts">
import { DocumentArrowDownIcon, PencilIcon } from '@heroicons/vue/24/outline';
import Client from '@/client';
import Goban from '@/components/Goban.vue';
import router from '@/router';
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
  for (let { x, y, color } of record.stones) {
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

async function download() {
  await client.downloadRecord(id);
}

async function modify() {
  router.push({ name: 'update', params: { id } })
}

async function pass() {
  await client.pass(id);
}

</script>
    
<template>
  <Goban v-if="size" :size="size" :matrix="matrix" :onClick="onClick">
  </Goban>
  <div v-else>Loading game...{{size}}</div>
  <div class="flex items-center mx-auto" style="max-width: calc(100vh - 128px);">
    <button @click="download" class="rounded-md ring m-2">
      <DocumentArrowDownIcon class="block h-8 w-8 m-2" />
    </button>
    <button @click="modify" class="rounded-md ring m-2">
      <PencilIcon class="block h-8 w-8 m-2" />
    </button>
    <button @click="pass" class="grow m-2 h-12 rounded-md bg-red-600 text-gray-800">Pass</button>
    <button @click="undo" class="grow m-2 h-12 rounded-md bg-yellow-600 text-gray-800">Undo</button>
  </div>
</template>
