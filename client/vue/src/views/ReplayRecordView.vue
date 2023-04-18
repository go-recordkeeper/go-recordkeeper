<script setup lang="ts">
import { ArrowUturnLeftIcon } from "@heroicons/vue/24/outline";
import Client from "@/client";
import Goban from "@/components/Goban.vue";
import router from "@/router";
import { reactive, ref, watch } from "vue";
import type { Ref } from "vue";

const props = defineProps({
  id: {
    type: Number,
    required: true,
  },
});

let { id } = props;

let client = new Client();
let size = ref(0);

// 0 means empty board
// 1 means after the first stone has been placed
// {moves.length} means after the final move has been made
let move = ref(0);
let moves: Ref<
  {
    position: { x: number; y: number } | null;
    color: "B" | "W";
    captures: { x: number; y: number }[];
  }[]
> = ref([]);

let matrix: ("B" | "W" | " ")[][] = reactive([]);

watch(move, (newMove) => {
  for (let x = 0; x < size.value; x += 1) {
    for (let y = 0; y < size.value; y += 1) {
      matrix[x][y] = " ";
    }
  }
  for (let i = 0; i < newMove; i += 1) {
    const { position, color, captures } = moves.value[i];
    if (position) {
      const { x, y } = position;
      matrix[x][y] = color;
    }
    for (const { x, y } of captures) {
      matrix[x][y] = " ";
    }
  }
});

client.getRecord(id).then((record) => {
  size.value = record.board_size;
  moves.value = record.moves;
  move.value = moves.value.length;
  for (let x = 0; x < size.value; x += 1) {
    let column: ("B" | "W" | " ")[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      column.push(" ");
    }
    matrix.push(column);
  }
});

async function undo() {
  let { add, remove } = await client.undo(id);
  for (let move of add) {
    let { x, y, color } = move;
    matrix[x][y] = color;
  }
  for (let capture of remove) {
    let { x, y } = capture;
    matrix[x][y] = " ";
  }
}

function back() {
  router.back();
}

function skipToBeginning() {
  move.value = 0;
}

function skipTenBack() {
  move.value = Math.max(0, move.value - 10);
}

function goBack() {
  move.value = Math.max(0, move.value - 1);
}

function goForward() {
  move.value = Math.min(moves.value.length, move.value + 1);
}

function skipTenForward() {
  move.value = Math.min(moves.value.length, move.value + 10);
}

function skipToEnd() {
  move.value = moves.value.length;
}
</script>

<template>
  <div class="mx-auto" style="max-width: calc(100vh - 220px)">
    <Goban
      v-if="size"
      :size="size"
      :matrix="matrix"
      :onClick="() => {}"
      style="max-width: calc(100vh - 220px); max-height: calc(100vh - 220px)"
    >
    </Goban>
    <div v-else>Loading game...{{ size }}</div>
    <div class="flex items-center">
      <button @click="back" class="rounded-md ring m-2 bg-green-400">
        <ArrowUturnLeftIcon class="block h-7 w-7 m-2" />
      </button>
      <button @click="skipToBeginning" class="rounded-md ring m-2">
        <div class="block h-7 w-7 m-2">|&lt;</div>
      </button>
      <button @click="skipTenBack" class="rounded-md ring m-2">
        <div class="block h-7 w-7 m-2">&lt;&lt;</div>
      </button>
      <button @click="goBack" class="flex grow rounded-md ring m-2">
        <div class="grow block h-7 w-7 m-2 text-center">&lt;</div>
      </button>
      <button @click="goForward" class="flex grow rounded-md ring m-2">
        <div class="grow block h-7 w-7 m-2 text-center">&gt;</div>
      </button>
      <button @click="skipTenForward" class="rounded-md ring m-2">
        <div class="block h-7 w-7 m-2">&gt;&gt;</div>
      </button>
      <button @click="skipToEnd" class="rounded-md ring m-2">
        <div class="block h-7 w-7 m-2">&gt;|</div>
      </button>
    </div>
    <div class="mx-auto my-4 text-center">
      Move {{ move }} / {{ moves.length }}
    </div>
    <div class="flex mx-4 my-4">
      <input
        type="range"
        :min="0"
        :max="moves.length"
        v-model="move"
        class="grow"
      />
    </div>
  </div>
</template>
