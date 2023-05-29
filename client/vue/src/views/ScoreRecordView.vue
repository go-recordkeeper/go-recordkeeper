<script setup lang="ts">
import { ArrowUturnLeftIcon } from "@heroicons/vue/24/outline";
import { FlagIcon } from "@heroicons/vue/24/outline";
import Client from "@/client";
import Goban from "@/components/Goban.vue";
import router from "@/router";
import { computed, reactive, ref, watch } from "vue";
import type { Ref } from "vue";

type Color = "B" | "W";

type Group = {
  color: Color,
  dead: boolean,
  stones: {x:number, y:number}[],
};

function inverse(color: Color) {
  if (color === "B") {
    return "W";
  } else {
    return "B";
  }
}

const props = defineProps({
  id: {
    type: Number,
    required: true,
  },
});

const client = new Client();
const size = ref(0);
let komi = ref(0);
const blackCaptures = ref(0);
const whiteCaptures = ref(0);
const blackTerritories = ref(0);
const whiteTerritories = ref(0);
function blackPoints() {
  return blackCaptures.value + blackTerritories.value;
}
function whitePoints() {
  return komi.value + whiteCaptures.value + whiteTerritories.value;
}


const matrix: (Color | " ")[][] = reactive([]);
const decorations: (Color | " ")[][] = reactive([]);
const groups: (Group | null)[][] = reactive([]);
const territories: (Color | " " | null)[][] = reactive([]);

client.getRecord(props.id).then((record) => {
  komi.value = record.komi;
  size.value = record.board_size;
  for (let x = 0; x < size.value; x += 1) {
    const matrixColumn: ("B" | "W" | " ")[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      matrixColumn.push(" ");
    }
    matrix.push(matrixColumn);
    const decorationColumn: (Color | " ")[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      decorationColumn.push(" ");
    }
    decorations.push(decorationColumn);
    const groupColumn: (Group | null)[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      groupColumn.push(null);
    }
    groups.push(groupColumn);
  }
  for (const {x, y, color} of record.stones) {
    matrix[x][y] = color;
  }
  for (const move of record.moves) {
    if (move.color == "B") {
      blackCaptures.value += move.captures.length;
    }
    if (move.color == "W") {
      whiteCaptures.value += move.captures.length;
    }
  }
  initializeGroups();
  findTerritories();
});

function *adjacents(x: number, y:number) {
  if (x > 0) {
    yield {x:x - 1, y};
  }
  if (y > 0) {
    yield {x, y:y - 1};
  }
  if (x < size.value - 1) {
    yield {x:x + 1, y};
  }
  if (y < size.value - 1) {
    yield {x, y:y + 1};
  }
}

function initializeGroups() {
  for (let x = 0; x < size.value; x += 1) {
    for (let y = 0; y < size.value; y += 1) {
      const stone = matrix[x][y];
      if (stone === " ") {
        // There is no stone at this point
        continue;
      }
      if (groups[x][y] !== null) {
        // This stone has already been added to a group
        continue;
      }
      // There is a new stone in a new group
      const group: {color: Color, dead: boolean, stones: {x:number, y:number}[]} = reactive({
        color: stone,
        dead: false,
        stones: [],
      });
      const placesToVisit: {x:number, y:number}[] = [{x,y}];
      while (placesToVisit.length > 0) {
        const { x, y } = placesToVisit.pop() as {x:number, y:number};
        group.stones.push({x,y});
        groups[x][y] = group;
        for (const {x: dx, y: dy} of adjacents(x,y)) {
          if (matrix[dx][dy] === stone && !placesToVisit.includes({x: dx, y: dy}) && groups[dx][dy] === null) {
            placesToVisit.push({x: dx, y: dy});
          }
        }
      }
    }
  }
}


function toPos(x:number, y: number) {
  return x + (19*y);
}

function toXY(pos: number) {
  return {x: pos % 19, y: Math.floor(pos/19)};
}


function findTerritories() {
  blackTerritories.value = 0;
  whiteTerritories.value = 0;
  const spaces = [];
  for (let x = 0; x < size.value; x += 1) {
    const spaceColumn: (Color | " " | null)[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      spaceColumn.push(null);
    }
    spaces.push(spaceColumn);
  }

  for (let x = 0; x < size.value; x += 1) {
    for (let y = 0; y < size.value; y += 1) {
      const stone = matrix[x][y];
      if (stone !== " ") {
        // There is a stone at this point, it's not territory
        continue;
      }
      if (spaces[x][y] !== null) {
        // This stone has already been evaluated
        continue;
      }
      // There is a new point in a new territory
      let blacks = 0;
      let whites = 0;
      const territory = [];
      const placesToVisit: number[] = [toPos(x,y)];
      while (placesToVisit.length > 0) {
        const { x, y } = toXY(placesToVisit.pop() as number);
        territory.push(toPos(x,y));
        for (const {x: dx, y: dy} of adjacents(x,y)) {
          const group = groups[dx][dy];
          if (group && !group.dead) {
            // It's a live group, so not territory
            // We don't care about double counting border colors
            if (group.color === "B") {
              blacks += 1;
            } else if (group.color === "W") {
              whites += 1;
            }
          } else if (!placesToVisit.includes(toPos(dx, dy)) && !territory.includes(toPos(dx, dy))) {
            placesToVisit.push(toPos(dx,dy));
          }
        }
      }
      let fill: Color | " " = " ";
      if (blacks == 0 && whites >= 1) {
        fill = "W";
        whiteTerritories.value += territory.length;
      }
      if (whites == 0 && blacks >= 1) {
        fill = "B";
        blackTerritories.value += territory.length;
      }
      for (const pos of territory) {
        const {x, y} = toXY(pos);
        spaces[x][y] = fill;
      }
    }
  }
  // territories.value = spaces;
  Object.assign(territories, spaces);
}

watch(territories, ()=>{
  for (let x = 0; x < size.value; x += 1) {
    for (let y = 0; y < size.value; y += 1) {
      let group = groups[x][y];
      let t = territories[x][y];
      if (group && group.dead) {
        decorations[x][y] = inverse(group.color);
      } else if (t !== " " && t !== null){
        decorations[x][y] = t;
      } else {
        decorations[x][y] = " ";
      }
    }
  }
});

function click(x:number, y:number) {
  let group = groups[x][y];
  if (group) {
    group.dead = !group.dead;
    if (group.dead && group.color == "B") {
      // Killed a black group, white gets points
      whiteCaptures.value += group.stones.length;
    }
    if (group.dead && group.color == "W") {
      // Killed a white group, black gets points
      blackCaptures.value += group.stones.length;
    }
    if (!group.dead && group.color == "B") {
      // Revived a black group, white loses points
      whiteCaptures.value -= group.stones.length;
    }
    if (!group.dead && group.color == "W") {
      // Revived a white group, black loses points
      blackCaptures.value -= group.stones.length;
    }
  }
  findTerritories();
}

function back() {
  router.back();
}

async function save() {
  console.log("Saving");
  const record = await client.getRecord(props.id);
  if (whitePoints() > blackPoints()) {
    record.winner = "W";
  } else {
    record.winner = "B";
  }
  await client.updateRecord(props.id, record);
  router.back();
}

</script>

<template>
  <div class="mx-auto" style="max-width: calc(100vh - 220px)">
    <Goban
      v-if="size"
      :size="size"
      :matrix="matrix"
      :decorations="decorations"
      :onClick="click"
      style="max-width: calc(100vh - 220px); max-height: calc(100vh - 220px)"
    >
    </Goban>
    <div v-else>Loading game...{{ size }}</div>
    <div class="flex items-center">
      <button @click="back" class="rounded-md ring m-2 bg-green-400">
        <ArrowUturnLeftIcon class="block h-7 w-7 m-2" />
      </button>
      <div class="text-2xl mx-2">
        Black: {{ blackPoints() }}
      </div>
      <div class="text-base mx-2">
        <div>
          {{ blackCaptures }} captures
        </div>
        <div>
          {{ blackTerritories }} territory
        </div>
      </div>
      <div class="text-2xl mx-2">
        White: {{ whitePoints() }}
      </div>
      <div class="text-base mx-2">
        <div>
          {{ whiteCaptures }} captures
        </div>
        <div>
          {{ whiteTerritories }} territory
        </div>
        <div>
          {{ komi }} komi
        </div>
      </div>
      <button @click="save" class="grow flex items-center rounded-md ring m-2">
        <FlagIcon class="block h-7 w-7 m-2" />
        Save Result
      </button>
    </div>
  </div>
</template>
