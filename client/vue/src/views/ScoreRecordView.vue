<script setup lang="ts">
import { ArrowUturnLeftIcon } from "@heroicons/vue/24/outline";
import Client from "@/client";
import Goban from "@/components/Goban.vue";
import router from "@/router";
import { reactive, ref, watch } from "vue";
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

const matrix: (Color | " ")[][] = reactive([]);
const decorations: (Color | " ")[][] = reactive([]);
const groups: (Group | null)[][] = reactive([]);

client.getRecord(props.id).then((record) => {
  size.value = record.board_size;
  for (let x = 0; x < size.value; x += 1) {
    const matrixColumn: ("B" | "W" | " ")[] = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      matrixColumn.push(" ");
    }
    matrix.push(matrixColumn);
    const decorationColumn = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      decorationColumn.push(" ");
    }
    decorations.push(decorationColumn);
    const groupColumn = reactive([]);
    for (let y = 0; y < size.value; y += 1) {
      groupColumn.push(null);
    }
    groups.push(groupColumn);
  }
  for (const {x, y, color} of record.stones) {
    matrix[x][y] = color;
  }
  initializeGroups();
  refresh();
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
      const group = {
        color: stone,
        dead: false,
        stones: [],
      };
      const placesToVisit: {x:number, y:number}[] = [{x,y}];
      while (placesToVisit.length > 0) {
        const { x, y } = placesToVisit.pop();
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
      console.log("Starting",x,y)
      let blacks = 0;
      let whites = 0;
      const territory = [];
      const placesToVisit: number[] = [toPos(x,y)];
      while (placesToVisit.length > 0) {
        const { x, y } = toXY(placesToVisit.pop());
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
      console.log(x, y, blacks, whites, territory);
      let fill = " ";
      if (blacks == 0 && whites >= 1) {
        console.log("WHITE");
        fill = "W";
      }
      if (whites == 0 && blacks >= 1) {
        console.log("BLACK");
        fill = "B";
      }
      for (const pos of territory) {
        const {x, y} = toXY(pos);
        console.log("POINT",x,y);
        spaces[x][y] = fill;
      }
    }
  }
  return spaces;
}

function refresh() {
  let territories = findTerritories();
  console.log(territories);
  for (let x = 0; x < size.value; x += 1) {
    for (let y = 0; y < size.value; y += 1) {
      let group = groups[x][y];
      if (group && group.dead) {
        console.log(x,y,"DEAD");
        decorations[x][y] = inverse(matrix[x][y]);
      } else if (territories[x][y] !== " " && territories[x][y] !== null){
        console.log(x,y,"TERRITORY");
        decorations[x][y] = territories[x][y];
      } else {
        decorations[x][y] = " ";
      }
    }
  }
}

function click(x, y) {
  let group = groups[x][y];
  if (group) {
    group.dead = !group.dead;
  }
  refresh();
}

function back() {
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
    </div>
  </div>
</template>
