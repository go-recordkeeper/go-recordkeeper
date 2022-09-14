<script setup lang="ts">
import type { CreateRecordRequest, Record, Ruleset, UpdateRecordRequest } from '@/client';
import type { PropType, Ref } from 'vue';
import { ref } from 'vue';

const props = defineProps({
    defaults: Object as PropType<Record>,
    create: Function as PropType<(request: CreateRecordRequest) => Promise<void>>,
    update: Function as PropType<(request: UpdateRecordRequest) => Promise<void>>,
})

let includeBoardSize = !!props.create;
// let boardSize = ref(19);
// let name = ref('');
// let blackPlayer = ref('Black');
// let whitePlayer = ref('White');
// let comment = ref('');
// let handicap = ref(0);
// let komi = ref(7.5);
// let ruleset: Ref<Ruleset> = ref('AGA');

let { board_size, name, black_player, white_player, comment, handicap, komi, ruleset } = {
    board_size: ref(19),
    name: ref(''),
    black_player: ref('Black'),
    white_player: ref('White'),
    comment: ref(''),
    handicap: ref(0),
    komi: ref(7.5),
    ruleset: ref('AGA') as Ref<Ruleset>,
};
if (props.defaults) {
    board_size.value = props.defaults.board_size;
    name.value = props.defaults.name;
    black_player.value = props.defaults.black_player;
    white_player.value = props.defaults.white_player;
    comment.value = props.defaults.comment;
    handicap.value = props.defaults.handicap;
    komi.value = props.defaults.komi;
    ruleset.value = props.defaults.ruleset;
}

async function _submit(e: Event) {
    e.preventDefault();
    if (!!props.create) {
        let createRequest: CreateRecordRequest = { board_size: board_size.value, name: name.value, black_player: black_player.value, white_player: white_player.value, comment: comment.value, handicap: handicap.value, komi: komi.value, ruleset: ruleset.value };
        props.create(createRequest);
    } else if (!!props.update) {
        let updateRequest: UpdateRecordRequest = { name: name.value, black_player: black_player.value, white_player: white_player.value, comment: comment.value, handicap: handicap.value, komi: komi.value, ruleset: ruleset.value };
        props.update(updateRequest);
    }
}

</script>
    
<template>
    <form @submit="_submit">
        <div class="m-6 flex">
            <div class="grow">
                <input v-model="black_player" class="px-2 bg-gray-900 text-gray-200 w-full rounded-md" />
            </div>
            <div class="grow">
                <input v-model="white_player"
                    class="px-2 bg-white text-gray-900 placeholder:italic placeholder:text-gray-300 w-full rounded-md" />
            </div>
        </div>
        <div v-if="includeBoardSize" class="m-6 flex">
            <div class="mr-4">
                Board Size
            </div>
            <select v-model="board_size"  class="grow rounded-md">
                <option :value="9">9x9</option>
                <option :value="13">13x13</option>
                <option :value="19">19x19</option>
            </select>
        </div>
        <div class="m-6 flex">
            <div class="mr-4">
                Handicap
            </div>
            <input v-model="handicap" class="grow rounded-md" />
        </div>
        <div class="m-6 flex">
            <div class="mr-4">
                Komi
            </div>
            <input v-model="komi" class="grow rounded-md" />
        </div>
        <div class="m-6 flex">
            <div class="mr-4">
                Ruleset
            </div>
            <select v-model="ruleset" class="grow rounded-md">
                <option value="AGA">AGA</option>
                <option value="JPN">Japanese</option>
                <option value="CHN">Chinese</option>
            </select>
        </div>
        <div class="m-6 flex">
            <div class="mr-4">
                Name
            </div>
            <input v-model="name" placeholder="Optional" class="grow rounded-md" />
        </div>
        <div class="m-6 flex">
            <div class="mr-4">
                Comment
            </div>
            <textarea v-model="comment" placeholder="Optional" class="grow rounded-md" />
        </div>

        <button type="submit" class="my-2 w-full bg-gray-200 rounded-md">Done</button>
    </form>
</template>
    