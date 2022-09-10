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
        <select v-model="board_size" v-if="includeBoardSize">
            <option :value="9">9x9</option>
            <option :value="13">13x13</option>
            <option :value="19">19x19</option>
        </select>
        <div>
            Black <input v-model="black_player" />
        </div>
        <div>
            White <input v-model="white_player" />
        </div>
        <div>
            Name of the game <input v-model="name" />
        </div>
        <div>
            Comment <input v-model="comment" />
        </div>
        <div>
            Handicapp <input v-model="handicap" />
        </div>
        <div>
            Komi <input v-model="komi" />
        </div>
        <select v-model="ruleset">
            <option value="AGA">AGA</option>
            <option value="JPN">Japanese</option>
            <option value="CHN">Chinese</option>
        </select>

        <button type="submit">Create</button>
    </form>
</template>
    