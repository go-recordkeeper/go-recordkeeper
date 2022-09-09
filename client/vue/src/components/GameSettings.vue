<script setup lang="ts">
import type { CreateRecordRequest, Ruleset, UpdateRecordRequest } from '@/client';
import type { PropType, Ref } from 'vue';
import { ref } from 'vue';

const props = defineProps({
  create: Function as PropType<(request: CreateRecordRequest) => Promise<void>>,
  update: Function as PropType<(request: UpdateRecordRequest) => Promise<void>>,
})

let includeBoardSize = !!props.create;
let boardSize = ref(19);
let name = ref('');
let blackPlayer = ref('Black');
let whitePlayer = ref('White');
let comment = ref('');
let handicap = ref(0);
let komi = ref(7.5);
let ruleset: Ref<Ruleset> = ref('AGA');

async function _submit(e: Event) {
    e.preventDefault();
    let updateRequest: UpdateRecordRequest = {
        name: name.value,
        black_player: blackPlayer.value,
        white_player: whitePlayer.value,
        comment: comment.value,
        handicap: handicap.value,
        komi: komi.value,
        ruleset: ruleset.value,
    };
    if (!!props.create) {
        let createRequest: CreateRecordRequest = (updateRequest as CreateRecordRequest)
        createRequest.board_size = boardSize.value;
        props.create(createRequest);
    } else if (!!props.update) {
        props.update(updateRequest);
    }
}

</script>
    
<template>
    <form @submit="_submit">
        <select v-model="boardSize" v-if="includeBoardSize">
            <option :value="9">9x9</option>
            <option :value="13">13x13</option>
            <option :value="19">19x19</option>
        </select>
        <div>
            Black <input v-model="blackPlayer" />
        </div>
        <div>
            White <input v-model="whitePlayer" />
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
    