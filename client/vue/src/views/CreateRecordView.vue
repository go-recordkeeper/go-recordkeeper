<script setup lang="ts">
import type { Ruleset } from '@/client';
import Client from '@/client';
import type { Record } from '@/client';
import type { Ref } from 'vue';
import { ref } from 'vue';
import router from '@/router';

let client = new Client();

let boardSize = ref(19);
let name = ref('');
let blackPlayer = ref('Black');
let whitePlayer = ref('White');
let comment = ref('');
let handicap = ref(0);
let komi = ref(7.5);
let ruleset: Ref<Ruleset> = ref('AGA');

async function createRecord(e: Event) {
    e.preventDefault();
    let id = await client.createNewRecord({
        board_size: boardSize.value,
        name: name.value,
        black_player: blackPlayer.value,
        white_player: whitePlayer.value,
        comment: comment.value,
        handicap: handicap.value,
        komi: komi.value,
        ruleset: ruleset.value,
    });
    await router.push({ name: 'record', params: { id } });
}

</script>

<template>
    <div class="about">
        <h1>Make a new record</h1>
        <form @submit="createRecord">
            <select v-model="boardSize">
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

            <!-- black -->
            <!-- white -->
            <!-- name -->
            <!-- comment -->
            <!-- handicap -->
            <!-- ruleset -->
            <!-- komi -->
            <button type="submit">Create</button>
        </form>
    </div>
</template>
