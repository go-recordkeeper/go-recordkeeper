<script setup lang="ts">
import Client from '@/client';
import type { Record } from '@/client';
import type { Ref } from 'vue';
import { ref } from 'vue';
import router from '@/router';

let client = new Client();

let boardSize = ref(19);

async function createRecord(e: Event) {
    e.preventDefault();
    let id = await client.createNewRecord(boardSize.value);
    await router.push({ name: 'record', params: { id } });
}

</script>

<template>
    <div class="about">
        <h1>Make a new record</h1>
        <form @submit="createRecord">
            <!-- name -->
            <select v-model="boardSize">
                <option :value="9">9x9</option>
                <option :value="13">13x13</option>
                <option :value="19">19x19</option>
            </select>
            <!-- black -->
            <!-- white -->
            <!-- comment -->
            <!-- handicap -->
            <!-- ruleset -->
            <!-- komi -->
            <button type="submit">Create</button>
        </form>
    </div>
</template>
