<script setup lang="ts">
import type { CreateRecordRequest, Ruleset } from '@/client';
import Client from '@/client';
import router from '@/router';
import GameSettings from '@/components/GameSettings.vue';

let client = new Client();

async function createRecord(request: CreateRecordRequest) {
    let id = await client.createNewRecord(request);
    await router.push({ name: 'record', params: { id } });
}

</script>

<template>
    <div class="about">
        <h1>Make a new record</h1>
        <GameSettings :create="createRecord" />
    </div>
</template>
