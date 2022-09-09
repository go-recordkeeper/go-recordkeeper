<script setup lang="ts">
import type { UpdateRecordRequest, Ruleset } from '@/client';
import Client from '@/client';
import router from '@/router';
import GameSettings from '@/components/GameSettings.vue';

const props = defineProps({
  id: {
    type: Number,
    required: true,
  },
})

let client = new Client();

async function updateRecord(request: UpdateRecordRequest) {
    console.log('called updateRecord', props.id, props, request);
    await client.updateRecord(props.id, request);
    await router.push({ name: 'record', params: { id: props.id } });
}

</script>

<template>
    <div class="about">
        <h1>Make a new record</h1>
        <GameSettings :update="updateRecord" />
    </div>
</template>
