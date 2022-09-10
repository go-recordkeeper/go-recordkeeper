<script setup lang="ts">
import type { UpdateRecordRequest, Ruleset, Record } from '@/client';
import Client from '@/client';
import router from '@/router';
import GameSettings from '@/components/GameSettings.vue';
import { ref } from 'vue';
import type { Ref } from 'vue';

const props = defineProps({
    id: {
        type: Number,
        required: true,
    },
})

let client = new Client();

let isLoaded = ref(false);
let record = ref(null) as Ref;
client.getRecord(props.id).then((detail) => {
    record.value = detail as Record;
    isLoaded.value = true;
})

async function updateRecord(request: UpdateRecordRequest) {
    console.log('called updateRecord', props.id, props, request);
    await client.updateRecord(props.id, request);
    await router.push({ name: 'record', params: { id: props.id } });
}

</script>

<template>
    <div class="about">
        <h1>Make a new record</h1>
        <GameSettings v-if="isLoaded" :defaults="record" :update="updateRecord" />
    </div>
</template>
