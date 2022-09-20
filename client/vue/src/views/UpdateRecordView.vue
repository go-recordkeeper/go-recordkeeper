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
    let response = await client.updateRecord(props.id, request);
    if (response.is_ok()) {
        await router.push({ name: 'record', params: { id: props.id } });
    }
    return response;
}

</script>

<template>
    <GameSettings v-if="isLoaded" :defaults="record" :update="updateRecord" />
</template>
