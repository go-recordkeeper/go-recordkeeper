<script setup lang="ts">
import Client from '@/client';
import type { Record } from '@/client';
import type { Ref } from 'vue';
import { ref } from 'vue';
import router from '@/router';

let client = new Client();
let records: Ref<Record[]> = ref([]);
client.getRecords().then((rs) => {
    records.value = rs;
});

async function newRecord() {
    await router.push({name: 'create'});
}

async function deleteRecord(id: number) {
    await client.deleteRecord(id);
    records.value = await client.getRecords();
}

async function downloadRecord(id: number) {
    await client.downloadRecord(id);
}
</script>

<template>
    <div class="about">
        <h1>These are your game records</h1>
        <div v-for="record of records" :key="record.id">
            <router-link :to="{ name: 'record', params: { id: record.id } }">
                record {{ record.id }}
            </router-link>
            <button @click="deleteRecord(record.id)">delete me</button>
            <button @click="downloadRecord(record.id)">SGF</button>
        </div>
        <button @click="newRecord">Make a new record</button>
    </div>
</template>
