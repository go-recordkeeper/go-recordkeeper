<script setup lang="ts">
import { DocumentArrowDownIcon, PencilIcon, TrashIcon } from '@heroicons/vue/24/outline';
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
    await router.push({ name: 'create' });
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
    <div>
        <div class="text-xl m-4">Games</div>
        <table class="table-auto w-full">
            <tbody>
                <tr v-for="record of records" :key="record.id" class="border-b">
                    <td class="p-4">
                        <router-link :to="{ name: 'record', params: { id: record.id } }">
                            {{ record.name }}
                        </router-link>
                    </td>
                    <td class="p-4">
                        <router-link :to="{ name: 'record', params: { id: record.id } }">
                            {{ record.black_player }} vs. {{ record.white_player }}
                        </router-link>
                    </td>
                    <td class="p-4">
                        <router-link :to="{ name: 'record', params: { id: record.id } }">
                            {{ new Date(record.created).toLocaleString() }}
                        </router-link>
                    </td>
                    <td class="p-4">
                        <button @click="downloadRecord(record.id)">
                            <DocumentArrowDownIcon class="block h-6 w-6" />
                        </button>
                    </td>
                    <td class="p-4">
                        <RouterLink :to="{ name: 'update', params: { id: record.id } }">
                            <PencilIcon class="block h-6 w-6" />
                        </RouterLink>
                    </td>
                    <td class="p-4">
                        <button @click="deleteRecord(record.id)">
                            <TrashIcon class="block h-6 w-6" />
                        </button>
                    </td>
                </tr>
            </tbody>
        </table>
    </div>
</template>
