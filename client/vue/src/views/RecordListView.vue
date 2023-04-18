<script setup lang="ts">
import {
  DocumentArrowDownIcon,
  PencilIcon,
  TrashIcon,
} from "@heroicons/vue/24/outline";
import Client from "@/client";
import type { Record, ListRecordResponse } from "@/client";
import type { Ref } from "vue";
import { watchEffect, ref } from "vue";
import router from "@/router";

const client = new Client();
const page: Ref<number> = ref(1);
const records: Ref<ListRecordResponse | null> = ref(null);

watchEffect(() => {
  client.getRecords(page.value).then((rs) => {
    records.value = rs;
  });
});

async function deleteRecord(id: number) {
  await client.deleteRecord(id);
  records.value = await client.getRecords(page.value);
}

async function downloadRecord(id: number) {
  await client.downloadRecord(id);
}

async function clickPage(pageNum: number) {
  page.value = pageNum;
}

function canPageBack() {
  return page.value > 1;
}

function pageBack() {
  if (canPageBack()) {
    page.value -= 1;
  }
}

function canPageForward() {
  return records.value && page.value < records.value.pages;
}

function pageForward() {
  if (canPageForward()) {
    page.value += 1;
  }
}
</script>

<template>
  <div>
    <div class="text-xl m-4">Records</div>
    <table v-if="records" class="table-auto w-full">
      <tbody>
        <!-- Desktop sized -->
        <tr
          v-for="record of records.results"
          :key="record.id"
          class="border-b hidden md:table-row"
        >
          <td class="p-4">
            <router-link :to="{ name: 'record', params: { id: record.id } }">
              <div class="text-lg">{{ record.black_player }} vs. {{ record.white_player }}</div>
              <div class="text-md text-gray-700">
                {{ record.name }}
              </div>
            </router-link>
          </td>
          <td class="p-4 text-sm">
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
        <!-- Mobile sized -->
        <tr
          v-for="record of records.results"
          :key="record.id"
          class="border-b table-row md:hidden"
        >
          <td class="p-4 block">
            <router-link :to="{ name: 'record', params: { id: record.id } }">
              <div>{{ record.black_player }} vs. {{ record.white_player }}</div>
              <div class="text-sm text-gray-700">
                {{ record.name }}
              </div>
              <div class="text-sm text-gray-700">
                {{ new Date(record.created).toLocaleString() }}
              </div>
            </router-link>
          </td>
          <td class="p-1">
            <button @click="downloadRecord(record.id)">
              <DocumentArrowDownIcon class="block h-6 w-6" />
            </button>
          </td>
          <td class="p-1">
            <RouterLink :to="{ name: 'update', params: { id: record.id } }">
              <PencilIcon class="block h-6 w-6" />
            </RouterLink>
          </td>
          <td class="p-1">
            <button @click="deleteRecord(record.id)">
              <TrashIcon class="block h-6 w-6" />
            </button>
          </td>
        </tr>
      </tbody>
    </table>
    <div v-if="records" class="my-4 flex flex-row space-x-2">
      <div class="grow"></div>
      <!-- Back arrow -->
      <button
        v-if="canPageBack()"
        @click="pageBack"
        class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"
      >
        &lt;
      </button>
      <button
        v-else
        class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-200 text-gray-400"
      >
        &lt;
      </button>
      <!-- The page numbers -->
      <div
        v-for="pageNum in records.pages"
        :key="pageNum"
        @click="clickPage(pageNum)"
      >
        <!-- Highlight the currently selected page -->
        <button
          v-if="pageNum === page"
          class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-300"
        >
          {{ pageNum }}
        </button>
        <!-- Normal color for everything else -->
        <button
          v-else
          class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"
        >
          {{ pageNum }}
        </button>
      </div>
      <!-- Forward arrow -->
      <button
        v-if="canPageForward()"
        @click="pageForward"
        class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"
      >
        &gt;
      </button>
      <button
        v-else
        class="w-8 h-8 rounded-md flex items-center justify-center bg-gray-200 text-gray-400"
      >
        &gt;
      </button>
      <div class="grow"></div>
    </div>
  </div>
</template>
