<script setup lang="ts">
import type { CreateRecordRequest, Ruleset } from "@/client";
import Client from "@/client";
import router from "@/router";
import GameSettings from "@/components/GameSettings.vue";

const client = new Client();

async function createRecord(request: CreateRecordRequest) {
  const response = await client.createNewRecord(request);
  if (response.is_ok()) {
    await router.push({ name: "record", params: { id: response.json().id } });
  }
  return response;
}
</script>

<template>
  <GameSettings :create="createRecord" />
</template>
