<script setup lang="ts">
import type { PropType, Ref } from "vue";
import { RouterLink } from "vue-router";
import Client, { user } from "@/client";
import router from "@/router";

const props = defineProps({
  closeDialog: Function as PropType<() => {}>,
});
const client = new Client();

function logout() {
  client.logout();
  pleaseCloseDialog();
  router.push({ name: "home" });
}

function pleaseCloseDialog() {
  if (props.closeDialog) {
    props.closeDialog();
  }
}
</script>

<template>
  <div v-if="!user">
    <div class="hidden sm:block">
      <RouterLink
        :to="{ name: 'login' }"
        @click="pleaseCloseDialog"
        class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
      >
        Log in
      </RouterLink>
      <RouterLink
        :to="{ name: 'register' }"
        @click="pleaseCloseDialog"
        class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
      >
        Sign up
      </RouterLink>
    </div>
    <div class="sm:hidden">
      <RouterLink
        :to="{ name: 'login' }"
        @click="pleaseCloseDialog"
        class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium"
      >
        Log in
      </RouterLink>
      <RouterLink
        :to="{ name: 'register' }"
        @click="pleaseCloseDialog"
        class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium"
      >
        Sign up
      </RouterLink>
    </div>
  </div>
  <div v-else>
    <div class="hidden sm:block">
      <span class="text-gray-300 px-3 py-2 rounded-md text-sm font-medium">
        Welcome {{ user.username }}!
      </span>
      <button
        @click="logout"
        class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
      >
        Log out
      </button>
    </div>
    <div class="sm:hidden">
      <span
        class="text-gray-300 block px-3 py-2 rounded-md text-base font-medium"
      >
        Welcome {{ user.username }}!
      </span>
      <button
        @click="logout"
        class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium"
      >
        Log out
      </button>
    </div>
  </div>
</template>
