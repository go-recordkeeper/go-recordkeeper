<script setup lang="ts">
import type { Ref } from 'vue';
import type { User } from '@/client';
import { inject } from 'vue';
import { RouterLink } from 'vue-router'
import Client from '@/client';

let user = inject<Ref<User | null>>('user') as Ref<User | null>;
let client = new Client();

function logout() {
    client.logout();
    user.value = null;
}
</script>

<template>
    <div v-if="!user">
        <RouterLink :to="{ 'name': 'login' }"
            class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Log in
        </RouterLink>
    </div>
    <div v-else>
        <span class="text-gray-300 px-3 py-2 rounded-md text-sm font-medium">
            Welcome {{user.username}}!
        </span>
        <button @click="logout" class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">
            Log out
        </button>
    </div>
</template>