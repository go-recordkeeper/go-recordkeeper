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
        <RouterLink :to="{ 'name': 'login' }">Log in</RouterLink>
    </div>
    <div v-else>
        Welcome {{user.username}}!
        <button @click="logout">Log out</button>
    </div>
</template>