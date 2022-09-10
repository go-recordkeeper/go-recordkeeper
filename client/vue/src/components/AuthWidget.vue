<script setup lang="ts">
import type { PropType, Ref } from 'vue';
import type { User } from '@/client';
import { inject } from 'vue';
import { RouterLink } from 'vue-router'
import Client from '@/client';

const props = defineProps({
    closeDialog: Function as PropType<() => {}>,
})

let user = inject<Ref<User | null>>('user') as Ref<User | null>;
let client = new Client();

function logout() {
    client.logout();
    user.value = null;
    pleaseCloseDialog();
}

function pleaseCloseDialog() {
    if (props.closeDialog) {
        props.closeDialog();
    }
}
</script>

<template>
    <div v-if="!user">
        <div class="hidden md:block">
            <RouterLink :to="{ 'name': 'login' }" @click="pleaseCloseDialog"
                class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">Log in
            </RouterLink>
        </div>
        <div class="md:hidden">
            <RouterLink :to="{ 'name': 'login' }" @click="pleaseCloseDialog"
                class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">Log in
            </RouterLink>
        </div>
    </div>
    <div v-else>
        <div class="hidden md:block">
            <span class="text-gray-300 px-3 py-2 rounded-md text-sm font-medium">
                Welcome {{user.username}}!
            </span>
            <button @click="logout"
                class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">
                Log out
            </button>
        </div>
        <div class="md:hidden">
            <span class="text-gray-300 block px-3 py-2 rounded-md text-base font-medium">
                Welcome {{user.username}}!
            </span>
            <button @click="logout"
                class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">
                Log out
            </button>
        </div>
    </div>
</template>