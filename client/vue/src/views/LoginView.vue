<script setup lang="ts">
import { ref } from "vue";
import Client from "@/client";
import router from "@/router";

let client = new Client();
let username = ref("");
let password = ref("");

async function login(e: Event) {
    e.preventDefault();
    await client.login(username.value, password.value);
    router.push({ "name": "records" });
}
</script>

<template>
    <div class="mx-auto max-w-lg">
        <div class="my-10 text-4xl text-center">Log in</div>
        <form @submit="login">
            <div class="my-6 flex">
                <div class="mr-4">Username</div>
                <input v-model="username" type="text" class="grow rounded-md" />
            </div>
            <div class="my-6 flex">
                <div class="mr-4">Password</div>
                <input v-model="password" type="password" class="grow rounded-md" />
            </div>
            <button type="submit" class="my-2 w-full bg-gray-200 rounded-md">Log in</button>
        </form>
        <div class="my-6 text-center">
            No account?
            <RouterLink :to="{ 'name': 'register' }" class="text-blue-800 underline">Sign up!</RouterLink>
        </div>
    </div>
</template>