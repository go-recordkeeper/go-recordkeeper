<script setup lang="ts">
import { inject, ref } from "vue";
import Client, { user } from "@/client";
import type { User } from "@/client";
import type { Ref } from "vue";
import router from "@/router";

let client = new Client();
let username = ref("");
let email = ref("");
let password = ref("");

async function register(e: Event) {
    e.preventDefault();
    await client.register(username.value, email.value, password.value);
    user.value = await client.getCurrentUser();
    router.push({"name": "records"});
}
</script>

<template>
    <div class="mx-auto max-w-lg">
        <div class="my-10 text-4xl text-center">Sign up</div>
        <form @submit="register">
            <div class="my-6 flex">
                <div class="mr-4">Username</div>
                <input v-model="username" type="text" class="grow rounded-md" />
            </div>
            <div class="my-6 flex">
                <div class="mr-4">Email</div>
                <input v-model="email" type="email" class="grow rounded-md" />
            </div>
            <div class="my-6 flex">
                <div class="mr-4">Password</div>
                <input v-model="password" type="password" class="grow rounded-md" />
            </div>
            <button type="submit" class="my-2 w-full bg-gray-200 rounded-md">Sign up</button>
        </form>
        <div class="my-6 text-center">
            Already have an account?
            <RouterLink :to="{ 'name': 'login' }" class="text-blue-800 underline">Log in!</RouterLink>
        </div>
    </div>
</template>