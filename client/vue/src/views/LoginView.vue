<script setup lang="ts">
import { inject, ref } from "vue";
import Client, { user } from "@/client";
import type { User } from "@/client";
import type { Ref } from "vue";
import router from "@/router";

let client = new Client();
let username = ref("");
let password = ref("");

async function login(e: Event) {
    e.preventDefault();
    await client.login(username.value, password.value);
    user.value = await client.getCurrentUser();
    router.push({"name": "records"});
}
</script>

<template>
    <div class="about">
        <h1>would you like to log in?</h1>
        <form @submit="login">
            <div>
                <input v-model="username" type="text" />
            </div>
            <div>
                <input v-model="password" type="password" />
            </div>
            <button type="submit">Go!</button>
        </form>
        <h1>Maybe you wanna register?</h1>
        <RouterLink :to="{ 'name': 'register' }">Sign up!</RouterLink>
    </div>
</template>