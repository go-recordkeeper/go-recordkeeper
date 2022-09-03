<script setup lang="ts">
import { inject, ref } from "vue";
import Client from "@/client";
import type { User } from "@/client";
import type { Ref } from "vue";

let user = inject<Ref<User | null>>('user') as Ref<User | null>;
let client = new Client();
let username = ref("");
let password = ref("");

async function login(e: Event) {
    e.preventDefault();
    console.log("Login in", username.value, password.value);
    await client.login(username.value, password.value);
    user.value = await client.getCurrentUser();
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
    </div>
</template>