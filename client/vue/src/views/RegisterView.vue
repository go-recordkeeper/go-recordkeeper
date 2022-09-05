<script setup lang="ts">
import { inject, ref } from "vue";
import Client from "@/client";
import type { User } from "@/client";
import type { Ref } from "vue";
import router from "@/router";

let user = inject<Ref<User | null>>('user') as Ref<User | null>;
let client = new Client();
let username = ref("");
let email = ref("");
let password = ref("");

async function register(e: Event) {
    e.preventDefault();
    console.log("Register", username.value, email.value, password.value);
    await client.register(username.value, email.value, password.value);
    user.value = await client.getCurrentUser();
    router.push({"name": "records"});
}
</script>

<template>
    <div class="about">
        <h1>would you like to register?</h1>
        <form @submit="register">
            <div>
                <input v-model="username" type="text" />
            </div>
            <div>
                <input v-model="email" type="email" />
            </div>
            <div>
                <input v-model="password" type="password" />
            </div>
            <button type="submit">Go!</button>
        </form>
    </div>
</template>