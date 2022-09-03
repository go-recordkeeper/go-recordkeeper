<script setup lang="ts">
import { ref } from "vue";
import Client from "@/client";

let client = new Client();
let username = ref("");
let password = ref("");

async function login(e: Event) {
    e.preventDefault();
    console.log("Login in", username.value, password.value);
    let token = await client.login(username.value, password.value);
    console.log('token',token);
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