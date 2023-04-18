<script setup lang="ts">
import { ref } from "vue";
import type { Ref } from "vue";
import Client from "@/client";
import type { UserAuthError } from "@/client";
import router from "@/router";

let client = new Client();
let username = ref("");
let email = ref("");
let password = ref("");

let fieldErrors: Ref<UserAuthError> = ref({});

async function register(e: Event) {
  e.preventDefault();
  let response = await client.register(
    username.value,
    email.value,
    password.value
  );
  if (response.is_ok()) {
    router.push({ name: "records" });
  } else {
    fieldErrors.value = response.error();
  }
}
</script>

<template>
  <div class="mx-auto max-w-lg">
    <div class="my-10 text-4xl text-center">Sign up</div>
    <form @submit="register">
      <div class="my-6 flex">
        <div class="mr-4">Username</div>
        <div class="grow">
          <div>
            <input v-model="username" class="w-full rounded-md" />
          </div>
          <ul v-if="fieldErrors.username">
            <li
              v-for="error in fieldErrors.username"
              :key="error"
              class="text-sm text-red-600"
            >
              {{ error }}
            </li>
          </ul>
        </div>
      </div>
      <div class="my-6 flex">
        <div class="mr-4">Email</div>
        <div class="grow">
          <div>
            <input v-model="email" type="email" class="w-full rounded-md" />
          </div>
          <ul v-if="fieldErrors.email">
            <li
              v-for="error in fieldErrors.email"
              :key="error"
              class="text-sm text-red-600"
            >
              {{ error }}
            </li>
          </ul>
        </div>
      </div>
      <div class="my-6 flex">
        <div class="mr-4">Password</div>
        <div class="grow">
          <div>
            <input
              v-model="password"
              type="password"
              class="w-full rounded-md"
            />
          </div>
          <ul v-if="fieldErrors.password">
            <li
              v-for="error in fieldErrors.password"
              :key="error"
              class="text-sm text-red-600"
            >
              {{ error }}
            </li>
          </ul>
        </div>
      </div>
      <button type="submit" class="my-2 w-full bg-gray-200 rounded-md">
        Sign up
      </button>
    </form>
    <div class="my-6 text-center">
      Already have an account?
      <RouterLink :to="{ name: 'login' }" class="text-blue-800 underline"
        >Log in!</RouterLink
      >
    </div>
  </div>
</template>
