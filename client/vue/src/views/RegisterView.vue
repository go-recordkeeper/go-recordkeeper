<script setup lang="ts">
import { ref } from "vue";
import type { Ref } from "vue";
import Client from "@/client";
import type { UserAuthError } from "@/client";
import router from "@/router";

const client = new Client();
const username = ref("");
const email = ref("");
const password = ref("");

const fieldErrors: Ref<UserAuthError> = ref({});

async function register(e: Event) {
  e.preventDefault();
  const response = await client.register(
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
      <div class="mt-4 mb-2 mx-0 text-gray-800">Username</div>
      <div class="flex">
        <div class="grow">
          <div>
            <input v-model="username" class="w-full rounded-md text-lg px-2" />
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
      <div class="mt-4 mb-2 mx-0 text-gray-800">Email</div>
      <div class="flex">
        <div class="grow">
          <div>
            <input
              v-model="email"
              type="email"
              class="w-full rounded-md text-lg px-2"
            />
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
      <div class="mt-4 mb-2 mx-0 text-gray-800">Password</div>
      <div class="flex">
        <div class="grow">
          <div>
            <input
              v-model="password"
              type="password"
              class="w-full rounded-m text-lg px-2"
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
      <button
        type="submit"
        class="my-6 py-1 w-full bg-gray-200 rounded-md text-xl"
      >
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
