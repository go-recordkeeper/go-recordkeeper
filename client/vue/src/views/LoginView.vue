<script setup lang="ts">
import { ref } from "vue";
import type { Ref } from "vue";
import Client from "@/client";
import type { UserAuthError } from "@/client";
import router from "@/router";

const client = new Client();
const username = ref("");
const password = ref("");

const fieldErrors: Ref<UserAuthError> = ref({});

async function login(e: Event) {
  e.preventDefault();
  const response = await client.login(username.value, password.value);
  if (response.is_ok()) {
    router.push({ name: "records" });
  } else {
    fieldErrors.value = response.error();
  }
}
</script>

<template>
  <div class="mx-auto max-w-lg">
    <form @submit="login" class="my-20">
      <div class="mt-6 mb-2 mx-0 text-gray-800">Username</div>
      <div class="mb-6 flex">
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
      <div class="mt-6 mb-2 mx-0 text-gray-800">Password</div>
      <div class="mb-6 flex">
        <div class="grow">
          <div>
            <input
              v-model="password"
              type="password"
              class="w-full rounded-md text-lg px-2"
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
      <div
        v-if="fieldErrors.authFailed"
        class="my-2 text-md text-red-600 text-center"
      >
        Incorrect username or password
      </div>
      <button
        type="submit"
        class="my-2 py-1 w-full bg-gray-200 rounded-md text-xl"
      >
        Log in
      </button>
    </form>
    <div class="my-6 text-center">
      No account?
      <RouterLink :to="{ name: 'register' }" class="text-blue-800 underline">
        Sign up!
      </RouterLink>
    </div>
  </div>
</template>
