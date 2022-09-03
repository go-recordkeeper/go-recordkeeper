<script setup lang="ts">
import type { Ref } from 'vue';
import { inject } from 'vue';
import { RouterLink } from 'vue-router'

let user = inject<Ref<any>>('user');
console.log('ayy', user, user?.value)
</script>

<template>
    <RouterLink :to="{ 'name': 'login' }">Log in</RouterLink>
</template>