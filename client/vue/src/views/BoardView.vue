<script setup lang="ts">
import Goban from '@/components/Goban.vue';
import { ref } from 'vue';

const props = defineProps({
  id: {
      type: Number,
      required: true,
  },
})

</script>
    
<template>
    <Goban :id="id"></Goban>
</template>
