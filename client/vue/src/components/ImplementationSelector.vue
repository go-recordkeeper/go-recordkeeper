<script setup lang="ts">
import { Disclosure, DisclosureButton, DisclosurePanel, Menu, MenuButton, MenuItem, MenuItems } from '@headlessui/vue';
import Client from '@/client';
import { ref } from 'vue';

let props = defineProps({
  buttonClass: String,
});

let client = new Client();
let currentImpl = ref(client.getImplementation());

function selectImpl(impl, closeDropdown) {
  client.setImplementation(impl);
  currentImpl.value = impl;
  closeDropdown();
}

</script>

<template>
<Disclosure as="nav" v-slot="open">
  <DisclosureButton :class="props.buttonClass">
  Implementation
  </DisclosureButton>

  <DisclosurePanel v-slot="{ close }" class="absolute top-15 bg-gray-800 rounded">
    <div v-for="impl in client.implementations" @click="selectImpl(impl, close)" class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">
      <div v-if="currentImpl === impl" class="font-bold">
        {{ impl }}
      </div>
      <div v-else>
        {{ impl }}
      </div>
    </div>
  </DisclosurePanel>
</Disclosure>
</template>
