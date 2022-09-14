<script setup lang="ts">
import { Disclosure, DisclosureButton, DisclosurePanel, Menu, MenuButton, MenuItem, MenuItems } from '@headlessui/vue';
import { Bars3Icon, BellIcon, XMarkIcon } from '@heroicons/vue/24/outline';
import { defineComponent, provide, ref } from 'vue';
import { RouterLink, RouterView } from 'vue-router'
import AuthWidget from '@/components/AuthWidget.vue';
import { user } from '@/client';
</script>

<template>
  <!-- <header>
    <img alt="Vue logo" class="logo" src="@/assets/logo.svg" width="125" height="125" />

    <div class="wrapper">
      <HelloWorld msg="You did it!" />

      <nav>
        <RouterLink to="/">Home</RouterLink>
        <RouterLink to="/about">About</RouterLink>
      </nav>
    </div>
  </header> -->

  <div class="min-h-full">
    <Disclosure as="nav" class="bg-gray-800" v-slot="{ open }">
      <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
        <div class="flex h-16 items-center justify-between">
          <div class="flex items-center">
            <!-- <div class="flex-shrink-0">
              <img class="h-8 w-8" src="https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=500"
                alt="Your Company" />
            </div> -->
            <div class="hidden sm:block">
              <div class="flex items-baseline space-x-4">
                <!-- <a v-for="item in navigation" :key="item.name" :href="item.href"
                  :class="[item.current ? 'bg-gray-900 text-white' : 'text-gray-300 hover:bg-gray-700 hover:text-white', 'px-3 py-2 rounded-md text-sm font-medium']"
                  :aria-current="item.current ? 'page' : undefined">{{ item.name }}</a> -->
                <RouterLink :to="{ 'name': 'home' }"
                  class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">
                  Home
                </RouterLink>
                <RouterLink :to="{ 'name': 'records' }" v-if="user"
                  class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">
                  Games
                </RouterLink>
                <RouterLink :to="{ 'name': 'create' }" v-if="user"
                  class="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium">
                  New Game
                </RouterLink>
              </div>
            </div>
          </div>

          <div class="hidden sm:block">
            <div class="ml-4 flex items-center sm:ml-6">
              <AuthWidget />
            </div>
          </div>
          <div class="-mr-2 flex sm:hidden">
            <!-- Mobile menu button -->
            <DisclosureButton
              class="inline-flex items-center justify-center rounded-md bg-gray-800 p-2 text-gray-400 hover:bg-gray-700 hover:text-white focus:outline-none focus:ring-2 focus:ring-white focus:ring-offset-2 focus:ring-offset-gray-800">
              <span class="sr-only">Open main menu</span>
              <Bars3Icon v-if="!open" class="block h-6 w-6" aria-hidden="true" />
              <XMarkIcon v-else class="block h-6 w-6" aria-hidden="true" />
            </DisclosureButton>
          </div>
        </div>
      </div>

      <DisclosurePanel class="sm:hidden" v-slot="{ close }">
        <div class="space-y-1 px-2 pt-2 pb-3 sm:px-3">
          <RouterLink :to="{ 'name': 'home' }" @click="close"
            class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">
            Home
          </RouterLink>
          <RouterLink :to="{ 'name': 'records' }" v-if="user" @click="close"
            class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">
            Games
          </RouterLink>
          <RouterLink :to="{ 'name': 'create' }" v-if="user" @click="close"
            class="text-gray-300 hover:bg-gray-700 hover:text-white block px-3 py-2 rounded-md text-base font-medium">
            New Game
          </RouterLink>
          <AuthWidget :closeDialog="close" />
        </div>
      </DisclosurePanel>
    </Disclosure>
  </div>

  <div class="mx-auto max-w-7xl">
    <RouterView />
  </div>
</template>
