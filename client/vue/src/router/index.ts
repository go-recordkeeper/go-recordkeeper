import { createRouter, createWebHistory } from 'vue-router'
import HomeView from '@/views/HomeView.vue'
import { user } from '@/client';

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes: [
    {
      path: '/',
      name: 'home',
      component: () => import('@/views/HomeView.vue')
    },
    {
      path: '/records',
      name: 'records',
      component: () => import('@/views/RecordListView.vue')
    },
    {
      path: '/records/:id',
      name: 'record',
      props: (route) => ({ id: Number(route.params.id) }),
      component: () => import('@/views/RecordView.vue')
    },
    {
      path: '/records/create',
      name: 'create',
      component: () => import('@/views/CreateRecordView.vue')
    },
    {
      path: '/records/:id/update',
      name: 'update',
      props: (route) => ({ id: Number(route.params.id) }),
      component: () => import('@/views/UpdateRecordView.vue')
    },
    {
      path: '/records/:id/replay',
      name: 'replay',
      props: (route) => ({ id: Number(route.params.id) }),
      component: () => import('@/views/ReplayRecordView.vue')
    },
    {
      path: '/login',
      name: 'login',
      component: () => import('@/views/LoginView.vue')
    },
    {
      path: '/register',
      name: 'register',
      component: () => import('@/views/RegisterView.vue')
    },
  ]
})
router.beforeEach((to, from) => {
  if (user.value === null) {
    let { name } = to;
    if (name !== 'home' && name !== 'login' && name !== 'register') {
      return { name: 'home' };
    }
  }
});

export default router
