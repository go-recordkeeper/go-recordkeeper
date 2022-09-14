import { createApp } from 'vue'
import App from '@/App.vue'
import router from '@/router'
import '@/index.css'
import Client from '@/client';

// import './assets/main.css'
const client = new Client();
await client.initializeUser();

const app = createApp(App)

app.use(router)

app.mount('#app')
