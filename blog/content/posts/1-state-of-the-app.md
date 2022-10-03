---
title: "State of the App"
date: 2022-09-22T10:48:24-04:00
type: post
slug: state-of-the-app
tags:
  - django
  - vue
  - nginx
  - infrastructure
showTableOfContents: true
---

Before I get into the technical details of the new apps I'm building, it would be helpful to describe the reference implementation, you know, for reference.

## The app, in a nutshell

Go Recordkeeper is a relatively simple Single Page Application. You can:

- Log in + Sign up
- Create new records
- See/edit your old records
- Add/undo moves in a record
- Review the moves played in a record
- Download .sgf files containing records

The intended user is of course me, but I wanted to add a proper auth flow and support multiple users so that anyone who wants to use it, can.

### The stack

I built the reference implementation with the stack I'm most familiar with:

- [**Web client**](https://github.com/go-recordkeeper/goban-vue): [TypeScript](https://www.typescriptlang.org/) + [Vue](https://vuejs.org/), styled with [Tailwind CSS](https://tailwindcss.com/)
  
  Normally I would use [Vuetify](https://vuetifyjs.com/en/) for styling, but when I started the project [Vuetify v3](https://vuetifyjs.com/en/introduction/roadmap/#v30-titan) was still unreleased and I wanted to try Vue 3.

- [**Server**](https://github.com/go-recordkeeper/goban-server-django): [Django](https://www.djangoproject.com/) + [Django REST framework (DRF)](https://www.django-rest-framework.org/)

  Django is designed to serve static pages with HTML templates, but it is also a swiss army knife of handy features: a nice admin page, a powerful ORM, and a great DB schema migration system. DRF tacks on the ability to build REST APIs on top of Django's substrate of tools.

- **Database**: [PostgreSQL](https://www.postgresql.org/)

  I honestly could have just used something like [SQLite](https://www.sqlite.org/index.html) for the scale of usage I expect, but PostgreSQL is more "realistic" and not that much harder to support. Also, if I ever want to deploy multiple servers simultaneously, I can simply plug them all in to the same PostgreSQL DB.

### Deployment

To deploy the application, I use [Docker](https://www.docker.com/) and [Docker compose](https://docs.docker.com/compose/). I use an [nginx](https://nginx.org/en/) container to deal with SSL termination and serving static files. Everything deployment related is kept in the [goban-deploy](https://github.com/go-recordkeeper/goban-deploy) repository to make deployments easily reproducible.

The application is physically being hosted on a [Raspberry Pi](https://www.raspberrypi.com/) sitting on a shelf in my living room. There's a bit of lag, but nothing too awful considering the theoretical number of maximum concurrent users is one.

## Dirty details

Now that we've summarized the basics, lets get more technical.

### The web client

Most of the front end is built in Vue, but just for fun I built the actual go board (in Japanese, "[goban](https://en.wikipedia.org/wiki/Go_equipment#Board)", hence the repository names) in [pure JS](https://github.com/go-recordkeeper/goban-js). Instead of fighting with CSS styles to render a nice looking board, I thought it would be easier to use an [HTML &lt;canvas&gt; element](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API). This also has the benefit of being a separate repository, so if I ever decide to build alternative frontends, I can recycle the board rendering code.

As previously mentioned, I spent an [innovation token](https://mcfunley.com/choose-boring-technology) on using Tailwind CSS to style the web client. Vuetify is more in my comfort zone, but mechanically they are not so different. Both allow fine grained control of CSS styles by adding CSS classes to elements. The big difference is that Vuetify provides a library of Vue components with an opinionated aesthetic, while Tailwind CSS only provides CSS classes (albeit more powerful ones).

Tailwind CSS felt less abstract and more powerful than Vuetify. Given that I am not a designer, I prefer less powerful, more opinionated solutions, but Tailwind CSS did work well.

I used [vite](https://vitejs.dev/) instead of [webpack](https://webpack.js.org/) for dev tooling, and I must say it was much nicer out of the box. I haven't had to dive in to deep configuration options yet so I cannot speak to the its flexibility, but the fact that I haven't had to do that yet is an endorsement in and of itself.

I opted not to use a state store like [Pinia](https://pinia.vuejs.org/) because the app is so simple. The only global state is the currently logged-in user. For simple applications, I have found that the [reactive refs](https://vuejs.org/api/reactivity-core.html) introduced in Vue 3's Composition API provide everything I need for sharing state reactively. Exposing the current user was as simple as:

```js
// client/index.ts

class Client {
    // ... 
    async login(username: string, password: string): Promise<APIResponse<User, UserAuthError>> {
        // ... send the login request ...
        user.value = await this.getCurrentUser();
        // ...
    }
    async logout() {
        // ...
        user.value = null;
    }
}

let user: Ref<User | null> = ref(null);
export { user };
```

Now any component or module that needs to reference the current user can simply import that ref from `'@client'` and it will be updated reactively whenever the user logs in or logs out:

```js
import { user } from '@/client';
```

The only other interesting thing I did with the web client was how I handled API errors around submitting forms. If a user submitted, say, a login request with an empty username input, I wanted the form validation error to pop up next to the username field. In TypeScript, it is impossible to type exceptions, which makes them much harder to work with safely. I probably could have figured something out, but it wouldn't have felt good.

Instead, I implemented a type `APIResponse` that works something like Rust's [Result](https://doc.rust-lang.org/std/result/index.html) or Haskell's [Either](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Either.html). Now instead of throwing exceptions on 400 errors, the API client returns the error. Any code that calls one of the relevant methods must check if the value returned was valid or contains the validation errors and handle each case accordingly. This was also somewhat against the grain of what TypeScript was designed to do, but I really like that pattern and I think the code that uses it turned out nicer, even if the implementation of `APIResponse` is somewhat clumsy.

### Authentication

To support multiple users, I created `/login`, `/register`, and `/user` API endpoints to login, register, and get the currently logged in user, respectively. `/login` takes a JSON object containing a username and password and, if the credentials are valid, returns a [JWT](https://jwt.io/). The JWT must be included in the `Authorization` header to most requests to the API; the only unprotected endpoints are `/login` and `/register`.

An OAuth 2.0 flow with PKCE is the industry standard way of supporting logins, but frankly it's quite complex and tedious and implementing it repeatedly didn't sound very fun.

### The server

There's nothing particularly remarkable about the server implementation. The API has the aforementioned authentication endpoints and some fairly generic CRUD endpoints for Records and Moves, which are the only entities in the domain model. I did have to write some interesting logic to determine when a move captured a group of stones, and I did use [sgfmill](https://github.com/mattheww/sgfmill) to generate the exported .sgf files, but that's about it for novel stuff in the server.

I decided to use something familiar and reliable for the first implementation specifically to make managing the database schema easier. Django has a good ORM, a great schema migration system, and it includes a default `User` model by default. When I implement new servers, I plan on using the Django implementation to set up the database tables and to deal with any migrations.

### Deployment

Since I'm deploying the application on my own hardware (specifically a Raspberry Pi), I wanted it to be as automated as possible to make updates and redeploys smoother. Using docker-compose means that I can keep everything deployment related in [one repository](https://github.com/go-recordkeeper/goban-deploy), and installing on the server is as trivial as possible.

When setting up the server, I put a fresh installation of [Manjaro](https://manjaro.org/) onto an unused Raspberry Pi, installed `git` so I could pull the `goban-deploy` repository, installed `docker` and `docker-compose` so I could run it, and ran `docker-compose up -d`. It was pretty much that simple.

The docker-compose configuration took some fiddling, mostly because of HTTPS support. I've generated my own [Let's Encrypt certificates](https://letsencrypt.org/) before by hand, but that sucked and I didn't want to have an annual process to renew them. I found a cool container called [`nginx-proxy`](https://github.com/nginx-proxy/nginx-proxy) that automatically generates nginx configuration files to proxy other containers in the docker-compose environment, and also automatically provision SSL certificates. It probably took me more time to configure than just writing my own nginx configuration, but it's still a pretty slick tool.

I did have some issues building the static content so that it can be served through nginx. My initial plan was to have [goban-deploy](https://github.com/go-recordkeeper/goban-deploy) contain only configuration files and run the build on the Raspberry Pi using containers to avoid having to install anything natively, but installing node+npm+vue+vite and running a build was so glacially slow that that wasn't an option. Instead, I run the builds on my laptop and commit the artifacts back into the goban-deploy repo, which is then pulled and served from the Raspberry Pi. There's probably a better way, but it's not a large website so it's only ~150 KB.

Going forward, I'd like to set up some scheduled database dumps as backups (even though the data is not very important and I'm backing it up manually), and set up a systemd service to start/restart the service automatically if my power ever goes out.
