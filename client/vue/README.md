# Vue Web Application 

![Vue](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/vue.yml/badge.svg)

The frontend client.

## Architecture
This is a Single Page App that uses the REST API to authenticate, do CRUD stuff with records, and play moves on the board.

The layout is pretty standard. `components` contains reusable UI elements, while `views` contains "pages". I use [Vue Router](https://router.vuejs.org/) for managing the different views and URLs. I opted not to use a data store like [Pinia](https://pinia.vuejs.org/) because there is practically no global app state to manage.

I decided to render the game board itself using an HTML canvas instead of using Vue components and a lot of CSS. Because that wasn't Vue-specific at all, I broke that out into a [separate package](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/client/board).

## Development
### Recommended IDE Setup

[VSCode](https://code.visualstudio.com/) + [Volar](https://marketplace.visualstudio.com/items?itemName=Vue.volar) (and disable Vetur) + [TypeScript Vue Plugin (Volar)](https://marketplace.visualstudio.com/items?itemName=Vue.vscode-typescript-vue-plugin).

### Project Setup

```sh
npm install
```

#### Compile and Hot-Reload for Development

```sh
npm run dev
```

#### Lint with [ESLint](https://eslint.org/)

```sh
npm run lint
```

#### Format code with Prettier

```sh
npx prettier --write .
```
