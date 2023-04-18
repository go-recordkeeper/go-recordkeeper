/* eslint-env node */
require("@rushstack/eslint-patch/modern-module-resolution");

module.exports = {
  root: true,
  extends: [
    "plugin:vue/vue3-essential",
    "eslint:recommended",
    "@vue/eslint-config-typescript/recommended",
    "@vue/eslint-config-prettier",
  ],
  rules: {
    // vue/no-unused-vars works correctly with vue components, thankfully
    "@typescript-eslint/no-unused-vars": "off",
    // "Goban" should not be "GoBan"
    "vue/multi-word-component-names": "off",
  },
  parser: "vue-eslint-parser",
  parserOptions: {
    ecmaVersion: "latest",
  },
};
