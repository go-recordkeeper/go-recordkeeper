import { register } from "/router.ts";

register(
  "POST",
  "/api/login/",
  (request, params) => {
    console.log("fooo", params);
    return new Response("aaa", { status: 200 });
  },
);
