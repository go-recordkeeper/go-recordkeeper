import { register } from "/router.ts";

register(
  "POST",
  "/api/login/",
  async (request, params) => {
    console.log("fooo", await request.json());
    return new Response("aaa", { status: 200 });
  },
);
