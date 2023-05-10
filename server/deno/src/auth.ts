import { register } from "/router.ts";

// Testing the router.
register(
  "/foo/",
  (request, params) => {
    console.log("fooo", params);
    return new Response("aaa", { status: 200 });
  },
);
register("/bar/{baz}/", (request, { baz }) => {
  return new Response(baz, { status: 200 });
});
