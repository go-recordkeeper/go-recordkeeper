// export function add(a: number, b: number): number {
//   return a + b;
// }
//
// // Learn more at https://deno.land/manual/examples/module_metadata#concepts
// if (import.meta.main) {
//   console.log("Add 2 + 3 =", add(2, 3));
// }

import { handle, register } from "./src/router.ts";

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

// Start listening on port 8080 of localhost.
const server = Deno.listen({ port: 8080 });
console.log(`HTTP webserver running.  Access it at:  http://localhost:8080/`);

// Connections to the server will be yielded up as an async iterable.
for await (const conn of server) {
  // In order to not be blocking, we need to handle each connection individually
  // without awaiting the function
  serveHttp(conn);
}

async function serveHttp(conn: Deno.Conn) {
  // This "upgrades" a network connection into an HTTP connection.
  const httpConn = Deno.serveHttp(conn);
  // Each request sent over the HTTP connection will be yielded as an async
  // iterator from the HTTP connection.
  for await (const event of httpConn) {
    handle(event);
  }
}
