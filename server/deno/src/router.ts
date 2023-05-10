const handlers: [
  RegExp,
  (event: Request, params: { [key: string]: string }) => Response,
][] = [];

export function register(
  pattern: string,
  handler: (request: Request, params: { [key: string]: string }) => Response,
) {
  // Replace "{foo}" with "(?<foo>...)" to make a valid regex.
  const regex = RegExp(
    pattern.replace(
      /{([a-zA-Z0-9]+)}/,
      (_, name) => `(?<${name}>[a-zA-Z0-9]+)`,
    ) + "$",
  );
  handlers.push([regex, handler]);
}

export function handle(event: Deno.RequestEvent) {
  console.log(event.request.url);
  for (const [regex, handler] of handlers) {
    const match = regex.exec(event.request.url);
    if (match) {
      const groups = match.groups || {};
      event.respondWith(handler(event.request, groups));
      return;
    }
  }
  // Nothing matched, 404
  event.respondWith(
    new Response("Not found", {
      status: 404,
    }),
  );
}
