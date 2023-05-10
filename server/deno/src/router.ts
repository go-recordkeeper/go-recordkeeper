const handlers: [
  string,
  RegExp,
  (event: Request, params: { [key: string]: string }) => Response,
][] = [];

export function register(
  method: string,
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
  handlers.push([method, regex, handler]);
}

export function handle(event: Deno.RequestEvent) {
  for (const [method, regex, handler] of handlers) {
    if (event.request.method != method) {
      continue;
    }
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
