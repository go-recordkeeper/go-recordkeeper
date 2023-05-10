const handlers: [
  string,
  RegExp,
  (event: Request, params: { [key: string]: string }) => Promise<Response>,
][] = [];

export function register(
  method: string,
  pattern: string,
  handler: (
    request: Request,
    params: { [key: string]: string },
  ) => Promise<Response>,
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

export async function handle(event: Deno.RequestEvent) {
  for (const [method, regex, handler] of handlers) {
    if (event.request.method != method) {
      continue;
    }
    const match = regex.exec(event.request.url);
    if (match) {
      const groups = match.groups || {};
      try {
        const response = await handler(event.request, groups);
        await event.respondWith(response);
      } catch (e) {
        console.error(e);
        await event.respondWith(
          new Response("Internal Server Error", { status: 500 }),
        );
      }
      return;
    }
  }
  // Nothing matched, 404
  await event.respondWith(
    new Response("Not found", {
      status: 404,
    }),
  );
}
