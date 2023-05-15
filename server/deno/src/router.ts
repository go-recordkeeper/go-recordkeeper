import { ajv, json_schema as J } from "/deps.ts";

class ValidationError extends Error {}

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
    ) + "(\\\?.*)?$",
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
        if (e instanceof ValidationError) {
          await event.respondWith(
            new Response("Validation Error", { status: 400 }),
          );
        } else if (e instanceof Response) {
          await event.respondWith(e);
        } else {
          await event.respondWith(
            new Response("Internal Server Error", { status: 500 }),
          );
        }
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

export function validator<T>(schema: J.JsonBuilder<T>) {
  const validate = ajv.compile(J.print(schema));
  return async (request: Request) => {
    const json: J.TypeOf<typeof schema> = await request.json();
    if (!validate(json)) {
      console.log(validate.errors);
      throw new ValidationError();
    }
    return json;
  };
}
