---
title: "Deno"
date: 2023-05-25T15:09:49-04:00
type: post
slug: "28-deno"
tags:
  - deno
  - typescript
showTableOfContents: true
---

For the fifth implementation, I have chosen to use [Deno](https://deno.com/runtime), a JavaScript runtime and competitor to [Node.js](https://nodejs.org/en). I've been avoiding a JavaScript/TypeScript implementation because I frankly don't like JavaScript or Node very much, so when I was reminded of Deno I thought I'd give it a shot.

## Thoughts on Deno
Overall, I quite enjoyed the UX. Deno supports TypeScript out of the box with no headache configuring Babel or webpack or what have you. Deno has an opinionated linter/formatter, so no wrangling ESLint or prettier. The package file, `deno.jsonc`, is incredibly terse:

```json
{
  "tasks": {
    "dev": "deno run --watch --allow-net --allow-env main.ts",
    "test": "deno test --allow-env --coverage=coverage && deno coverage --lcov --output=coverage/lcov.info coverage/"
  }
  // Map "/" to "./src/" for imports
  "imports": {
    "/": "./src/",
    "./": "./"
  }
}
```

It consists solely of some build scripts that make development easier, and some import mapping to make relative imports slightly less verbose.

Most interestingly, Deno doesn't have a package manager. Dependencies are specified in the import statement via URL:

```js
import { default as postgres } from "https://deno.land/x/postgresjs@v3.3.4/mod.js";
```

Anyone can host a blob of code anywhere and import it with Deno. There's still a central repository of Deno specific packages (`deno.land`, as per the example above), but it is optional. Deno caches these dependencies locally and manages a lockfile for you. To avoid respecifying dependencies everywhere, the standard methodology is to have a central `deps.ts` file where you import all of your dependencies, export them from there, and import them from `deps.ts` as needed. It's a neat concept, and it worked well for me.

### No web framework
Deno has a remarkably complete standard library for web development. Ultimately, my dependencies are:

* [postgresjs](https://deno.land/x/postgresjs@v3.3.4) - The DB engine
* [fun](https://deno.land/x/fun@v.2.0.0-alpha.11) - Used exclusively for the [json_schema](https://doc.deno.land/https://raw.githubusercontent.com/baetheus/fun/main/json_schema.ts) module, which I use for generating schemas for request body validation.
* [ajv](https://ajv.js.org/) - The JSON schema validator
* [djwt](https://deno.land/x/djwt@v2.8) - JWT library

Notable absences from that list include a crypto library and a web framework. Deno has builtin support for all the password hashing I needed to do from the [WebCrypto API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API). After poking around [ExpressJS](http://expressjs.com/), I decided it would be simpler and more informative to just use the tools Deno has out of the box to build my own rudimentary framework. This is the entirety of the boilerplate for handling incoming connections:
```typescript
// main.ts
import { handle } from "/router.ts";

// Import endpoint files to register all endpoints.
import "/auth.ts";
import "/record.ts";

export default async function main() {
  // Start listening on port 8000 of 0.0.0.0
  const server = Deno.listen({ port: 8000 });
  console.log(`HTTP webserver running.  Access it at:  http://localhost:8000/`);

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
}
```

And this is the entirety of my router code that dispatches requests to the correct handler. It also parses out URL parameters like `/record/{id}/undo/` and does rudimentary validation error handling:
```typescript
// router.ts
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
  console.debug("Handling", event.request.method, event.request.url);
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
  console.log("404", event.request.url);
  await event.respondWith(
    new Response("Not found", {
      status: 404,
    }),
  );
}
```

There are some more utility functions for validation and authentication that would be implemented as middleware in a normal web framework, but the fundamentals are all there in less than 100 lines of code. That's pretty cool.

### SQL
[postgresjs](https://deno.land/x/postgresjs@v3.3.4) has an interesting API for executing sql queries that uses [tagged templates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates). A simple query would look like this:
```
const rows = await sql`SELECT board_size FROM record_record WHERE id=${recordId}`;
```
The `sql` is actually a function that is applied with the template string and template parameters, which handles input sanitization and dispatching the query. I'd never heard of this syntax feature of JavaScript, but it works remarkably well for this use case.

### Request body validation
When handling a request with a JSON body, it's nice to verify that all required fields are present and typed correctly before doing anything else. Sadly, despite using TypeScript and everything in JavaScript basically being a JSON blob, there is no nice way to do this natively. TypeScript is entirely transpile time and cannot be enforced at runtime, and [JSON Schema](http://json-schema.org/) is annoying to write and work with.

I ended up using [json_schema](https://doc.deno.land/https://raw.githubusercontent.com/baetheus/fun/main/json_schema.ts) to generate a JSON schema in a legible way, then use [ajv](https://ajv.js.org/) to validate it.

```typescript
// router.ts
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

// inside a request handler
const PlayRequest = J.struct({
  x: J.number(),
  y: J.number(),
});
const body = validator(PlayRequest);
// ...
const { x, y } = await body(request);
```

It doesn't do any reporting to the client of what failed to validate because I didn't include that in the API specification, and also because I am lazy. The reward for my laziness is a surprisingly terse and usable function that correctly and tersely rejects all malformed requests.

### The `play` endpoint
Now that I've described all the moving parts, let's take a look at how they work in practice. The `play` endpoint is traditionally pretty complex, since it needs to look up a record and all the moves for that record, play them out, handle errors, insert a new row, and return a JSON blob. Nothing out of the ordinary, but a little bit of everything.

```typescript
// imports, nothing very interesting here.
import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { getUserId } from "/auth/util.ts";
import { Board, Color, Coord, deserializeMoves, GoError } from "/go.ts";

// request body validator
const PlayRequest = J.struct({
  x: J.number(),
  y: J.number(),
});
const body = validator(PlayRequest);
```

`body` is a function that will throw a ValidationError if the request body doesn't match the schema, and will return the object if it does match. Crucially, the returned object is typed correctly, in this case, `{x: number, y: number}`.

```typescript
register(
  "POST",
  "/api/records/{recordId}/play/",
  async (request, { recordId }) => {
    const userId = await getUserId(request);
    const { x, y } = await body(request);

    ...
  },
);
```

This is the syntax I chose for registering routes. The `{recordId}` portion of the request URL is parsed out and passed to the closure as the variable `recordId`.

The first step of the request handler is to verify that the user is logged in. The `getUserId(request)` function checks the `Authorization` header on the request extracts the user ID from the JWT payload, if the JWT is valid.

The next step is to verify and extract the content of the request body using the previously created `body` function.

```typescript
    const records =
      await sql`SELECT board_size, handicap FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    if (records.length != 1) {
      return new Response("Not Found", { status: 404 });
    }
    const { board_size, handicap } = records[0];
    const moves =
      await sql`SELECT position, color FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
```

Now we do some SQL queries to get the record and the moves for that record from the DB. I opted to do this clumsy `if` check to return 404 if the record doesn't exist. It would be nice to abstract this somehow, but I couldn't think of a good way to do it. Having it be extremely explicit has its advantages, I suppose.

```typescript
    let color: Color;
    if (moves.length == 0 || moves.length < handicap) {
      // First move is always black
      // If we are still in the handicap phase, next move is black
      color = Color.Black;
    } else if (moves[moves.length - 1].color == "W") {
      // If the last move was white, the next move is black
      color = Color.Black;
    } else {
      color = Color.White;
    }
    const board = new Board(board_size);
    let captures: Coord[];
    try {
      board.playMoves(deserializeMoves(board, moves));
      captures = board.playMove([[x, y], color]);
    } catch (e) {
      if (e instanceof GoError) {
        return new Response(e.message, { status: 403 });
      }
      throw e;
    }
    const move_number = moves.length + 1;
    const pos = board.toPos([x, y]);
    await sql`INSERT INTO record_move
      (record_id, position, color, move)
      VALUES
      (${recordId}, ${pos}, ${Color.toString(color)}, ${move_number})`;
```

This is a big blob of logic specific to how moves are played, which isn't Deno specific and also isn't very interesting. We need to make sure the next move is the correct color, accounting for handicaps. We need to play out the moves from the database, then play the latest move and get any captures from it, handling any exceptions encountered from illegal moves. Finally, we need to insert the new move into the database.

```typescript
    return new Response(
      JSON.stringify({
        add: [{ x, y, color: Color.toString(color) }],
        remove: captures.map(([x, y]) => ({ x, y })),
      }),
      {
        status: 201,
      },
    );
  },
);
```
Finally, we return a JSON blob, after some type corrections.

Honestly, this is one of the more succinct and legible `play` implementations I've done. I'm rather pleased with it.

## Speed bumps
As always, some snags were encountered along the way.

### Password hashing
Deno includes the [WebCrypto API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API) out of the box, so I thought I would just use that to do all the hashing myself. Deciphering the API was difficult, and examples for what I wanted to achieve were thin. I couldn't find a better alternative though, so I soldiered on until it worked.

### Haskell JWT skew
For some unfathomable reason, the Haskell JWT library works fine with every other API implementation, except for Deno. JWTs signed by the Deno API and then evaluated by the Haskell API would consistently appear to be showing up before they claimed to be signed, which is obviously grounds for rejection. The two APIs were running on two different docker containers, but I have no idea why their system clocks wouldn't be synchronized.

The solution was to add a bit of allowed clock skew to the Haskell implementation to add some fudge factor for the offset. This was painful, as I needed to decrypt the dubiously documented `lens` library once again to figure out how to change JWT validation settings. Can't say I enjoy `lens` very much.

### FastAPI bug
I noticed that there was no integration testing coverage for undoing a pass, so I added a test and lo and behold, FastAPI didn't pass. It was an easy fix, but not great for my ego. I'm sure there are other forgotten bugs which have never been uncovered my solitary QA.

### Speeding up integration tests in CI
As I continue to stack up implementations, CI time continues to multiply. Specifically, every CI run needed to build every implementation into a fresh Docker image so that it could be tested against. This is especially problematic for Haskell, which takes ~20 minutes to build in CI.

I had looked into caching docker builds before, but came up empty. This time, I found the `gha` cache target in Docker buildx. It's labeled "experimental", so it's possible that it didn't exist last time I checked. It's also possible I just didn't want to bother figuring out how to enable buildx (which did prove difficult). I really wanted to enable build caching from docker compose somehow, but I wasn't able to crack that nut. I'm confident it is possible, but an easier, less elegant solution is acceptable.

Excerpt from my Haskell GitHub actions workflows:
```yml
      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2
      - name: Build and cache haskell
        uses: docker/build-push-action@v4
        with:
          context: server/haskell
          push: false
          load: true
          cache-from: type=gha,scope=haskell
          cache-to: type=gha,scope=haskell,mode=min
      # More "Build and cache ..." steps, one for each implementation
      # ...
      - name: Build all docker compose images
        run: docker compose build
      - name: Run tests
        shell: bash
        run: poetry run test -- -k "haskell"
```

### Deno on the Pi
After finishing the implementation, I was quite disappointed to learn that Deno is [officially unsupported](https://github.com/denoland/deno/issues/2295) on ARM processors. Apparently the Deno project exclusively uses GitHub Actions for releases, and GitHub Actions doesn't have good enough support for ARM architectures, so they just, don't do that? Fortunately, it still technically works, and since it's written in Rust (yay Rust) it isn't particularly hard to build yourself.

Using the install script from [Luke Channings](https://github.com/LukeChannings/deno-arm64/tree/main), I built a docker image that would install deno on both x86 and ARM processors:
```dockerfile
FROM debian:buster-slim as install

RUN apt-get update -y && apt-get upgrade -y
RUN apt-get install -y unzip curl
COPY ./deno_install.sh /opt/deno/deno_install.sh
RUN sh /opt/deno/deno_install.sh

FROM debian:buster-slim

COPY --from=install /root/.deno/bin/deno /usr/bin/deno

EXPOSE 8000
WORKDIR /opt/deno

# Cache the dependencies
COPY ./src/deps.ts /opt/deno/src/deps.ts
RUN deno cache src/deps.ts

COPY deno.* /opt/deno/
COPY ./src /opt/deno/src
COPY main.ts /opt/deno/main.ts

ENTRYPOINT ["/usr/bin/deno"]
CMD ["run", "--allow-net", "--allow-env", "main.ts"]
```

I'm pretty proud of that, now tests work in development and in CI and the multistage build means there's practically no overhead.

## Next steps
I'm constantly annoyed by the inability to score a game after recording it, so I think it may finally be time to invest into some serious client side logic. My TypeScript chops are all warmed up, anyway.

I'm also feeling a perverse desire to write an implementation in bash. I know for a fact it's technically possible, and it would be beneficial to know more about how to do bash scripting effectively. It's more a question of how much pain I am willing to suffer.
