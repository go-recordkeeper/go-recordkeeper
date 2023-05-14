import { djwt } from "/deps.ts";

const buf2b64 = (buffer: ArrayBuffer) =>
  self.btoa(
    Array.prototype.map.call(
      new Uint8Array(buffer),
      (x) => String.fromCharCode(x),
    ).join(""),
  );

/**
 * Generates a salt by converting some random data to base64 so it is easily representable as a string.
 */
export async function generateSalt() {
  const saltkey = await crypto.subtle.generateKey(
    // The 2^6 = 64, so there are 6 bits per base64 character
    // We want a salt of length 16, so 16 * 6 bits
    { name: "HMAC", hash: "SHA-256", length: 16 * 6 },
    true,
    [
      "sign",
      "verify",
    ],
  );
  return buf2b64(await crypto.subtle.exportKey("raw", saltkey));
}

export async function hashPassword(password: string, salt: string) {
  const passkey: CryptoKey = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(password),
    { name: "PBKDF2" },
    false,
    ["deriveKey", "deriveBits"],
  );
  const hashedkey = await crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt: new TextEncoder().encode(salt),
      iterations: 390000,
      hash: "SHA-256",
    },
    passkey,
    { name: "HMAC", hash: "SHA-256", length: 32 * 8 },
    true,
    ["sign", "verify"],
  );
  return buf2b64(await crypto.subtle.exportKey("raw", hashedkey));
}

export async function createJwt(id: number) {
  const secret_key = Deno.env.get("GOBAN_SECRET_KEY") ||
    "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj";
  const jwtkey = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(secret_key),
    { name: "HMAC", hash: "SHA-512" },
    true,
    ["sign", "verify"],
  );
  const jwt = await djwt.create(
    { alg: "HS512" },
    {
      aud: "go-recordkeeper",
      exp: djwt.getNumericDate(24 * 60 * 60),
      iat: djwt.getNumericDate(new Date()),
      iss: "go-recordkeeper",
      sub: id.toString(),
    },
    jwtkey,
  );
  return jwt;
}

async function verifyJwt(token: string) {
  const secret_key = Deno.env.get("GOBAN_SECRET_KEY") ||
    "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj";
  const jwtkey = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(secret_key),
    { name: "HMAC", hash: "SHA-512" },
    true,
    ["sign", "verify"],
  );
  const { sub } = await djwt.verify(token, jwtkey, {
    audience: "go-recordkeeper",
  });
  return sub;
}

export async function getUserId(request: Request) {
  const header = request.headers.get("Authorization");
  if (!header || !header.startsWith("Bearer ")) {
    throw new Response("Failed to authenticate", { status: 403 });
  }
  const token = header.replace("Bearer ", "");
  const sub = await verifyJwt(token);
  if (!sub) {
    throw new Response("Failed to authenticate", { status: 403 });
  }
  return parseInt(sub, 10);
}
