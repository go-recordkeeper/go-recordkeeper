import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";

register("GET", "/api/records/", async (request) => {
  const userId = await getUserId(request);
  const url = new URL(request.url);
  const pageSize = Number.parseInt(
    url.searchParams.get("page_size") || "10",
    10,
  );
  const page = Number.parseInt(url.searchParams.get("page") || "1", 10);
  if (pageSize < 1) {
    return new Response("Invalid page size", { status: 404 });
  }
  const count = Number.parseInt(
    (await sql`SELECT COUNT(*) FROM record_record WHERE owner_id=${userId}`)[0]
      .count,
    10,
  );
  const pages = Math.floor((Math.max(count, 1) + pageSize - 1) / pageSize);
  if (page < 1 || page > pages) {
    return new Response("Invalid page number", { status: 404 });
  }
  const limit = pageSize;
  const offset = (page - 1) * pageSize;
  const records =
    await sql`SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id=${userId} ORDER BY CREATED DESC LIMIT ${limit} OFFSET ${offset}`;
  return new Response(
    JSON.stringify({
      count,
      pages,
      results: records.map((record) => ({
        ...record,
        id: Number.parseInt(record.id),
        owner: userId,
      })),
    }),
  );
});
