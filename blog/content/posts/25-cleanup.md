---
title: "Cleanup"
date: 2023-04-20T14:33:42-04:00
type: post
slug: "25-cleanup"
tags:
  - django
  - vue
  - nginx
showTableOfContents: true
---

I've been leaving a mess in my wake as I've been pushing through the Haskell and Rust implementations, so it's time to pay down some technical debt. Fortunately the GitHub issue tracker has been a useful place to dump problems I didn't want to deal with in the moment, so at least I had an angle of attack.

## API Cleanup

### X/Y > Index
For a while now, I've been suffering from some data modeling gotcha's that are sort of a generally hard problem. A go board is really just a 19x19 grid, so as a human being it makes sense to use X/Y coordinates to represent different places on the board. This is legible and easy to debug, and translates nicely to `(x, y)` tuples in most languages.

Sadly, there are two very important languages that do not have tuples: JSON and Postgres SQL. JSON does have arrays, but arrays can be of arbitrary length and that opens us up to all kinds of wild serialization bugs. The correct answer is to use an object like `{"x": ..., "y": ...}`, which is wordy but serviceable. Postgres technically also has arrays, but that's an even worse idea.

To represent positions on the board in the database, I used this formula: `position = x + (boardSize * y)`. This maps every position on the board to an index in reading order, left to right, top to bottom. By happy coincidence, this also allows use to model passing (i.e. not placing a stone anywhere) by simply making the `move` column nullable; a `NULL` move is a pass. Unfortunately, the downside of this single integer position is that it's much harder to read and much less generally intuitive for determining adjacency.

When I was first writing the Django implementation, I was tragically lazy when writing the get record endpoint. Instead of creating an `{"x": ..., "y": ...}` dict like I should, I simply grabbed the `position` from the DB and passed it straight to the API response. As I finished the rest of the endpoints I used the X/Y values everywhere else, but I never got around to going back and fixing it up in that last spot.

Finally, I noticed it again while cleaning up the types for the FastAPI OpenAPI page, and decided enough was enough. Despite my fears of having to update four implementations simultaneously, it was surprisingly easy. I just updated the integration tests to reflect the values I wanted, then made the relatively small tweaks necessary to convert the indexes to X/Y objects. I've put enough work into optimizing the builds on the Raspberry Pi that the Haskell build only crashed a couple of times before it succeeded.

Big success!

### Paginating Records List
As I've been using the app, I've been gradually accreting games that have been gradually stretching the records page longer and longer. It isn't painfully slow yet, but I figured it would be good to nip that in the bud while I'm already messing around with the API.

According to the general implementation philosophy, Django is the reference implementation, so I added pagination there first. The return value of the records endpoint changed from a simple JSON list to an object like this:

```JSON
{
  "count": 18, // How many records there are total
  "pages": 2, // duh
  "results": [...] // The original response list
}
```

Additionally, the endpoint now takes two query parameters, `page` and `page_size`.

Adding pagination in Django is very easy since Django and Django REST framework both support it out of the box. Most of the work was figuring out how to add the `pages` field, since it wasn't included by default.

The rest of the implementations followed a similar, straightforward pattern:
1. Figure out how to get query parameters from the web framework
1. Add a DB query to get the total number of records (for the count field, and the page offset math)
1. Do math to figure out the start and end index in the record lists for the given `page` and `page_size`
1. Adjust the records query using `LIMIT` and `OFFSET` to only return the desired records
1. Wrap the response with the pagination info

The hardest part was, surprisingly, the pagination math. To calculate `pages`, I eventually figured out this formula (`/` is integer division):
```
(max(count, 1) + page_size - 1) / page_size
```

The basic idea is that you add an extra page of "ghost" records, then use integer division to divide by `page_size`, which rounds down and discards the unnecessary ghosts. The `- 1` is necessary to avoid having an extra empty page when the count is an exact multiple of the page size. The `max(count, 1)` makes it so that a count of `0` results in a single empty page instead of no pages.

I also got to add the pagination UI to the frontend. I'm not a huge fan of Tailwind CSS, but it's more reasonable than pretty much every other styling method I've tried, so oh well.

## General Cleanup
After that I tackled a bunch of things from the backlog, as well as stuff I just felt like doing at the time:

* Allow all CORS requests in development mode in Rust. The integration tests are so good that I never bothered actually testing the implementation with the UI locally. Something to add to the implementation checklist.
* Set `init: true` on the server services in all the `docker-compose.yml`. See the [docs](https://docs.docker.com/compose/compose-file/compose-file-v3/#init). Doing `docker compose down` always took at least 10 seconds because none of the containers were responding to the `SIGINT` signal; after 10 seconds docker compose gives up playing nice and sends `SIGQUIT`. Setting `init: true` runs the service using systemd inside the container, which handles signals better.
* Use `uvicorn` to run the Django server. Previously I was just using the development server because it didn't have any extra dependencies, but that is officially a bad practice.
* Use `prettier` to format the Vue client.
* Type check and lint the Vue client in CI.
* Clean up the login/register forms a little bit.
* Reorganize the record rows on the records page at smaller breakpoints to improve mobile UX.
* Use a popup alert to confirm record deletion.
* Homogenize documentation a bit.
* Add nifty logos to the implementation READMEs.

I knocked out most of the little things in the backlog, but there's still a few bigger ticket items, so look forward to that.
