# Hyperlith: the hypermedia based monolith

This is a small and very opinionated fullstack [Datastar](https://data-star.dev/) framework mostly
for my personal use. However, I felt there were some ideas in here that were worth sharing.

Hyperlith only uses a subset of Datastar's feartures. If you want a production ready full featured Datastar Clojure SDK use [the official SDK](https://github.com/starfederation/datastar/tree/main/sdk/clojure). 

>⚠️ **WARNING:**  API can change at any time! Use at your own risk. 

## Goals / Priorities

- **Opinionated** - does not try to be a general solution.
- **Server driven** - push based multiplayer and streaming updates from day 1.
- **Production REPL first** - a running system should be simple to: introspect,  modify and patch.
- **Minimal dependencies** - ensures the project is robust and secure long term.
- **Operationally Simple** - handles all operational tasks in process.
- **Sovereign** - can be deployed on any VPS provider.

## F.A.Q

**Q:** *Why do I get 403 when running the examples locally in Chrome or Safari?*

**A:** *The session and csrf cookies use `Secure` this means that these cookies won't be set on localhost when using chrome or safari (as they require HTTPS) . If you want to use Chrome or Safari for local development you can run `caddy run` to start a local https proxy on https://localhost:3030. The local `caddyfile` can be [found here](https://github.com/andersmurphy/hyperlith/blob/master/Caddyfile). The goal is for development to be as close to production as possible, and Hyperlith is designed to be run behind a reverse proxy.*

## Rational (more like a collection of opinions)

#### Why large/fat/main morphs (immediate mode)?

By only using `data: mergeMode morph` and always targeting the `main` element of the document the API can be massively simplified. This avoids having the explosion of endpoints you get with HTMX and makes reasoning about your app much simpler.

#### Why have single render function per page?

By having a single render function per page you can simplify the reasoning about your app to `view = f(state)`. You can then reason about your pushed updates as a continuous signal rather than discrete event stream. The benefit of this is you don't have to handle missed events, disconnects and reconnects. When the state changes on the server you push down the latest view, not the delta between views. On the client idiomorph can translate that into fine grained dom updates.

#### Why re-render on any database change?

When your events are not homogeneous, you can't miss events, so you cannot throttle your events without losing data.

But, wait! Won't that mean every change will cause all users to re-render? Yes, but at a maximum rate determined by the throttle. This, might sound scary at first but in practice:

- The more shared views the users have the more likely most of the connected users will have to re-render when a change happen.

- The more events that are happening the more likely most users will have to re-render.

This means you actually end up doing more work with a non homogeneous event system under heavy load than with this simple homogeneous event system that's throttled (especially it there's any sort of common/shared view between users).

#### Why no diffing?

In theory you can optimise network and remove the need for idiomorph if you do diffing between the last view and the current view. However, in practice because the SSE stream is being compressed for the duration of a connection and html compresses really well you get amazing compression (reduction in size by 90-100x! Sometimes more) over a series of view re-renders. The compression is so good that in my experience it's more network efficient and more performant that fine grained updates with diffing (without any of the additional  complexity).

This approach avoids the additional challenges of view and session maintenance (increased server load and memory usage).

My suspicion is websocket approaches in this space like Phoenix Liveview haven't stumbled across this because you don't get compression out of the box with websockets, and idiomorph is a relatively new invention. Intuitively you would think the diffing approach would be more performant so you wouldn't even consider this approach.

#### Signals are only for ephemeral client side state

Signals should only be used for ephemeral client side state. Things like: the current value of a text input, whether a popover is visible, current csrf token, input validation errors. Signals can be controlled on the client via expressions, or from the backend via `merge-signals`.

#### Signals in fragments should be declared __ifmissing

Because signals are only being used to represent ephemeral client state that means they can only be initialised by fragments and they can only be changed via expressions on the client or from the server via `merge-signals` in an action. Signals in fragments should be declared `__ifmissing` unless they are "view only".

#### View only signals

View only signals, are signals that can only be changed by the server. These should not be declared `__ifmissing` instead they should be made "local" by starting their key with an `_` this prevents the client from sending them up to the server.

#### Actions should not update the view themselves directly

Actions should not update the view via merge fragments. This is because the changes they make would get overwritten on the next `render-fn` that pushes a new view down the updates SSE connection. However, they can still be used to update signals as those won't be changed by fragment merges. This allows you to do things like validation on the server.

#### Stateless

The only way for actions to affect the view returned by the `render-fn` running in a connection is via the database. The ensures CQRS. This means there is no connection state that needs to be persisted or maintained (so missed events and shutdowns/deploys will not lead to lost state). Even when you are running in a single process there is no way for an action (command) to communicate with/affect a view render (query) without going through the database.

#### CQRS

- Actions modify the database and return a 204 or a 200 if they `merge-signals`.
- Render functions re-render when the database changes and send an update down the updates SSE connection.

#### Work sharing (caching)

Work sharing is the term I'm using for sharing renders between connected users. This can be useful when a lot of connected users share the same view. For example a leader board, game board, presence indicator etc. It ensures the work (eg: query and html generation) for that view is only done once regardless of the number of connected users. 

There's a lot of ways you can do this. I've settled on a simple cache that gets invalidate when a `:refresh-event` is fired. This means the cache is invalidated at most every X msec (determined by `:max-refresh-ms`) and only if the db state has changed.

To add something to the cache wrap the function in the `cache` higher order function.

#### Batching

Batching pairs really well with CQRS as you have a resolution window, this defines the maximum frequency the view can update, or in other terms the granularity/resolution of the view. Batching can generally be used to improve throughput by batching changes. 

However, there is one downsides with batching to keep in mind and that is you don't get atomic transactions. The transaction move to the batch level, not the transact/insert level. Transaction matter when you are dealing with constraints you want to deal with at the database level, classic example is accounting systems or ledgers where you want to be able to fail an atomic transaction that violates a constraint (like user balance going negative). The problem with batching is that that transaction constraint failure, fails the whole batch not only the transact that was culpable.

Some systems don't need manual batching. For example [datalevin](https://github.com/juji-io/datalevin) (my go to database with Hyperlith) recently added it's [own batching mechanism](https://github.com/juji-io/datalevin/tree/master/benchmarks/write-bench) when you use `transact-async` this gives you really good write performance (it does dynamic batching) and atomic transaction failures. This gives you the best of both worlds: fast writes and atomic transactions.

If you are using a database like sqlite, don't require fine grained transaction and want better write performance I'd recommend batching as it's trivial to implement in a system that has a resolution window.

#### Use `data-on-mousedown` over  `data-on-click`

This is a small one but can make even the slowest of networks feel much snappier.

## Other Radical choices

#### No CORS

By hosting all assets on the same origin we avoid the need for CORS. This avoids additional server round trips and helps reduce latency.

#### Cookie based sessions

Hyperlith uses a simple unguessable random uid for managing sessions. This should be used to look up further auth/permission information in the database.

#### CSRF

Double submit cookie pattern is used for CSRF.

#### Rendering an initial shim

Rather than returning the whole page on initial render and having two render paths, one for initial render and one for subsequent rendering a shell is rendered and then populated when the page connects to the updates endpoint for that page. This has a few advantages:

- The page will only render dynamic content if the user has javascript and first party cookies enabled.

- The initial shell page can generated and compressed once.

- The server only does more work for actual users and less work for link preview crawlers and other bots (that don't support javascript or cookies).

#### Routing

Router is a simple map, this means path parameters are not supported use query parameters or body instead. I've found over time that path parameters force you to adopt an arbitrary hierarchy that is often wrong (and place oriented programming). Removing them avoids this and means routing can be simplified to a map and have better performance than a more traditional adaptive radix tree router.

>📝 Note: The Hyperlith router is completely optional and you can swap it out for reitit if you want to support path params. 

#### Reverse proxy

Hyperlith is designed to be deployed between a reverse proxy like caddy for handling HTTP2/3 (you want to be using HTTP2/3 with SSE).

#### Minimal middleware 

Hyperlith doesn't expose middleware and keeps the internal middleware to a minimum.

#### Minimal dependencies

Hyperlith tries to keep dependencies to a minimum. 
