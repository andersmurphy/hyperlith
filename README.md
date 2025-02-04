# Hyperlith: the hypermedia based monolith

This is a small and very opinionated fullstack Datastar framework mostly
for my personal use. However, I felt there were some ideas in here that were worth sharing.

**WARNING:**  API can change at any time! Use at your own risk.

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

This means you actually end up doing more work with a homogeneous event system under heavy load than with this simple homogeneous event system that's throttled (especially it there's any sort of common/shared view between users).

#### Why no diffing?

In theory you can optimise network and remove the need for idiomorph if you do diffing between the last view and the current view. However, in practice because the SSE stream is being compressed for the duration of a connection and html compresses really well you get amazing compression (reduction in size by 10-20x!) over a series of view re-renders. The compression is so good that in my experience it's more network efficient and more performant that fine grained updates with diffing (without any of the additional  complexity).

This approach avoids the additional challenges of view and session maintenance (increased server load and memory usage).

My suspicion is websocket approaches in this space like Phoenix Liveview haven't stumbled across this because you don't get compression out of the box with websockets, and idiomorph is a relatively new invention. Intuitively you would think the diffing approach would be more performant so you wouldn't even consider this approach.

#### CQRS

- Actions modify the database and return a 204.
- Render functions re-render when the database changes and send an update down the `/updates` SSE connection.

## Other Radical choices

#### No CORS

By hosting all assets on the same origin we avoid the need for CORS. This avoids additional server round trips and helps reduce latency.

#### Cookie based sessions

Hyperlith uses a simple unguessable random uid for managing sessions. This should be used to look up further auth/permission information in the database.

#### CSRF

Double submit cookie pattern is used for CSRF.

#### Rendering an initial shim

Rather than returning the whole page on initial render and having two render paths, one for initial render and one for subsequent rendering a shell is rendered and then populated when the page connects to the '/updates' endpoint for that page. This has a few advantages:

- The page will only render dynamic content if the user has javascript and first party cookies enabled.

- The initial shell page can generated and compressed once.

- The server only does more work for actual users and less work for link preview crawlers and other bots (that don't support javascript or cookies).

#### Routing

Router is a simple map, this means path parameters are not supported use query parameters or body instead. I've found over time that path parameters force you to adopt an arbitrary hierarchy that is often wrong (and place oriented programming) . Removing them avoids this and means routing can be simplified to a map and have better performance than a more traditional adaptive radix tree router.

#### Reverse proxy

Hyperlith is designed to be deployed between a reverse proxy like caddy for handling HTTP2 (you want to be using HTTP2 with SSE).

#### Minimal middleware 

Hyperlith doesn't expose middleware and keeps the internal middleware to a minimum.

#### Minimal dependencies

Hyperlith tries to keep dependencies to a minimum. 
