This architecture doesn't need to be implemented but I'm feeling increasingly uncomfortable without
it planned.

* Where I want to be
  * Horizontal scalability (for both handling scale and zero-downtime deploys)
  * User management + enterprise support
    * multiple login/signup methods (oauth?)
    * mailing list ~~~ system accounts ~~~ ownership/perms wrt relation to data
    * groups/org etc for phase 3
  * Encryption
  * Fast SSR (caching or node)


CURRENT STATE
=============

* Lift -- GET RID OF IT
  * Currently using for...
    * sessions
    * comets
    * ajax
    * template injection

* Shiro -- GET RID OF IT

* Sessions currently used for
  * userId
  * ajax request handlers


TARGET STATE
============

* Remove web-server library; interface with web-server directly (most likely via the Servlet API)

* Use JWT for session management
  * only needed for login proof / userId
  * can avoid DB lookup by including user metadata (like `@golly`)
  * Java lib: https://github.com/jwtk/jjwt

* Server-side procs
  * Scala.JS makes regular ajax calls
  * auth via JWT
  * single endpoint, cmd type -> cmd -> resp

* Event pub/sub
  * Use Redis channels
  * webapp-server publishes new events to Redis
  * webapp-server subscribes and receives events, then publishes to clients through existing connections
  * webapp-server maintains connections with clients via websockets
  * send new project modification commands from client to server thru open websocket
  * webapp-server-logic to get websocket & redis abstractions
  * use websockets & redis to provide user with social real-time data such as:
    * connected users
    * local & remote uncommitted dirty fields
  * auto/manual reconnect (and catchup) on broken connection

* Project caching (for quick responses to new events & project loads)
  * Use AWS Redis
    * Initially in non-cluster mode because no need to scale
    * Later in cluster node which supports online up/down scaling of shards and/or replicas
  * Algorithm for snapshot/event caching detailed in `tla+/project_cache.*`


REJECTED / FUTURE IDEAS
=======================

* Jetty
  * Good for now. Maybe switch in the future to Netty or something based on Quasar fibers.
  * Some benchmarking:
    * https://gist.github.com/dhanji/81ccc0e6652eccaf43cf
    * https://dzone.com/articles/benchmarking-high-concurrency-http-servers-on-the
  * Any alternative would need to support:
    * HTTPS
    * HTTP2
    * gzip/brotli
    * static asset serving with e-tags
    * websockets

* Store sessions in Redis
  * automatic expiry via TTL
  * update atomicity
    * supported via `WATCH; MULTI; EXEC`
    * might be faster via Lua script: https://redislabs.com/ebook/part-3-next-steps/chapter-11-scripting-redis-with-lua/
  * scaling
    * is manual by ops person
    * in non-cluster mode, the primary node goes down and comes back up, reads continue in the interim (if replicas exist)
    * in cluster-mode, can scale up/down online as you'd expect
  * persistence
    * AWS seems to only support: in-memory, manual snapshots, n-daily automated backups
