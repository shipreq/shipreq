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
  * organise as one of the following
    * single endpoint, cmd type -> cmd -> resp
    * endpoint/spa,    cmd type -> cmd -> resp
    * endpoint/cmd,                cmd -> resp

* Event subscription
  * Use Redis project channels and pub/sub
  * webapp-server publishes new events to Redis
  * webapp-server subscribes and receives events, then publishes to clients through existing connections
  * webapp-server maintains connections with clients using one of two options:
    1. Keep Lift just for comets (and have all HTTP be stateless routes)
    2. Use websockets directly

* Project caching (for quick responses to new events & project loads)
  * Use Redis
  * Details TBD wrt snapshot/events & concurrency/traffic-size & DB/staleness


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
