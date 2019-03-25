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
    * Removing Lift's private SSP tokens => cachability
    * Maybe just on / path


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


UNDER CONSIDERATION
===================

* Server-side procs
  * single endpoint, cmd type -> cmd -> resp
  * multiple endpoints
  * live updates with generated binary codecs could result in breakage

* AWS Cognito
  * would get:
    * ability to sign up / login via other services (FB, whatever)
    * MFA
    * verification (eg. SMS)
    * users can manage own accounts (change password, email address, etc)
    * admin console
    * analytics
  * what about …?
    * roles - definition, management by users & admin
    * organisations - definition, management by users & admin
    * mailchimp sync
    * local envs - dev & test

* Encryption
  * One of the following:
    * use an `F[_]` in event types for {,un}encrypted
    * separate structure & content, encrypt content only (holistically? ∃ only snapshots & events right?)
  * Support different types of encryption
  * Keys
    * Store on the server and link to users and/or projects? (they can just store though)
    * User's problem - means impossible for us to decrypt - probably a requirement for Telstra
    * Some other hybrid? Like users/roles/owners/perms? Where a central key is stored with us, but it's encrypted,
      maybe stored multiple times under different keys? eg. 4 users have keys and there are 4 encrypted versions
      of the master key, this would allow for user-facing key management without the need to reencrypt events.
    * Use "secret sharing"? Like Shamir's Secret Sharing
  * Requirements = ?
    * Telstra: No PII ever sent to ShipReq (this means S encrypting with a secret master key is not allowed)

TARGET STATE
============

* Remove web-server library; interface with web-server directly (most likely via the Servlet API)

* Use JWT for session management
  * only needed for login proof / userId
  * can avoid DB lookup by including user metadata (like `@golly`)
  * Java lib: https://github.com/jwtk/jjwt
  * store in a httpOnly cookie (don't use {local,session}Storage)
  * use expiryTime

* Server-side procs
  * Scala.JS makes regular ajax calls
  * auth via JWT
  * endpoint(s) require a valid JWT - even if not logged in

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
  * Algorithm for snapshot/event caching detailed in `Format/project.tla`

MIGRATION
=========
* PublicSpa
  * Read JWT from request
  * Add JWT to response (or at least update expiry time)
  * New SSPs
    * New defns, new types, new codecs
    * Server-side JWT check
    * Client side AJAX (compare with Lift's. Add features? Auto retry maybe?)
  * Use Lift stateless dispatch
* HomeSpa
  * use new AJAX procs
  * use Lift stateless dispatch
* ProjectSpa
  * Establish WebSocket
  * Re-establist WebSocket on loss
  * Generic req/respond over WebSocket (eg. (ReqId, Req) => | => (ReqId, Resp))
  * Project AJAX over WS
  * Push events over WS
    * Add Redis
    * Redis pub/sub
    * Remove comets
  * use Lift stateless dispatch
* Remove
  * Old SSP
  * Shiro
* Implement protocol & caching according to TLA+ spec


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
