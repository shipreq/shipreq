MIGRATION
=========
* [x] PublicSpa
  * [x] Capabilities:
    * [x] Read/write cookies
    * [x] Read JWT from request
    * [x] Add JWT to response (or at least update expiry time)
    * [x] New SSPs (AJAX)
      * [x] New defns, new types, new codecs
      * [x] Server-side JWT check
      * [x] Client side AJAX (compare with Lift's. Add features? Auto retry maybe?)
  * [x] Replace:
    * [x] Shiro with JWT
    * [x] Use JWT & new SSPs
* [x] HomeSpa
  * [x] Use JWT & new SSPs
* [ ] ProjectSpa
  * [x] Capabilities:
    * [x] Establish WebSocket
    * [x] Re-establish WebSocket on loss
    * [x] Generic req/respond over WebSocket (eg. (ReqId, Req) => | => (ReqId, Resp))
    * [x] Server push (and typed protocol)
    * [x] Typed WS protocol combining req/resp & push/recv
  * [ ] New ProjectSpa logic using TLA spec
    * [x] load
    * [x] update
    * [x] Redis algebra
    * [ ] reload
    * [ ] sync
  * [ ] New client logic
    * [ ] load (initAppData) on inital WS connection
    * [ ] reload
    * [ ] sync
    * [ ] keepalive
  * [ ] Replace:
    * [ ] Use JWT
    * [ ] Use WebSocket
    * [ ] ClientState or whatever - new one with ProjectAndOrd + future event logic
    * [ ] Project AJAX over WS
    * [ ] Push events over WS
* [ ] only use Lift stateless dispatch
* [ ] Remove
  * [ ] Old SSP
  * [x] Shiro
  * [ ] Lift statelful dispatch
  * [x] webapp-logic: Security (v1)
  * [ ] webapp-logic: Server.Session
  * [ ] Remove comets
  * [x] Remove Promse logic (probably)

* [ ] Redis
  * [ ] Add async typeclass and/or support to Fx (?)
  * [ ] Add Redis to project & env
  * [ ] LUA scripts
  * [ ] Real algebra impl
  * [ ] Real pub/sub

* Add a correlation ID to JWTs?
