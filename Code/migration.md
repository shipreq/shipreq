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
    * [x] Use Lift stateless dispatch
* [ ] HomeSpa
  * [ ] Use JWT & new SSPs
  * [ ] use Lift stateless dispatch
* [ ] ProjectSpa
  * [ ] Capabilities:
    * [x] Establish WebSocket
    * [ ] Re-establish WebSocket on loss
    * [x] Generic req/respond over WebSocket (eg. (ReqId, Req) => | => (ReqId, Resp))
    * [x] Server push (and typed protocol)
    * [x] Typed WS protocol combining req/resp & push/recv
  * [ ] New ProjectSpa logic - implement TLA spec (minus caching)
  * [ ] Replace:
    * [ ] Use JWT
    * [ ] Use WebSocket
    * [ ] Project AJAX over WS
    * [ ] Push events over WS
  * [ ] use Lift stateless dispatch
* [ ] Remove
  * [ ] Old SSP
  * [x] Shiro
  * [ ] Lift statelful dispatch
  * [x] webapp-logic: Security (v1)
  * [ ] webapp-logic: Server.Session
  * [ ] Remove comets
  * [ ] Remove Promse logic (probably)
* [ ] Implement protocol & caching according to TLA+ spec
  * [ ] Add async typeclass and/or support to Fx
  * [ ] Add Redis
  * [ ] Redis pub/sub

* Add a correlation ID to JWTs?
