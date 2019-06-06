* Where I want to be
  * User management + enterprise support
    * multiple login/signup methods (oauth?)
    * mailing list ~~~ system accounts ~~~ ownership/perms wrt relation to data
    * groups/org etc for phase 3
  * Encryption


CURRENT STATE
=============

* Lift -- GET RID OF IT
  * Currently using for...
    * template injection


UNDER CONSIDERATION
===================

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

* Use websockets & redis to provide user with social real-time data such as:
  * connected users
  * local & remote uncommitted dirty fields


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
