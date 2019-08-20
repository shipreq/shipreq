Where are codecs used?
======================

* Server <--> Client
  * Platforms: JVM, JS
  * Interface: Ajax, WebSocket
  * Goals: should be fast & small

* Client <--> WebWorkers
  * Platforms: JS
  * Interface: Manual
  * Goals: should be fast

* Server <--> Redis
  * Platforms: JVM
  * Interface: Redis
  * Goals: must be fast & small

* Server <--> SSR
  * Platforms: JVM, JS
  * Interface: scala-graal
  * Goals: should reuse codecs used between Server and Client to avoid additional dev

* Server <--> Postgres
  * Platforms: JVM
  * Interface: JDBC & fixed table columns
  * Goals: should be fast & small, should be easily readable & writable by humans

* Server <--> Staff
  * Platforms: JVM
  * Interface: Code, REST
  * Goals: must be easily readable & writable by humans

* Taskman <--> 3rdPartyServices
  * Platforms: JVM
  * Interface: REST
  * Goals: should be easy to read/write codecs


Libraries
=========

| Protocol               | Format           | Library          |
| ---------------------- | ---------------- | ---------------- |
| Server <--> Client     | Binary           | BooPickle (Size) |
| Client <--> WebWorkers | Binary           | BooPickle (Size) |
| Server <--> Redis      | Binary           | BooPickle (Size) |
| Server <--> SSR        | Binary           | BooPickle (Size) |
| Server <--> Postgres   | JSON tiny + cols | μPickle custom   |
| Server <--> Staff      | JSON             | Circe            |
| Taskman <--> 3PS       | JSON             | Circe            |

Rejections:
* New μPickle is much faster but the codecs are really hard to write and most library support is hardcoded into the macros
* Not going to replace μPickle-custom with Circe for DB because it's all already working, and evolvable
* scodec would be nice but BooPickle is faster (I believe) and already integrated in ShipReq
* The BooPickle Speed codecs are rejected because the speed difference is negligible where as the size-savings kick in *a lot*


Protocol Evolution
==================

* Binary
  * Each codec is opaque and ready to gobble the entire bitstream. Either:
    * Add version at the beginning
    * Add a magic number header (no guarantee that a codec wont coincidentally also result in the same magic number)
  * For true forward compatibility, we should first serialise something like a `Map[String, BinaryData]` as a header so
    that all versions can read all the key:values, and older protocols will ignore unrecognised (i.e. new) fields.

* JSON
  * no need to preemptively add anything - easily evolvable over time by
    * adding a new `"version"` field to an object
    * increasing a `"version"` field value
    * wrapping a value in an object with a version field

* Time
  * It's not just about protocols and breaking changes, required classes might not be on the classpath.
    Example: new event type is added, root event decoder gets a new dispatch key, protocol is backwards-compatible
    but older readers will still crash not knowing which event is being read. No evolutionary strategy can fix this.
    It's a genuine incompatibility between parties that can't be avoided.
  * One side is going to need to know how to handle being fatally out-of-date.
  * Major = entire tree of codecs
  * Minor = info only
    * `w ≤ r => ∀i.ok(r(w(i)))` - reader can read everything from writer
    * `w > r => ∃i.ko(r(w(i)))` - reader can't read some things from writer


Handling Protocol Failure
=========================

Both sides will need to consider protocol version.
In many cases the server will reject the request and/or disconnect, forcing the Client to retry
(or in the case of WebSocket, automatically-reconnect). This is because a Client being ahead of
Server will nearly always mean that we're part-way through a rollout and some servers in the
cluster have and haven't been upgraded. Lower probability is a rollback occurred.

| Protocol               | l > r : r (l x)       | l < r : l (r x)                           |
| ---------------------- | --------------------- | ----------------------------------------- |
| Server <--> Client     | Client to reload page | Server rejects req, Client to retry       |
| Client <--> WebWorkers | impossible            | impossible                                |
| Server <--> Redis      | no prob               | Can't read Redis => can't read DB either. |
|                        |                       | Server rejects req, Client to retry       |
| Client <--> SSR        | impossible            | impossible                                |
| Server <--> Postgres   | no prob               | Server rejects req, Client to retry       |
| Server <--> Staff      | no prob               | Server rejects req, Staff to retry        |
| Taskman <--> 3PS       | N/A                   | N/A: 3P determines protocol               |


Integrity
=========

* What am I guarding against?
  * partial messages (where the tail is lost)
  * mistargetted messages (eg. data is sent the wrong endpoint)
  * any kind of corruption for piece of mind (maybe?)

* Binary
  * We already get partial integrity from the codec
    More dangerous than JSON because an invalid message might start with 0 and the codec could interpret that as 0 elements and stop.
  * Idea: magic numbers at beginning and end
  * Idea: hash message binary and append/prepend
  * **Conclusion:** wrap messages in magic numbers - very low effort, most of the benefit

* JSON
  * Structural integity we already get from parsing from String to JSON.
  * We already get partial content integrity from the codec
  * Full content integrity would require hashing...
    * Hashing over source objects has already been removed and is a nightmare in terms of evolvability
    * Hashing over the JSON itself would be quite simple
  * Is there really any value in content integrity?
    * Between Server and DB: no, structural is plenty
    * Between Server and Staff: definately not, structural is plenty
    * Between Taskman and 3PS: not possible
  * **Conclusion:** normal JSON (with normal implicit structural integrity is plenty)