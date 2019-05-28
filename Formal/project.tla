---------------------------------------------------- MODULE project ----------------------------------------------------

EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANT User, \* Really this is a unique client/connection, not a ShipReq user/identity
         Request,
         MCVerLimit,
         MCAllowSyncRequests,
         MCAllowUserDisconnect,
         MCAllowUserReconnect,
         MCAllowWebappDeath

MCSymmetry == Permutations(User) \union Permutations(Request)

ASSUME /\ IsFiniteSet(User)
       /\ IsFiniteSet(Request)
       /\ MCVerLimit \in Nat
       /\ MCVerLimit > 1
       /\ MCAllowSyncRequests \in BOOLEAN
       /\ MCAllowUserDisconnect \in BOOLEAN
       /\ MCAllowUserReconnect \in BOOLEAN
       /\ MCAllowWebappDeath \in BOOLEAN
       /\ MCAllowUserReconnect => MCAllowUserDisconnect
       /\ MCAllowWebappDeath => MCAllowSyncRequests

VARIABLES db,       \* The state of the Db
          redis,    \* The state of Redis
          procsL,   \* The state of load   processors (i.e. threads in webapps)
          procsR,   \* The state of reload processors (i.e. threads in webapps)
          procsU,   \* The state of update processors (i.e. threads in webapps)
          procsS,   \* The state of sync   processors (i.e. threads in webapps)
          pub,      \* Set of events being published
          sub,      \* Users subscribed to project event topic
          userState \* Users' states

vars == << db, redis, procsL, procsR, procsU, procsS, pub, sub, userState >>

TypeInvariants ==
  /\ db \in [ver: Nat] \* The version of the Project aka the number of events

  /\ redis \in [
       ver   : Nat,        \* The version of a Project snapshot, or 0 if cache empty
       events: SUBSET Nat] \* A set of events represented by their version numbers

  /\ procsL \in SUBSET [
       status     : {"Subscribe", "ReadDbVer", "ReadRedis", "ReadDbFull", "WriteRedis", "Respond"},
       user       : User,
       dbLatestVer: Nat, \* Latest known version in Db (just the seq number, not the event)
       dbVer      : Nat, \* Snapshot + events loaded from Db
       respondVer : Nat] \* Snapshot + events to send to the user

  /\ procsR \in SUBSET [
       status     : {"Subscribe", "ReadEvents", "Respond"},
       user       : User,
       userVer    : Nat,
       events     : SUBSET Nat]

  \* TODO Should be modelled by Request
  /\ procsU \in SUBSET [
       req     : Request,
       status  : {"ReadRedis", "ReadDb", "WriteRedis1", "WriteDb", "WriteRedis2", "Respond"},
       user    : User,
       redisVer: Nat, \* The version of Redis at the last read from Redis
       ver     : Nat] \* The version of the Project in memory (0=none)

  /\ procsS \in [User -> SUBSET Nat] \* Per user-request, set of events in memory

  /\ pub \in SUBSET (User \X Nat) \* Set of (target user, event)

  /\ sub \in SUBSET User

  /\ userState \in [
       User -> [
         status : {"offline", "loading", "active", "reloading"},
         ver    : Nat,              \* The version of the built Project
         future : SUBSET Nat,       \* Future events that can't be applied cos intermediate event is missing
         reqs   : SUBSET Request ]] \* Requests for which a response hasn't be received

varDesc == [db        |-> db.ver,
            redis     |-> <<redis.ver, redis.events>>,
            procsL    |-> procsL,
            procsR    |-> procsR,
            procsU    |-> procsU,
            procsS    |-> procsS,
            pub       |-> pub,
            sub       |-> sub,
            userState |-> userState]

------------------------------------------------------------------------------------------------------------------------

Min[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a <= b
Max[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a >= b

Replace(set, old, new) == { IF a = old THEN new ELSE a : a \in set }

------------------------------------------------------------------------------------------------------------------------

EmptyProcsS == [u \in User |-> {}]

InitProcL(u) == [
  user        |-> u,
  status      |-> "Subscribe",
  dbLatestVer |-> 0,
  dbVer       |-> 0,
  respondVer  |-> 0]

OfflineUserState == [
  status |-> "offline",
  ver    |-> 0,
  future |-> {},
  reqs   |-> {}]

GoOffline(u) == [u EXCEPT !.status = "offline", !.reqs = {}]

OnlineUsers == {u \in User : userState[u].status /= "offline"}

IsUserInUse(u) ==
  \/ userState[u].status /= "offline"
  \/ procsS[u] /= {}
  \/ \E p \in procsL : p.user = u
  \/ \E p \in procsR : p.user = u
  \/ \E p \in procsU : p.user = u
  \/ \E p \in pub    : p[1]   = u
  \/ u \in sub

RedisTotalVer ==
  LET IsComplete ==
        \/ redis.events = {}
        \/ redis.ver + 1 = Min[redis.events]
      Ver ==
        IF redis.events = {} THEN redis.ver ELSE Max[redis.events]
  IN IF IsComplete THEN Ver ELSE 0

------------------------------------------------------------------------------------------------------------------------

Init ==
  /\ db    \in [ver: 1..MCVerLimit]
  /\ redis \in UNION { { [ver |-> v, events |-> {}]} \union
                       { [ver |-> v, events |-> (v+1)..e2] : e2 \in (v+1)..(db.ver) }
                     : v \in 0..(db.ver) }
  /\ procsL    = {}
  /\ procsR    = {}
  /\ procsU    = {}
  /\ procsS    = EmptyProcsS
  /\ pub       = {}
  /\ sub       = {}
  /\ userState = [u \in User |-> OfflineUserState]

DataInvariants ==
  \* /\ PrintT(varDesc)
  /\ \A u \in User :
    LET s == userState[u]
    IN /\ s.status = "active" => s.ver > 0
       /\ s.status /= "active" => s.reqs = {}
       /\ s.ver <= db.ver
       /\ \A e \in s.future : e > s.ver + 1
       /\ (\E p \in procsL : p.user = u) => s.status \in {"offline","loading"}
       /\ s.status = "offline" => (u \notin sub)
  /\ redis.ver <= db.ver
  /\ \A p \in procsL :
    /\ p.respondVer /= 0 <=> p.status = "Respond"
  /\ \A e \in redis.events :
    /\ \* No gaps in Redis events
       IF redis.ver > 0
       THEN (e - 1) \in (redis.events \union {redis.ver}) \* all events proceed snapshot
       ELSE (e - 1) \in (redis.events) \/ e = Min[redis.events]

MCAllowAct == db.ver < MCVerLimit

------------------------------------------------------------------------------------------------------------------------

ApplyEvents[ver \in Nat, es \in SUBSET Nat] ==
  LET n == ver + 1
  IN IF n \in es
     THEN ApplyEvents[n, es \ {n}]
     ELSE <<ver,es>>

PublishEvents(es) ==
  pub' = pub \union (sub \X es)

RedisWriteSnapshot(ver, eventsToPublish, OnOk, OnFail) ==
  /\ IF ver > redis.ver
     THEN
       /\ redis' = [ver    |-> ver,
                    events |-> {e \in redis.events : e > ver}]
       /\ OnOk
     ELSE
       \* Redis has a more recent state than this proc
       /\ UNCHANGED redis
       /\ OnFail
  /\ PublishEvents(eventsToPublish)

RedisWriteEvents(eventsToCache, eventsToCacheAndPublish, OnOk, OnFail) ==
  LET events     == eventsToCache \union eventsToCacheAndPublish
      newEvents  == {e \in events : e > RedisTotalVer}
      fail       == /\ OnFail
                    /\ UNCHANGED redis
      apply      == /\ OnOk
                    /\ redis' = [redis EXCEPT !.events = @ \union newEvents]
  IN /\ IF newEvents = {} THEN
          fail
        ELSE IF Min[newEvents] > RedisTotalVer + 1 THEN
          fail \* Gaps in redis.events prohibited
        ELSE
          apply
     /\ PublishEvents(eventsToCacheAndPublish)

RedisSubscribe(u) ==
  (* Redis doesn't know users' status; Redis just has subscribers. This is here because:
     - a user in this model represents a live WebSocket connection
     - WebSocket connections are not reused when they die, the client will establish a new connection
     - WebSockets will close their Redis topic subscriptions onClose (outside of this model because trivial)
     - therefore, a process subscribing mid-response, after the connection to the user is broken, has no effect:
       the actual user is now disconnected and the WebSocket process will handle errors and clean up
     - therefore peeking at the userState in this model is equivalent with the exception of the temporary processor
       state as described above.
  *)
  IF userState[u].status /= "offline" THEN
    /\ sub' = sub \union {u}
    \* /\ Assert(u \notin sub, TRUE)
  ELSE
    UNCHANGED sub

------------------------------------------------------------------------------------------------------------------------

UserConnect ==
  /\ MCAllowAct
  /\ \E u \in User :
     /\ ~IsUserInUse(u) \* A new user (connection) is distinct. If model value is still is use, it can't be recycled here yet
     /\ userState' = [userState EXCEPT ![u] = [ver |-> 0, status |-> "loading", future |-> {}, reqs |-> {}]]
     /\ procsL' = procsL \union {InitProcL(u)}
     /\ UNCHANGED << db, redis, procsR, procsU, procsS, pub, sub >>

Load_Subscribe == \E p \in procsL :
  /\ p.status = "Subscribe"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.status = "ReadRedis"])
  /\ RedisSubscribe(p.user)
  /\ UNCHANGED << db, procsR, procsU, procsS, pub, redis, userState >>

Load_ReadDbVer == \E p \in procsL :
  /\ p.status = "ReadDbVer"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.dbLatestVer = db.ver, !.status = "ReadRedis"])
  /\ UNCHANGED << db, procsR, procsU, procsS, pub, redis, sub, userState >>

Load_ReadRedis == \E p \in procsL :
  /\ p.status = "ReadRedis"
  /\ LET cacheHit == RedisTotalVer > 0 /\ RedisTotalVer = p.dbLatestVer
         p2 == IF cacheHit
               THEN [p EXCEPT !.status = "Respond", !.respondVer = RedisTotalVer]
               ELSE [p EXCEPT !.status = "ReadDbFull"]
     IN procsL' = Replace(procsL, p, p2)
  /\ UNCHANGED << db, procsR, procsU, procsS, pub, redis, sub, userState >>

Load_ReadDbFull == \E p \in procsL :
  /\ p.status = "ReadDbFull"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.dbVer  = db.ver, !.status = "WriteRedis"])
  /\ UNCHANGED << db, procsR, procsU, procsS, pub, redis, sub, userState >>

Load_WriteRedis == \E p \in procsL :
  /\ p.status = "WriteRedis"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.status = "Respond", !.respondVer = p.dbVer])
  /\ RedisWriteSnapshot(p.dbVer, {}, TRUE, TRUE)
  /\ UNCHANGED << db, procsR, procsU, procsS, sub, userState >>

Load_Respond == \E p \in procsL :
  /\ p.status = "Respond"
  /\ procsL' = procsL \ {p}
  /\ LET us  == userState[p.user]
         v   == p.respondVer
         r   == ApplyEvents[v, {e \in us.future : e > v}]
         us2 == [us EXCEPT !.ver = r[1], !.future = r[2], !.status = "active"]
     IN CASE us.status = "offline" -> UNCHANGED userState
          [] us.status = "loading" -> userState' = [userState EXCEPT ![p.user] = us2]
  /\ UNCHANGED << db, procsR, procsU, procsS, pub, sub, redis >>

Load ==
  \/ Load_Subscribe
  \/ Load_ReadDbVer
  \/ Load_ReadRedis
  \/ Load_ReadDbFull
  \/ Load_WriteRedis
  \/ Load_Respond

------------------------------------------------------------------------------------------------------------------------

UserReconnect ==
  /\ MCAllowUserReconnect
  /\ MCAllowAct
  /\ \E u \in User :
     /\ ~IsUserInUse(u) \* A new user (connection) is distinct. If model value is still is use, it can't be recycled here yet
     /\ userState[u].ver /= 0
     /\ userState' = [userState EXCEPT ![u].status = "reloading"]
     /\ procsR' = procsR \union {[user |-> u, status |-> "Subscribe", userVer |-> userState[u].ver, events |-> {}]}
     /\ UNCHANGED << db, redis, procsU, procsL, procsS, pub, sub >>

Reload_Subscribe == \E p \in procsR :
  /\ p.status = "Subscribe"
  /\ procsR' = Replace(procsR, p, [p EXCEPT !.status = "ReadEvents"])
  /\ RedisSubscribe(p.user)
  /\ UNCHANGED << db, procsU, procsS, procsL, pub, redis, userState >>

\* Read = from DB
Reload_ReadEvents == \E p \in procsR :
  /\ p.status = "ReadEvents"
  /\ procsR' = Replace(procsR, p, [p EXCEPT !.events = (p.userVer..db.ver), !.status = "Respond"])
  /\ UNCHANGED << db, procsU, procsS, procsL, pub, redis, sub, userState >>

Reload_Respond == \E p \in procsR :
  /\ p.status = "Respond"
  /\ procsR' = procsR \ {p}
  /\ LET us  == userState[p.user]
         r   == ApplyEvents[us.ver, us.future \union {e \in p.events : e > us.ver}]
         us2 == [us EXCEPT !.ver = r[1], !.future = r[2], !.status = "active"]
     IN CASE us.status = "offline"   -> UNCHANGED userState
          [] us.status = "reloading" -> userState' = [userState EXCEPT ![p.user] = us2]
  /\ UNCHANGED << db, procsU, procsS, procsL, pub, redis, sub >>

Reload ==
  \/ Reload_Subscribe
  \/ Reload_ReadEvents
  \/ Reload_Respond

------------------------------------------------------------------------------------------------------------------------

UpdateRequest ==
  /\ MCAllowAct
  /\ \E u \in User : userState[u].status = "active"                   \* For an online user
     /\ \E r \in Request : \A i \in User : r \notin userState[i].reqs \* get a unique req Id
        /\ userState' = [userState EXCEPT ![u].reqs = @ \union {r}]
        /\ procsU'    = procsU \union {[user |-> u, req |-> r, status |-> "ReadRedis", redisVer |-> 0, ver |-> 0]}
        /\ UNCHANGED << db, redis, pub, procsL, procsR, procsS, sub >>

Update_ReadRedis == \E p \in procsU :
  /\ p.status = "ReadRedis"
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.ver      = RedisTotalVer,
                                            !.redisVer = RedisTotalVer,
                                            !.status   = IF RedisTotalVer > p.ver THEN "WriteDb" ELSE "ReadDb"])
  /\ UNCHANGED << db, redis, pub, userState, procsL, procsR, procsS, sub >>

Update_ReadDb == \E p \in procsU :
  /\ p.status = "ReadDb"
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.ver = db.ver, !.status = "WriteRedis1"])
  /\ UNCHANGED << db, redis, pub, userState, procsL, procsR, procsS, sub >>

Update_WriteRedis1 == \E p \in procsU :
  /\ p.status = "WriteRedis1"
  /\ LET Continue == procsU' = Replace(procsU, p, [p EXCEPT !.status = "WriteDb"])
         Retry    == procsU' = Replace(procsU, p, [p EXCEPT !.status = "ReadRedis"])
         WriteEvents ==
           LET firstEvent == p.redisVer + 1
               tryEvents  == firstEvent .. p.ver
           IN RedisWriteEvents(tryEvents, {}, Continue, Retry)
     IN \/ RedisWriteSnapshot(p.ver, {}, Continue, Retry)
        \/ WriteEvents
  /\ UNCHANGED << db, userState, procsL, procsR, procsS, sub >>

Update_WriteDb == \E p \in procsU :
  /\ p.status = "WriteDb"
  /\ \/ \* Request is valid
        /\ IF p.ver = db.ver
           THEN LET newVer == db.ver + 1
                IN /\ db'     = [ver |-> newVer]
                   /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "WriteRedis2", !.ver = newVer])
           ELSE \* Db has been updated without our knowledge; INSERT fails
                /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "ReadRedis"])
                /\ UNCHANGED db
     \/ \* Request is invalid
        /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "Respond"])
        /\ UNCHANGED db
  /\ UNCHANGED << redis, procsL, pub, userState, procsR, procsS, sub >>


Update_WriteRedis2 == \E p \in procsU :
  /\ p.status = "WriteRedis2"
  /\ \/ RedisWriteSnapshot(p.ver, {p.ver}, TRUE, TRUE)
     \/ RedisWriteEvents({}, {p.ver}, TRUE, TRUE)
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "Respond"])
  /\ UNCHANGED << db, procsL, procsS, procsR, userState, sub >>

\* Responds to user
Update_Respond == \E p \in procsU :
  /\ p.status = "Respond"
  /\ IF userState[p.user].status = "active"
     THEN userState' = [userState EXCEPT ![p.user].reqs = @ \ {p.req}]
     ELSE UNCHANGED userState
  /\ procsU' = procsU \ {p}
  /\ UNCHANGED << db, redis, procsL, procsS, procsR, pub, sub >>

UpdateRespond ==
  \/ Update_ReadRedis
  \/ Update_ReadDb
  \/ Update_WriteRedis1
  \/ Update_WriteDb
  \/ Update_WriteRedis2
  \/ Update_Respond

------------------------------------------------------------------------------------------------------------------------

(*
If a client notices a missing event (and a certain amount of time passes)
they can send a sync request for their missing events. The server will then
load the events from Redis and/or the DB (step 1), then (in step 2) it will
add the events to Redis and publish them through the project topic.

There's no point having extra steps for the read from redis/DB as it would
affect nothing; events always exist in DB, and redis is transient such that
it's state before the write doesn't matter.
*)

\* In the client logic there would be a delay before this is triggered but that's unneccessary for the spec
SyncRequest ==
  /\ MCAllowSyncRequests
  /\ \E u \in User : LET s == userState[u] IN
    /\ procsS[u] = {} \* Non-empty means request by this user already in progress
    /\ s.status = "active"
    /\ s.reqs = {}
    /\ s.future /= {} \* by this point we know we have missing events
    /\ LET missing == ((s.ver + 1) .. (Max[s.future] - 1)) \ s.future
       IN procsS' = [procsS EXCEPT ![u] = missing]
    /\ UNCHANGED << db, redis, procsU, procsL, procsR, pub, userState, sub >>

SyncPush ==
  \E u \in User : LET events == procsS[u] IN
    /\ events /= {}
    /\ procsS' = [procsS EXCEPT ![u] = {}]
    /\ RedisWriteEvents({}, events, TRUE, TRUE)
    /\ UNCHANGED << db, procsU, procsL, procsR, userState, sub >>

------------------------------------------------------------------------------------------------------------------------

RedisPublishEvent ==
  LET RecvEvent(s, v) ==
        IF v <= s.ver
        THEN s
        ELSE LET r == ApplyEvents[s.ver, s.future \union {v}]
             IN [s EXCEPT !.ver = r[1], !.future = r[2]]
  IN
    /\ \E <<u,v>> \in pub :
      /\ IF userState[u].status /= "offline" \* status=loading included because as soon as the websocket is established, the loading proc subscribes
         THEN userState' = [userState EXCEPT ![u] = RecvEvent(@, v)]
         ELSE UNCHANGED userState
      /\ pub' = pub \ {<<u,v>>}
      /\ UNCHANGED << db, redis, procsU, procsR, procsL, procsS, sub >>

UserDisconnect ==
  /\ MCAllowUserDisconnect
  /\ MCAllowAct
  /\ \E u \in User : userState[u].status /= "offline"
    /\ userState' = [userState EXCEPT ![u] = GoOffline(@)]
    /\ sub'       = sub \ {u}
    /\ pub'       = {usrEvt \in pub : usrEvt[1] /= u}
    /\ UNCHANGED << db, redis, procsU, procsL, procsR, procsS >>

RedisEviction ==
  /\ MCAllowAct
  /\ \/ redis' = [redis EXCEPT !.ver = 0]
     \/ redis' = [redis EXCEPT !.events = {}]
  /\ UNCHANGED << db, procsU, procsL, procsR, procsS, pub, sub, userState >>

WebappDeath ==
  \* Start with a subset of users because each user communicates through a websocket which is tied to a specific worker
  \* Also remember the user isn't a ShipReq user; it's a browser tab, a session.
  /\ MCAllowWebappDeath
  /\ MCAllowAct
  /\ \E affectedUsers \in (SUBSET(OnlineUsers) \ {{}}) :
    /\ procsL'    = {p \in procsL : p.user \notin affectedUsers}
    /\ procsR'    = {p \in procsR : p.user \notin affectedUsers}
    /\ procsU'    = {p \in procsU : p.user \notin affectedUsers}
    /\ procsS'    = [u \in User |-> IF u \in affectedUsers THEN {} ELSE procsS[u]]
    /\ pub'       = {usrEvt \in pub : usrEvt[1] \notin affectedUsers}
    /\ sub'       = sub \ affectedUsers
    /\ userState' = [u \in User |-> IF u \in affectedUsers THEN GoOffline(userState[u]) ELSE userState[u]]
    /\ UNCHANGED << db, redis >>

------------------------------------------------------------------------------------------------------------------------

\* These are all guarded by MCAllowAct but the guard is inlined, else TLC won't report what its doing in stack traces
Act ==
  \/ UserConnect
  \/ UserReconnect
  \/ UpdateRequest
  \/ RedisEviction
  \/ UserDisconnect
  \/ WebappDeath

React ==
  \/ Load
  \/ Reload
  \/ UpdateRespond
  \/ RedisPublishEvent
  \/ SyncRequest
  \/ SyncPush

Next == Act \/ React

Spec == Init /\ [][Next]_<<vars>>

MCDone     == ~MCAllowAct /\ ~ENABLED(React)
MCContinue == ~MCDone

AllUsersUpToDate ==
  \A user \in User :
    /\ LET u == userState[user]
           s == u.status
       IN CASE s = "offline" -> TRUE
            [] s = "loading" -> FALSE
            [] s = "active"  -> u.ver = db.ver

FinalInvariants == MCDone => AllUsersUpToDate

========================================================================================================================
