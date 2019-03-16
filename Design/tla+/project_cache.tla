------------------------------------------------- MODULE project_cache -------------------------------------------------

EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANT User,
         Request,
         MCVerLimit,
         MCAllowUserDisconnect

MCSymmetry == Permutations(User) \union Permutations(Request)

ASSUME /\ IsFiniteSet(User)
       /\ IsFiniteSet(Request)
       /\ MCVerLimit \in Nat
       /\ MCVerLimit > 1
       /\ MCAllowUserDisconnect \in BOOLEAN

VARIABLES db,       \* The state of the Db
          redis,    \* The state of Redis
          procsL,   \* The state of load processors (i.e. threads in webapps)
          procsU,   \* The state of update processors (i.e. threads in webapps)
          procsS,   \* The state of sync processors (i.e. threads in webapps)
          pub,      \* Set of events being published
          userState \* Users' states

vars == << db, redis, procsL, procsU, procsS, pub, userState >>

TypeInvariants ==
  /\ db \in [ver: Nat] \* The version of the Project aka the number of events

  /\ redis \in [
       ver   : Nat,        \* The version of a Project snapshot, or 0 if cache empty
       events: SUBSET Nat] \* A set of events represented by their version numbers

  /\ procsL \in SUBSET [
       status     : {"ReadDbVer", "ReadRedis", "ReadDbFull", "WriteRedis", "Respond"},
       user       : User,
       dbLatestVer: Nat, \* Latest known version in Db (just the seq number, not the event)
       dbVer      : Nat, \* Snapshot + events loaded from Db
       respondVer : Nat] \* Snapshot + events to send to the user

  /\ procsU \in SUBSET [
       req     : Request,
       status  : {"ReadRedis", "ReadDb", "WriteRedis1", "WriteDb", "WriteRedis2", "Respond"},
       user    : User,
       redisVer: Nat, \* The version of Redis at the last read from Redis
       ver     : Nat] \* The version of the Project in memory (0=none)

  /\ procsS \in [User -> SUBSET Nat] \* Per user-request, set of events in memory

  /\ pub \in SUBSET (User \X Nat) \* Set of (target user, event)

  /\ userState \in [
       User -> [
         status : {"offline", "loading", "active"},
         ver    : Nat,              \* The version of the built Project
         future : SUBSET Nat,       \* Future events that can't be applied cos intermediate event is missing
         reqs   : SUBSET Request ]] \* Requests for which a response hasn't be received

varDesc == [db        |-> db.ver,
            redis     |-> <<redis.ver, redis.events>>,
            procsL    |-> procsL,
            procsU    |-> procsU,
            procsS    |-> procsS,
            pub       |-> pub,
            userState |-> userState]

------------------------------------------------------------------------------------------------------------------------

Min[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a <= b
Max[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a >= b

Replace(set, old, new) == { IF a = old THEN new ELSE a : a \in set }

------------------------------------------------------------------------------------------------------------------------

EmptyProcsS == [u \in User |-> {}]

InitProcL(u) == [
  user        |-> u,
  status      |-> "ReadRedis",
  dbLatestVer |-> 0,
  dbVer       |-> 0,
  respondVer  |-> 0]

OfflineUserState == [
  status |-> "offline",
  ver    |-> 0,
  future |-> {},
  reqs   |-> {}]

OnlineUsers == {u \in User : userState[u].status /= "offline"}

RedisPartialVer == IF redis.events = {} THEN redis.ver ELSE Max[redis.events]
RedisTotalVer   == IF redis.ver    = 0  THEN 0         ELSE RedisPartialVer

------------------------------------------------------------------------------------------------------------------------

Init ==
  /\ db    \in [ver: 1..MCVerLimit]
  /\ redis \in UNION { { [ver |-> v, events |-> {}]} \union
                       { [ver |-> v, events |-> (v+1)..e2] : e2 \in (v+1)..(db.ver) }
                     : v \in 0..(db.ver) }
  /\ procsL    = {}
  /\ procsU    = {}
  /\ procsS    = EmptyProcsS
  /\ pub       = {}
  /\ userState = [u \in User |-> OfflineUserState]

DataInvariants ==
  \* /\ PrintT(varDesc)
  /\ \A u \in User :
    LET s == userState[u]
    IN /\ s.status = "active" => s.ver > 0
       /\ s.status /= "active" => s.reqs = {}
       /\ s.ver <= db.ver
       /\ \A e \in s.future : e > s.ver + 1
  /\ redis.ver <= db.ver
  /\ \A p \in procsL :
    /\ p.respondVer /= 0 <=> p.status = "Respond"
  /\ \A e \in redis.events :
    /\ \* No gaps in Redis events
       IF redis.ver > 0
       THEN (e - 1) \in (redis.events \union {redis.ver})
       ELSE (e - 1) \in (redis.events) \/ e = Min[redis.events]

MCAllowAct == db.ver < MCVerLimit

------------------------------------------------------------------------------------------------------------------------

ApplyEvents[v \in Nat, es \in SUBSET Nat] ==
  LET n == v + 1
  IN IF n \in es
     THEN ApplyEvents[n, es \ {n}]
     ELSE <<v,es>>

RedisWriteSnapshot(ver, OnOk, OnFail) ==
  IF ver > RedisTotalVer
  THEN
    /\ redis' = [ver    |-> ver,
                 events |-> IF ver + 1 \in redis.events THEN {e \in redis.events : e > ver} ELSE {}]
    /\ OnOk
  ELSE \* Redis has a more recent state than this proc
    /\ UNCHANGED redis
    /\ OnFail

------------------------------------------------------------------------------------------------------------------------

\* In reality this is: open page, establish websocket, receive project, subscribe to pub/sub channel
UserConnect ==
  /\ MCAllowAct
  /\ \E u \in User :
     /\ userState[u].status = "offline"
     /\ \A p \in procsU : p.user /= u \* A new user (connection) is distinct.
                                      \* If the model value is still being used in an orphan proc, it can be recycled here yet
     /\ userState' = [userState EXCEPT ![u].status = "loading"]
     /\ procsL' = procsL \union {InitProcL(u)}
     /\ UNCHANGED << db, redis, procsU, procsS, pub >>

\* TODO Also model the fact that i'll start preloading on HTTP GET before the websocket connects

Load_ReadDbVer == \E p \in procsL :
  /\ p.status = "ReadDbVer"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.dbLatestVer = db.ver, !.status = "ReadRedis"])
  /\ UNCHANGED << db, procsU, procsS, pub, redis, userState >>

Load_ReadRedis == \E p \in procsL :
  /\ p.status = "ReadRedis"
  /\ LET cacheHit == RedisTotalVer > 0 /\ RedisTotalVer = p.dbLatestVer
         p2 == IF cacheHit
               THEN [p EXCEPT !.status = "Respond", !.respondVer = RedisTotalVer]
               ELSE [p EXCEPT !.status = "ReadDbFull"]
     IN procsL' = Replace(procsL, p, p2)
  /\ UNCHANGED << db, procsU, procsS, pub, redis, userState >>

Load_ReadDbFull == \E p \in procsL :
  /\ p.status = "ReadDbFull"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.dbVer  = db.ver, !.status = "WriteRedis"])
  /\ UNCHANGED << db, procsU, procsS, pub, redis, userState >>

Load_WriteRedis == \E p \in procsL :
  /\ p.status = "WriteRedis"
  /\ procsL' = Replace(procsL, p, [p EXCEPT !.status = "Respond", !.respondVer = p.dbVer])
  /\ RedisWriteSnapshot(p.dbVer, TRUE, TRUE)
  /\ UNCHANGED << db, procsU, procsS, pub, userState >>

Load_Respond == \E p \in procsL :
  /\ p.status = "Respond"
  /\ procsL' = procsL \ {p}
  /\ LET us  == userState[p.user]
         v   == p.respondVer
         r   == ApplyEvents[v, {e \in us.future : e > v}]
         us2 == [us EXCEPT !.ver = r[1], !.future = r[2], !.status = "active"]
     IN userState' = [userState EXCEPT ![p.user] = us2]
  /\ UNCHANGED << db, procsU, procsS, pub, redis >>

Load ==
  \/ Load_ReadDbVer
  \/ Load_ReadRedis
  \/ Load_ReadDbFull
  \/ Load_WriteRedis
  \/ Load_Respond

------------------------------------------------------------------------------------------------------------------------

UpdateRequest ==
  /\ MCAllowAct
  /\ \E u \in User : userState[u].status = "active"                   \* For an online user
     /\ \E r \in Request : \A i \in User : r \notin userState[i].reqs \* get a unique req Id
        /\ userState' = [userState EXCEPT ![u].reqs = @ \union {r}]
        /\ procsU'    = procsU \union {[user |-> u, req |-> r, status |-> "ReadRedis", redisVer |-> 0, ver |-> 0]}
        /\ UNCHANGED << db, redis, pub, procsL, procsS >>

Update_ReadRedis == \E p \in procsU :
  /\ p.status = "ReadRedis"
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.ver      = RedisTotalVer,
                                            !.redisVer = RedisTotalVer,
                                            !.status   = IF RedisTotalVer > p.ver THEN "WriteDb" ELSE "ReadDb"])
  /\ UNCHANGED << db, redis, pub, userState, procsL, procsS >>

Update_ReadDb == \E p \in procsU :
  /\ p.status = "ReadDb"
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.ver = db.ver, !.status = "WriteRedis1"])
  /\ UNCHANGED << db, redis, pub, userState, procsL, procsS >>

Update_WriteRedis1 == \E p \in procsU :
  /\ p.status = "WriteRedis1"
  /\ LET Continue == procsU' = Replace(procsU, p, [p EXCEPT !.status = "WriteDb"])
         Retry    == procsU' = Replace(procsU, p, [p EXCEPT !.status = "ReadRedis"])
         WriteEvents ==
           LET firstEvent == p.redisVer + 1
               tryEvents  == firstEvent .. p.ver
               newEvents  == {e \in tryEvents : e > RedisTotalVer}
           IN IF redis.ver = 0 \/ firstEvent > RedisTotalVer + 1 \* Is there a missing event? Would this create a gap?
              THEN /\ UNCHANGED redis
                   /\ Retry
              ELSE /\ redis' = [redis EXCEPT !.events = @ \union newEvents]
                   /\ Continue
     IN \/ RedisWriteSnapshot(p.ver, Continue, Retry)
        \/ WriteEvents
  /\ UNCHANGED << db, pub, userState, procsL, procsS >>

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
        /\ UNCHANGED << redis, procsL, pub, userState, procsS >>
     \/ \* Request is invalid
        /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "Respond"])
        /\ UNCHANGED << db, redis, procsL, pub, userState, procsS >>

Update_WriteRedis2 == \E p \in procsU :
  /\ p.status = "WriteRedis2"
  /\ pub' = pub \union { <<p.user, p.ver>> }                \* Proc does this
                \union { <<u, p.ver>> : u \in OnlineUsers } \* Redis does this
  /\ \/ \* Send a snapshot to Redis
        IF p.ver > RedisTotalVer
        THEN redis' = [ver |-> p.ver, events |-> {}]
        ELSE UNCHANGED redis
     \/ \* Send an event to Redis
        IF p.ver = RedisPartialVer + 1
        THEN redis' = [redis EXCEPT !.events = @ \union {p.ver}]
        ELSE UNCHANGED redis
  /\ procsU' = Replace(procsU, p, [p EXCEPT !.status = "Respond"])
  /\ UNCHANGED << db, procsL, procsS, userState >>

\* Responds to user
Update_Respond == \E p \in procsU :
  /\ p.status = "Respond"
  /\ IF userState[p.user].status = "active"
     THEN userState' = [userState EXCEPT ![p.user].reqs = @ \ {p.req}]
     ELSE UNCHANGED userState
  /\ procsU' = procsU \ {p}
  /\ UNCHANGED << db, redis, procsL, procsS, pub >>

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
  \E u \in User : LET s == userState[u] IN
    /\ procsS[u] = {} \* Non-empty means request by this user already in progress
    /\ s.status = "active"
    /\ s.reqs = {}
    /\ s.future /= {} \* by this point we know we have missing events
    /\ LET missing == ((s.ver + 1) .. (Max[s.future] - 1)) \ s.future
       IN procsS' = [procsS EXCEPT ![u] = missing]
    /\ UNCHANGED << db, redis, procsU, procsL, pub, userState >>

SyncPush ==
  \E u \in User : LET events == procsS[u] IN
    /\ events /= {}
    /\ procsS' = [procsS EXCEPT ![u] = {}]
    /\ pub'    = pub \union (OnlineUsers \X events)
    /\ UNCHANGED << db, procsU, procsL, userState, redis >>
    \* Writing to redis.events can invalidate the invariant that redis has no gaps
    \* Not sure how valuable that invariant is but OTOH,
    \* not sure how valuable it would be to write non-latest events...
    \* /\ redis'  = [redis EXCEPT !.events = @ \union {e \in events : e > redis.ver}]

------------------------------------------------------------------------------------------------------------------------

Publish ==
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
      /\ UNCHANGED << db, redis, procsU, procsL, procsS >>

\* This is the websocket being closed and not being restablished (i.e. user closes tab)
\* TODO: If the tab remains open on a disconnect, the client should reestablish a websocket and say where it's up to
UserDisconnect ==
  /\ MCAllowAct
  /\ MCAllowUserDisconnect
  /\ \E u \in User : userState[u].status /= "offline"
    /\ userState' = [userState EXCEPT ![u] = OfflineUserState]
    /\ pub'       = {usrEvt \in pub : usrEvt[1] /= u}
    /\ UNCHANGED << db, redis, procsU, procsL, procsS >>

RedisEviction ==
  /\ MCAllowAct
  /\ \/ redis' = [redis EXCEPT !.ver = 0]
     \/ redis' = [redis EXCEPT !.events = {}]
  /\ UNCHANGED << db, procsU, procsL, procsS, pub, userState >>

WebappDeath ==
  \* Start with a subset of users because each user communicates through a websocket which is tied to a specific worker
  \* Also remember the user isn't a ShipReq user; it's a browser tab, a session.
  /\ MCAllowAct
  /\ \E affectedUsers \in (SUBSET(OnlineUsers) \ {}) :
    /\ userState' = [u \in User |-> IF u \in affectedUsers
                                    THEN OfflineUserState \* TODO Add reconnect suuport - this is the user reloading page
                                    ELSE userState[u]]
    /\ procsL' = {p \in procsL : p.user \notin affectedUsers}
    /\ procsU' = {p \in procsU : p.user \notin affectedUsers}
    /\ procsS' = [u \in User |-> IF u \in affectedUsers THEN {} ELSE procsS[u]]
    /\ pub' = {usrEvt \in pub : usrEvt[1] \notin affectedUsers}
    /\ UNCHANGED << db, redis >>

------------------------------------------------------------------------------------------------------------------------

\* These are all guarded by MCAllowAct but the guard is inlined, else TLC won't report what its doing in stack traces
Act ==
  \/ UserConnect
  \/ UpdateRequest
  \/ RedisEviction
  \/ UserDisconnect
  \/ WebappDeath

React ==
  \/ Load
  \/ UpdateRespond
  \/ Publish
  \/ SyncRequest
  \/ SyncPush

Next == Act \/ React

\*Fairness ==
\*  /\ WF_vars(UpdateRequest)
\*  /\ SF_vars(Load)
\*  /\ UserConnect ~> Load_Respond
\*  /\ SF_vars(UpdateRespond)
\*  /\ SF_vars(Publish)

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
