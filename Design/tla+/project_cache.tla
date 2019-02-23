------------------------------------------------- MODULE project_cache -------------------------------------------------

EXTENDS Naturals

CONSTANT Request,
         User

VARIABLES db,       \* The state of the DB
          redis,    \* The state of Redis
          procs,    \* The state of request processors (i.e. threads in webapps)
          pub,      \* Set of events being published
          userState \* Users' states

vars == << db, redis, procs, pub, userState >>

TypeInvariants ==
  /\ db \in [
       ver: Nat] \* The version of the Project aka the number of events

  /\ redis \in [
       ver   : Nat,        \* The version of a Project snapshot, or 0 if cache empty
       events: SUBSET Nat] \* A set of events represented by their version numbers

  /\ pub \in SUBSET (User \X Nat) \* Set of (target user, event)

  /\ procs \in SUBSET [
       req   : Request,
       status: {"ready", "post-read-redis", "init-redis", "create", "post-create", "done"},
       user  : User,
       ver   : Nat] \* The version of the Project in memory (0=none)

  /\ userState \in [
       User -> [
         online : BOOLEAN,
         ver    : Nat,              \* The version of the built Project
         reqs   : SUBSET Request ]] \* Requests for which a response hasn't be received

RedisVer == IF redis.events = {}
            THEN redis.ver
            ELSE CHOOSE e \in redis.events : \A f \in redis.events : e >= f

DataInvariants ==
  /\ \A u \in User :
    /\ userState[u].online => userState[u].ver > 0
    /\ userState[u].ver <= db.ver
  /\ \A p \in procs : userState[p.user].online
  /\ \A e \in redis.events : e - 1 \in ({redis.ver} \union redis.events)
  /\ RedisVer <= db.ver


OfflineUser == [
  online |-> FALSE,
  reqs   |-> {},
  ver    |-> 0]

Init ==
  /\ db        = [ver |-> 1]
  /\ redis     = [ver |-> 0, events |-> {}]
  /\ procs     = {}
  /\ pub       = {}
  /\ userState = [u \in User |-> OfflineUser]


------------------------------------------------------------------------------------------------------------------------

Remove(set, el) == {\A a \in set : a /= el}

------------------------------------------------------------------------------------------------------------------------

\* In reality this is: open page, establish websocket, receive project, subscribe to pub/sub channel
UserConnect == \E u \in User :
  /\ ~userState[u].online
  /\ userState' = [userState EXCEPT ![u].online = TRUE, ![u].ver = db.ver]
  /\ UNCHANGED << db, redis, procs, pub >>

\* This is the websocket being closed and not being restablished (i.e. user closes tab)
\* TODO: If the tab remains open on a disconnect, the client should reestablish a websocket and say where it's up to
UserDisconnect == \E u \in User : userState[u].online
  /\ userState' = [userState EXCEPT ![u] = OfflineUser]
  /\ procs'     = {p \in procs : p.user /= u} \* TODO User disconnection doesn't interrupt the proc
  /\ pub'       = {usrEvt \in pub : usrEvt[1] /= u}
  /\ UNCHANGED << db, redis >>

ModRequest == \E u \in User : userState[u].online                  \* For an online user
  /\ \E r \in Request : \A i \in User : r \notin userState[i].reqs \* get a unique req Id
    /\ userState' = [userState EXCEPT ![u].reqs = @ \union {r}]
    /\ procs'     = procs \union {[user |-> u, req |-> r, status |-> "ready", ver |-> 0]}
    /\ UNCHANGED << db, redis, pub >>

Respond_ReadRedis(p) ==
  /\ p.status = "ready"
  /\ procs' = [procs EXCEPT ![p].ver = RedisVer, ![p].status = "post-read-redis"]
  /\ UNCHANGED << db, redis, pub, userState >>

Respond_ReadDB(p) ==
  /\ p.status = "post-read-redis"
  /\ p.ver = 0
  /\ procs' = [procs EXCEPT ![p].ver = db.ver, ![p].status = "init-redis"]
  /\ UNCHANGED << db, redis, pub, userState >>

Respond_InitRedis(p) ==
  /\ p.status = "init-redis"
  /\ IF p.ver >= RedisVer
     THEN /\ redis' = [ver |-> p.ver, events |-> {}]
          /\ procs' = [procs EXCEPT ![p].status = "create"]
     ELSE \* Redis has a more recent state than this proc
          /\ procs' = [procs EXCEPT ![p].status = "ready"]
          /\ UNCHANGED redis
  /\ UNCHANGED << db, userState >>

Respond_NewEventWriteDB(p) ==
  /\ p.status = "create"
  /\ IF p.ver = db.ver
     THEN /\ db'    = [ver |-> p.ver + 1]
          /\ procs' = [procs EXCEPT ![p].status = "post-create", ![p].ver = @ + 1]
     ELSE \* DB has been updated without our knowledge
          /\ procs' = [procs EXCEPT ![p].status = "ready"]
          /\ UNCHANGED db
  /\ UNCHANGED << redis, pub, userState >>

Respond_WriteRedis(p) ==
  /\ p.status = "post-create"
  /\ pub'     = pub \union { <<u, p.ver>> : u \in (\A u \in User : userState[u].online) }
  /\ redis'   = [ver |-> p.ver, events |-> {}] \* TODO: Write to Redis properly
  /\ procs'   = [procs EXCEPT ![p].status = "done"]
  /\ UNCHANGED << db, userState >>

Respond_Done(p) ==
  /\ p.status = "done"
  /\ IF userState[p.user].online
     THEN userState' = [userState EXCEPT ![p.user].reqs = Remove(@, p.req)]
     ELSE UNCHANGED userState
  /\ procs' = {\A q \in procs : q.req /= p.req }
  /\ UNCHANGED << db, redis, pub >>

ModRespond ==
  /\ procs /= {}
  /\ \E p \in procs :
    \/ Respond_ReadRedis(p)
    \/ Respond_ReadDB(p)
    \/ Respond_InitRedis(p)
    \/ Respond_NewEventWriteDB(p)
    \/ Respond_WriteRedis(p)
    \/ Respond_Done(p)

Publish ==
  /\ pub /= {}
  /\ \E <<u,v>> \in pub :
    \* TODO Remove ver check, add futureEvents to userState
    \/ IF userState[u].online /\ (v = 1 + userState[u].ver)
       THEN userState' = [userState EXCEPT ![u].ver = v]
       ELSE UNCHANGED userState
    \/ pub' = Remove(pub, <<u,v>>)
  /\ UNCHANGED << db, redis, procs >>


Action ==
  \/ UserConnect
  \/ UserDisconnect
  \/ ModRequest
  \/ ModRespond
  \/ Publish

------------------------------------------------------------------------------------------------------------------------

Spec == Init /\ [][Action]_<<vars>>

THEOREM  Spec => [](TypeInvariants /\ DataInvariants)

========================================================================================================================
