------------------------------------------------- MODULE project_cache -------------------------------------------------

EXTENDS Naturals,
        TLC

CONSTANT Request,
         User,
         IncludeUserDisconnect

VARIABLES db,       \* The state of the DB
          redis,    \* The state of Redis
          procs,    \* The state of request processors (i.e. threads in webapps)
          pub,      \* Set of events being published
          userState \* Users' states

ASSUME IncludeUserDisconnect \in {TRUE,FALSE}

vars == << db, redis, procs, pub, userState >>

TypeInvariants ==
  /\ db \in [
       ver: Nat] \* The version of the Project aka the number of events

  /\ redis \in [
       ver   : Nat,        \* The version of a Project snapshot, or 0 if cache empty
       events: SUBSET Nat] \* A set of events represented by their version numbers

  /\ pub \in SUBSET (User \X Nat) \* Set of (target user, event)

  /\ procs \in SUBSET [
       req     : Request,
       status  : {"ReadRedis", "ReadDB", "WriteRedis1", "WriteDB", "WriteRedis2", "Done"},
       user    : User,
       redisVer: Nat, \* The version of Redis at the last read from Redis
       ver     : Nat] \* The version of the Project in memory (0=none)

  /\ userState \in [
       User -> [
         online : BOOLEAN,
         ver    : Nat,              \* The version of the built Project
         future : SUBSET Nat,       \* Future events that can't be applied cos intermediate event is missing
         reqs   : SUBSET Request ]] \* Requests for which a response hasn't be received

DataInvariants ==
  /\ \A u \in User :
    LET s == userState[u]
    IN /\ s.online => s.ver > 0
       /\ s.ver <= db.ver
       /\ \A e \in s.future : e > s.ver + 1
  /\ redis.ver <= db.ver
  /\ \A e \in redis.events :
    /\ redis.ver > 0 => \* Snapshot may have been evicted by Redis
       (e - 1) \in ({redis.ver} \union redis.events)
    /\ e <= db.ver

OfflineUser == [
  online |-> FALSE,
  ver    |-> 0,
  future |-> {},
  reqs   |-> {}]

Init ==
  /\ db        = [ver |-> 1]
  /\ redis     = [ver |-> 0, events |-> {}]
  /\ procs     = {}
  /\ pub       = {}
  /\ userState = [u \in User |-> OfflineUser]

------------------------------------------------------------------------------------------------------------------------

Remove(set, el) == {a \in set : a /= el}

Replace(set, old, new) == { IF a = old THEN new ELSE a : a \in set}

ApplyEvents[v \in Nat, es \in SUBSET Nat] ==
  LET n == v + 1
  IN IF n \in es
     THEN ApplyEvents[n, es \ {n}]
     ELSE <<v,es>>

RedisVer ==
  IF redis.events = {}
  THEN redis.ver
  ELSE CHOOSE e \in redis.events : \A e2 \in redis.events : e >= e2

OnlineUsers == {u \in User : userState[u].online}

------------------------------------------------------------------------------------------------------------------------

\* In reality this is: open page, establish websocket, receive project, subscribe to pub/sub channel
UserConnect == \E u \in User :
  /\ ~userState[u].online
  /\ \A p \in procs : p.user /= u \* A new user (connection) is distinct.
                                  \* If the model value is still being used in an orphan proc, it can be recycled here yet
  /\ userState' = [userState EXCEPT ![u].online = TRUE, ![u].ver = db.ver]
  /\ UNCHANGED << db, redis, procs, pub >>

\* This is the websocket being closed and not being restablished (i.e. user closes tab)
\* TODO: If the tab remains open on a disconnect, the client should reestablish a websocket and say where it's up to
UserDisconnect ==
  /\ IncludeUserDisconnect
  /\ \E u \in User : userState[u].online
    /\ userState' = [userState EXCEPT ![u] = OfflineUser]
    /\ pub'       = {usrEvt \in pub : usrEvt[1] /= u}
    /\ UNCHANGED << db, redis, procs >>

ModRequest == \E u \in User : userState[u].online                  \* For an online user
  /\ \E r \in Request : \A i \in User : r \notin userState[i].reqs \* get a unique req Id
    /\ userState' = [userState EXCEPT ![u].reqs = @ \union {r}]
    /\ procs'     = procs \union {[user |-> u, req |-> r, status |-> "ReadRedis", redisVer |-> 0, ver |-> 0]}
    /\ UNCHANGED << db, redis, pub >>

Respond_ReadRedis == procs /= {} /\ \E p \in procs :
  /\ p.status = "ReadRedis"
  /\ procs' = Replace(procs, p, [p EXCEPT !.ver      = RedisVer,
                                          !.redisVer = RedisVer,
                                          !.status   = IF RedisVer > p.ver THEN "WriteDB" ELSE "ReadDB"])
  /\ UNCHANGED << db, redis, pub, userState >>

Respond_ReadDB == procs /= {} /\ \E p \in procs :
  /\ p.status = "ReadDB"
  /\ procs' = Replace(procs, p, [p EXCEPT !.ver = db.ver, !.status = "WriteRedis1"])
  /\ UNCHANGED << db, redis, pub, userState >>

Respond_WriteRedis1 == \E p \in procs :
  /\ p.status = "WriteRedis1"
  /\ LET Continue == procs' = Replace(procs, p, [p EXCEPT !.status = "WriteDB"])
         Retry    == procs' = Replace(procs, p, [p EXCEPT !.status = "ReadRedis"])
         WriteSnapshot ==
           IF p.ver > RedisVer
           THEN /\ redis' = [ver |-> p.ver, events |-> {}]
                /\ Continue
           ELSE \* Redis has a more recent state than this proc
                /\ Retry
                /\ UNCHANGED redis
         WriteEvents ==
           LET firstEvent == p.redisVer + 1
               tryEvents  == firstEvent .. p.ver
               newEvents  == {e \in tryEvents : e > RedisVer}
           IN IF firstEvent + 1 > RedisVer \* Is there a missing event? Would this create a gap?
              THEN WriteSnapshot
              ELSE /\ redis' = [redis EXCEPT !.events = @ \union newEvents]
                   /\ Continue
     IN \/ WriteSnapshot
        \/ WriteEvents
  /\ UNCHANGED << db, pub, userState >>

Respond_WriteDB == procs /= {} /\ \E p \in procs :
  /\ p.status = "WriteDB"
  /\ \/ \* Request is valid
        /\ IF p.ver = db.ver
           THEN LET newVer == db.ver + 1
                IN /\ db'    = [ver |-> newVer]
                   /\ procs' = Replace(procs, p, [p EXCEPT !.status = "WriteRedis2", !.ver = newVer])
           ELSE \* DB has been updated without our knowledge; INSERT fails
                /\ procs' = Replace(procs, p, [p EXCEPT !.status = "ReadRedis"])
                /\ UNCHANGED db
        /\ UNCHANGED << redis, pub, userState >>
     \/ \* Request is invalid
        /\ procs' = Replace(procs, p, [p EXCEPT !.status = "Done"])
        /\ UNCHANGED << db, redis, pub, userState >>

Respond_WriteRedis2 == procs /= {} /\ \E p \in procs :
  /\ p.status = "WriteRedis2"
  /\ pub' = pub \union { <<p.user, p.ver>> }                \* Proc does this
                \union { <<u, p.ver>> : u \in OnlineUsers } \* Redis does this
  /\ \/ \* Send a snapshot to Redis
        IF p.ver > RedisVer
        THEN redis' = [ver |-> p.ver, events |-> {}]
        ELSE UNCHANGED redis
     \/ \* Send an event to Redis
        IF p.ver = RedisVer + 1
        THEN redis' = [redis EXCEPT !.events = @ \union {p.ver}]
        ELSE UNCHANGED redis
  /\ procs' = Replace(procs, p, [p EXCEPT !.status = "Done"])
  /\ UNCHANGED << db, userState >>

\* Responds to user
Respond_Done == procs /= {} /\ \E p \in procs :
  /\ p.status = "Done"
  /\ IF userState[p.user].online
     THEN userState' = [userState EXCEPT ![p.user].reqs = Remove(@, p.req)]
     ELSE UNCHANGED userState
  /\ procs' = Remove(procs, p)
  /\ UNCHANGED << db, redis, pub >>

ModRespond ==
  \/ Respond_ReadRedis
  \/ Respond_ReadDB
  \/ Respond_WriteRedis1
  \/ Respond_WriteDB
  \/ Respond_WriteRedis2
  \/ Respond_Done

Publish ==
  LET RecvEvent(s, v) ==
        IF v <= s.ver
        THEN s
        ELSE LET r == ApplyEvents[s.ver, s.future \union {v}]
             IN [s EXCEPT !.ver = r[1], !.future = r[2]]
  IN
    /\ pub /= {}
    /\ \E <<u,v>> \in pub :
      /\ IF userState[u].online
         THEN userState' = [userState EXCEPT ![u] = RecvEvent(@, v)]
         ELSE UNCHANGED userState
      /\ pub' = Remove(pub, <<u,v>>)
      /\ UNCHANGED << db, redis, procs >>

RedisEviction ==
  /\ \/ redis' = [redis EXCEPT !.ver = 0]
     \/ redis' = [redis EXCEPT !.events = {}]
  /\ UNCHANGED << db, procs, pub, userState >>

ActionAct ==
  \/ UserConnect
  \/ ModRequest
  \/ RedisEviction
  \/ UserDisconnect

ActionReact ==
  \/ ModRespond
  \/ Publish

Action == ActionAct \/ ActionReact

------------------------------------------------------------------------------------------------------------------------

Fairness ==
  /\ WF_vars(ModRequest)
  /\ SF_vars(ModRespond)
  /\ SF_vars(Publish)

Spec == Init /\ [][Action]_<<vars>> /\ Fairness

THEOREM  Spec => [](TypeInvariants /\ DataInvariants)

------------------------------------------------------------------------------------------------------------------------

NothingInFlight ==
  /\ procs = {}
  /\ pub = {}
  /\ \A u \in User : userState[u].reqs = {}

AllUsersUpToDate ==
  \A u \in User : userState[u].online => userState[u].ver = db.ver

CONSTANT MCVerLimit

MCSymmetry        == Permutations(User) \union Permutations(Request)
MCLimitReached    == db.ver >= MCVerLimit
MCDone            == MCLimitReached /\ NothingInFlight
MCFinalInvariants == MCDone => AllUsersUpToDate
MCContinue        == ~MCDone
MCAction          == (~MCLimitReached /\ ActionAct) \/ ActionReact
MCSpec            == Init /\ [][MCAction]_<<vars>> /\ Fairness

========================================================================================================================
