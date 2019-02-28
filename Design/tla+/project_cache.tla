------------------------------------------------- MODULE project_cache -------------------------------------------------

EXTENDS Naturals,
        TLC

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
       status: {"ready", "read-db", "init-redis", "create", "post-create", "done"},
       user  : User,
       ver   : Nat] \* The version of the Project in memory (0=none)

  /\ userState \in [
       User -> [
         online : BOOLEAN,
         ver    : Nat,              \* The version of the built Project
         future : SUBSET Nat,       \* Future events that can't be applied cos intermediate event is missing
         reqs   : SUBSET Request ]] \* Requests for which a response hasn't be received

RedisVer == IF redis.events = {}
            THEN redis.ver
            ELSE CHOOSE e \in redis.events : \A f \in redis.events : e >= f

DataInvariants ==
  /\ \A u \in User :
    LET s == userState[u]
    IN /\ s.online => s.ver > 0
       /\ s.ver <= db.ver
       /\ \A e \in s.future : e > s.ver + 1
  /\ \A e \in redis.events : e - 1 \in ({redis.ver} \union redis.events)
  /\ RedisVer <= db.ver

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

------------------------------------------------------------------------------------------------------------------------

\* In reality this is: open page, establish websocket, receive project, subscribe to pub/sub channel
UserConnect == \E u \in User :
  /\ ~userState[u].online
  /\ \A p \in procs : p.user /= u \* This is a model constraint, not a reflection of reality - TODO think
  /\ userState' = [userState EXCEPT ![u].online = TRUE, ![u].ver = db.ver]
  /\ UNCHANGED << db, redis, procs, pub >>

\* This is the websocket being closed and not being restablished (i.e. user closes tab)
\* TODO: If the tab remains open on a disconnect, the client should reestablish a websocket and say where it's up to
UserDisconnect == \E u \in User : userState[u].online
  /\ userState' = [userState EXCEPT ![u] = OfflineUser]
  /\ pub'       = {usrEvt \in pub : usrEvt[1] /= u}
  /\ UNCHANGED << db, redis, procs >>

ModRequest == \E u \in User : userState[u].online                  \* For an online user
  /\ \E r \in Request : \A i \in User : r \notin userState[i].reqs \* get a unique req Id
    /\ userState' = [userState EXCEPT ![u].reqs = @ \union {r}]
    /\ procs'     = procs \union {[user |-> u, req |-> r, status |-> "ready", ver |-> 0]}
    /\ UNCHANGED << db, redis, pub >>

Respond_ReadRedis == procs /= {} /\ \E p \in procs :
  /\ p.status = "ready"
  /\ procs' = Replace(procs, p, [p EXCEPT !.ver = RedisVer, !.status = IF RedisVer > p.ver
                                                                       THEN "create"
                                                                       ELSE "read-db"])
  /\ UNCHANGED << db, redis, pub, userState >>
\*  /\ PrintT([db |-> db.ver, redis |-> redis.ver, proc |-> p.ver])

Respond_ReadDB == procs /= {} /\ \E p \in procs :
  /\ p.status = "read-db"
  /\ procs' = Replace(procs, p, [p EXCEPT !.ver = db.ver, !.status = "init-redis"])
  /\ UNCHANGED << db, redis, pub, userState >>

Respond_InitRedis == \E p \in procs :
  /\ p.status = "init-redis"
  /\ IF p.ver >= RedisVer
     THEN /\ redis' = [ver |-> p.ver, events |-> {}]
          /\ procs' = Replace(procs, p, [p EXCEPT !.status = "create"])
     ELSE \* Redis has a more recent state than this proc
          /\ procs' = Replace(procs, p, [p EXCEPT !.status = "ready"])
          /\ UNCHANGED redis
  /\ UNCHANGED << db, pub, userState >>

Respond_NewEventWriteDB == procs /= {} /\ \E p \in procs :
  /\ p.status = "create"
  /\ IF p.ver = db.ver
     THEN /\ db'    = [ver |-> p.ver + 1]
          /\ procs' = Replace(procs, p, [p EXCEPT !.status = "post-create", !.ver = @ + 1])
     ELSE \* DB has been updated without our knowledge
          /\ procs' = Replace(procs, p, [p EXCEPT !.status = "ready"])
          /\ UNCHANGED db
  /\ UNCHANGED << redis, pub, userState >>

Respond_WriteRedis == procs /= {} /\ \E p \in procs :
  /\ p.status = "post-create"
  /\ pub'     = pub \union { <<u, p.ver>> : u \in {u \in User : userState[u].online} }
  /\ redis'   = [ver |-> p.ver, events |-> {}] \* TODO: Write to Redis properly
  /\ procs'   = Replace(procs, p, [p EXCEPT !.status = "done"])
  /\ UNCHANGED << db, userState >>

\* Responds to user
Respond_Done == procs /= {} /\ \E p \in procs :
  /\ p.status = "done"
  /\ IF userState[p.user].online
     THEN userState' = [userState EXCEPT ![p.user].reqs = Remove(@, p.req)]
     ELSE UNCHANGED userState
  /\ procs' = Remove(procs, p)
  /\ UNCHANGED << db, redis, pub >>

ModRespond ==
  \/ Respond_ReadRedis
  \/ Respond_ReadDB
  \/ Respond_InitRedis
  \/ Respond_NewEventWriteDB
  \/ Respond_WriteRedis
  \/ Respond_Done

Publish ==
  LET ApplyFutureEvents[v \in Nat, f \in SUBSET Nat] ==
        LET n == v + 1
        IN IF n \in f
           THEN ApplyFutureEvents[n, f \ {n}]
           ELSE <<v,f>>
      RecvEvent(s, v) ==
        IF v <= s.ver
        THEN s
        ELSE LET r == ApplyFutureEvents[s.ver, s.future \union {v}]
             IN [s EXCEPT !.ver = r[1], !.future = r[2]]
  IN
    /\ pub /= {}
    /\ \E <<u,v>> \in pub :
      /\ PrintT(userState[u])
      /\ IF userState[u].online
         THEN userState' = [userState EXCEPT ![u] = RecvEvent(@, v)]
         ELSE UNCHANGED userState
      /\ pub' = Remove(pub, <<u,v>>)
      /\ UNCHANGED << db, redis, procs >>

ActionAct ==
  \/ UserConnect
  \/ UserDisconnect
  \/ ModRequest

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
