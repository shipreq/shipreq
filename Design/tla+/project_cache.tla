------------------------------------------------- MODULE project_cache -------------------------------------------------

EXTENDS Naturals

CONSTANT RequestId,
         User

VARIABLES db,    \* The state of the DB
          redis, \* The state of Redis
          proc,  \* The state of request processors (i.e. threads in webapps)
          user   \* Subscribed users' states
          
vars == << db, redis, proc, user >>

TypeInvariants ==
  /\ db    \in [ver: Nat] \* The version of the Project aka the number of events
  /\ redis = {}
  /\ proc  = {}
  /\ user  \in [User -> [
                  online : BOOLEAN,
                  ver    : Nat,                \* The version of the built Project
                  reqs   : SUBSET RequestId ]] \* Requests for which a response hasn't be received
  /\ \A u \in User : user[u].online => user[u].ver > 0

OfflineUser == [
  online |-> FALSE,
  reqs   |-> {},
  ver    |-> 0]

Init ==
  /\ db    = [ver |-> 1]
  /\ redis = {}
  /\ proc  = {}
  /\ user  = [u \in User |-> OfflineUser]


------------------------------------------------------------------------------------------------------------------------

UserConnect == \E u \in User :
  /\ ~user[u].online
  /\ user' = [user EXCEPT ![u].online = TRUE, ![u].ver = db.ver]
  /\ UNCHANGED << db, redis, proc >>
  
UserDisconnect == \E u \in User :
  /\ user[u].online
  /\ user' = [user EXCEPT ![u] = OfflineUser]
  /\ UNCHANGED << db, redis, proc >>


\*RequestMod ==
\*  /\ \E reqId \in RequestId : 
\*  /\ UNCHANGED << db, redis >>

\* DbSelectEvents
\* DbInsertEvent
\* RespondModOK
\* RespondModFailed

Action ==
  \/ UserConnect
  \/ UserDisconnect

------------------------------------------------------------------------------------------------------------------------

Spec == Init /\ [][Action]_<<vars>>

THEOREM  Spec => []TypeInvariants 

========================================================================================================================
