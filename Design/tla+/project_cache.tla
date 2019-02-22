------------------------------------------------- MODULE project_cache -------------------------------------------------

CONSTANT RequestId,
         User

VARIABLES db,    \* The state of the DB
          redis, \* The state of Redis
          proc,  \* The state of request processors (i.e. threads in webapps)
          user   \* Subscribed users' states
          
vars == << db, redis, proc, user >>

TypeInvariants ==
  /\ db    = {}
  /\ redis = {}
  /\ proc  = {}
  /\ user \in [User -> [
       online: BOOLEAN,
       reqs  : SUBSET RequestId \* Requests for which a response hasn't be received
       \* TODO: project, futureEvents
     ]]

OfflineUser == [online |-> FALSE, reqs |-> {}]

Init ==
  /\ db    = {}
  /\ redis = {}
  /\ proc  = {}
  /\ user  = [u \in User |-> OfflineUser]


------------------------------------------------------------------------------------------------------------------------

UserConnect == \E u \in User :
  /\ ~user[u].online
  /\ user' = [user EXCEPT ![u].online = TRUE]
  /\ UNCHANGED << db, redis, proc >>
  \* TODO new user needs the latest project
  
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
