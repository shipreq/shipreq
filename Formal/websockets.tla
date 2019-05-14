-------------------------------------------------- MODULE websockets --------------------------------------------------

(* PURPOSE:

Use cases
=========
- User starts page
  Init WS
  Request initial state
  (retry until ok?)

- User connected
  Updates sent
  Connection lost
  Retry until re-establish
  On re-establish, re-sync
*)

EXTENDS TLC

VARIABLES cli, svr

vars == << cli, svr >>

connecting == "connecting"
open       == "open      "
closing    == "closing   "
closed     == "closed    "

WSStates == {connecting, open, closing, closed}

TypeInvariants ==
  /\ PrintT([cli |-> cli, svr |-> svr])
  /\ cli \in WSStates
  /\ svr \in WSStates

Init ==
  /\ cli = closed
  /\ svr = closed

-----------------------------------------------------------------------------------------

ClientConnect ==
  /\ cli = closed
  /\ svr = closed
  /\ cli' = connecting
  /\ UNCHANGED svr

ClientConnected ==
  /\ cli = connecting
  /\ svr \in {connecting,open}
  /\ cli' = open
  /\ UNCHANGED svr

ClientClose ==
  /\ cli /= closed
  /\ cli' = closing
  /\ UNCHANGED svr

ClientClosed ==
  /\ cli' = closed
  /\ UNCHANGED svr

Client ==
  \/ ClientConnect
  \/ ClientConnected
  \/ ClientClose
  \/ ClientClosed

-----------------------------------------------------------------------------------------

ServerConnect ==
  /\ svr = closed
  /\ cli = connecting
  /\ svr' = connecting
  /\ UNCHANGED cli

ServerConnected ==
  /\ svr = connecting
  /\ cli \in {connecting,open}
  /\ svr' = open
  /\ UNCHANGED cli

ServerClose ==
  /\ svr /= closed
  /\ svr' = closing
  /\ UNCHANGED cli

ServerClosed ==
  /\ svr' = closed
  /\ UNCHANGED cli

Server ==
  \/ ServerConnect
  \/ ServerConnected
  \/ ServerClose
  \/ ServerClosed

-----------------------------------------------------------------------------------------

Next ==
  \/ Client
  \/ Server

Spec == Init /\ [][Next]_vars

========================================================================================================================
