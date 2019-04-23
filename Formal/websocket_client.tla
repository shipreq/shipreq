----------------------------------------------- MODULE websocket_client -----------------------------------------------

EXTENDS TLC

VARIABLES retry,     \* Whether more retries are allowed
          scheduled, \* Whether a new connection has been scheduled
          ws         \* The current websocket state

vars == << retry, scheduled, ws >>

None       == "None"
Connecting == "Connecting"
Open       == "Open"
Closing    == "Closing"
Closed     == "Closed"

TypeInvariants ==
  /\ retry     \in BOOLEAN
  /\ scheduled \in BOOLEAN
  /\ ws        \in {None, Connecting, Open, Closing, Closed}
\*  /\ PrintT([retry |-> retry, scheduled |-> scheduled, ws |-> ws])

DataInvariants ==
  /\ scheduled => ws \in {None, Closed}

Init ==
  /\ retry \in BOOLEAN
  /\ scheduled = FALSE
  /\ ws \in {None, Connecting}

------------------------------------------------------------------------------------------------------------------------

WS_Open ==
  /\ ws = Connecting
  /\ ws' = Open
  /\ retry' \in BOOLEAN \* reset retry status (counter)
  /\ UNCHANGED << scheduled >>

WS_Closing ==
  /\ ws \in {Connecting, Open}
  /\ ws' = Closing
  /\ UNCHANGED << retry, scheduled >>

scheduleReconnect ==
 IF retry
 THEN /\ scheduled' = TRUE
      /\ retry' \in BOOLEAN \* move on to next retry
 ELSE /\ scheduled' = FALSE
      /\ UNCHANGED retry

WS_Closed ==
  /\ ws \in {Connecting, Open, Closing}
  /\ ws' = Closed
  /\ scheduleReconnect

Schedule ==
  /\ scheduled = TRUE
  /\ \/ \* Connection succeeds
        /\ scheduled' = FALSE
        /\ ws' = Connecting
        /\ UNCHANGED << retry >>
     \/ \* Connection fails
        /\ ws' = None
        /\ scheduleReconnect

ConnectNow ==
  /\ ws \in {None,Closed}
  \* clearTimer here
  /\ \/ \* Connection succeeds
        /\ ws' = Connecting
        /\ scheduled' = FALSE
        /\ retry' \in BOOLEAN \* reset retry status (counter)
     \/ \* Connection fails
        /\ ws' = None
        /\ scheduleReconnect

Next ==
  \/ WS_Open
  \/ WS_Closing
  \/ WS_Closed
  \/ Schedule
  \/ ConnectNow

------------------------------------------------------------------------------------------------------------------------

Liveness ==
  /\ WF_vars(WS_Open)
  /\ WF_vars(Schedule)
  /\ WF_vars(ConnectNow)

Spec == Init /\ [][Next]_vars /\ Liveness

========================================================================================================================
