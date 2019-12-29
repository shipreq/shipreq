----------------------------------------------- MODULE websocket_client -----------------------------------------------

EXTENDS TLC

VARIABLES authorised, \* Whether the client considers itself authorised or not
          retry,      \* Whether more retries are allowed
          scheduled,  \* Whether a new connection has been scheduled
          ws          \* The current websocket state

vars == << authorised, retry, scheduled, ws >>

None       == "None"       \* None: Option[Instance]
Connecting == "Connecting" \* Some(ws) if ws.readyState = Connecting
Open       == "Open"       \* Some(ws) if ws.readyState = Open
Closing    == "Closing"    \* Some(ws) if ws.readyState = Closing
Closed     == "Closed"     \* Some(ws) if ws.readyState = Closed

TypeInvariants ==
  /\ authorised \in BOOLEAN
  /\ retry      \in BOOLEAN
  /\ scheduled  \in BOOLEAN
  /\ ws         \in {None, Connecting, Open, Closing, Closed}
  /\ PrintT([authorised |-> authorised, retry |-> retry, scheduled |-> scheduled, ws |-> ws])

DataInvariants ==
  /\ scheduled => ws \in {None, Closed}

Init ==
  /\ authorised = TRUE
  /\ retry \in BOOLEAN
  /\ scheduled = FALSE
  /\ ws = None

------------------------------------------------------------------------------------------------------------------------

relogin ==
  /\ Assert(~authorised /\ ws = None, "Relogin preconditions failed.")
  /\ \/ \* Success
        \* It's implicit in the spec but the next action is ConnectNow
        /\ retry' \in BOOLEAN \* reset retry status (counter)
        /\ authorised' = TRUE
        /\ UNCHANGED << scheduled, ws >>
     \/ \* Failure
        \* Becuase WebSockets don't have access to cookies, it will need to be an AJAX call to relogin
        \* meaning that retries are handled outside of this spec.
        /\ retry' = FALSE
        /\ UNCHANGED << authorised, scheduled, ws >>

scheduleReconnect(assertNotScheduled) ==
  IF retry
  THEN /\ assertNotScheduled => Assert(~scheduled, "Zombie scheduled task detected.")
       /\ scheduled' = TRUE
       /\ retry' \in BOOLEAN \* move on to next retry
       /\ UNCHANGED authorised
  ELSE /\ scheduled' = FALSE
       /\ UNCHANGED << authorised, retry >>

------------------------------------------------------------------------------------------------------------------------

WS_Open ==
  /\ ws = Connecting
  /\ ws' = Open
  /\ retry' \in BOOLEAN \* reset retry status (counter)
  /\ UNCHANGED << authorised, scheduled >>

WS_Closing ==
  /\ ws \in {Connecting, Open}
  /\ ws' = Closing
  /\ UNCHANGED << authorised, retry, scheduled >>

WS_Closed ==
  /\ ws \in {Connecting, Open, Closing}
  /\ \/ \* Connection lost, or server closes
        /\ ws' = Closed
        /\ scheduleReconnect(TRUE)
     \/ \* Server closes because JWT (is) expired
        /\ authorised' = FALSE
        /\ ws' = None
        /\ retry' = FALSE
        /\ UNCHANGED scheduled

ScheduledTaskExecutes ==
  /\ scheduled = TRUE
  /\ \/ \* Connection succeeds
        /\ scheduled' = FALSE
        /\ ws' = Connecting
        /\ UNCHANGED << authorised, retry >>
     \/ \* Connection fails
        /\ ws' = None
        /\ scheduleReconnect(FALSE)

ConnectNow ==
  /\ ws \in {None, Closed}
  /\ IF authorised
     THEN /\ TRUE \* clearTimer here
          /\ \/ \* Connection succeeds
                /\ ws' = Connecting
                /\ scheduled' = FALSE
                /\ retry' \in BOOLEAN \* reset retry status (counter)
                /\ UNCHANGED authorised
             \/ \* Connection fails
                /\ ws' = None
                /\ scheduleReconnect(FALSE) \* because clearTimer above
     ELSE relogin

Close ==
  /\ retry' = FALSE
  /\ IF ws = Open
     THEN ws' = Closing
     ELSE UNCHANGED << authorised, ws >>
  /\ UNCHANGED << authorised, scheduled >>

Next ==
  \/ WS_Open
  \/ WS_Closing
  \/ WS_Closed
  \/ ScheduledTaskExecutes
  \/ ConnectNow
  \/ Close

------------------------------------------------------------------------------------------------------------------------

Liveness ==
  /\ WF_vars(WS_Open)
  /\ WF_vars(ScheduledTaskExecutes)
  /\ WF_vars(ConnectNow)

Spec == Init /\ [][Next]_vars /\ Liveness

========================================================================================================================
