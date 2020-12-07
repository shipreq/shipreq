---------------------------------------------------- MODULE drafts ----------------------------------------------------
(*
What's does this spec provide?
==============================
- Drafts should eventually propagate to all live tabs
- Drafts are never lost unless they're merged (manually or automatically), or completed by a user (whether aborted or committed)
- Drafts are removed from all storage locations once obsolete
- Users are prompted to keep a tab open iff we can't guarantee the draft won't be lost
- Reliability in the face of failures such as
  - network packet loss
  - machines crashing or dying without warning

See https://shipreq.com/project/d6My#/reqs/DE-5


How does it work?
=================

+--------------------------------------------------+
|                    Browser                       |
|                                                  |
|                                                  |
|  ------------+                                   |
|  | indexedDB |                                   |
|  +-----------+                                   |
|        ↕                                         |                     +----------------+
|   +----------+                       +-------+   |                     |                |
|   |          |     +-----------+     |       |   |   +-----------+     |     Remote     |
|   |  Worker  |<--->|  Network  |<--->|  Tab  |<----->|  Network  |<--->|                |
|   |          |     +-----------+     |       |   |   +-----------+     |   (database)   |
|   +----------+                       +-------+   |                     |                |
|        ↕                                         |                     +----------------+
| +--------------+                                 |
| | localStorage |                                 |
| +--------------+                                 |
+--------------------------------------------------+

Important notes
===============

- WW time always starts at 1
- Provenance maps have keys for all workers but when the value=0 it means the K:V entry doesn't really exist in the map

TODO
====
- Add a user-write budget (maybe)
*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Browser
CONSTANT BrowserSrcAsync
CONSTANT BrowserSrcSync
CONSTANT Tab
CONSTANT Worker

\* Legal combinations of {browser, worker, tab}
CONSTANT Assignments

CONSTANT MCBrowserStorageAlwaysAvailable

ASSUME & IsFiniteSet(Browser)
       & IsFiniteSet(BrowserSrcAsync)
       & IsFiniteSet(BrowserSrcSync)
       & IsFiniteSet(Tab)
       & IsFiniteSet(Worker)
       & IsFiniteSet(Assignments)
       & Cardinality(Worker) <= Cardinality(Tab) \* each tab is assigned a worker. More workers than tabs is useless.
       & Cardinality(Worker) >= Cardinality(Browser) \* 2w in 1b = diff versions, 1w in 2b doesn't make sense
       & MCBrowserStorageAlwaysAvailable \in BOOLEAN

MCSymmetry ==
  SymmetrySets(<<
    Browser,
    BrowserSrcAsync,
    BrowserSrcSync,
    Tab,
    Worker
  >>)

VARIABLE browsers
VARIABLE network
VARIABLE remote
VARIABLE tabs
VARIABLE workers

vars == << browsers, network, remote, tabs, workers >>

state == [
  browsers |-> browsers,
  network  |-> network,
  remote   |-> remote,
  tabs     |-> tabs,
  workers  |-> workers]

LogStates ==
  Log(state)

clean          == "clean"
conflicted     == "conflicted"
dirty          == "dirty"
live           == "live"
nonExistant    == "-"
server         == "server"
loading        == "loading"
Remote         == "Remote"
syncTW         == "sync:T->W"
syncWT         == "sync:W->T"
syncTR         == "sync:T->R"
syncRT         == "sync:R->T"
RemoteStoreCmd == "RemoteStoreCmd"
ackRT          == "ack:R->T"
ackTW          == "ack:T->W"

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Invariants

Provenance  == [Worker -> Nat]                               \* i.e. Map[WorkerId, Time]
Draft       == [worker: Worker, time: Nat, prov: Provenance] \* no need to include draft content
Drafts      == SUBSET Draft                                  \* i.e. Set[Draft]
DraftsNE    == Drafts -- {{}}                                \* i.e. NonEmptySet[Draft]

IsValidAssignment(b, w, t) ==
  {b,w,t} \in Assignments

Msg == [
  type   : {syncTW},
  from   : Tab,
  to     : Worker,
  drafts : Drafts,
  newEdit: Option(Provenance)
] ++ [
  type   : {syncWT},
  from   : Worker,
  to     : Tab,
  drafts : Drafts,
  newEdit: Option(Draft)
] ++ [
  type   : {syncTR},
  from   : Tab,
  to     : {Remote},
  drafts : DraftsNE,
  id     : Nat
] ++ [
  type   : {RemoteStoreCmd},
  from   : Worker,
  to     : Tab,
  drafts : Drafts,
  id     : Nat
] ++ [
  type   : {ackRT},
  from   : {Remote},
  to     : Tab,
  id     : Nat
] ++ [
  type   : {ackTW},
  from   : Tab,
  to     : Worker,
  id     : Nat
] ++ [
  type   : {syncRT},
  from   : {Remote},
  to     : Tab,
  drafts : DraftsNE
]

NetworkState ==
  Seq(Msg) \* i.e. List[Msg]

BrowserSrc ==
  BrowserSrcAsync ++ BrowserSrcSync

BrowserState ==
  [BrowserSrc -> Option(Drafts)] \* None means not supported by browser

AnySrc ==
  BrowserSrc ++ {Remote}

ActiveTabStatus ==
  {clean, dirty, conflicted}

TabState ==
  [ status: {nonExistant}] ++
  [
    status  : {loading},
    worker  : Worker,
    drafts  : Drafts,
    awaiting: SUBSET AnySrc
  ] ++
  [
    status: {clean},
    worker: Worker
  ] ++
  [
    status     : {dirty},
    worker     : Worker,
    draft      : Option(Draft), \* last known draft for editor state
    localChange: BOOLEAN        \* editor has change that hasn't been sent to WW yet
  ] ++
  [
    status     : {conflicted},
    worker     : Worker,
    drafts     : { ds \in Drafts : Cardinality(ds) > 1 },
    localChange: BOOLEAN \* editor has change that hasn't been sent to WW yet
  ]

WorkerSyncState ==
  [
    desired: Nat, \* The promise id we want ack'd to know that we've synced
    lastReq: Nat, \* The last promise id issued
    lastAck: Nat  \* The highest ack'd promise id
  ]

WorkerState ==
  [status: {nonExistant}] ++
  [
    status        : {live},
    browser       : Browser,
    time          : Nat,
    drafts        : Drafts,
    sync          : [AnySrc -> WorkerSyncState]
  ]

StorageInvariants(s) ==
  & Assert1(
      Cardinality(s) = Cardinality({d.worker : d \in s}),
      "Duplicate drafts/worker:", s)
  & \A d \in s: Assert1(
      d.prov[d.worker] = 0,
      "Draft contains itself in its own provenance:", d)

WorkerSyncStateInvariants(s) ==
  & s.lastReq <= s.desired
  & s.lastAck <= s.lastReq

InvariantsForBrowsers ==
  & browsers \in [Browser -> BrowserState]
  & \A b \in Browser :
      LET bs == browsers[b]
      IN \A src \in BrowserSrc:
        bs[src].isEmpty | StorageInvariants(bs[src].get)

InvariantsForNetwork ==
  network \in NetworkState

InvariantsForRemote ==
  & remote \in Drafts
  & StorageInvariants(remote)

InvariantsForTabs ==
  tabs \in [Tab -> TabState]

InvariantsForWorkers ==
  & workers \in [Worker -> WorkerState]
  & \A w \in Worker :
      LET ws == workers[w]
      IN ws.status = live =>
          & ws.time > 0
          & \A s \in AnySrc : WorkerSyncStateInvariants(ws.sync[s])

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

WorkerSyncStateIsStable(s) ==
  & s.lastReq = s.desired
  & s.lastAck = s.lastReq

WorkerSyncStateEmpty ==
  [desired |-> 0, lastReq |-> 0, lastAck |-> 0]

\* WorkerSyncState => Option[WorkerSyncState]
WorkerSyncStart(s) ==
  LET doIt == & s.desired > s.lastReq \* we have new content to sync
              & s.lastAck = s.lastReq \* wait for ACK so that model checking isn't infinite
      next == [s EXCEPT !.lastReq = s.desired]
  IN SomeWhen(doIt, next)

WorkerSyncAck(s, id) ==
  IF id <= s.lastReq & id != s.lastAck THEN
    [s EXCEPT !.lastAck = Max[@, id]]
  ELSE
    Fail1("Invalid ACK", [syncState |-> s, id |-> id])

WorkerSyncLater(syncState) ==
  LET f(s) == IF s.desired > s.lastReq THEN s ELSE [s EXCEPT !.desired = @ + 1]
  IN [src \in AnySrc |-> f(syncState[src])]

StartWorker(b) ==
  [
    status  |-> live,
    browser |-> b,
    time    |-> 1,
    drafts  |-> {}, \* TODO load in stages just like new tabs (?)
    sync    |-> [s \in AnySrc |-> WorkerSyncStateEmpty]
  ]

Connected(tab, worker) ==
  & tabs[tab].status != nonExistant
  & tabs[tab].worker = worker

WorkerTabs(w) ==
  { t \in Tab : Connected(t, w) }

ActiveTabs ==
  { t \in Tab : tabs[t].status \in ActiveTabStatus }

RemoteConnectedTabs ==
  LET statuses == ActiveTabStatus ++ {loading}
  IN { t \in Tab : tabs[t].status \in statuses }

TabDrafts(t) ==
  LET ts == tabs[t]
      s  == ts.status
  IN CASE s = nonExistant -> {}
       [] s = loading     -> ts.drafts
       [] s = clean       -> {}
       [] s = dirty       -> OptionToSet(ts.draft)
       [] s = conflicted  -> ts.drafts

TabHasLocalChange(tOrTS) ==
  LET ts == IF tOrTS \in Tab THEN tabs[tOrTS] ELSE tOrTS
      s  == ts.status
  IN CASE s = nonExistant -> FALSE
       [] s = loading     -> FALSE
       [] s = clean       -> FALSE
       [] s = dirty       -> ts.localChange
       [] s = conflicted  -> ts.localChange

TabStateWithLocalChange(ts, lc) ==
  LET s  == ts.status
  IN CASE s = nonExistant -> ts
       [] s = loading     -> ts
       [] s = clean       -> ts
       [] s = dirty       -> [ts EXCEPT !.localChange = lc]
       [] s = conflicted  -> [ts EXCEPT !.localChange = lc]

NewTabState(w, prunedDrafts, localChange) ==
  LET cleanState    == [worker |-> w, status |-> clean]
      dirtyState(d) == [worker |-> w, status |-> dirty, draft |-> Some(d), localChange |-> localChange]
      conflictState == [worker |-> w, status |-> conflicted, drafts |-> prunedDrafts, localChange |-> localChange]
      soleDraft     == SetSoleElement(prunedDrafts)
  IN
    IF prunedDrafts = {} THEN
      cleanState
    ELSE IF ~soleDraft.isEmpty THEN
      dirtyState(soleDraft.get)
    ELSE
      conflictState

TabStateWithDrafts(t, drafts) ==
  LET ts     == tabs[t]
      s      == ts.status
      lc     == TabHasLocalChange(t)
  IN CASE s = nonExistant       -> ts
       [] s = loading           -> [ts EXCEPT !.drafts = drafts]
       [] s \in ActiveTabStatus -> NewTabState(ts.worker, drafts, lc)

\* Set[(Browser, BrowserSrc)]
AvailableBrowserStores ==
  { x \in Browser \X BrowserSrc : ~browsers[x[1]][x[2]].isEmpty }

ActiveWorkers ==
  { w \in Worker : workers[w].status != nonExistant }

\* Set[Storage]
AllStores ==
  LET draftsB == { browsers[x[1]][x[2]].get : x \in AvailableBrowserStores }
      draftsT == { TabDrafts(t) : t \in ActiveTabs }
      draftsW == { workers[w].drafts : w \in ActiveWorkers }
      draftsR == { remote }
  IN draftsB ++ draftsT ++ draftsW ++ draftsR

\* Set[Draft]
AllDrafts ==
  UNION AllStores

SendMsg(msg) ==
  network' = Append(network, msg)

RecvMsg(i) ==
  network' = RemoveAt(network, i)

RecvResp(recv, resp) ==
  network' = Append(RemoveAt(network, recv), resp)

NewDraft(w, prevProv) ==
  [
    worker |-> w,
    time   |-> workers[w].time,
    prov   |-> [prevProv EXCEPT ![w] = 0]
  ]

NoProv ==
  [w \in Worker |-> 0]

MergeProvs(p1, p2) ==
  [w \in Worker |-> Max[p1[w], p2[w]]]

AddProv(draft, prov) ==
  [draft EXCEPT !.prov = MergeProvs(@, prov)]

AddSelfToOwnProv(draft) ==
  LET w == draft.worker
  IN  [draft EXCEPT !.prov[w] = draft.time]

(* NOTE: Doesn't prune *)
AddDraft(storage, draft) ==
  LET sibling == SetFind(storage, LAMBDA d: d.worker = draft.worker)
  IN IF sibling.isEmpty THEN
       storage ++ {draft}
     ELSE
      LET s   == sibling.get
          new == IF s.time > draft.time THEN s ELSE draft
          old == IF s.time > draft.time THEN draft ELSE s
      IN IF s = draft
         THEN storage
         ELSE (storage -- {s}) ++ {AddProv(new, old.prov)}

(* NOTE: Doesn't prune *)
AddDrafts(storage, drafts) ==
  SetFold(drafts, storage, AddDraft)

(*
Prunes according to provenance. Eg.
  {
    (w1:4, prov: {w1:0, w2:3, w3:0})
    (w2:3, prov: {w1:0, w2:0, w3:1}) <-- prunable cos w2:3 <= w2:3 in prov above
    (w3:2, prov: {w1:0, w2:0, w3:0})
  }
is pruned to
  {
    (w1:4, prov: {w1:0, w2:3, w3:1}) <-- inherits w3:1 from removed draft's provenance
    (w3:2, prov: {w1:0, w2:0, w3:0})
  }
or if w1 and w3 drafts have the same content:
  {
    (w1:4, prov: {w1:0, w2:3, w3:2})
  }
*)
RECURSIVE _PruneByProv(_, _)
_PruneByProv(ds, depth) ==

  \* LET f2(d1, d2) ==
  \*       LET pt     == d1.prov[d2.worker]
  \*           byProv == d1.worker != d2.worker & pt > 0 & d2.time <= pt
  \*           bySrc  == d1.worker = d2.worker & d1.time > d2.time
  \*       IN SomeWhen(bySrc | byProv, <<d1, d2>>)
  \*     f(d1) == SetCollectFirst(ds, LAMBDA d2: f2(d1, d2))
  \*     match == SetCollectFirst(ds, f)

  LET mergeByProv(x,y) == LET pt == x.prov[y.worker]
                          IN x.worker != y.worker & pt > 0 & y.time <= pt
      mergeBySrc(x,y)  == x.worker = y.worker & x.time > y.time
      merge(xy)        == LET x == xy[1]
                              y == xy[2]
                          IN mergeByProv(x,y) | mergeBySrc(x,y)
      pairs            == { x \in (ds \X ds) : x[1] != x[2] }
      match            == SetFind(pairs, merge)
  IN
    IF depth >= 3 THEN
      Fail("Maximum depth of PruneByProv exceeded")
    ELSE IF match.isEmpty THEN
      ds
    ELSE
      LET d1  == match.get[1]
          d2  == match.get[2]
          d   == AddProv(d1, AddSelfToOwnProv(d2).prov) \* TODO Need AddSelfToOwnProv here?
          ds2 == (ds -- {d1, d2}) ++ {d}
      IN \* LogRet(
        \* [BEFORE |-> ds, AFTER |-> ds2],
        \* IF Cardinality(ds2) <= 1 THEN ds2 ELSE
        _PruneByProv(ds2, depth + 1)
      \* )
PruneByProv(ds) == _PruneByProv(ds, 1)

\* Returns a set of possible outcomes
PruneByEq(ds) ==
  LET equalSets    == { x \in SUBSET(ds) : Cardinality(x) > 1 } \* Set[Set[Drafts]]
      merge(x, y)  == AddProv(x, AddSelfToOwnProv(y).prov)
      mergeAll(es) == SetReduce(es, merge)
      result       == { (ds -- es) ++ {mergeAll(es)} : es \in equalSets } ++ {ds}
  IN
    \* LogRet([ BEFORE |-> ds, AFTER |-> result ], result)
    result

\* Returns a set of possible outcomes
Prune(drafts) ==
  {PruneByProv(ds) : ds \in PruneByEq(drafts)}

\* Returns multiple possibilities in the form of Set[(tabs',Seq[NetworkMsg])]
AddDraftsToTab(t, ds, canResolveLocalChanges) ==
  LET ts2          == TabStateWithDrafts(t, ds)
      lc2          == TabHasLocalChange(ts2)
      w            == ts2.worker
      localChanges == IF canResolveLocalChanges & lc2 THEN
                        BOOLEAN \* Maybe the new draft clears out the status, maybe there are more changes since
                      ELSE
                        {lc2}
      msgWW(ts3)   == [
                        type    |-> syncTW,
                        from    |-> t,
                        to      |-> w,
                        drafts  |-> ds,
                        newEdit |-> IF TabHasLocalChange(ts3) & ts3.status = dirty
                                    THEN OptionMap(ts3.draft, LAMBDA d: d.prov)
                                    ELSE None
                      ]
  IN {
    LET ts3   == TabStateWithLocalChange(ts2, lc3)
        tabs3 == [tabs EXCEPT ![t] = ts3]
        msgs  == IF ds = TabDrafts(t) THEN <<>> ELSE <<msgWW(ts3)>>
    IN << tabs3, msgs >>
    : lc3 \in localChanges
  }

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Tests

PruneTest ==
  LET w1     == CHOOSE w \in Worker : TRUE
      w2     == CHOOSE w \in Worker : w != w1
      input  == {[worker |-> w1, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 1)],
                 [worker |-> w2, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 0)]}
      exp1   == {[worker |-> w1, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 1)]}
      expect == {exp1}
      actual == Prune(input)
  IN (Cardinality(Worker) >= 2) => Assert1(actual = expect, "Prune test failed.", actual)

SanityCheck ==
  & UtilSanityCheck
  & PruneTest

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Actions

RemoteRecvDrafts ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncTR)
  IN
    & i != 0
    & LET msg == network[i]
          resp == [
            type |-> ackRT,
            from |-> Remote,
            to   |-> msg.from,
            id   |-> msg.id
          ]
          broadcastTo(tab, ds) == [
            type   |-> syncRT,
            from   |-> Remote,
            to     |-> tab,
            drafts |-> ds
          ]
          otherTabs      == { t \in RemoteConnectedTabs : t != msg.from }
          broadcasts(ds) == SetFold(otherTabs, <<>>, LAMBDA q,t: q \o <<broadcastTo(t, ds)>>)
          result(ds)     == <<ds, <<resp>> \o broadcasts(ds)>>
          dss            == \*LogRet([ADD |-> AddDrafts(remote, msg.drafts), PRU |-> Prune(AddDrafts(remote, msg.drafts))],
                            Prune(AddDrafts(remote, msg.drafts))
          results        == { result(ds) : ds \in dss }
      IN \E r \in results:
        & remote' = r[1]
        & network' = RemoveAt(network, i) \o r[2]
        & UNCHANGED << browsers, tabs, workers >>

TabRecvDraftsFromRemote ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncRT)
  IN
    & i != 0
    & LET msg     == network[i]
          t       == msg.to
          ts      == tabs[t]
          w       == ts.worker
          dss     == Prune(AddDrafts(TabDrafts(t), msg.drafts))
          net1    == RemoveAt(network, i)
          results == UNION { AddDraftsToTab(t, ds, FALSE) : ds \in dss }
      IN \E r \in results:
        & tabs'    = r[1]
        & network' = net1 \o r[2]
        & UNCHANGED << browsers, remote, workers >>

TabRecvDraftsFromWorker ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncWT)
  IN
    & i != 0
    & LET msg     == network[i]
          t       == msg.to
          ts      == tabs[t]
          w       == ts.worker
          dss     == Prune(AddDrafts(TabDrafts(t), msg.drafts))
          net1    == RemoveAt(network, i)
          results == UNION { AddDraftsToTab(t, ds, ~msg.newEdit.isEmpty) : ds \in dss }
      IN \E r \in results:
        & tabs'    = r[1]
        & network' = net1 \o r[2]
        & UNCHANGED << browsers, remote, workers >>

TabLoad ==
  \E t \in Tab:
    & tabs[t].status = loading
    & tabs[t].awaiting != {}
    & UNCHANGED << browsers, network, remote, workers >>
    & LET ts == tabs[t]
          w  == ts.worker
          b  == workers[w].browser
          bs == browsers[b]
          Attempt(src, srcDrafts) ==
            IF src \notin ts.awaiting
            THEN {}
            ELSE LET drafts2s  == Prune(AddDrafts(ts.drafts, srcDrafts))
                     awaiting2 == ts.awaiting -- {src}
                     ts2(ds2)  == [ts EXCEPT !.drafts = ds2, !.awaiting = awaiting2]
                 IN {[tabs EXCEPT ![t] = ts2(ds2)] : ds2 \in drafts2s }
          AttemptOption(src, o) ==
            IF o.isEmpty
            THEN {[tabs EXCEPT ![t].awaiting = @ -- {src}]}
            ELSE Attempt(src, o.get)
          browserAttempts ==
            UNION { AttemptOption(src, bs[src]) : src \in BrowserSrc }
      IN tabs' \in (browserAttempts ++ Attempt(Remote, remote))

TabNew ==
  \E t \in Tab:
    & tabs[t].status = nonExistant
    & UNCHANGED << browsers, network, remote >>
    & \E w \in Worker:
      & \* Connect to worker
        | \* New worker
          & workers[w].status = nonExistant
          & \E b \in Browser:
            & IsValidAssignment(b, w, t)
            & workers' = [workers EXCEPT ![w] = StartWorker(b)]
        | \* Existing worker
          & workers[w].status = live
          & IsValidAssignment(workers[w].browser, w, t)
          & UNCHANGED workers
      & tabs' = [tabs EXCEPT ![t] = [
          status   |-> loading,
          worker   |-> w,
          drafts   |-> {},
          awaiting |-> AnySrc
        ]]

TabStart ==
  \E t \in Tab:
    LET ts == tabs[t]
        w  == ts.worker
    IN
      & ts.status = loading
      & ts.awaiting = {}
      & tabs' = [tabs EXCEPT ![t] = NewTabState(w, ts.drafts, FALSE)]
      & IF ts.drafts = {}
        THEN UNCHANGED network
        ELSE SendMsg([
              type    |-> syncTW,
              from    |-> t,
              to      |-> w,
              drafts  |-> ts.drafts,
              newEdit |-> None
             ])
      & UNCHANGED << browsers, remote, workers >>

TabRecvRemoteStoreCmd ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = RemoteStoreCmd)
  IN
    & i != 0
    & LET msg       == network[i]
          t         == msg.to
          dss       == Prune(AddDrafts(TabDrafts(t), msg.drafts))
          msgW      == [
                         type |-> ackTW,
                         from |-> t,
                         to   |-> msg.from,
                         id   |-> msg.id
                       ]
          msgR(ds)  == [
                         type   |-> syncTR,
                         from   |-> t,
                         to     |-> Remote,
                         drafts |-> ds,
                         id     |-> msg.id
                       ]
          saveT(ds) == [tabs EXCEPT ![t] = TabStateWithDrafts(t, ds)]
          send(res) == Append(RemoveAt(network, i), res)
          resultsNE == { <<saveT(ds), send(msgR(ds))>> : ds \in dss }
      IN
        IF dss = {} THEN
          & RecvResp(i, msgW)
          & UNCHANGED << browsers, workers, remote, tabs >>
        ELSE
          \E r \in resultsNE:
            & tabs'    = r[1]
            & network' = r[2]
            & UNCHANGED << browsers, workers, remote >>

TabSendChangesToWorker ==
  \E t \in Tab:
    LET ts == tabs[t]
    IN
      & ts.status = dirty
      & ts.localChange
      & tabs' = [tabs EXCEPT ![t].localChange = FALSE]
      & SendMsg([
          type    |-> syncTW,
          from    |-> t,
          to      |-> ts.worker,
          drafts  |-> {},
          newEdit |-> SomeWhen(ts.localChange, IF ts.draft.isEmpty THEN NoProv ELSE ts.draft.get.prov)
        ])
      & UNCHANGED << browsers, workers, remote >>

UserEditClean ==
  \E t \in Tab:
    LET ts  == tabs[t]
        w   == ts.worker
        ts2 == [worker |-> w, status |-> dirty, draft |-> None, localChange |-> TRUE]
    IN
      & ts.status = clean
      & tabs' = [tabs EXCEPT ![t] = ts2]
      & UNCHANGED << browsers, workers, network, remote >>

\* No need for this because
\* 1. In TabRecvDraftsFromWorker we handle the case that a local change has been made after sending it to WW
\* 2. Enabling makes it very hard to keep the model space finite
\* 3. The only thing we're missing is an editor sending multiple revisions of a draft to WW before getting a result
\*    which is extremely low probability (if possible at all) PLUS we can handle that logic easily enough
\*    outside of the model. Having it in the model shouldn't change anything.
UserEditDirty ==
  FALSE
\*   \E t \in Tab:
\*     & tabs[t].status = dirty
\*     & ~tabs[t].localChange
\*     & tabs' = [tabs EXCEPT ![t].localChange = TRUE]
\*     & UNCHANGED << browsers, workers, network, remote >>

WorkerRecvChanges ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncTW)
  IN
    & i != 0
    & LET msg      == network[i]
          w        == msg.to
          ws       == workers[w]
          t2       == IF msg.newEdit.isEmpty THEN ws.time ELSE ws.time + 1
          new      == OptionMap(msg.newEdit, LAMBDA n: NewDraft(w, n))
          dss      == Prune(AddDrafts(ws.drafts, AddDrafts(msg.drafts, OptionToSet(new))))
          dss2     == { ds \in dss : ds != ws.drafts }
          ws2(ds)  == [workers EXCEPT ![w].drafts = ds, ![w].time = t2, ![w].sync = WorkerSyncLater(@)]
          msgs(ds) == { [
                          type    |-> syncWT,
                          from    |-> w,
                          to      |-> t,
                          drafts  |-> ds,
                          newEdit |-> new
                        ]
                        : t \in WorkerTabs(w)
                      }
          results  == { <<ws2(ds), msgs(ds)>> : ds \in dss2 }
          net1     == RemoveAt(network, i)
      IN \E r \in results:
        & workers' = r[1]
        & network' = SetFold(r[2], net1, Append)
        & UNCHANGED << browsers, remote, tabs >>

\* This happens periodically without a trigger event
WorkerSyncWithBrowserStorage ==
  FALSE
  \* TODO
  \* \E w \in Worker:
  \*   LET ws == workers[w]
  \*       b  == workers[w].browser
  \*       bs == browsers[b]
  \*       Attempt(src) ==
  \*         IF bs[src].isEmpty | bs[src].get = ws.drafts THEN
  \*           {}
  \*         ELSE
  \*           LET dss == Prune(AddDrafts(ws.drafts, bs[src].get))
  \*               ws2(ds) == [workers EXCEPT ![w].drafts = ds]
  \*               bs2(ds) == [browsers EXCEPT ![b][src] = Some(ds)]
  \*           IN
  \*             \* IF ~Log([WW |-> ws.drafts, BS |-> bs[src].get, RES |-> dss]) THEN {} ELSE
  \*             { <<ws2(ds), bs2(ds)>> : ds \in dss }
  \*       results == UNION { Attempt(s) : s \in BrowserSrc }
  \*   IN \E r \in results:
  \*     & workers[w].status = live
  \*     & workers'  = r[1]
  \*     & browsers' = r[2]
  \*     & UNCHANGED << remote, network, tabs >>

\* TODO Track online/offline status of tabs
\* TODO Assumes that an ack will always be received to (for the sake of model checking)
WorkerSendRemoteStoreCmd ==
  \E w \in Worker:
    LET ws  == workers[w]
        s1  == ws.sync[Remote]
        s2  == WorkerSyncStart(s1)
        t   == CHOOSE t \in WorkerTabs(w) : TRUE \* TODO CHOOSE or \E?
        cmd == [
          type   |-> RemoteStoreCmd,
          from   |-> w,
          to     |-> t,
          drafts |-> ws.drafts,
          id     |-> s2.get.lastReq
        ]
    IN
      & ws.status = live
      & ~s2.isEmpty
      & ws.drafts != {}
      & SendMsg(cmd)
      & workers' = [workers EXCEPT ![w].sync[Remote] = s2.get]
      & UNCHANGED << browsers, remote, tabs >>

TabRecvRemoteAck ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = ackRT)
  IN
    & i != 0
    & LET msg    == network[i]
          t      == msg.to
          newMsg == [
            type |-> ackTW,
            from |-> t,
            to   |-> tabs[t].worker,
            id   |-> msg.id
          ]
      IN
        & RecvResp(i, newMsg)
        & UNCHANGED << browsers, remote, workers, tabs >>

WorkerRecvRemoteAck ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = ackTW)
  IN
    & i != 0
    & LET msg == network[i]
          w   == msg.to
          ws  == workers[w]
      IN
        & workers' = [workers EXCEPT ![w].sync[Remote] = WorkerSyncAck(@, msg.id)]
        & RecvMsg(i)
        & UNCHANGED << browsers, remote, tabs >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & network    = <<>>
  & remote     = {}
  & tabs       = [t \in Tab |-> [status |-> nonExistant]]
  & workers    = [w \in Worker |-> [status |-> nonExistant]]
  & IF MCBrowserStorageAlwaysAvailable
    THEN browsers = [b \in Browser |-> [s \in BrowserSrc |-> Some({})]]
    ELSE browsers \in [Browser -> [BrowserSrc -> NoneAndSome({})]]

Next ==
  | RemoteRecvDrafts
  | TabLoad
  | TabNew
  | TabRecvDraftsFromRemote
  | TabRecvDraftsFromWorker
  | TabRecvRemoteAck
  | TabRecvRemoteStoreCmd
  | TabSendChangesToWorker
  | TabStart
  | UserEditClean
  | WorkerRecvChanges
  | WorkerRecvRemoteAck
  | WorkerSendRemoteStoreCmd
  | WorkerSyncWithBrowserStorage

Fairness ==
  & WF_<<vars>>(RemoteRecvDrafts)
  & WF_<<vars>>(TabLoad)
  \* & WF_<<vars>>(TabNew)
  & WF_<<vars>>(TabRecvDraftsFromRemote)
  & WF_<<vars>>(TabRecvDraftsFromWorker)
  & WF_<<vars>>(TabRecvRemoteAck)
  & WF_<<vars>>(TabRecvRemoteStoreCmd)
  & WF_<<vars>>(TabSendChangesToWorker)
  & WF_<<vars>>(TabStart)
  \* & WF_<<vars>>(UserEditClean)
  & WF_<<vars>>(WorkerRecvChanges)
  & WF_<<vars>>(WorkerRecvRemoteAck)
  & SF_<<vars>>(WorkerSendRemoteStoreCmd)
  & SF_<<vars>>(WorkerSyncWithBrowserStorage)

Spec ==
  & SanityCheck
  & Init
  & [][Next]_<<vars>>
  & Fairness

\* Have all mechanical processes stopped such that the world is in a stable state that will stutter until
\* the user does something.
IsStable ==
  & network = <<>>
  & ~ENABLED(TabLoad)
  & ~ENABLED(TabSendChangesToWorker)
  & ~ENABLED(TabStart)
  & ~ENABLED(WorkerSendRemoteStoreCmd)
  & ~ENABLED(WorkerSyncWithBrowserStorage)
  \* & Log(state)

Liveness ==
  []<>IsStable \* We always stablise eventually
  \* <>[]IsStable \* We end in a stable state

StableInvariants ==
  IsStable =>
    \* & LogStates

    & Assert1(
      \A t \in Tab : ~TabHasLocalChange(t),
      "Local changes aren't stored", tabs)

    & Assert1(
      \A w \in ActiveWorkers : \A s \in AnySrc : WorkerSyncStateIsStable(workers[w].sync[s]),
      "Worker failed to sync.", workers)

    & Assert1(
      Cardinality(AllStores) <= 1,
      "Drafts are not eventually-consistent", [
          AB     |-> AvailableBrowserStores,
          AW     |-> ActiveWorkers,
          AT     |-> ActiveTabs,
          Stores |-> AllStores
        ])

    & Assert1(
      remote = AllDrafts,
      "Drafts are not stored remotely", [all |-> AllDrafts, remote |-> remote])

MCContinue ==
  TLCGet("diameter") <= 50
  \* TLCGet("level") <= 22
  \* TLCGet("duration") <= 4
  \* \A w \in ActiveWorkers:
  \*   & workers[w].time < 4
  \*   & \A s \in AnySrc:
  \*       workers[w].sync[s].desired < 4

========================================================================================================================
