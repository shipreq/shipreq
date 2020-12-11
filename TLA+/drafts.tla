---------------------------------------------------- MODULE drafts ----------------------------------------------------
(*
What's does this spec provide?
==============================
- [x] Drafts should eventually propagate to all live tabs
- [ ] Drafts and new events interact as targeted
- [ ] Drafts are never lost unless they're merged (manually or automatically), or completed by a user (whether aborted or committed)
- [ ] Drafts are removed from all storage locations once obsolete
- [ ] Users are prompted to keep a tab open iff we can't guarantee the draft won't be lost
- Reliability in the face of failures such as
  - [ ] network packet loss
  - [ ] machines crashing or dying without warning

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
CONSTANT MCMaxLocalChangesInFlight
CONSTANT MCMaxEditsPerTab
CONSTANT MCMaxPruneByProvDepth
CONSTANT MCNormaliseNatsSetSize

ASSUME IsFiniteSet(Browser)
ASSUME IsFiniteSet(BrowserSrcAsync)
ASSUME IsFiniteSet(BrowserSrcSync)
ASSUME IsFiniteSet(Tab)
ASSUME IsFiniteSet(Worker)
ASSUME IsFiniteSet(Assignments)
ASSUME Cardinality(Worker) <= Cardinality(Tab) \* each tab is assigned a worker. More workers than tabs is useless.
ASSUME Cardinality(Worker) >= Cardinality(Browser) \* 2w in 1b = diff versions, 1w in 2b doesn't make sense
ASSUME Assignments \in SUBSET SUBSET (Browser ++ Tab ++ Worker)
ASSUME \A a \in Assignments :
         & \E b \in Browser : b \in a
         & \E t \in Tab     : t \in a
         & \E w \in Worker  : w \in a
ASSUME MCBrowserStorageAlwaysAvailable \in SUBSET (BrowserSrcSync ++ BrowserSrcAsync)
ASSUME MCMaxLocalChangesInFlight \in Nat
ASSUME MCMaxEditsPerTab \in Nat
ASSUME MCMaxPruneByProvDepth \in Nat
ASSUME MCNormaliseNatsSetSize \in Nat

\* Async browser storage (like idb) not supported yet (if ever).
\* In other to faithfully spec it out, we need to break the process into multiple steps
\* like we do when WW creates promises around sending data back to tab/remote and back.
\* Currently WorkerSyncWithBrowserStorage ignores all of that.
ASSUME BrowserSrcAsync = {}

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
VARIABLE target \* Expected result state

vars == << browsers, network, remote, tabs, workers, target >>

state == [
  browsers |-> browsers,
  network  |-> network,
  remote   |-> remote,
  tabs     |-> tabs,
  workers  |-> workers,
  target   |-> target]

LogStates ==
  Log(state)

clean          == "clean"
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
RemoteStoreCmd == "cmd:T->R"
ackRT          == "ack:R->T"
ackTW          == "ack:T->W"

StateWithNormalisedNats ==
  NormaliseNats(state, 0, MCNormaliseNatsSetSize)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Invariants

Provenance ==
  [Worker -> Nat] \* i.e. Map[WorkerId, Time]

DraftWithTombstoneIn(T) == [
  worker   : Worker,
  time     : Nat,
  prov     : Provenance,
  tombstone: T
]

DraftsWithTombstoneIn(T) ==
  SUBSET DraftWithTombstoneIn(T) \* i.e. Set[Draft]

Draft ==
  DraftWithTombstoneIn(BOOLEAN)

Drafts ==
  SUBSET Draft \* i.e. Set[Draft]

DraftsNE ==
  Drafts -- {{}} \* i.e. NonEmptySet[Draft]

Tombstones ==
  DraftsWithTombstoneIn({TRUE})

IsValidAssignment(b, w, t) ==
  {b,w,t} \in Assignments

Msg == [
  type   : {syncTW},
  from   : Tab,
  to     : Worker,
  drafts : Drafts,
  edit   : Option([prov: Provenance, rev: Nat, editCount: Nat]) \* editCount is just for updating target
] ++ [
  type   : {syncWT},
  from   : Worker,
  to     : Tab,
  drafts : Drafts,
  edit   : Option([draft: Draft, rev: Nat])
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

AsyncSrc ==
  BrowserSrcAsync ++ {Remote}

ActiveTabStatus ==
  {clean, dirty}

TabState ==
  [ status: {nonExistant}] ++
  [
    status  : {loading},
    worker  : Worker,
    drafts  : Drafts,
    awaiting: SUBSET AnySrc
  ] ++
  [
    status     : {clean},
    worker     : Worker,
    \* editRevSent: Nat, \* dirty state we want to maintain between clean <-> dirty transitions
    \* editRevAck : Nat, \* dirty state we want to maintain between clean <-> dirty transitions
    editCount  : Nat, \* dirty state we want to maintain between clean <-> dirty transitions
    tombstones : Tombstones \* Tombstones drafts that need to be sent to WW
  ] ++
  [
    status     : {dirty},
    worker     : Worker,
    drafts     : Drafts, \* includes editDraft when its defined
    editDraft  : Option(Draft), \* last known draft for editor state - defined iff editRev>0
    editRev    : Nat, \* Local change revision                                     (is reset back to zero when moved into a draft)
    editRevSent: Nat, \* The highest revision sent to WW to be turned into a draft (is reset back to zero when moved into a draft)
    editRevAck : Nat, \* The highest revision ack'd by WW                          (is reset back to zero when moved into a draft)
    editCount  : Nat, \* The number of edits made by a user in this tab
    aborted    : BOOLEAN
  ]

WorkerSyncState ==
  [
    desired   : Nat, \* The promise id we want ack'd to know that we've synced
    lastReq   : Nat, \* The last promise id issued
    lastAck   : Nat, \* The highest ack'd promise id
    tombstones: SUBSET (Nat \X Tombstones) \* Set[(ReqId, Tombstones)]
  ]

WorkerState ==
  [status: {nonExistant}] ++
  [
    status : {live},
    browser: Browser,
    time   : Nat,
    drafts : Drafts,
    sync   : [AsyncSrc -> WorkerSyncState]
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
  & \A e \in s.tombstones :
    & e[1] > s.lastAck
    & e[1] <= s.desired
    & \A f \in s.tombstones : (e[1] = f[1]) => (e[2] = f[2]) \* Prevent multiple entries per reqId

InvariantsForBrowsers ==
  & browsers \in [Browser -> BrowserState]
  & \A b \in Browser :
      LET bs == browsers[b]
      IN \A src \in BrowserSrc:
        bs[src].isEmpty | StorageInvariants(bs[src].get)

InvariantsForNetwork ==
  network \in NetworkState

InvariantsForRemote ==
  & remote \in [ord: Nat, drafts: Drafts]
  & StorageInvariants(remote.drafts)

InvariantsForTabs ==
  & tabs \in [Tab -> TabState]
  & \A t \in Tab :
      LET ts == tabs[t]
      IN
        & ts.status \in {clean,dirty} =>
          & ts.editCount <= MCMaxEditsPerTab
        & ts.status = dirty =>
          & ts.editRevSent <= ts.editRev
          & ts.editRev <= ts.editCount
          & ts.editRevAck <= ts.editRevSent
          & ~ts.editDraft.isEmpty => ts.editDraft.get \in ts.drafts \* drafts includes editDraft
          & ~(ts.editRev = 0 & ts.drafts = {}) \* ensure actually dirty
          & ~ts.editDraft.isEmpty => ts.editRev != 0 \* editDraft only defined when there are local changes

InvariantsForWorkers ==
  & workers \in [Worker -> WorkerState]
  & \A w \in Worker :
      LET ws == workers[w]
      IN ws.status = live =>
          & ws.time > 0
          & \A s \in AsyncSrc : WorkerSyncStateInvariants(ws.sync[s])

InvariantsForTarget ==
  target \in [
    pending: SUBSET [tab: Tab, editCount: Nat],
    drafts : Drafts
  ]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

WorkerSyncStateIsStable(s) ==
  & s.lastReq = s.desired
  & s.lastAck = s.lastReq

WorkerSyncStateEmpty == [
  desired    |-> 0,
  lastReq    |-> 0,
  lastAck    |-> 0,
  tombstones |-> {}
]

\* WorkerSyncState => Option[WorkerSyncState]
WorkerSyncStart(s, drafts) ==
  LET doIt  == & s.desired > s.lastReq \* we have new content to sync
               & s.lastAck = s.lastReq \* wait for ACK so that model checking isn't infinite
      id    == s.desired
      tombs == LET ts == << id, { d \in drafts : d.tombstone } >>
               IN IF ts[2] = {} THEN {} ELSE {ts}
      next  == [s EXCEPT !.lastReq = id, !.tombstones = @ ++ tombs]
  IN SomeWhen(doIt, next)

WorkerSyncAck(s, id) ==
  IF id <= s.lastReq & id != s.lastAck THEN
    [s EXCEPT !.lastAck = Max[@, id], !.tombstones = {e \in @ : e[1] != id}]
  ELSE
    Fail1("Invalid ACK", [syncState |-> s, id |-> id])

WorkerSyncLater(syncState) ==
  LET f(s) == IF s.desired > s.lastReq THEN s ELSE [s EXCEPT !.desired = @ + 1]
  IN [src \in AsyncSrc |-> f(syncState[src])]

StartWorker(b) ==
  [
    status  |-> live,
    browser |-> b,
    time    |-> 1,
    drafts  |-> {}, \* Start with 0 drafts. In reality we would run WorkerSyncWithBrowserStorage immediately.
    sync    |-> [s \in AsyncSrc |-> WorkerSyncStateEmpty]
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

TabDrafts(tOrTS) ==
  LET ts == IF tOrTS \in Tab THEN tabs[tOrTS] ELSE tOrTS
      s  == ts.status
  IN CASE s = dirty       -> ts.drafts
       [] s = loading     -> ts.drafts
       [] s = clean       -> {}
       [] s = nonExistant -> {}

ActiveWorkers ==
  { w \in Worker : workers[w].status != nonExistant }

ActiveBrowsers ==
  { workers[w].browser : w \in ActiveWorkers }

\* Set[(Browser, BrowserSrc)]
AvailableBrowserStores ==
  { x \in ActiveBrowsers \X BrowserSrc : ~browsers[x[1]][x[2]].isEmpty }

\* Set[Storage]
AllStores ==
  LET draftsB == { browsers[x[1]][x[2]].get : x \in AvailableBrowserStores }
      draftsT == { TabDrafts(t) : t \in ActiveTabs }
      draftsW == { workers[w].drafts : w \in ActiveWorkers }
      draftsR == { remote.drafts }
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
    worker    |-> w,
    time      |-> workers[w].time,
    prov      |-> [prevProv EXCEPT ![w] = 0],
    tombstone |-> FALSE
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
  LET mergeByProv(x,y) == LET pt == x.prov[y.worker]
                          IN x.worker != y.worker & pt > 0 & y.time <= pt
      mergeBySrc(x,y)  == x.worker = y.worker &
                            | x.time > y.time
                            | x.time = y.time & x != y
      merge(xy)        == LET x == xy[1]
                              y == xy[2]
                          IN mergeByProv(x,y) | mergeBySrc(x,y)
      pairs            == { x \in (ds \X ds) : x[1] != x[2] }
      match            == SetFind(pairs, merge)
  IN
    IF depth >= MCMaxPruneByProvDepth THEN
      Fail("Maximum depth of PruneByProv exceeded")
    ELSE IF match.isEmpty THEN
      ds
    ELSE
      LET d1a == match.get[1]
          d1b == match.get[2]
          d2a == AddProv(d1a, AddSelfToOwnProv(d1b).prov) \* TODO: Need AddSelfToOwnProv here?
          d2  == [d2a EXCEPT !.tombstone = @ | d1b.tombstone]
          ds2 == (ds -- {d1a, d1b}) ++ {d2}
      IN _PruneByProv(ds2, depth + 1)
PruneByProv(ds) == _PruneByProv(ds, 0)

\* Returns a set of possible outcomes
PruneByEq(ds) ==
  LET equalSets    == { es \in SUBSET(ds) : Cardinality(es) > 1 & (\A d \in es : ~d.tombstone) } \* Set[Set[Drafts]]
      merge(x, y)  == AddProv(x, AddSelfToOwnProv(y).prov)
      mergeAll(es) == SetReduce(es, merge)
      result       == { (ds -- es) ++ {mergeAll(es)} : es \in equalSets } ++ {ds}
  IN result \* LogRet([ BEFORE |-> ds, AFTER |-> result ], result)

\* Returns a set of possible outcomes
Prune(drafts) ==
  {PruneByProv(ds) : ds \in PruneByEq(drafts)}

TargetAddPending(tgt, t, newEditCount) ==
  [tgt EXCEPT !.pending = @ ++ {[tab |-> t, editCount |-> newEditCount]}]

TargetDelPending(tgt, t, editCount) ==
  LET del(p) == p.tab = t & p.editCount <= editCount
  IN [tgt EXCEPT !.pending = { p \in @ : ~del(p) }]

TargetAddDrafts(tgt, drafts) ==
  \* IF drafts \notin Drafts THEN Fail1("Not a set: ", drafts) ELSE
  [tgt EXCEPT !.drafts = PruneByProv(AddDrafts(@, drafts))]

TargetDelDrafts(tgt, drafts) ==
  \* IF drafts \notin Drafts THEN Fail1("Not a set: ", drafts) ELSE
  TargetAddDrafts(tgt, { [d EXCEPT !.tombstone = TRUE] : d \in drafts })

NewCleanTabState(workerOrPrevState, tombstones) ==
  LET havePrev  == workerOrPrevState \notin Worker
      w         == IF havePrev THEN workerOrPrevState.worker ELSE workerOrPrevState
      wasActive == havePrev & workerOrPrevState.status \in {clean,dirty}
  IN [
    status      |-> clean,
    worker      |-> w,
    \* editRevSent |-> IF wasActive THEN workerOrPrevState.editRevSent ELSE 0,
    \* editRevAck  |-> IF wasActive THEN workerOrPrevState.editRevAck  ELSE 0,
    editCount   |-> IF wasActive THEN workerOrPrevState.editCount   ELSE 0,
    tombstones  |-> tombstones
  ]

NewDirtyTabState(prevState, ds, editDraft, editRevInc) ==
  LET wasActive == prevState.status \in {clean,dirty}
      wasDirty  == wasActive & prevState.status = dirty
  IN [
    status      |-> dirty,
    worker      |-> prevState.worker,
    drafts      |-> ds,
    editDraft   |-> editDraft,
    editRev     |-> (IF wasDirty  THEN prevState.editRev     ELSE 0) + editRevInc,
    editRevSent |-> (IF wasDirty  THEN prevState.editRevSent ELSE 0),
    editRevAck  |-> (IF wasDirty  THEN prevState.editRevAck  ELSE 0),
    editCount   |-> (IF wasActive THEN prevState.editCount   ELSE 0) + editRevInc,
    aborted     |-> FALSE
  ]

\* NonEmptySet[Draft] => Possibilities[Draft]
MergeDraftsIntoTombstoneNE(ds) ==
  LET squashInto(d) ==
    SetFold(ds -- {d},
            [d EXCEPT !.tombstone = TRUE],
            LAMBDA x,y: AddProv(x, AddSelfToOwnProv(y).prov))
  IN { squashInto(d) : d \in ds }

\* editResp is from WW: Option (draft,rev)
\* Returns multiple possibilities in the form of Set[TabState]
AddDraftsToTab(ts, newDrafts, editResp, retainTombstones) ==
  LET ds2       == AddDrafts(TabDrafts(ts), newDrafts)
      ds3       == IF editResp.isEmpty THEN ds2 ELSE AddDraft(ds2, editResp.get.draft)
      ds4       == IF retainTombstones THEN ds3 ELSE { d \in ds3 : ~d.tombstone }
      dss       == Prune(ds4)
      noDrafts  == dss = {{}}
      resolveEd == ~editResp.isEmpty & ts.status = dirty & editResp.get.rev = ts.editRev
      editDraft == IF ts.status = dirty THEN ts.editDraft ELSE None
      ts2       == IF editResp.isEmpty THEN
                     ts
                   ELSE
                     LET e == editResp.get
                     IN
                       IF ts.status != dirty THEN
                         Fail1("[AddDraftsToTab] Edit response but tab isn't dirty.", ts)
                       ELSE IF e.rev != ts.editRev THEN
                         ts \* Newer response targeted from WW soon...
                       ELSE IF ts.editRev != ts.editRevSent THEN
                         Fail1(
                           "[AddDraftsToTab] editResp.rev = ts.editRev but ts.editRev != ts.editRevSent",
                           [resp |-> e.rev, editRev |-> ts.editRev, editRevSent |-> ts.editRevSent, editRevAck |-> ts.editRevAck])
                       ELSE
                         \* Latest edit=>draft request ACK'd
                         [ts EXCEPT !.editRev = 0, !.editRevSent = 0, !.editRevAck = 0, !.editDraft = None]
      tss       == IF noDrafts THEN
                     { NewCleanTabState(ts2, {}) }
                   ELSE
                     LET ed == IF resolveEd THEN None ELSE editDraft
                     IN { NewDirtyTabState(ts2, ds, ed, 0) : ds \in dss }
  IN
    IF ts.status \in {clean,dirty} THEN
      tss
    ELSE
      Fail1("[AddDraftsToTab] Don't know how to handle tab state:", ts)

    \* IF ts.status \in {dirty,loading} THEN
    \*   IF ts.aborted & ~editResp.isEmpty & ts2.editRev = 0 THEN
    \*     { NewCleanTabState(ts2, {d}) : d \in MergeDraftsIntoTombstone(ds4) }
    \*   ELSE IF dss = {{}} THEN
    \*     { NewCleanTabState(ts2, {}) }
    \*   ELSE
    \*     { [ts2 EXCEPT !.drafts = ds] : ds \in dss }
    \* ELSE IF ts.status = clean THEN
    \*   IF dss = {{}} THEN
    \*     { NewCleanTabState(ts2, {}) }
    \*   ELSE
    \*     { NewDirtyTabState(ts, ds, None, 0) : ds \in dss }
    \* ELSE
    \*   Fail1("[AddDraftsToTab] Don't know how to handle tab state:", ts)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Tests

PruneTest ==
  IF Cardinality(Worker) < 2 THEN
    PrintT("Skipping PruneTest")
  ELSE
    LET w1 == CHOOSE w \in Worker : TRUE
        w2 == CHOOSE w \in Worker : w != w1
    IN

      & LET input  == {[worker |-> w1, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 1), tombstone |-> FALSE],
                       [worker |-> w2, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 0), tombstone |-> FALSE]}
            actual == Prune(input)
            exp1   == {[worker |-> w1, time |-> 1, prov |-> (w1 :> 0 @@ w2 :> 1), tombstone |-> FALSE]}
            expect == {exp1}
        IN Assert1(actual = expect, "Prune test 1 failed.", actual)

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w1, time |-> 2, prov |-> (w1 :> 0 @@ w2 :> 4), tombstone |-> b],
                       [worker |-> w2, time |-> 3, prov |-> (w1 :> 0 @@ w2 :> 0), tombstone |-> ~b]}
            actual == Prune(input)
            exp1   == {[worker |-> w1, time |-> 2, prov |-> (w1 :> 0 @@ w2 :> 4), tombstone |-> TRUE]}
            expect == {exp1}
        IN Assert1(actual = expect, "Prune test 2 failed.", actual)

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w1, time |-> 2, prov |-> (w1 :> 0 @@ w2 :> 0), tombstone |-> b],
                       [worker |-> w2, time |-> 3, prov |-> (w1 :> 0 @@ w2 :> 0), tombstone |-> ~b]}
            actual == Prune(input)
            expect == {input}
        IN Assert1(actual = expect, "Prune test 3 failed.", actual)

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
                            Prune(AddDrafts(remote.drafts, msg.drafts))
          results        == { result(ds) : ds \in dss }
      IN \E r \in results:
        & remote' = [remote EXCEPT !.drafts = r[1]]
        & network' = RemoveAt(network, i) \o r[2]
        & UNCHANGED << browsers, tabs, workers, target >>

TabRecvDraftsFromRemote ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncRT)
  IN
    & i != 0
    & LET msg        == network[i]
          t          == msg.to
          ts         == tabs[t]
          w          == ts.worker
          tss        == AddDraftsToTab(ts, msg.drafts, None, TRUE)
          msgWW(ts2) == [
            type   |-> syncTW,
            from   |-> t,
            to     |-> w,
            drafts |-> ts2.drafts,
            edit   |-> None
          ]
      IN \E ts2 \in tss:
        & tabs' = [tabs EXCEPT ![t] = ts2]
        & RecvResp(i, msgWW(ts2))
        & UNCHANGED << browsers, remote, workers, target >>

TabRecvDraftsFromWorker ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncWT)
  IN
    & i != 0
    & LET msg == network[i]
          t   == msg.to
          ts  == tabs[t]
          tss == AddDraftsToTab(ts, msg.drafts, msg.edit, FALSE)
      IN
        & \E ts2 \in tss:
          tabs' = [tabs EXCEPT ![t] = ts2]
        & RecvMsg(i)
        & UNCHANGED << browsers, remote, workers, target >>

TabLoad ==
  \E t \in Tab:
    & tabs[t].status = loading
    & tabs[t].awaiting != {}
    & UNCHANGED << browsers, network, remote, workers, target >>
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
      IN tabs' \in (browserAttempts ++ Attempt(Remote, remote.drafts))

TabNew ==
  \E t \in Tab:
    & tabs[t].status = nonExistant
    & UNCHANGED << browsers, network, remote, target >>
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
    LET ts  == tabs[t]
        w   == ts.worker
        ts2 ==
          IF ts.drafts = {} THEN
            NewCleanTabState(w, {})
          ELSE
            NewDirtyTabState(w, ts.drafts, None, 0)
    IN
      & ts.status = loading
      & ts.awaiting = {}
      & tabs' = [tabs EXCEPT ![t] = ts2]
      & IF ts.drafts = {} THEN
          UNCHANGED network
        ELSE
          SendMsg([
            type   |-> syncTW,
            from   |-> t,
            to     |-> w,
            drafts |-> ts.drafts,
            edit   |-> None
           ])
      & UNCHANGED << browsers, remote, workers, target >>

TabRecvRemoteStoreCmd ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = RemoteStoreCmd)
  IN
    & i != 0
    & LET msg       == network[i]
          t         == msg.to
          ts        == tabs[t]
          tss       == AddDraftsToTab(ts, msg.drafts, None, FALSE)
          tombs     == { d \in msg.drafts : d.tombstone }
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
                         drafts |-> ds ++ tombs,
                         id     |-> msg.id
                       ]
          \* saveT(ds) == [tabs EXCEPT ![t] = TabStateWithDrafts(t, ds)]
          \* send(res) == Append(RemoveAt(network, i), res)
          \* resultsNE == { <<saveT(ds), send(msgR(ds))>> : ds \in dss }
      IN \E ts2 \in tss:
        IF TabDrafts(ts2) = {} THEN
          \* Nothing to send
          & RecvResp(i, msgW)
          & UNCHANGED << browsers, workers, remote, tabs, target >>
        ELSE
          \* Send drafts to remote
          & tabs' = [tabs EXCEPT ![t] = ts2]
          & RecvResp(i, msgR(ts2.drafts))
          & UNCHANGED << browsers, workers, remote, target >>

TabSendChangesToWorker ==
  \E t \in Tab:
    LET ts == tabs[t]
    IN
      & ts.status = dirty
      & ts.editRev > ts.editRevSent
      & SendMsg([
          type   |-> syncTW,
          from   |-> t,
          to     |-> ts.worker,
          drafts |-> {},
          edit   |-> Some([
                       prov      |-> IF ts.editDraft.isEmpty THEN NoProv ELSE ts.draft.get.prov,
                       rev       |-> ts.editRev,
                       editCount |-> ts.editCount
                     ])
        ])
      & tabs' = [tabs EXCEPT ![t].editRevSent = ts.editRev]
      & UNCHANGED << browsers, workers, remote, target >>

TabSendTombstonesToWorker ==
  \E t \in Tab:
    LET ts == tabs[t]
    IN
      & ts.status = clean
      & ts.tombstones != {}
      & SendMsg([
          type   |-> syncTW,
          from   |-> t,
          to     |-> ts.worker,
          drafts |-> ts.tombstones,
          edit   |-> None
        ])
      & tabs' = [tabs EXCEPT ![t].tombstones = {}]
      & UNCHANGED << browsers, workers, remote, target >>

UserAbort ==
  \E t \in Tab:
    LET ts == tabs[t]
        w  == ts.worker
    IN
      & ts.status = dirty
      & ~ts.aborted
      & ts.editCount < MCMaxEditsPerTab
      & IF ts.drafts /= {} THEN
          \* At least one confirmed drafts exists that now needs to be deleted
          LET ds == MergeDraftsIntoTombstoneNE(ts.drafts)
          IN \E d \in ds:
            tabs' = [tabs EXCEPT ![t].aborted   = TRUE,
                                 ![t].editDraft = SomeWhen(~@.isEmpty, d),
                                 ![t].editRev   = @ + 1, \* Inc to activate TabSendChangesToWorker
                                 ![t].editCount = @ + 1]
        ELSE IF ts.editRev /= ts.editRevAck THEN
          \* First edit sent to worker, waiting for first draft
          tabs' = [tabs EXCEPT ![t].aborted = TRUE]
        ELSE
          \* No non-local state exists, act immediately
          tabs' = [tabs EXCEPT ![t] = NewCleanTabState(ts, {})]
      & target' = TargetDelPending(TargetDelDrafts(target, ts.drafts), t, ts.editCount)
      & UNCHANGED << browsers, workers, network, remote >>

UserEditClean ==
  \E t \in Tab:
    LET ts  == tabs[t]
        w   == ts.worker
        ts2 == NewDirtyTabState(ts, ts.tombstones, None, 1)
    IN
      & ts.status = clean
      & tabs' = [tabs EXCEPT ![t] = ts2]
      & target' = TargetAddPending(target, t, ts2.editCount)
      & UNCHANGED << browsers, workers, network, remote >>

UserEditDirty ==
  \E t \in Tab: LET ts == tabs[t] IN
    & ts.status = dirty
    & ~ts.aborted
    & \* Limit model space
      & ts.editCount < MCMaxEditsPerTab
      & ts.editRev < MCMaxLocalChangesInFlight
      & ts.editRevSent = IF ts.editRev = 0 THEN 0 ELSE ts.editRev - 1
    & tabs' = [tabs EXCEPT ![t].editRev = @ + 1, ![t].editCount = @ + 1]
    & target' = TargetAddPending(target, t, tabs'[t].editCount)
    & UNCHANGED << browsers, workers, network, remote >>

WorkerBroadcastToTabMsgs(w, newDrafts, edit(_)) ==
  LET ws == workers[w] IN
    IF ws.drafts = newDrafts THEN
      {}
    ELSE
      LET msg(t) == [
            type   |-> syncWT,
            from   |-> w,
            to     |-> t,
            drafts |-> newDrafts,
            edit   |-> edit(t)
          ]
      IN { msg(t) : t \in WorkerTabs(w) }

WorkerRecvChanges ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = syncTW)
  IN
    & i != 0
    & LET msg      == network[i]
          w        == msg.to
          ws       == workers[w]
          t2       == IF msg.edit.isEmpty THEN ws.time ELSE ws.time + 1
          editF(e) == [draft |-> NewDraft(w, e.prov), rev |-> e.rev]
          edit     == OptionMap(msg.edit, editF)
          dss      == Prune(AddDrafts(ws.drafts, AddDrafts(msg.drafts, {e.draft : e \in OptionToSet(edit)})))
          dss2     == { ds \in dss : ds != ws.drafts }
          ws2(ds)  == [workers EXCEPT ![w].drafts = ds, ![w].time = t2, ![w].sync = WorkerSyncLater(@)]
          msgs(ds) == WorkerBroadcastToTabMsgs(w, ds, LAMBDA t: IF t = msg.from THEN edit ELSE None)
          results  == { <<ws2(ds), msgs(ds)>> : ds \in dss2 }
          net1     == RemoveAt(network, i)
      IN
        & IF results = {} THEN
            & RecvMsg(i)
            & UNCHANGED workers
          ELSE
            \E r \in results:
              & workers' = r[1]
              & network' = SetFold(r[2], net1, Append)
        & UNCHANGED << browsers, remote, tabs >>
        & IF msg.edit.isEmpty THEN
            UNCHANGED target
          ELSE
            LET e    == msg.edit.get
                t    == msg.from
                d    == NewDraft(w, e.prov)
                tgt2 == TargetDelPending(target, t, e.editCount)
            IN
              IF tgt2 = target THEN
                \* The edit was removed from target.pending meaning we don't want it added to target.drafts anymore
                UNCHANGED target
              ELSE
                target' = TargetAddDrafts(tgt2, {d})

ActiveBrowserSrcs(b) ==
  { s \in BrowserSrc : ~browsers[b][s].isEmpty }

\* This happens periodically without a trigger event.
\* No need to track whether a write is needed (i.e. BrowserSrcSync isn't in .sync) because we need to check
\* for reads too.
WorkerSyncWithBrowserStorage ==
  \E w \in Worker : LET ws == workers[w] IN
    & ws.status = live
    & \E s \in ActiveBrowserSrcs(ws.browser) :
      LET b             == ws.browser
          bs            == browsers[b]
          browsers2(ds) == [browsers EXCEPT ![b][s] = Some(ds)]
          workers2(ds)  == [workers EXCEPT
                             ![w].drafts = ds,
                             ![w].sync = IF ws.drafts = ds THEN @ ELSE WorkerSyncLater(@)
                           ]
          network2(ds)  == SetFold(WorkerBroadcastToTabMsgs(w, ds, LAMBDA t: None), network, Append)
          dss           == Prune(AddDrafts(bs[s].get, ws.drafts))
          results       == { <<browsers2(ds), workers2(ds), network2(ds)>> : ds \in dss }
      IN \E r \in results:
        & browsers' = r[1]
        & workers'  = r[2]
        & network'  = r[3]
        & \* We want ENABLED(WorkerSyncWithBrowserStorage) to be FALSE in the case of a NO-OP
          | browsers != browsers'
          | workers != workers'
          | network != network'
        & UNCHANGED << remote, tabs, target >>

\* No need to track online/offline status of tabs. This works by broadcasting to any live tab.
\* If all tabs are offline then it would just be a no-op.
\* Eventual consistency obviously can't happen when a connection is down so there's nothing to test.
WorkerSendRemoteStoreCmd ==
  \E w \in Worker:
    LET ws  == workers[w]
        ds  == ws.drafts
        s1  == ws.sync[Remote]
        s2  == WorkerSyncStart(s1, ds)
        cmd(t) == [
          type   |-> RemoteStoreCmd,
          from   |-> w,
          to     |-> t,
          drafts |-> ds,
          id     |-> s2.get.lastReq
        ]
    IN
      & ws.status = live
      & ~s2.isEmpty
      & ws.drafts != {}
      & \E t \in WorkerTabs(w):
        & SendMsg(cmd(t))
        & workers' = [workers EXCEPT ![w].sync[Remote] = s2.get]
        & UNCHANGED << browsers, remote, tabs, target >>

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
        & UNCHANGED << browsers, remote, workers, tabs, target >>

WorkerRecvRemoteAck ==
  LET i == SeqIndexOf(network, LAMBDA m: m.type = ackTW)
  IN
    & i != 0
    & LET msg   == network[i]
          id    == msg.id
          w     == msg.to
          ws    == workers[w]
          sync  == ws.sync[Remote]
          sync2 == WorkerSyncAck(sync, id)
          tombs == SetFind(sync.tombstones, LAMBDA e: e[1] = id)
          ds2   == IF tombs.isEmpty THEN ws.drafts ELSE ws.drafts -- tombs.get[2]
          ws2   == [ws EXCEPT !.sync[Remote] = sync2, !.drafts = ds2]
      IN
        & workers' = [workers EXCEPT ![w] = ws2]
        & RecvMsg(i)
        & UNCHANGED << browsers, remote, tabs, target >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & target  = [pending |-> {}, drafts |-> {}]
  & network = <<>>
  & remote  = [ord |-> 0, drafts |-> {}]
  & tabs    = [t \in Tab |-> [status |-> nonExistant]]
  & workers = [w \in Worker |-> [status |-> nonExistant]]
  & browsers \in [Browser -> (
      LET mandatory == [MCBrowserStorageAlwaysAvailable -> {Some({})}]
          optional  == [(BrowserSrc -- MCBrowserStorageAlwaysAvailable) -> NoneAndSome({})]
      IN { r[1] @@ r[2] : r \in (mandatory \X optional)}
    )]

Next ==
  | RemoteRecvDrafts
  | TabLoad
  | TabNew
  | TabRecvDraftsFromRemote
  | TabRecvDraftsFromWorker
  | TabRecvRemoteAck
  | TabRecvRemoteStoreCmd
  | TabSendChangesToWorker
  | TabSendTombstonesToWorker
  | TabStart
  | UserAbort
  | UserEditClean
  | UserEditDirty
  | WorkerRecvChanges
  | WorkerRecvRemoteAck
  | WorkerSendRemoteStoreCmd
  | WorkerSyncWithBrowserStorage

Fairness ==
  & WF_<<vars>>(RemoteRecvDrafts)
  & WF_<<vars>>(TabLoad)
  & WF_<<vars>>(TabRecvDraftsFromRemote)
  & WF_<<vars>>(TabRecvDraftsFromWorker)
  & WF_<<vars>>(TabRecvRemoteAck)
  & WF_<<vars>>(TabRecvRemoteStoreCmd)
  & WF_<<vars>>(TabSendChangesToWorker)
  & WF_<<vars>>(TabSendTombstonesToWorker)
  & WF_<<vars>>(TabStart)
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
  & ~ENABLED(TabSendTombstonesToWorker)
  & ~ENABLED(TabStart)
  & ~ENABLED(WorkerSendRemoteStoreCmd)
  & ~ENABLED(WorkerSyncWithBrowserStorage)
  \* & Log(state)

Liveness ==
  []<>IsStable \* We always, eventually stablise

StableInvariants ==
  IsStable =>
    \* & LogStates

    & \A t \in Tab :
      LET ts            == tabs[t]
          info          == [tab |-> t, tabState |-> ts]
          hasEditBudget == ts.editCount < MCMaxEditsPerTab
      IN
        | ts.status = clean
          & Assert1(ts.tombstones = {}, "Tombstones not cleared out from tab", info)
        | ts.status = dirty
          & Assert1(ts.editDraft.isEmpty, "Local changes not propagated", info)
          & Assert1(~ts.aborted, "Tab stuck in aborted state", info)
          & hasEditBudget => Assert1(ENABLED(UserAbort), "Tab has draft but can't be aborted", info)
        | ts.status = nonExistant

    & Assert1(
        target.pending = {},
        "Incomplete edits in target ", target.pending)

    & \A w \in ActiveWorkers : \A s \in AsyncSrc :
      Assert1(
        WorkerSyncStateIsStable(workers[w].sync[s]),
        "Worker failed to sync", w :> s)

    & Assert1(
      Cardinality(AllStores) <= 1,
      "Drafts are not eventually-consistent", [
          AB     |-> AvailableBrowserStores,
          AW     |-> ActiveWorkers,
          AT     |-> ActiveTabs,
          Stores |-> AllStores
        ])

    & Assert1(
      remote.drafts = AllDrafts,
      "Drafts are not stored remotely", [all |-> AllDrafts, remote |-> remote.drafts])

    & LET targetLive == { d \in target.drafts : ~d.tombstone }
          missing    == targetLive -- AllDrafts
          unexpected == AllDrafts -- targetLive
      IN Assert1(missing = {} & unexpected = {},
           "Final state doesn't match target state",
           [missing |-> missing, unexpected |-> unexpected])

MCContinue ==
  TLCGet("diameter") <= 50

========================================================================================================================
