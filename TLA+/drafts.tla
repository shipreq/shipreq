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
CONSTANT MCCheckTombstonesRemoval
CONSTANT MCMaxEditsPerTab
CONSTANT MCMaxPruneByProvDepth
CONSTANT MCMergeDraftsByContent
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
ASSUME MCCheckTombstonesRemoval \in BOOLEAN
ASSUME MCMaxEditsPerTab \in Nat
ASSUME MCMaxPruneByProvDepth \in Nat
ASSUME MCMergeDraftsByContent \in BOOLEAN
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
RemoteStoreCmd == "rcmd:T->R"
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
  type    : {syncTW},
  from    : Tab,
  to      : Worker,
  drafts  : Drafts,
  edit    : Option([prov: Provenance, editCount: Nat]) \* editCount is just for updating target
] ++ [
  type    : {syncWT},
  from    : Worker,
  to      : Tab,
  drafts  : Drafts,
  edit    : Option([draft: Draft, editCount: Nat]) \* editCount is just for updating target
] ++ [
  type    : {syncTR},
  from    : Tab,
  to      : {Remote},
  drafts  : DraftsNE
] ++ [
  type    : {RemoteStoreCmd},
  from    : Worker,
  to      : Tab,
  drafts  : Drafts
] ++  [
  type    : {ackRT},
  from    : {Remote},
  to      : Tab
] ++ [
  type    : {ackTW},
  from    : Tab,
  to      : Worker,
  rejected: BOOLEAN
] ++ [
  type    : {syncRT},
  from    : {Remote},
  to      : Tab,
  drafts  : DraftsNE
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
    status    : {clean},
    worker    : Worker,
    editCount : Nat,       \* Total number of edits made by a user in this tab
    tombstones: Tombstones \* Tombstones drafts that need to be sent to WW
  ] ++
  [
    status     : {dirty},
    worker     : Worker,
    drafts     : Drafts,        \* includes editDraft when its defined
    editDraft  : Option(Draft), \* last known draft for editor state - only defined when editUnsent | editSent
    editUnsent : BOOLEAN,       \* Whether a local edit exists that hasn't been sent to WW to be turned into a draft yet
    editSent   : BOOLEAN,       \* Whether a local edit has been sent to WW to be turned into a draft
    editCount  : Nat,           \* Total number of edits made by a user in this tab
    aborted    : BOOLEAN
  ]

WorkerSyncState ==
  [
    sync      : BOOLEAN, \* Whether we need to sync again
    syncing   : BOOLEAN, \* Whether we need to sync is in progress, awaiting an ACK
    tombstones: Tombstones
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

DraftInvariants(ds) ==
  & Assert1(
      Cardinality(ds) = Cardinality({d.worker : d \in ds}),
      "Duplicate drafts/worker:", ds)
  & \A d \in ds: Assert1(
      d.prov[d.worker] = 0,
      "Draft contains itself in its own provenance:", d)

StorageInvariants(s) ==
  DraftInvariants(s)

WorkerSyncStateInvariants(s) ==
  ~s.syncing => s.tombstones = {}

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
          & Assert1M("C1", ts.editCount <= MCMaxEditsPerTab, ts)
        & ts.status = clean =>
          & DraftInvariants(ts.tombstones)
        & ts.status = dirty =>
          & Assert1M("D1", ts.editUnsent | ts.editSent | ts.drafts != {}, ts)           \* ensure actually dirty
          & Assert1M("D2", ~ts.editDraft.isEmpty => ts.editDraft.get \in ts.drafts, ts) \* drafts includes editDraft
          & Assert1M("D3", ~(ts.editUnsent | ts.editSent) => ts.editDraft.isEmpty, ts)  \* editDraft only defined when there are local changes
          & DraftInvariants(ts.drafts)

InvariantsForWorkers ==
  & workers \in [Worker -> WorkerState]
  & \A w \in Worker :
      LET ws == workers[w]
      IN ws.status = live =>
          & ws.time > 0
          & \A s \in AsyncSrc : WorkerSyncStateInvariants(ws.sync[s])
          & DraftInvariants(ws.drafts)

InvariantsForTarget ==
  target \in [
    pending  : SUBSET [tab: Tab, editCount: Nat, tombstone: BOOLEAN],
    returning: SUBSET [tab: Tab, editCount: Nat, draft: Draft],
    drafts   : Drafts \* Note: provenance here solely represents, and is the source of truth for draft equality
  ]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

NonTombs(ds) == {d \in ds : ~d.tombstone}
JustTombs(ds) == {d \in ds : d.tombstone}

\* Find the index of a msg on the network and, make sure it's the next msg between .from and .to.
\* This is the communication channels between a source and target are not commutative; we can rely on the order not
\* changing.
PopMsgOfType(type) ==
  LET isNotNext(i) ==
        LET n == network[i] IN
          \E j \in 1..(i-1) :
            LET m == network[j] IN
              (n.from = m.from) & (n.to = m.to)
      i == SeqIndexOf(network, LAMBDA m: m.type = type)
  IN IF i = 0 | (i != 1 & isNotNext(i)) THEN
       0
     ELSE
       i

WorkerSyncStateIsStable(s) ==
  & ~s.sync
  & ~s.syncing

WorkerSyncStateEmpty == [
  sync       |-> FALSE,
  syncing    |-> FALSE,
  tombstones |-> {}
]

\* Start syncing.
\* WorkerSyncState => Option[WorkerSyncState]
WorkerSyncStart(s, drafts) ==
  LET doIt  == & s.sync
               & ~s.syncing
      next  == [ sync       |-> FALSE,
                 syncing    |-> TRUE,
                 tombstones |-> JustTombs(drafts)
               ]
  IN SomeWhen(doIt, next)

WorkerSyncComplete(s) ==
  IF s.syncing THEN
    [s EXCEPT !.syncing = FALSE, !.tombstones = {}]
  ELSE
    Fail1("[WorkerSyncComplete] Syncing not in progress.", s)

WorkerSyncQueueUp(s) ==
  [s EXCEPT !.sync = TRUE]

WorkerSyncQueueUpAllSrcs(syncState) ==
  [src \in AsyncSrc |-> WorkerSyncQueueUp(syncState[src])]

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
       [] s = clean       -> ts.tombstones
       [] s = nonExistant -> {}

ActiveWorkers ==
  { w \in Worker : workers[w].status != nonExistant }

ActiveBrowsers ==
  { workers[w].browser : w \in ActiveWorkers }

\* Set[(Browser, BrowserSrc)]
AvailableBrowserStores ==
  { x \in ActiveBrowsers \X BrowserSrc : ~browsers[x[1]][x[2]].isEmpty }

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

AddProv(draft, prov) ==
  LET mergeProv(w) ==
        IF w = draft.worker THEN
          0
        ELSE
          Max[draft.prov[w], prov[w]]
  IN [draft EXCEPT !.prov = [w \in Worker |-> mergeProv(w)]]

AddSelfToOwnProv(draft) ==
  LET w == draft.worker
  IN  [draft EXCEPT !.prov[w] = draft.time]

DraftsHaveSameId(x, y) ==
  & x.worker = y.worker
  & x.time = y.time

MergeDrafts(x, y) ==
  IF x.worker != y.worker THEN
    AddProv(x, AddSelfToOwnProv(y).prov)
  ELSE IF x.time = y.time THEN
    LET d == AddProv(x, y.prov)
    IN [d EXCEPT !.tombstone = @ | y.tombstone]
  ELSE
    LET new == IF x.time > y.time THEN x ELSE y
        old == IF x.time > y.time THEN y ELSE x
    IN AddProv(new, old.prov)

(* NOTE: Doesn't prune *)
AddDraft(storage, draft) ==
  LET sibling == SetFind(storage, LAMBDA d: d.worker = draft.worker)
  IN IF sibling.isEmpty THEN
       storage ++ {draft}
     ELSE
      LET s == sibling.get
      IN (storage -- {s}) ++ {MergeDrafts(s, draft)}

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
      LET new == match.get[1]
          old == match.get[2]
          ds2 == (ds -- {new, old}) ++ {MergeDrafts(new, old)}
      IN _PruneByProv(ds2, depth + 1)
PruneByProv(ds) == _PruneByProv(ds, 0)

\* Uses the target state to simulate comparison of drafts by equality.
PruneByEq(ds, tgt) ==
  IF MCMergeDraftsByContent THEN
    LET
      fix(ds2, d) ==
        LET o == SetFind(tgt.drafts, LAMBDA e: DraftsHaveSameId(d, e) & d.prov != e.prov)
        IN
          IF o.isEmpty THEN
            ds2
          ELSE
            LET t           == o.get
                es1         == { e \in ds : t.prov[e.worker] = e.time }
                es          == es1 ++ {d}
                merge(x, y) == AddProv(x, AddSelfToOwnProv(y).prov)
            IN
              IF es1 = {} THEN
                ds2
              ELSE
                (ds -- es) ++ {SetReduce(es, merge)}
    IN SetFold(ds, ds, fix)
  ELSE
    ds

Prune(drafts, tgt) ==
  PruneByProv(PruneByEq(drafts, tgt))

TargetAddPending(tgt, t, newEditCount, wantLive) ==
  LET add == [tab |-> t, editCount |-> newEditCount, tombstone |-> ~wantLive]
      rem == [add EXCEPT !.tombstone = ~@]
  IN [tgt EXCEPT !.pending = (@ -- {rem}) ++ {add}]

TargetDelPending(tgt, t, editCount) ==
  LET del(p) == p.tab = t & p.editCount <= editCount
  IN [tgt EXCEPT !.pending = { p \in @ : ~del(p) }]

TargetAddDrafts(tgt, drafts) ==
  \* IF drafts \notin Drafts THEN Fail1("Not a set: ", drafts) ELSE
  [tgt EXCEPT !.drafts = PruneByProv(AddDrafts(@, drafts))]

TargetDelDrafts(tgt, drafts) ==
  \* IF drafts \notin Drafts THEN Fail1("Not a set: ", drafts) ELSE
  TargetAddDrafts(tgt, { [d EXCEPT !.tombstone = TRUE] : d \in drafts })

\* When adding a new draft to our target state, we create a bunch of branches where the new draft
\* has the same content as an existing draft.
\* Returns a set of possible outcomes.
TargetAddNewDraft(tgt, d) ==
  LET notEqual     == TargetAddDrafts(tgt, {d})
      mkEqualTo(e) == [tgt EXCEPT !.drafts = (@ -- {e}) ++ {AddProv(e, AddSelfToOwnProv(d).prov)}]
      candidates   == { e \in tgt.drafts : e.worker != d.worker }
  IN
    IF MCMergeDraftsByContent & ~d.tombstone THEN
      { notEqual } ++ { mkEqualTo(e) : e \in candidates }
    ELSE
      { notEqual }

NewCleanTabState(workerOrPrevState, tombstones, editCountInc) ==
  LET havePrev  == workerOrPrevState \notin Worker
      w         == IF havePrev THEN workerOrPrevState.worker ELSE workerOrPrevState
      wasActive == havePrev & workerOrPrevState.status \in {clean,dirty}
  IN [
    status      |-> clean,
    worker      |-> w,
    editCount   |-> IF wasActive THEN workerOrPrevState.editCount + editCountInc ELSE editCountInc,
    tombstones  |-> tombstones
  ]

NewDirtyTabState(workerOrPrevState, ds, editDraft, editUnsent) ==
  LET havePrev  == workerOrPrevState \notin Worker
      w         == IF havePrev THEN workerOrPrevState.worker ELSE workerOrPrevState
      ps        == workerOrPrevState
      wasActive == havePrev & ps.status \in {clean,dirty}
      wasDirty  == wasActive & ps.status = dirty
  IN [
    status      |-> dirty,
    worker      |-> w,
    drafts      |-> ds,
    editDraft   |-> editDraft,
    editUnsent  |-> (IF wasDirty THEN ps.editUnsent | editUnsent ELSE editUnsent),
    editSent    |-> (IF wasDirty THEN ps.editSent ELSE FALSE),
    editCount   |-> (IF wasActive THEN ps.editCount ELSE 0) + (IF editUnsent THEN 1 ELSE 0),
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
AddDraftsToTab(ts, tgt, newDrafts, editResp, retainTombstones) ==
  LET info       == [ts |-> ts, newDrafts |-> newDrafts, edit |-> editResp, retainTombstones |-> retainTombstones]
      aborted    == ts.status = dirty & ts.aborted
      ds1        == newDrafts
      ds2        == IF editResp.isEmpty THEN ds1 ELSE LET d == editResp.get.draft IN AddDraft(ds1, IF aborted THEN [d EXCEPT !.tombstone=TRUE] ELSE d)
      \* ds3        == IF retainTombstones THEN ds2 ELSE { d \in ds2 : ~d.tombstone }
      ds4        == AddDrafts(TabDrafts(ts), ds2)
      ds         == Prune(ds4, tgt)
      ts2        == IF editResp.isEmpty THEN
                      ts
                    ELSE
                      LET e == editResp.get
                      IN
                        IF ts.status != dirty THEN
                          Fail1("[AddDraftsToTab] Edit response found but tab isn't dirty.", info)
                        ELSE IF ~ts.editSent THEN
                          Fail1("[AddDraftsToTab] Edit response found but tab editSent=FALSE", info)
                        ELSE IF ts.editUnsent THEN
                          \* Receive draft for old edit, but new edit still exists
                          [ts EXCEPT !.editSent = FALSE, !.editDraft = Some(e.draft)]
                        ELSE
                          \* Edit resolved by new draft
                          [ts EXCEPT !.editSent = FALSE, !.editDraft = None]
      pending    == ts2.status = dirty & (ts2.editUnsent | ts2.editSent)
      editDraft2 == IF ts.status = dirty THEN ts2.editDraft ELSE None
      result     == IF ts.status = clean THEN
                      IF (\A d \in ds : d.tombstone) THEN
                        NewCleanTabState(ts2, ds, 0)
                      ELSE
                        NewDirtyTabState(ts2, ds, None, FALSE)
                   ELSE IF ts.status = loading THEN
                      [ts2 EXCEPT !.drafts = ds]
                    ELSE IF pending THEN
                      IF editDraft2.isEmpty THEN
                        [ts2 EXCEPT !.drafts = ds]
                      ELSE
                        LET e          == editDraft2.get
                            w          == e.worker
                            match(d)   == | d.worker = w
                                          | d.prov[w] >= e.time
                            editDraft3 == SetSoleElement({ d \in ds : match(d) })
                        IN
                          IF editDraft3.isEmpty THEN
                            Fail1("[AddDraftsToTab] Didn't find editDraft in new draft set.", [ds |-> ds, editDraft |-> e])
                          ELSE
                            [ts2 EXCEPT !.drafts = ds, !.editDraft = editDraft3]
                    ELSE IF aborted THEN
                      NewCleanTabState(ts2, JustTombs(ds), 0)
                    ELSE IF ds = {} THEN
                      NewCleanTabState(ts2, {}, 0)
                    ELSE
                      NewDirtyTabState(ts2, ds, editDraft2, FALSE)
  IN
    IF ts.status \in {clean,dirty,loading} THEN
      \* LET lookingFor(tsx) == & TLCGet("diameter") = 13 & ts.status = dirty & tsx.status = dirty & ~editResp.isEmpty & editResp.get.rev = 1
      \*                        & \E ds \in dss : \E d \in ds : d.tombstone & d.time = 2
      \*     debug           == \E x \in tss : lookingFor(x)
      \* IN
      \*   IF debug THEN
      \*     LogRet([
      \*                     step |-> TLCGet("diameter"),
      \*                     aborted |-> aborted,
      \*                     pending |-> pending,
      \*                     ts |-> ts,
      \*                     ts2 |-> ts2,
      \*                     ds |-> [
      \*                     ds0 |-> TabDrafts(ts),
      \*                     ds2 |-> ds2,
      \*                     ds4 |-> ds4,
      \*                     dss |-> dss
      \*                     ],
      \*                     edit |-> editResp,
      \*                     tss |-> tss
      \*     ], tss) ELSE
        result
    ELSE
      Fail1("[AddDraftsToTab] Don't know how to handle tab state:", info)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Tests

AddDraftTest1 ==
  IF Cardinality(Worker) < 1 THEN
    PrintT("Skipping AddDraftTest1")
  ELSE
    LET w    == CHOOSE w \in Worker : TRUE
        p(a) == [NoProv EXCEPT ![w] = a]
    IN

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w, time |-> 1, prov |-> p(0), tombstone |-> b]}
            draft  ==  [worker |-> w, time |-> 1, prov |-> p(0), tombstone |-> ~b]
            actual == AddDraft(input, draft)
            expect == {[worker |-> w, time |-> 1, prov |-> p(0), tombstone |-> TRUE]}
        IN AssertEq("AddDraftTest1.1", actual, expect)

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w, time |-> 1, prov |-> p(0), tombstone |-> ~b]}
            draft  ==  [worker |-> w, time |-> 2, prov |-> p(0), tombstone |-> b]
            actual == AddDraft(input, draft)
            expect == {draft}
        IN AssertEq("AddDraftTest1.2", actual, expect)

PruneTest1 ==
  IF Cardinality(Worker) < 1 THEN
    PrintT("Skipping PruneTest1")
  ELSE
    LET w    == CHOOSE w \in Worker : TRUE
        p(a) == [NoProv EXCEPT ![w] = a]
        tgt  == [drafts |-> {}]
    IN

      & \A b \in BOOLEAN :
        LET d1     == [worker |-> w, time |-> 1, prov |-> p(0), tombstone |-> b]
            d2     == [worker |-> w, time |-> 2, prov |-> p(0), tombstone |-> ~b]
            actual == Prune({d1, d2}, tgt)
            expect == {d2}
        IN AssertEq("PruneTest1.1", actual, expect)

PruneTest2 ==
  IF Cardinality(Worker) < 2 THEN
    PrintT("Skipping PruneTest2")
  ELSE
    LET w1     == CHOOSE w \in Worker : TRUE
        w2     == CHOOSE w \in Worker : w != w1
        p(a,b) == [NoProv EXCEPT ![w1] = a, ![w2] = b]
        tgt    == [drafts |-> {}]
    IN

      & LET input  == {[worker |-> w1, time |-> 1, prov |-> p(0, 1), tombstone |-> FALSE],
                       [worker |-> w2, time |-> 1, prov |-> p(0, 0), tombstone |-> FALSE]}
            expect == {[worker |-> w1, time |-> 1, prov |-> p(0, 1), tombstone |-> FALSE]}
            actual == Prune(input, tgt)
        IN AssertEq("PruneTest2.1", actual, expect)

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w1, time |-> 2, prov |-> p(0, 4), tombstone |-> b],
                       [worker |-> w2, time |-> 3, prov |-> p(0, 0), tombstone |-> ~b]}
            expect == {[worker |-> w1, time |-> 2, prov |-> p(0, 4), tombstone |-> b]}
            actual == Prune(input, tgt)
        IN AssertEq("PruneTest2.2", actual, expect)

      & \A b \in BOOLEAN :
        LET input  == {[worker |-> w1, time |-> 2, prov |-> p(0, 0), tombstone |-> b],
                       [worker |-> w2, time |-> 3, prov |-> p(0, 0), tombstone |-> ~b]}
            expect == input
            actual == Prune(input, tgt)
        IN AssertEq("PruneTest2.3", actual, expect)

SanityCheck ==
  & UtilSanityCheck
  & AddDraftTest1
  & PruneTest1
  & PruneTest2
  & PrintT("SanityCheck passed")

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Actions

RemoteRecvDrafts ==
  LET i == PopMsgOfType(syncTR)
  IN
    & i != 0
    & LET msg == network[i]
          resp == [
            type |-> ackRT,
            from |-> Remote,
            to   |-> msg.from
          ]
          broadcastTo(tab, ds) == [
            type   |-> syncRT,
            from   |-> Remote,
            to     |-> tab,
            drafts |-> ds
          ]
          otherTabs  == { t \in RemoteConnectedTabs : t != msg.from }
          ds1        == AddDrafts(remote.drafts, msg.drafts)
          ds2        == ds1 \* TODO: NonTombs(ds1) \* Don't store tombstones for now
          ds         == Prune(ds2, target)
          broadcasts == IF ds = {} THEN <<>> ELSE SetFold(otherTabs, <<>>, LAMBDA q,t: q \o <<broadcastTo(t, ds)>>)
      IN
        & remote' = [remote EXCEPT !.drafts = ds]
        & network' = RemoveAt(network, i) \o <<resp>> \o broadcasts
        & UNCHANGED << browsers, tabs, workers, target >>

TabRecvDraftsFromRemote ==
  LET i == PopMsgOfType(syncRT)
  IN
    & i != 0
    & LET msg        == network[i]
          t          == msg.to
          ts         == tabs[t]
          w          == ts.worker
          ts2        == AddDraftsToTab(ts, target, msg.drafts, None, TRUE)
          dsForWW    == PruneByProv(msg.drafts ++ TabDrafts(ts))
          msgWW      == [
            type   |-> syncTW,
            from   |-> t,
            to     |-> w,
            drafts |-> dsForWW,
            edit   |-> None
          ]
      IN
        & tabs' = [tabs EXCEPT ![t] = ts2]
        & RecvResp(i, msgWW)
        & UNCHANGED << browsers, remote, workers, target >>

TabRecvDraftsFromWorker ==
  LET i == PopMsgOfType(syncWT)
  IN
    & i != 0
    & LET msg        == network[i]
          t          == msg.to
          ts         == tabs[t]
          ts2(tgt)   == AddDraftsToTab(ts, tgt, msg.drafts, msg.edit, FALSE)
          tabs2(tgt) == [tabs EXCEPT ![t] = ts2(tgt)]
      IN
        & RecvMsg(i)
        & IF msg.edit.isEmpty THEN
            & tabs' = tabs2(target)
            & UNCHANGED target
          ELSE
            LET e    == msg.edit.get
                ret  == CHOOSE r \in target.returning : r.tab = t & r.editCount = e.editCount
                tgt1 == [target EXCEPT !.returning = @ -- {ret}]
                tgts == TargetAddNewDraft(tgt1, ret.draft)
            IN
              \E tgt \in tgts:
                & tabs' = tabs2(tgt)
                & target' = tgt
        & UNCHANGED << browsers, remote, workers >>

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
            ELSE LET ds2       == Prune(AddDrafts(ts.drafts, srcDrafts), target)
                     awaiting2 == ts.awaiting -- {src}
                     ts2       == [ts EXCEPT !.drafts = ds2, !.awaiting = awaiting2]
                 IN {[tabs EXCEPT ![t] = ts2]}
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
            NewCleanTabState(w, {}, 0)
          ELSE
            NewDirtyTabState(w, ts.drafts, None, FALSE)
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

\* In order to avoid lots of annoying and difficult logic, a tab will refuse to send drafts to remote
\* when the tab and worker are out of sync.
TabRecvRemoteStoreCmd ==
  LET i == PopMsgOfType(RemoteStoreCmd)
  IN
    & i != 0
    & LET msg    == network[i]
          w      == msg.from
          t      == msg.to
          ds     == msg.drafts
          reject == [
                      type     |-> ackTW,
                      from     |-> t,
                      to       |-> w,
                      rejected |-> TRUE
                    ]
          send   == [
                      type   |-> syncTR,
                      from   |-> t,
                      to     |-> Remote,
                      drafts |-> ds
                    ]
      IN
        & IF NonTombs(ds) = NonTombs(TabDrafts(t)) THEN
            RecvResp(i, send)
          ELSE
            RecvResp(i, reject)
        & UNCHANGED << browsers, workers, remote, tabs, target >>

TabSendChangesToWorker ==
  \E t \in Tab:
    LET ts == tabs[t]
    IN
      & ts.status = dirty
      & ts.editUnsent
      & ~ts.editSent
      & SendMsg([
          type   |-> syncTW,
          from   |-> t,
          to     |-> ts.worker,
          drafts |-> {},
          edit   |-> Some([
                       prov      |-> IF ts.editDraft.isEmpty THEN NoProv ELSE ts.editDraft.get.prov,
                       editCount |-> ts.editCount
                     ])
        ])
      & tabs' = [tabs EXCEPT ![t].editUnsent = FALSE, ![t].editSent = TRUE]
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
      & LET tgt1 == TargetDelDrafts(target, ts.drafts)
            tgt2 == [tgt1 EXCEPT
                      !.returning = { IF r.tab = t THEN [r EXCEPT !.draft.tombstone = TRUE] ELSE r : r \in @ },
                      !.pending   = { IF p.tab = t THEN [p EXCEPT !.tombstone       = TRUE] ELSE p : p \in @ }
                    ]
            tgt3 == TargetDelDrafts(tgt2, ts.drafts)
            \* clearPending(tgt) == [tgt EXCEPT !.pending = { p \in @ : p.tab != t }]
        IN
          & IF ts.editSent THEN
              \* Edit in-flight to worker.
              \* Note: If there's a queued edit, we still let it go through to WW so the user can restore it later.
              & tabs' = [tabs EXCEPT ![t].aborted = TRUE, ![t].editCount = @ + 1]
              & target' = tgt3
            ELSE IF ts.editUnsent THEN
              \* New edit queued.
              \* Note: We'll still let the queued edit go through before abortion because we still want it saved
              \* so the user can restore it later.
              & tabs' = [tabs EXCEPT ![t].aborted = TRUE, ![t].editCount = @ + 1]
              & target' = tgt3
            ELSE
              \* No non-local edit exists
              IF ts.drafts = {} THEN
                Fail1("[UserAbort] Tab has no local change and no drafts.", ts)
              ELSE
                \* User has marked existing drafts as aborted.
                \* Update the local state and mark it as needing to be sent to WW
                LET ds == MergeDraftsIntoTombstoneNE(ts.drafts)
                IN
                  & \E d \in ds:
                      tabs' = [tabs EXCEPT ![t].aborted    = TRUE,
                                           ![t].editDraft  = SomeWhen(~@.isEmpty, d),
                                           ![t].editUnsent = TRUE,
                                           ![t].editCount  = @ + 1]
                  & target' = TargetAddPending(tgt3, t, ts.editCount + 1, FALSE)
          & UNCHANGED << browsers, workers, network, remote >>

UserEditClean ==
  \E t \in Tab:
    LET ts   == tabs[t]
        w    == ts.worker
        ts2  == NewDirtyTabState(ts, ts.tombstones, None, TRUE)
        tgt1 == TargetAddPending(target, t, ts2.editCount, TRUE)
        tgt2 == [tgt1 EXCEPT !.drafts = { d \in @ : ~(d.tombstone & d.worker = w) }]
    IN
      & ts.status = clean
      & ts.editCount < MCMaxEditsPerTab
      & tabs' = [tabs EXCEPT ![t] = ts2]
      & target' = tgt2
      & UNCHANGED << browsers, workers, network, remote >>

UserEditDirty ==
  \E t \in Tab: LET ts == tabs[t] IN
    & ts.status = dirty
    & ~ts.aborted
    & ~ts.editUnsent
    & ts.editCount < MCMaxEditsPerTab
    & tabs' = [tabs EXCEPT ![t].editUnsent = TRUE, ![t].editCount = @ + 1]
    & target' = TargetAddPending(target, t, tabs'[t].editCount, TRUE)
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
  LET i == PopMsgOfType(syncTW)
  IN
    & i != 0
    & LET msg      == network[i]
          w        == msg.to
          ws       == workers[w]
          t2       == IF msg.edit.isEmpty THEN ws.time ELSE ws.time + 1
          editF(e) == [draft |-> NewDraft(w, e.prov), editCount |-> e.editCount]
          edit     == OptionMap(msg.edit, editF)
          ds       == Prune(AddDrafts(ws.drafts, AddDrafts(msg.drafts, {e.draft : e \in OptionToSet(edit)})), target)
          ws2      == [workers EXCEPT ![w].drafts = ds, ![w].time = t2, ![w].sync = WorkerSyncQueueUpAllSrcs(@)]
          msgs     == WorkerBroadcastToTabMsgs(w, ds, LAMBDA t: IF t = msg.from THEN edit ELSE None)
          net1     == RemoveAt(network, i)
      IN
        & IF ds = {} THEN
            & RecvMsg(i)
            & UNCHANGED workers
          ELSE
            & workers' = ws2
            & network' = SetFold(msgs, net1, Append)
        & UNCHANGED << browsers, remote, tabs >>
        & IF msg.edit.isEmpty THEN
            UNCHANGED target
          ELSE
            LET e  == msg.edit.get
                t  == msg.from
                p  == SetSoleElement({p \in target.pending : p.tab = t & p.editCount <= e.editCount}).get
                d1 == NewDraft(w, e.prov)
                d  == IF p.tombstone THEN [d1 EXCEPT !.tombstone = TRUE] ELSE d1
                r  == [tab |-> t, editCount |-> e.editCount, draft |-> d]
            IN target' = [target EXCEPT !.pending = @ -- {p}, !.returning = @ ++ {r}]

ActiveBrowserSrcs(b) ==
  { s \in BrowserSrc : ~browsers[b][s].isEmpty }

\* This happens periodically without a trigger event.
\* No need to track whether a write is needed (i.e. BrowserSrcSync isn't in .sync) because we need to check
\* for reads too.
WorkerSyncWithBrowserStorage ==
  \E w \in Worker : LET ws == workers[w] IN
    & ws.status = live
    & \E s \in ActiveBrowserSrcs(ws.browser) :
      LET b         == ws.browser
          bs        == browsers[b]
          ds        == Prune(AddDrafts(bs[s].get, ws.drafts), target)
          workers2  == [workers EXCEPT
                         ![w].drafts = ds,
                         ![w].sync = IF ws.drafts = ds THEN @ ELSE WorkerSyncQueueUpAllSrcs(@)
                       ]
      IN
        & browsers' = [browsers EXCEPT ![b][s] = Some(ds)]
        & workers'  = workers2
        & network'  = SetFold(WorkerBroadcastToTabMsgs(w, ds, LAMBDA t: None), network, Append)
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
          drafts |-> ds
        ]
    IN
      & ws.status = live
      & ~s2.isEmpty
      & IF ds = {} THEN
          \* Nothing to send
          & workers' = [workers EXCEPT ![w].sync[Remote] = WorkerSyncStateEmpty]
          & UNCHANGED network
        ELSE
          \E t \in WorkerTabs(w):
            & SendMsg(cmd(t))
            & workers' = [workers EXCEPT ![w].sync[Remote] = s2.get]
      & UNCHANGED << browsers, remote, tabs, target >>

TabRecvRemoteAck ==
  LET i == PopMsgOfType(ackRT)
  IN
    & i != 0
    & LET msg    == network[i]
          t      == msg.to
          newMsg == [
            type     |-> ackTW,
            from     |-> t,
            to       |-> tabs[t].worker,
            rejected |-> FALSE
          ]
      IN
        & RecvResp(i, newMsg)
        & UNCHANGED << browsers, remote, workers, tabs, target >>

WorkerRecvRemoteAck ==
  LET i == PopMsgOfType(ackTW)
  IN
    & i != 0
    & LET msg    == network[i]
          w      == msg.to
          ws     == workers[w]
          sync   == ws.sync[Remote]
          sync2  == WorkerSyncComplete(sync)
          sync3  == IF msg.rejected THEN WorkerSyncQueueUp(sync2) ELSE sync2
          remove == IF msg.rejected THEN {} ELSE sync.tombstones
          ds2    == ws.drafts -- remove
          ws2    == [ws EXCEPT !.sync[Remote] = sync3, !.drafts = ds2]
      IN
        & workers' = [workers EXCEPT ![w] = ws2]
        & RecvMsg(i)
        & UNCHANGED << browsers, remote, tabs, target >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & target  = [pending |-> {}, returning |-> {}, drafts |-> {}]
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

\* Set[Storage]
AllStores ==
  LET draftsB == { browsers[x[1]][x[2]].get : x \in AvailableBrowserStores }
      draftsT == { TabDrafts(t) : t \in ActiveTabs }
      draftsW == { workers[w].drafts : w \in ActiveWorkers }
      draftsR == { remote.drafts }
      all     == draftsB ++ draftsT ++ draftsW ++ draftsR
  IN
    IF MCCheckTombstonesRemoval THEN
      all
    ELSE
      { NonTombs(s) : s \in all }

\* Set[Draft]
AllDrafts ==
  UNION AllStores

StableInvariants ==
  IsStable =>
    \* & LogStates

    & \A t \in Tab :
      LET ts            == tabs[t]
          info          == [tab |-> t, tabState |-> ts]
          hasEditBudget == ts.editCount < MCMaxEditsPerTab
      IN
        |
          & ts.status = clean
          & Assert1(ts.tombstones = {}, "Tombstones not cleared out from tab", info)
        |
          & ts.status = dirty
          & Assert1(~ts.editUnsent, "Local changes not propagated", info)
          & Assert1(~ts.editSent, "Tab stuck waiting for new draft creation", info)
          & Assert1(~ts.aborted, "Tab stuck in aborted state", info)
          & hasEditBudget => Assert1(ENABLED(UserAbort), "Tab has draft but can't be aborted", info)
        |
          & ts.status = nonExistant

    & Assert1(
        target.pending = {} & target.returning = {},
        "Incomplete edits in target ", [pending |-> target.pending, returning |-> target.returning])

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

    & LET r == IF MCCheckTombstonesRemoval THEN remote.drafts ELSE NonTombs(remote.drafts)
      IN  Assert1(
            r = AllDrafts,
            "Drafts are not stored remotely", [all |-> AllDrafts, remote |-> r])

    & LET targetLive == NonTombs(target.drafts)
          missing    == targetLive -- AllDrafts
          unexpected == AllDrafts -- targetLive
      IN Assert1(missing = {} & unexpected = {},
           "Final state doesn't match target state",
           [missing |-> missing, unexpected |-> unexpected])

MCContinue ==
  TLCGet("diameter") <= 60

========================================================================================================================
