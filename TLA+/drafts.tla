---------------------------------------------------- MODULE drafts ----------------------------------------------------
(*
What's does this spec provide?
==============================
- Drafts should eventually propagate to all live tabs
- Drafts are never lost unless they're merged (manually or automatically), or completed by a user (whether aborted or committed)
- Drafts are removed from all storage locations once obsolete
- Users are prompted to keep a tab open iff we can't guarantee the draft won't be lost

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
- make each browser storage potentially unavailable
*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Browser
CONSTANT BrowserSrcAsync
CONSTANT BrowserSrcSync
CONSTANT Tab
CONSTANT Worker

ASSUME & IsFiniteSet(Browser)
       & IsFiniteSet(BrowserSrcAsync)
       & IsFiniteSet(BrowserSrcSync)
       & IsFiniteSet(Tab)
       & IsFiniteSet(Worker)

MCSymmetry ==
  Permutations(Browser) ++
  Permutations(BrowserSrcAsync) ++
  Permutations(BrowserSrcSync) ++
  Permutations(Tab) ++
  Permutations(Worker)

VARIABLE browsers
VARIABLE network
VARIABLE remote
VARIABLE tabs
VARIABLE workers

vars == << browsers, network, remote, tabs, workers >>

varDesc == [
  browsers |-> browsers,
  network  |-> network,
  remote   |-> remote,
  tabs     |-> tabs,
  workers  |-> workers]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Types

Provenance  == [Worker -> Nat]                               \* i.e. Map[WorkerId, Time]
Draft       == [worker: Worker, time: Nat, prov: Provenance] \* no need to include draft content
Drafts      == SUBSET Draft                                  \* i.e. Set[Draft]

clean       == "clean"
conflicted  == "conflicted"
dirty       == "dirty"
live        == "live"
nonExistant == "nonExistant"
server      == "server"
loading     == "loading"
Remote      == "Remote"

Msg ==
  [from: Tab, to: Worker, drafts: Drafts, newEdit: BOOLEAN]

NetworkState ==
  Seq(Msg) \* i.e. List[Msg]

BrowserSrc ==
  BrowserSrcAsync ++ BrowserSrcSync

BrowserState ==
  [BrowserSrc -> Drafts]

AnySrc ==
  BrowserSrc ++ {Remote}

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
    status: {conflicted},
    worker: Worker,
    drafts: { ds \in Drafts : Cardinality(ds) > 1 }
  ]

WorkerState ==
  [status: {nonExistant}] ++
  [
    status : {live},
    browser: Browser,
    time   : Nat,
    drafts : Drafts
  ]

TypeInvariantsBrowsers == browsers \in [Browser -> BrowserState]
TypeInvariantsNetwork  == network  \in NetworkState
TypeInvariantsRemote   == remote   \in Drafts
TypeInvariantsTabs     == tabs     \in [Tab -> TabState]
TypeInvariantsWorkers  == workers  \in [Worker -> WorkerState]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Data

StorageInvariants(s) ==
  & Assert1(
      Cardinality(s) = Cardinality({d.worker : d \in s}),
      "Duplicate drafts/worker:", s)
  & \A d \in s: Assert1(
      d.prov[d.worker] = 0,
      "Draft contains itself in its own provenance:", d)

DataInvariantsBrowsers ==
  \A b \in Browser :
    LET bs == browsers[b]
    IN \A src \in BrowserSrc : StorageInvariants(bs[src])

DataInvariantsNetwork ==
  \A i \in DOMAIN network :
    LET msg == network[i]
        to == msg.to
        participants(a, b) ==
          | msg.from \in a & to \in b
          | msg.from \in b & to \in a
    IN
      & participants(Tab, Worker) | participants(Tab, {server})
      & to \in Worker => workers[to].status = live
      & to \in Tab => tabs[to].status \in {clean, dirty, conflicted}

DataInvariantsRemote ==
  StorageInvariants(remote)

DataInvariantsTabs ==
  TRUE

DataInvariantsWorkers ==
  \A w \in Worker :
    LET ws == workers[w]
    IN ws.status = live => ws.time > 0

Init ==
  & browsers = [b \in Browser |-> [s \in BrowserSrc |-> {}]]
  & network  = <<>>
  & remote   = {}
  & tabs     = [t \in Tab |-> [status |-> nonExistant]]
  & workers  = [w \in Worker |-> [status |-> nonExistant]]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

SendMsg(msg) ==
  network' = Append(network, msg)

RecvMsg(i) ==
  network' = RemoveAt(network, i)

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

(* NOTE: Doesn't prune *)
AddDraft(storage, draft) ==
  LET old == SetFind(storage, LAMBDA d: d.worker = draft.worker)
  IN IF old.isEmpty
     THEN storage ++ {draft}
     ELSE (storage -- {old.get}) ++ {AddProv(draft, old.get.prov)}

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
RECURSIVE PruneByProv(_)
PruneByProv(ds) ==
  LET f2(d1, d2) ==
        LET t == d1.prov[d2.worker]
            p == d1.worker != d2.worker & t > 0 & d2.time <= t
        IN IF p THEN Some(<<d1, d2>>) ELSE None
      f(d1) == SetCollectFirst(ds, LAMBDA d2: f2(d1, d2))
      match == SetCollectFirst(ds, f)
  IN IF match.isEmpty
     THEN ds
     ELSE LET d1  == match.get[1]
              d2  == match.get[2]
              d   == AddProv(d1, d2.prov)
              ds2 == (ds -- {d1, d2}) ++ {d}
          IN PruneByProv(ds2)

\* Returns a set of possible outcomes
PruneByEq(ds) ==
  LET equalSets == { x \in SUBSET(ds) : Cardinality(x) > 1 }
      merge(es) == SetReduce(es, LAMBDA x,y: AddProv(x, y.prov))
  IN  { (ds -- es) ++ merge(es) : es \in equalSets }

\* Returns a set of possible outcomes
Prune(drafts) ==
  PruneByEq(drafts) ++ {PruneByProv(drafts)}

\* OnEdit(w) ==
\*   LET
\*     ws        == workers[w]
\*     t         == ws.time
\*     lastEdit2 == IF ws.editor.status = closed THEN ws.lastEdit ELSE t
\*     prevProv  == IF ws.editor.status = closed THEN NoProv ELSE ws.editor.draft.prov
\*     draft2    == NewDraft(w, prevProv)
\*     editor2   == [status |-> dirty, draft |-> draft2]
\*   IN [workers EXCEPT ![w] = [ws EXCEPT
\*         !.time      = t + 1,
\*         !.editor    = editor2,
\*         !.lastEdit  = lastEdit2,
\*         !.syncQueue = Store(@, draft2)
\*       ]]

NewTabState(w, prunedDrafts) ==
  LET cleanState    == [worker |-> w, status |-> clean]
      dirtyState(d) == [worker |-> w, status |-> dirty, prevDraft |-> Some(d), localChange |-> FALSE]
      conflictState == [worker |-> w, status |-> conflicted, drafts |-> prunedDrafts]
      soleDraft     == SetSoleElement(prunedDrafts)
  IN
    IF prunedDrafts = {} THEN
      cleanState
    ELSE IF soleDraft.isDefined THEN
      dirtyState(soleDraft.get)
    ELSE
      conflictState

\* ------------------------------------------------------------------------------------------------------------------------
\* Actions

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
          browserAttempts == UNION { Attempt(src, bs[src]) : src \in BrowserSrc }
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
            & workers' = [workers EXCEPT ![w] = [
                  status  |-> live,
                  browser |-> b,
                  time    |-> 1,
                  drafts  |-> {} \* TODO load in stages just like TabNew
                ]]
        | \* Existing worker
          & workers[w].status = live
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
      & tabs' = [tabs EXCEPT ![t] = NewTabState(w, ts.drafts)]
      & SendMsg([from |-> t, to |-> w, drafts |-> ts.drafts, newEdit |-> FALSE])
      & UNCHANGED << browsers, remote, workers >>

UserEditClean ==
  \E t \in Tab:
    LET ts  == tabs[t]
        w   == ts.worker
        ts2 == [worker |-> w, status |-> dirty, draft |-> None, localChange |-> TRUE]
    IN
      & ts.status = clean
      & tabs' = [tabs EXCEPT ![t] = ts2]
      & UNCHANGED << browsers, workers, network, remote >>

WorkerHearTab ==
  LET i == SeqIndexOf(network, LAMBDA m: m.to \in Worker)
  IN
    & i != 0
    & LET msg == network[i]
          w   == msg.to
          ws  == workers[w]
          dss == Prune(AddDrafts(ws.drafts, msg.drafts))
          wss == { [workers EXCEPT ![w].drafts = ds] : ds \in dss }
      IN
        & ~msg.newEdit \* TODO later
        & RecvMsg(i)
        & workers' \in wss
        & UNCHANGED << browsers, remote, tabs >>

\* WorkerSync ==
\*   \E w \in Worker:
\*     & workers[w].status = live
\*     & LET ws == workers[w]
\*           b  == workers[w].browser
\*           bs == browsers[b]
\*           SyncWith(src)
\*       IN

\* TabTellWorker ==
\*   \E t \in Tab:
\*     LET ts == tabs[t]
\*     IN
\*       & ts.status = dirty
\*       & ts.localChange
\*       & tabs' = [tabs EXCEPT ![t].localChange = FALSE]
\*       & SendMsg([from |-> t, to |-> ts.worker, drafts |-> ts.prevDraft.toSet, newEdit |-> TRUE])
\*       & UNCHANGED << browsers, workers, remote >>

\* UserEditDirty ==
\*   \E t \in Tab:
\*     & tabs[t].status = dirty
\*     & ~tabs[t].localChange
\*     & tabs' = [tabs EXCEPT ![t].localChange = TRUE]
\*     & UNCHANGED << browsers, workers, network, remote >>

\* DraftNew ==
\*   \E w \in Worker:
\*     & workers[w].status = live
\*     & LET ws == workers[w]
\*        IN & ws.editor.status = closed
\*           & workers' = OnEdit(w)
\*           & browsers' \in StoreClientSide(ws.browser, NewDraft(w, NoProv))
\*           & UNCHANGED << network, remote >>

\* DraftEdit ==
\*   \E w \in Worker : workers[w].status = live
\*     & LET ws == workers[w]
\*        IN & ws.editor.status = dirty
\*           & ws.lastEdit != ws.time - 1 \* Avoid consecutive edits / infinite model
\*           & workers' = OnEdit(w)
\*           & browsers' \in StoreClientSide(ws.browser, NewDraft(w, ws.editor.draft.prov))
\*           & UNCHANGED << network, remote >>

\* WorkerSend ==
\*   & \E w \in Worker :
\*     & workers[w].status = live
\*     & workers[w].syncQueue != {}
\*     & workers' = [workers EXCEPT ![w].syncQueue = {}] \* In reality we'll only clear after confirmed received
\*     & SendMsg([worker |-> w, toSvr |-> TRUE, drafts |-> workers[w].syncQueue])
\*     & UNCHANGED << browsers, remote >>

\* RemoteRecv ==
\*   \E i \in (1..Len(network)) :
\*     & network[i].toSvr
\*     & RecvMsg(i)
\*     & remote' = StoreAll(remote, network[i].drafts)
\*     & UNCHANGED << browsers, workers >>

\* \* Will websockets periodically push? Will workers request?
\* \* As far as the spec goes it doesn't matter.
\* RemoteSend ==
\*   & remote != {}
\*   & \E w \in Worker:
\*     & workers[w].status = live
\*     & ~(\E i \in DOMAIN network : ~network[i].toSvr & network[i].worker = w) \* Don't re-send if msg already on the way
\*     & SendMsg([worker |-> w, toSvr |-> FALSE, drafts |-> remote])
\*     & UNCHANGED << browsers, workers, remote >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Next ==
  \* | TabTellWorker
  \* | UserEditDirty
  | TabLoad
  | TabNew
  | TabStart
  | UserEditClean
  | WorkerHearTab
  \* | WorkerSync

Spec == Init & [][Next]_<<vars>>

========================================================================================================================
