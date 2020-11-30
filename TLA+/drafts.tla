---------------------------------------------------- MODULE drafts ----------------------------------------------------
(*
What's does this spec provide?
==============================
- Drafts should propagate to all live tabs
- All drafts must either be resolved/merged (manually or automatically), or present to be resolved/merged. Drafts can't be lost or disappear.
- Once all drafts are resolved, all drafts should be removed from local and remote storage

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

- Provenance maps have keys for all workers cos TLA+ is a piece of shit.
  When the value=0 it means the K:V entry doesn't really exist in the map.
*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Browser
CONSTANT Tab
CONSTANT Worker

ASSUME & IsFiniteSet(Browser)
       & IsFiniteSet(Tab)
       & IsFiniteSet(Worker)

MCSymmetry = Permutations(Browser) ++ Permutations(Tab) ++ Permutations(Worker)

VARIABLE browsers
VARIABLE network
VARIABLE remote
VARIABLE tabs
VARIABLE workers

vars = << browsers, network, remote, tabs, workers >>

varDesc = [
  browsers |-> browsers,
  network  |-> network,
  remote   |-> remote,
  tabs     |-> tabs,
  workers  |-> workers]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Types

Provenance  = [Worker -> Nat]                               \* i.e. Map[WorkerId, Time]
Draft       = [worker: Worker, time: Nat, prov: Provenance] \* no need to include draft content
Storage     = SUBSET Draft                                  \* i.e. Set[Draft]

clean       = "clean"
conflicted  = "conflicted"
dirty       = "dirty"
live        = "live"
nonExistant = "nonExistant"
server      = "server"

NetworkParticipant =
  Worker ++ Tab ++ server

Msg = [
  from  : NetworkParticipant,
  to    : NetworkParticipant,
  drafts: SUBSET Draft \* i.e. Set[Draft]
]

NetworkState =
  Seq(Msg) \* i.e. List[Msg]

BrowserState = [
  ls : Storage, \* localStorage
  idb: Storage] \* indexedDB

TabState =
  [status: {nonExistant}] ++
  [
    status    : {clean},
    worker    : Worker
  ] ++
  [
    status    : {dirty},
    worker    : Worker,
    draft     : Draft,
    dirtySince: Nat \* Value is worker time
  ] ++
  [
    status    : {conflicted},
    worker    : Worker,
    drafts    : SUBSET Draft
  ]

WorkerState =
  [status: {nonExistant}] ++
  [
    status   : {live},
    browser  : Browser,
    time     : Nat
    \* syncQueue: SUBSET Draft \* Drafts to send to the server
  ]

RemoteState =
  Storage

TypeInvariants =
  & browsers \in [Browser -> BrowserState]
  & network  \in NetworkState
  & remote   \in Storage
  & tabs     \in [Tab -> TabState]
  & workers  \in [Worker -> WorkerState]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Data

StorageInvariants(s) =
  & Assert1(
      Cardinality(s) == Cardinality({d.worker : d \in s}),
      "Duplicate drafts/worker:", s)
  & \A d \in s: Assert1(
      d.prov[d.worker] == 0,
      "Draft contains itself in its own provenance:", d)

DataInvariants =
  \* & PrintT(varDesc)

  & \A b \in Browser :
    LET bs = browsers[b]
    IN & StorageInvariants(bs.idb)
       & StorageInvariants(bs.ls)

  & \A t \in Tab :
    LET ts = tabs[t]
    IN ts.status == conflicted => ts.drafts != {}

  & \A w \in Worker :
    LET ws = workers[w]
    IN ws.status == live => ws.time > 0

  & \A i \in DOMAIN network :
    LET msg = network[i]
        participants(a, b) =
          | msg.from \in a & msg.to \in b
          | msg.from \in b & msg.to \in a
    IN
      | participants(Tab, Worker)
      | participants(Tab, {server})

  & StorageInvariants(remote)

Init =
  & browsers == [b \in Browser |-> [ls |-> {}, idb |-> {}]]
  & network  == <<>>
  & remote   == {}
  & tabs     == [t \in Tab |-> [status |-> nonExistant]]
  & workers  == [w \in Worker |-> [status |-> nonExistant]]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

SendMsg(msg) =
  network' == Append(network, msg)

RecvMsg(i) =
  network' == RemoveAt(network, i)

NewDraft(w, prevProv) =
  [
    worker |-> w,
    time   |-> workers[w].time,
    prov   |-> [prevProv EXCEPT ![w] == 0]
  ]

NoProv =
  [w \in Worker |-> 0]

MergeProvs(p1, p2) =
  [w \in Worker |-> Max[p1[w], p2[w]]]

AddProv(draft, prov) =
  [draft EXCEPT !.prov == MergeProvs(@, prov)]

(* NOTE: Doesn't prune *)
AddDraft(storage, draft) =
  LET old = SetFind(storage, LAMBDA d: d.worker == draft.worker)
  IN IF old == FALSE
     THEN storage ++ {draft}
     ELSE (storage -- {old}) ++ {AddProv(draft, old.prov)}

(* NOTE: Doesn't prune *)
AddDrafts(storage, drafts) =
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
PruneByProv(ds) =
  LET f2(d1, d2) =
        LET t = d1.prov[d2.worker]
            p = d1.worker != d2.worker & t > 0 & d2.time <= t
        IN IF p THEN <<d1, d2>> ELSE FALSE
      f(d1) = SetCollectFirst(ds, LAMBDA d2: f2(d1, d2))
      match = SetCollectFirst(ds, f)
  IN IF match == FALSE
     THEN ds
     ELSE LET d1  = match[1]
              d2  = match[2]
              d   = AddProv(d1, d2.prov)
              ds2 = (ds -- {d1, d2}) ++ {d}
          IN PruneByProv(ds2)

\* Returns a set of possible outcomes
PruneByEq(ds) =
  LET equalSets = { x \in SUBSET(ds) : Cardinality(x) > 1 }
      merge(es) = SetReduce(es, LAMBDA x,y: AddProv(x, y.prov))
  IN  { (ds -- es) ++ merge(es) : es \in equalSets }

\* Returns a set of possible outcomes
Prune(drafts) =
  PruneByEq(drafts) ++ {PruneByProv(drafts)}

\* \* TODO assumes idb always works
\* StoreClientSide(b, draft) =
\*   LET bs  = browsers[b]
\*       bs2 = [bs EXCEPT !.idb == Store(@, draft)]
\*       bs3 = [bs EXCEPT !.ls == Store(@, draft)]
\*   IN { [browsers EXCEPT ![b] == bs2]
\*     \* , [browsers EXCEPT ![b] == bs3]
\*   }

\* OnEdit(w) =
\*   LET
\*     ws        = workers[w]
\*     t         = ws.time
\*     lastEdit2 = IF ws.editor.status == closed THEN ws.lastEdit ELSE t
\*     prevProv  = IF ws.editor.status == closed THEN NoProv ELSE ws.editor.draft.prov
\*     draft2    = NewDraft(w, prevProv)
\*     editor2   = [status |-> dirty, draft |-> draft2]
\*   IN [workers EXCEPT ![w] == [ws EXCEPT
\*         !.time      == t + 1,
\*         !.editor    == editor2,
\*         !.lastEdit  == lastEdit2,
\*         !.syncQueue == Store(@, draft2)
\*       ]]

NewTabState(w, prunedDrafts) =
  LET cleanState    = [worker |-> w, status |-> clean]
      dirtyState(d) = [worker |-> w, status |-> dirty, draft |-> d, dirtySince |-> 0]
      conflictState = [worker |-> w, status |-> conflicted, drafts |-> prunedDrafts]
      soleDraft     = SetSoleElement(prunedDrafts)
  IN
    IF prunedDrafts == {} THEN
      cleanState
    ELSE IF soleDraft != FALSE THEN
      dirtyState(soleDraft)
    ELSE
      conflictState

\* ------------------------------------------------------------------------------------------------------------------------
\* Actions

\* New tab is started.
\* Remote drafts are received by web-socket in InitAppData.
TabNew =
  \E t \in Tab:
    & tabs[t].status == nonExistant
    & UNCHANGED << browsers, remote >>
    & \E w \in Worker:

      & \* Connect to worker
        | \* New worker
          & workers[w].status == nonExistant
          & \E b \in Browser:
            & workers' == [workers EXCEPT ![w] == [
                  status  |-> live,
                  browser |-> b,
                  time    |-> 1
                ]]
        | \* Existing worker
          & workers[w].status == live
          & UNCHANGED workers

      & \* Load drafts
        LET ws        = workers'[w]
            bs        = browsers[ws.browser]
            drafts1   = remote
            drafts2   = AddDrafts(drafts1, bs.ls)
            drafts    = AddDrafts(drafts2, bs.idb) \* TODO Sometimes unavailable
            draftSets = Prune(drafts)
            nextT(ds) = [tabs EXCEPT ![t] == NewTabState(w, ds)]
            msgWW(ds) = [from |-> t, to |-> w, drafts |-> ds]
            nextN(ds) = IF remote == {} THEN network ELSE Append(network, msgWW(ds))
            nexts     = { << nextT(ds), nextN(ds) >> : ds \in draftSets }
        IN
          & tabs'    \in { n[1] : n \in nexts }
          & network' \in { n[2] : n \in nexts }

\* DraftNew =
\*   \E w \in Worker : workers[w].status == live
\*     & LET ws = workers[w]
\*        IN & ws.editor.status == closed
\*           & workers' == OnEdit(w)
\*           & browsers' \in StoreClientSide(ws.browser, NewDraft(w, NoProv))
\*           & UNCHANGED << network, remote >>

\* DraftEdit =
\*   \E w \in Worker : workers[w].status == live
\*     & LET ws = workers[w]
\*        IN & ws.editor.status == dirty
\*           & ws.lastEdit != ws.time - 1 \* Avoid consecutive edits / infinite model
\*           & workers' == OnEdit(w)
\*           & browsers' \in StoreClientSide(ws.browser, NewDraft(w, ws.editor.draft.prov))
\*           & UNCHANGED << network, remote >>

\* WorkerSend =
\*   & \E w \in Worker :
\*     & workers[w].status == live
\*     & workers[w].syncQueue != {}
\*     & workers' == [workers EXCEPT ![w].syncQueue == {}] \* In reality we'll only clear after confirmed received
\*     & SendMsg([worker |-> w, toSvr |-> TRUE, drafts |-> workers[w].syncQueue])
\*     & UNCHANGED << browsers, remote >>

\* RemoteRecv =
\*   \E i \in (1..Len(network)) :
\*     & network[i].toSvr
\*     & RecvMsg(i)
\*     & remote' == StoreAll(remote, network[i].drafts)
\*     & UNCHANGED << browsers, workers >>

\* \* Will websockets periodically push? Will workers request?
\* \* As far as the spec goes it doesn't matter.
\* RemoteSend =
\*   & remote != {}
\*   & \E w \in Worker:
\*     & workers[w].status == live
\*     & ~(\E i \in DOMAIN network : ~network[i].toSvr & network[i].worker == w) \* Don't re-send if msg already on the way
\*     & SendMsg([worker |-> w, toSvr |-> FALSE, drafts |-> remote])
\*     & UNCHANGED << browsers, workers, remote >>

\* WorkerRecv =
\*   \E i \in (1..Len(network)) :
\*     & ~network[i].toSvr
\*     & RecvMsg(i)
\*     & workers' == workers \* TODO #########################################
\*     & UNCHANGED << browsers, remote >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Next =
  | TabNew
\*   | DraftNew
\*   | DraftEdit
\*   | WorkerSend
\*   | RemoteRecv
\*   | RemoteSend
\*   | WorkerRecv

Spec = Init & [][Next]_<<vars>>

========================================================================================================================
