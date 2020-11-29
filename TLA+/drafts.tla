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

+------------------+
|     Browser      |
|                  |
|                  |
|  ------------+   |
|  | indexedDB |   |
|  +-----------+   |
|        ↕         |             +----------------+
|   +----------+   |             |                |
|   |          |   |             |     Remote     |
|   |  Worker  |<--------------->|                |
|   |          |   |             |   (database)   |
|   +----------+   |             |                |
|        ↕         |             +----------------+
| +--------------+ |
| | localStorage | |
| +--------------+ |
+------------------+

Tabs are not modelled because they turn out to just be a refinement.
According to DE-5 local tabs will always be in sync.
Theoretic sub-second divergences are to be ignored
(eg. user making different changes in two WW-connected tabs before WW can sync them).
Therefore Worker also represents its connected Tabs as well.

Important notes
===============

- WW time always starts at 1

- Provenance maps have keys for all workers cos TLA+ is a piece of shit.
  When the value=0 it means the K:V entry doesn't really exist in the map.

*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Browser
CONSTANT Worker

ASSUME & IsFiniteSet(Browser)
       & IsFiniteSet(Worker)

MCSymmetry = Permutations(Browser) ++ Permutations(Worker)

VARIABLE browsers \* browser state
VARIABLE workers  \* worker states
VARIABLE remote   \* remote state
VARIABLE network  \* network between WW and remote

vars = << browsers, workers, network, remote >>

varDesc = [
  browsers |-> browsers,
  workers  |-> workers,
  network  |-> network,
  remote   |-> remote]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Types

Provenance  = [Worker -> Nat]                               \* i.e. Map[WorkerId, Time]
Draft       = [worker: Worker, time: Nat, prov: Provenance] \* no need to include draft content
Storage     = SUBSET Draft                                  \* i.e. Set[Draft]

nonExistant = "nonExistant"
live        = "live"
closed      = "closed"
dirty       = "dirty"

BrowserState = [
  ls  : Storage, \* localStorage
  idb : Storage] \* indexedDB

EditorState =
  [status: {closed}] ++
  [
    status: {dirty},
    draft: Draft
  ]

WorkerState =
  [status: {nonExistant}] ++
  [
    status   : {live},
    browser  : Browser,
    time     : Nat,
    editor   : EditorState,
    lastEdit : Nat, \* Last time that an edit to a dirty editor was made
    syncQueue: SUBSET Draft \* Drafts to send to the server
  ]

Msg = [
  worker: Worker,
  toSvr : BOOLEAN,
  drafts: SUBSET Draft \* i.e. Set[Draft]
]

NetworkState =
  Seq(Msg) \* i.e. List[Msg]

TypeInvariants =
  & browsers \in [Browser -> BrowserState]
  & workers \in [Worker -> WorkerState]
  & network \in NetworkState
  & remote \in Storage

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Data

StorageInvariants(s) =
  Assert1(
    Cardinality(s) == Cardinality({d.worker : d \in s}),
    "Duplicate drafts/worker:", s)

DataInvariants =
  \* & PrintT(varDesc)

  & \A b \in Browser :
    LET bs = browsers[b]
    IN & StorageInvariants(bs.idb)
       & StorageInvariants(bs.ls)

  & \A w \in Worker :
    LET ws = workers[w]
    IN & ws.status == live => ws.time > 0 & ws.lastEdit < ws.time

  & StorageInvariants(remote)

Init =
  & browsers == [b \in Browser |-> [ls |-> {}, idb |-> {}]]
  & workers == [w \in Worker |-> [status |-> nonExistant]]
  & network == <<>>
  & remote == {}

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

SendMsg(msg) =
  network' == Append(network, msg)

RecvMsg(i) =
  network' == RemoveAt(network, i)

NoProv =
 [w \in Worker |-> 0]

NewDraft(w, prevProv) =
  [
    worker |-> w,
    time   |-> workers[w].time,
    prov   |-> [prevProv EXCEPT ![w] == 0]
  ]

Store(storage, draft) =
  LET withoutOld = {d \in storage : d.worker != draft.worker}
  IN  withoutOld ++ {draft}

StoreAll(storage, drafts) =
  SetReduce(drafts, storage, Store)

\* TODO assumes idb always works
StoreClientSide(b, draft) =
  LET bs  = browsers[b]
      bs2 = [bs EXCEPT !.idb == Store(@, draft)]
      bs3 = [bs EXCEPT !.ls == Store(@, draft)]
  IN { [browsers EXCEPT ![b] == bs2]
    \* , [browsers EXCEPT ![b] == bs3]
  }

OnEdit(w) =
  LET
    ws        = workers[w]
    t         = ws.time
    lastEdit2 = IF ws.editor.status == closed THEN ws.lastEdit ELSE t
    prevProv  = IF ws.editor.status == closed THEN NoProv ELSE ws.editor.draft.prov
    draft2    = NewDraft(w, prevProv)
    editor2   = [status |-> dirty, draft |-> draft2]
  IN [workers EXCEPT ![w] == [ws EXCEPT
        !.time      == t + 1,
        !.editor    == editor2,
        !.lastEdit  == lastEdit2,
        !.syncQueue == Store(@, draft2)
      ]]

------------------------------------------------------------------------------------------------------------------------
\* Actions

WorkerNew =
  \E w \in Worker : workers[w].status == nonExistant
    & \E b \in Browser :
     \* TODO load from CSS
      & workers' == [workers EXCEPT ![w] == [
            status    |-> live,
            browser   |-> b,
            time      |-> 1,
            editor    |-> [status |-> closed],
            lastEdit  |-> 0,
            syncQueue |-> {}
          ]]
      & UNCHANGED << browsers, network, remote >>

DraftNew =
  \E w \in Worker : workers[w].status == live
    & LET ws = workers[w]
       IN & ws.editor.status == closed
          & workers' == OnEdit(w)
          & browsers' \in StoreClientSide(ws.browser, NewDraft(w, NoProv))
          & UNCHANGED << network, remote >>

DraftEdit =
  \E w \in Worker : workers[w].status == live
    & LET ws = workers[w]
       IN & ws.editor.status == dirty
          & ws.lastEdit != ws.time - 1 \* Avoid consecutive edits / infinite model
          & workers' == OnEdit(w)
          & browsers' \in StoreClientSide(ws.browser, NewDraft(w, ws.editor.draft.prov))
          & UNCHANGED << network, remote >>

WorkerSend =
  & \E w \in Worker :
    & workers[w].status == live
    & workers[w].syncQueue != {}
    & workers' == [workers EXCEPT ![w].syncQueue == {}] \* In reality we'll only clear after confirmed received
    & SendMsg([worker |-> w, toSvr |-> TRUE, drafts |-> workers[w].syncQueue])
    & UNCHANGED << browsers, remote >>

RemoteRecv =
  \E i \in (1..Len(network)) :
    & network[i].toSvr
    & RecvMsg(i)
    & remote' == StoreAll(remote, network[i].drafts)
    & UNCHANGED << browsers, workers >>

\* Will websockets periodically push? Will workers request?
\* As far as the spec goes it doesn't matter.
RemoteSend =
  & remote != {}
  & \E w \in Worker:
    & workers[w].status == live
    & ~(\E i \in DOMAIN network : ~network[i].toSvr & network[i].worker == w) \* Don't re-send if msg already on the way
    & SendMsg([worker |-> w, toSvr |-> FALSE, drafts |-> remote])
    & UNCHANGED << browsers, workers, remote >>

WorkerRecv =
  \E i \in (1..Len(network)) :
    & ~network[i].toSvr
    & RecvMsg(i)
    & workers' == workers \* TODO #########################################
    & UNCHANGED << browsers, remote >>

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Next =
  | WorkerNew
  | DraftNew
  | DraftEdit
  | WorkerSend
  | RemoteRecv
  | RemoteSend
  | WorkerRecv

Spec = Init & [][Next]_<<vars>>

========================================================================================================================
