-------------------------------------------------- MODULE tombstones --------------------------------------------------
(*

Drafts are greatly generalised here. There are only two properties we need drafts to exhibit:

1) Total ordering. Real drafts have partial ordering but we can just split the state space and only deal with
   totally-orderable drafts here because, drafts that can't be merged can be considered two separate and independent
   streams. If this process works out for one stream it should work for both. If not, that property can be spec'd and
   tested separately using only the conclusion of this spec, rather than the exponential composition of all this
   machinery. To be more specific, if this works out then I can seperately spec the case where we have Set[Draft] and
   drafts can go from live -> tombstone -> hard-deleted without worrying about the process that creates those state
   changes (eg. no network modelling required, just State₁ x State₂ = State').

2) Tombstoneability. [Is a draft a tombstone or not?] + [make this draft a tombstone].

Therefore all we need to model drafts here is: (Rev, Live).

Conclusions:

  1) Drafts are eventually consistent in this spec.
  2) Tombstones are required at all layers (i.e. remote, tab, workers)

*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Node
CONSTANT NodeGraph \* Set[Set[Node]] where if nodes X and Y are in a set together, they can talk to each other directly
CONSTANT Remote
CONSTANT UserNode \* Nodes which users access directly

CONSTANT MCMaxRev
CONSTANT MCAllowRemoval
CONSTANT MCCheckRemoval
CONSTANT MCNetworkLimit

ASSUME IsFiniteSet(Node)
ASSUME IsFiniteSet(NodeGraph)
ASSUME IsFiniteSet(UserNode)
ASSUME NodeGraph \in SUBSET SUBSET SUBSET Node
ASSUME Remote \in Node
ASSUME Remote \notin UserNode
ASSUME MCMaxRev \in Nat
ASSUME MCAllowRemoval \in BOOLEAN
ASSUME MCCheckRemoval \in BOOLEAN
ASSUME MCNetworkLimit \in Nat

NodeGraph2 ==
  UNION {
    SetReduce(el, LAMBDA a,b:{ SeqToSet(s) : s \in (a \X b) })
    : el \in NodeGraph }

\* ASSUME Log("NodeGraph result: " \o ToString(NodeGraph2))
ASSUME NodeGraph2 \in SUBSET {s \in SUBSET Node : Cardinality(s) > 1}

MCSymmetry == SymmetrySet(Node)

VARIABLE network
VARIABLE nodes
VARIABLE target \* Expected result state

Network == INSTANCE Network WITH queue <- network

vars == <<
  network,
  nodes,
  target
>>

state == [
  network |-> network,
  nodes   |-> nodes,
  target  |-> target
]

LogStates ==
  Log(state)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Types and invariants

Draft == [rev: Nat, live: BOOLEAN]

req  == "req"
send == "send"
resp == "resp"

Msg == [
  type : {req},
  from : Node,
  to   : Node
] ++ [
  type : {send},
  from : Node,
  to   : Node,
  draft: Draft
] ++ [
  type : {resp},
  from : Node,
  to   : Node,
  draft: Option(Draft)
]

InvariantsForNetwork ==
  & Network!Invariants(Msg)
  & (MCNetworkLimit != 0) =>
      Assert0(
        Len(network) <= MCNetworkLimit,
        "Max network size (" \o ToString(MCNetworkLimit) \o ") exceeded. " \o ToString(SeqCountDups(network)) \o " dups found.")
  & SeqForall(network, LAMBDA m: Assert1(nodes[m.to].online, "Network target offline.", [msg |-> m, node |-> nodes[m.to]]))

NodeState == [
  online   : BOOLEAN,
  draft    : Option(Draft),
  broadcast: SUBSET Node, \* Nodes we want to broadcast our state to
  awaiting : SUBSET Node  \* Nodes from which we're awaiting a response
]

InvariantsForNodes ==
  & nodes \in [Node -> NodeState]
  & \A n \in Node: LET ns == nodes[n] IN
    & n \notin ns.broadcast
    & n \notin ns.awaiting
    & (ns.broadcast != {}) =>
        Assert1(~ns.draft.isEmpty, "Node " \o ToString(n) \o " wants to broadcast but has no draft.", ns)
    & ~ns.online =>
        ns.awaiting = {}

InvariantsForTarget ==
  target \in Option(Draft)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

Merge(x, y) ==
  IF x.rev > y.rev THEN x ELSE y

MergeOption(ox, y) ==
  IF ox.isEmpty THEN y ELSE Merge(ox.get, y)

LiveRev(optionalDraft) ==
  IF ~optionalDraft.isEmpty & optionalDraft.get.live THEN
    optionalDraft.get.rev
  ELSE
    0

NextRev ==
  IF target.isEmpty THEN 1 ELSE target.get.rev + 1

WorkerNodes ==
  (Node -- UserNode) -- {Remote}

OnlineNodes ==
  { n \in Node : nodes[n].online }

OfflineNodes ==
  { n \in Node : ~nodes[n].online }

Reachable(node1, node2) ==
  & node1 != node2
  & \E g \in NodeGraph2 :
    & node1 \in g
    & node2 \in g

BroadcastTargets(from) ==
  { n \in Node : nodes[n].online & Reachable(n, from) }

\* Inputs:
\*   msg     :: Msg
\*   before  :: Option[Draft]
\*   received :: Draft
\*   updated :: Draft
\* Output:
\*   Set[Node]
BroadcastTargetsOnRecv(msg, before, received, updated) ==
  LET from == msg.from
      n    == msg.to
  IN
    IF received != updated THEN
      \* Merging the draft resulted in a new value; share with everyone
      \* {from} being removed here because it gets a direct response
      BroadcastTargets(n) -- {from}

    ELSE IF before = Some(updated) THEN
      \* Msg provided nothing new
      {}

    ELSE
      \* Msg is new info; share with everyone else
      BroadcastTargets(n) -- {from}

AllowRemoval(n, d) ==
  & MCAllowRemoval
  & ~d.live
  & n != Remote \* Keep tombstones on server, delete by age
  & n \notin UserNode \* Make tabs keep tombstones in memory

\* (Node, Draft) => Option[Draft]
ApplyHardDel(n, d) ==
  SomeWhen(~AllowRemoval(n, d), d)

ApplyHardDelOption(n, od) ==
  OptionFlatmap(od, LAMBDA d: ApplyHardDel(n, d))

FilterLive(od) ==
  IF ~od.isEmpty & od.get.live THEN od ELSE None

StableNodeState(ns) ==
  ns.online =>
    & ns.broadcast = {}
    & ns.awaiting = {}

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Actions

UserChange(n, live) ==
  LET d    == [rev |-> NextRev, live |-> live]
      od   == Some(d) \* ApplyHardDel(n, d)
      tgts == BroadcastTargets(n)
  IN
    & nodes' = [nodes EXCEPT ![n].draft = od, ![n].broadcast = tgts]
    & target' = Some(d)
    & UNCHANGED network

Edit ==
  & NextRev <= MCMaxRev
  & \E n \in UserNode:
    & UserChange(n, TRUE)

Kill ==
  & NextRev <= MCMaxRev
  & \E n \in UserNode:
    & ~nodes[n].draft.isEmpty
    & nodes[n].draft.get.live
    & UserChange(n, FALSE)

Send ==
  \E n \in Node:
    LET ns      == nodes[n]
        tgts    == ns.broadcast
        d       == ns.draft.get
        msg(to) == [type |-> send, from |-> n, to |-> to, draft |-> d]
        await   == IF n = Remote THEN {} ELSE tgts
    IN
      & ns.online
      & ns.awaiting = {}
      & tgts != {}
      & nodes' = [nodes EXCEPT ![n].awaiting = await, ![n].broadcast = {}]
      & Network!SendSet({ msg(t) : t \in tgts })
      & UNCHANGED target

RecvSend ==
  \E i \in Network!NextMsgsByType(send):
    LET msg     == network[i]
        n       == msg.to
        ns      == nodes[n]
        before  == ns.draft
        d       == MergeOption(before, msg.draft)
        od      == Some(d) \* ApplyHardDel(n, d)
        tgts    == BroadcastTargetsOnRecv(msg, before, msg.draft, d)
        altRes  == d != msg.draft
        respMsg == [type |-> resp, from |-> n, to |-> msg.from, draft |-> SomeWhen(altRes, d)]
    IN
      & IF msg.from = Remote THEN
          LET tgts2   == ns.broadcast ++ (IF altRes THEN tgts ++ {Remote} ELSE tgts)
              hardDel == AllowRemoval(n, msg.draft) & before.isEmpty & tgts2 = {}
          IN
            & Network!Recv(i)
            & nodes' = [nodes EXCEPT ![n].draft = IF hardDel THEN None ELSE od,
                                     ![n].broadcast = tgts2]
        ELSE
          & Network!RecvSend(i, respMsg)
          & nodes' = [nodes EXCEPT ![n].draft = od,
                                   ![n].broadcast = @ ++ tgts]
      & UNCHANGED target

RecvRequest ==
  \E i \in Network!NextMsgsByType(req):
    LET msg == network[i]
        n   == msg.to
        ns  == nodes[n]
        res == [type |-> resp, from |-> n, to |-> msg.from, draft |-> ns.draft]
    IN
      & Network!RecvSend(i, res)
      & UNCHANGED nodes
      & UNCHANGED target

RecvResponse ==
  \E i \in Network!NextMsgsByType(resp):
    LET msg  == network[i]
        n    == msg.to
        ns   == nodes[n]
        ns2  == [ns EXCEPT !.awaiting = @ -- {msg.from}]
        ns3  == IF ~msg.draft.isEmpty THEN
                  LET before  == ns.draft
                      recv    == msg.draft.get
                      upd     == MergeOption(before, recv)
                      tgts    == BroadcastTargetsOnRecv(msg, before, recv, upd)
                      ns3     == [ns2 EXCEPT !.broadcast = @ ++ tgts]
                      od      == IF StableNodeState(ns3) THEN ApplyHardDel(n, upd) ELSE Some(upd)
                  IN [ns3 EXCEPT !.draft = od]
                ELSE IF ~ns2.draft.isEmpty & StableNodeState(ns2) THEN
                  LET d == ns2.draft.get
                  IN IF AllowRemoval(n, d) THEN
                       [ns2 EXCEPT !.draft = FilterLive(@)]
                     ELSE
                      ns2
                ELSE
                  ns2
    IN
      & nodes' = [nodes EXCEPT ![n] = ns3]
      & Network!Recv(i)
      & UNCHANGED target

Disconnect ==
  \E n \in UserNode:
    LET ns       == nodes[n]
        ns2      == [ns EXCEPT !.online    = FALSE,
                               !.broadcast = IF ns.draft.isEmpty THEN @ ELSE @ ++ ns.awaiting,
                               !.awaiting  = {}
                    ]
        other(s) == [s EXCEPT !.broadcast = @ -- {n},
                              !.awaiting  = @ -- {n}
                    ]
        upd(m)   == IF m = n THEN ns2 ELSE other(nodes[m])
    IN
      & ns.online
      & nodes' = [m \in Node |-> upd(m)]
      & Network!RemoveAllFromOrTo(n)
      & UNCHANGED target

Reconnect ==
  \E n \in UserNode:
    LET ns   == nodes[n]
        tgts == {m \in Node : nodes[m].online & Reachable(n, m)}
        ns2  == [ns EXCEPT !.online = TRUE, !.awaiting = tgts]
    IN
      & ~ns.online
      & nodes' = [nodes EXCEPT ![n] = ns2]
      & Network!SendSet({[type |-> req, from |-> n, to |-> to] : to \in tgts})
      & UNCHANGED target

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & Network!Init
  & nodes = [n \in Node |-> [
                online    |-> TRUE,
                draft     |-> None,
                broadcast |-> {},
                awaiting  |-> {}
            ]]
  & target = None

Next ==
  | Edit
  | Kill
  | Send
  | RecvSend
  | RecvRequest
  | RecvResponse
  | Disconnect
  | Reconnect

Fairness ==
  & WF_<<vars>>(Send)
  & WF_<<vars>>(RecvSend)
  & WF_<<vars>>(RecvRequest)
  & WF_<<vars>>(RecvResponse)

Spec ==
  & Init
  & [][Next]_<<vars>>
  & Fairness

View ==
  NormaliseNats(state, 0, MCMaxRev + 1, "")

NodesAreStable ==
  \A n \in Node : StableNodeState(nodes[n])

IsStable ==
  & ~ENABLED(Send)
  & ~ENABLED(RecvSend)
  & ~ENABLED(RecvRequest)
  & ~ENABLED(RecvResponse)

StableInvariants ==
  IsStable =>

    LET assertLiveRevs(nodeSet, expect) ==
          \A n \in nodeSet:
            Assert1(
              LiveRev(nodes[n].draft) = expect,
              "Node " \o ToString(n) \o " doesn't have expected draft value.",
              [node |-> n, expect |-> expect, actual |-> LiveRev(nodes[n].draft)])

        assertTombstoneRemoval(nodeSet, expect) ==
          MCCheckRemoval & expect = 0 =>
            \A n \in nodeSet:
              Assert1(
                n = Remote | nodes[n].draft.isEmpty,
                "Node " \o ToString(n) \o " tombstone not removed.",
                [node |-> n, state |-> nodes[n]])

        assertConsistency(nodeSet, expect) ==
          & assertLiveRevs(nodeSet, expect)
          & assertTombstoneRemoval(nodeSet, expect)

        allNodesAreOnline ==
          \A n \in Node : nodes[n].online
    IN

      \* & LogStates

      & Assert0(
          Network!IsEmpty,
          "Network still has activity on it.")

      & \A n \in Node :
          Assert1(
            StableNodeState(nodes[n]),
            "Node " \o ToString(n) \o " is not stable: " \o ToString(n), nodes[n])

      & IF allNodesAreOnline THEN
          assertConsistency(Node, LiveRev(target))
        ELSE
          LET onlineTabs == {n \in UserNode : nodes[n].online}
              workers    == {w \in WorkerNodes : \E t \in onlineTabs : Reachable(w,t) }
              clientSide == onlineTabs ++ workers
              nodeSet    == IF nodes[Remote].online THEN clientSide ++ {Remote} ELSE clientSide
              targetRev  == LiveRev(target)
              hasTarget  == \E n \in nodeSet : LiveRev(nodes[n].draft) = targetRev
              expect     == IF hasTarget THEN targetRev ELSE LiveRev(nodes[CHOOSE n \in nodeSet : TRUE].draft)
          IN nodeSet != {} =>
               assertConsistency(nodeSet, expect)

Liveness ==
  []<>IsStable \* We always, eventually stablise

MCContinue ==
  (TLCGet("diameter") <= 60) | LogRet(state, FALSE)

========================================================================================================================
