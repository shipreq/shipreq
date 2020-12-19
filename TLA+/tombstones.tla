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

Msg == [
  from : Node,
  to   : Node,
  draft: Draft
]

InvariantsForNetwork ==
  & Network!Invariants(Msg)
  & (MCNetworkLimit != 0) =>
      Assert0(
        Len(network) <= MCNetworkLimit,
        "Max network size (" \o ToString(MCNetworkLimit) \o ") exceeded. " \o ToString(SeqCountDups(network)) \o " dups found.")

InvariantsForNodes ==
  nodes \in [Node -> Option(Draft)]

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

Reachable(node1, node2) ==
  \E g \in NodeGraph2 :
    & node1 \in g
    & node2 \in g

Broadcast(from, draft) ==
  LET tgts    == { n \in Node : n != from & Reachable(n, from) }
      msg(to) == [from |-> from, to |-> to, draft |-> draft]
  IN
    IF tgts = {} THEN
      Fail1("No targets exist to broadcast to.", [from |-> from])
    ELSE
      {msg(to) : to \in tgts}

BroadcastExcept(from, draft, except) ==
  LET tgts    == { n \in Node : n != from & n != except & Reachable(n, from) }
      msg(to) == [from |-> from, to |-> to, draft |-> draft]
  IN {msg(to) : to \in tgts}

SetNodeDraft(n, d) ==
  LET hardDel  == MCAllowRemoval & ~d.live & n != Remote
      newState == SomeWhen(~hardDel, d)
  IN nodes' = [nodes EXCEPT ![n] = newState]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Actions

UserChange(n, live) ==
  LET d == [rev |-> NextRev, live |-> live]
  IN
    & Network!NothingInFlightFromTo(n, Remote)
    & SetNodeDraft(n, d)
    & Network!SendSet(Broadcast(n, d))
    & target' = Some(d)

Edit ==
  & NextRev <= MCMaxRev
  & \E n \in UserNode:
    & UserChange(n, TRUE)

Kill ==
  & NextRev <= MCMaxRev
  & \E n \in UserNode:
    & ~nodes[n].isEmpty
    & nodes[n].get.live
    & UserChange(n, FALSE)

Recv ==
  \E i \in Network!NextMsgs:
    LET msg     == network[i]
        n       == msg.to
        before  == nodes[n]
        d       == MergeOption(before, msg.draft)
    IN
      & SetNodeDraft(n, d)
      &
        IF d != msg.draft THEN
          \* Merging the draft resulted in a new value; share with everyone
          Network!RecvSendSet(i, Broadcast(n, d))

        ELSE IF before = Some(d) THEN
          \* Msg provided nothing new
          Network!Recv(i)

        ELSE
          \* Msg is new info; share with everyone else
          Network!RecvSendSet(i, BroadcastExcept(n, d, msg.from))

      & UNCHANGED target

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & Network!Init
  & nodes = [n \in Node |-> None]
  & target = None

Next ==
  | Edit
  | Kill
  | Recv

Fairness ==
  & WF_<<vars>>(Recv)

Spec ==
  & Init
  & [][Next]_<<vars>>
  & Fairness

View ==
  NormaliseNats(state, 0, MCMaxRev + 1, "")

IsStable ==
  Network!IsEmpty

StableInvariants ==
  IsStable =>
    \* & LogStates

    & LET targetRev == LiveRev(target) IN
        \A n \in Node:
          Assert1(
            LiveRev(nodes[n]) = targetRev,
            "Node doesn't have expected draft value.",
            [node |-> n, expect |-> targetRev, actual |-> LiveRev(nodes[n])])

    &
      & MCCheckRemoval
      & LiveRev(target) = 0 =>
          \A n \in Node:
            Assert1(
              n = Remote | nodes[n].isEmpty,
              "Node tombstone not removed.",
              [node |-> n, state |-> nodes[n]])

Liveness ==
  []<>IsStable \* We always, eventually stablise

MCContinue ==
  (TLCGet("diameter") <= 60) | LogRet(state, FALSE)

========================================================================================================================
