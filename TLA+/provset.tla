--------------------------------------------------- MODULE provset ----------------------------------------------------
(*

Primary goals
=============
With monotonic clocks, can we ever end up with a state where we've got a greater item in provenance?
i.e. ∃ e ∈ Entry, p ∈ e.prov : p > e.key ?

Secondary goals
===============
Ensure our algorithm does what we expect.
Example: after merging two ProvSets, there are no comparable sibling entries.

*)

EXTENDS FiniteSets, Naturals, Sequences, TLC, Util

CONSTANT Node
ASSUME IsFiniteSet(Node)

CONSTANT MCMaxRev
ASSUME MCMaxRev \in Nat

MCSymmetry == SymmetrySet(Node)

VARIABLE clocks
VARIABLE nodes

vars == <<
  clocks,
  nodes
>>

state == [
  clocks |-> clocks,
  nodes  |-> nodes
]

LogStates ==
  Log(state)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Types

Key        == [node: Node, rev: Nat]
Provenance == SUBSET Key
Entry      == [key: Key, prov: Provenance]
ProvSet    == SUBSET Entry

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Functions

KeyComparable(x, y) == x.node = y.node
KeyLT        (x, y) == KeyComparable(x, y) & x.rev <  y.rev
KeyLE        (x, y) == KeyComparable(x, y) & x.rev <= y.rev
KeyGT        (x, y) == KeyComparable(x, y) & x.rev >  y.rev
KeyGE        (x, y) == KeyComparable(x, y) & x.rev >= y.rev

NewKey  (n) == [node |-> n, rev |-> clocks[n]]
NewEntry(n) == [key |-> NewKey(n), prov |-> {}]

EntryToProv(e) ==
  IF \E p \in e.prov : KeyGE(p, e.key) THEN
    e.prov
  ELSE
    e.prov ++ {e.key}

RemoveNodeFromProv(prov, n) ==
  { p \in prov : p.node != n }

AddProv(p, k) ==
  LET f == SetFind(p, LAMBDA x: KeyComparable(x, k)) IN
    IF f.isEmpty THEN
      p ++ {k}
    ELSE
      LET j == f.get IN
        IF KeyGE(j, k) THEN
          p
        ELSE
          (p -- {j}) ++ {k}


MergeProvs(p1, p2) ==
  SetFold(p2, p1, AddProv)

MergeEntries(e, into) ==
  LET newProv1 == AddProv(e.prov, e.key)
      fn(k)    == IF KeyGT(k, into.key) THEN
                    Fail1("Found P > K!", [e |-> e, into |-> into, p |-> k, k |-> into.key])
                  ELSE
                    KeyComparable(k, into.key)
      newProv  == { k \in newProv1 : ~fn(k) }
  IN [
    key  |-> into.key,
    prov |-> MergeProvs(into.prov, newProv)
  ]

RECURSIVE AddEntryToProvSet_go(_, _)
AddEntryToProvSet_go(base, e) ==
  LET o == SetFind(base, LAMBDA into: KeyComparable(e.key, into.key) | (\E p \in into.prov : KeyLE(e.key, p))) IN
    IF o.isEmpty THEN
      base ++ {e}
    ELSE
      LET i      == o.get
          merged == IF KeyGT(e.key, i.key) THEN MergeEntries(i, e) ELSE MergeEntries(e, i)
      IN AddEntryToProvSet_go(base -- {i}, merged)

AddEntryToProvSet(s, e) ==
  IF s = {} THEN
    {e}
  ELSE IF e \in s THEN
    s
  ELSE
    AddEntryToProvSet_go(s, e)

AddProvSetToProvSet(x, y) ==
  SetFold(x, y, AddEntryToProvSet)

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Invariants

InvariantsForProv(ps) ==
  \A p \in ps :
    \A q \in (ps -- {p}) :
      Assert1(
        ~KeyComparable(p, q),
        "Comparable keys in same provenance.",
        [ps |-> ps, p |-> p, q |-> q])

InvariantsForEntry(e) ==
  & \A p \in e.prov :
      Assert1(
        ~KeyLE(p, e.key),
        SeqJoin(<< "Entry", e.key, "has in its provenance, a key", p, "that is <=self." >>, " "),
        e)
  & InvariantsForProv(e.prov)

AssertEntryCoexistence(e1, e2) ==
  & Assert1(~KeyComparable(e1.key, e2.key), "Comparable sibling entries.", [key1 |-> e1.key, key2 |-> e2.key])
  & \A p \in e1.prov:
      Assert1(
        ~KeyLT(e1.key, p),
        SeqJoin(<< "Entry", e1.key, "is <= ", p, "in e2's provenance." >>, " "),
        [e1 |-> e1, e2 |-> e2, p |-> p])

InvariantsForProvSet(s) ==
  \A e \in s :
    & InvariantsForEntry(e)
    & \A f \in (s -- {e}) : AssertEntryCoexistence(e, f)

InvariantsForClocks ==
  clocks \in [Node -> Nat]

InvariantsForNodes ==
  nodes \in [Node -> ProvSet]

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Actions

IncClock(n) ==
  clocks' = [clocks EXCEPT ![n] = @ + 1]

Start ==
  \E n \in Node :
    & clocks[n] <= MCMaxRev
    & nodes[n] = {}
    & nodes' = [nodes EXCEPT ![n] = {NewEntry(n)}]
    & IncClock(n)

Edit ==
  \E n \in Node :
    & clocks[n] <= MCMaxRev
    & Cardinality(nodes[n]) = 1
    & LET e == CHOOSE e \in nodes[n] : TRUE
          p == RemoveNodeFromProv(EntryToProv(e), n)
      IN
        & nodes' = [nodes EXCEPT ![n] = {[key |-> NewKey(n), prov |-> p]}]
        & IncClock(n)

Merge ==
  \E n \in Node :
    & clocks[n] <= MCMaxRev
    & Cardinality(nodes[n]) > 1
    & LET msets  == { x \in SUBSET nodes[n] : Cardinality(x) >= 2 }
          ps(m)  == SetReduce({ EntryToProv(e) : e \in m }, MergeProvs)
          p(m)   == RemoveNodeFromProv(ps(m), n)
          nss(m) == (nodes[n] -- m) ++ {[key |-> NewKey(n), prov |-> p(m)]}
          res    == { [nodes EXCEPT ![n] = nss(m)] : m \in msets}
      IN
        & nodes' \in res
        & IncClock(n)

Gossip ==
  \E n \in Node :
    \E m \in Node :
      & n != m
      & nodes[n] != nodes[m]
      & nodes' = [nodes EXCEPT ![n] = AddProvSetToProvSet(@, nodes[m])]
      & UNCHANGED clocks

\* ███████████████████████████████████████████████████████████████████████████████████████████████████
\* Spec

Init ==
  & clocks = [n \in Node |-> 1]
  & nodes  = [n \in Node |-> {}]

Next ==
  | Start
  | Edit
  | Merge
  | Gossip

Spec ==
  & Init
  & [][Next]_<<vars>>

View ==
  NormaliseNats(state, 0, MCMaxRev + 2, "")

========================================================================================================================
