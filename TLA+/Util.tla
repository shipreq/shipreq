---------------------------------------------------- MODULE Util ----------------------------------------------------

LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Naturals
LOCAL INSTANCE Sequences
LOCAL INSTANCE TLC

------------------------------------------------------------------------------------------------------------------------
\* TLC

Assert0(ok, msg) ==
  ~ok => ~PrintT("Error: " \o msg)

Assert1(ok, msg, data1) ==
  ~ok =>
    & PrintT("Error: " \o msg)
    & ~PrintT(data1)

Log(msg) ==
  LET sep == "----------------------------------------------------------------------------------------------------"
  IN
    & PrintT(sep)
    & PrintT(msg)
    & PrintT(sep)

TapF(f(_), a) ==
  IF Log(f(a))
  THEN a
  ELSE "impossible"

Tap(a) ==
  TapF(LAMBDA x: x, a)

Show(name, a) ==
  TapF(LAMBDA x: [n \in {name} |-> x], a)

SymmetrySet(S) ==
  IF Cardinality(S) > 1 THEN
    Permutations(S)
  ELSE
    {}

SymmetrySets(Ss) ==
  UNION { SymmetrySet(Ss[i]) : i \in DOMAIN Ss }

------------------------------------------------------------------------------------------------------------------------
\* Nats

Min[x \in Nat, y \in Nat] == IF x < y THEN x ELSE y
Max[x \in Nat, y \in Nat] == IF x > y THEN x ELSE y

------------------------------------------------------------------------------------------------------------------------
\* Option

None ==
  [
    isEmpty   |-> TRUE,
    isDefined |-> FALSE,
    toSet     |-> {}
  ]

Some(s) ==
  [
    get       |-> s,
    isEmpty   |-> FALSE,
    isDefined |-> TRUE,
    toSet     |-> {s}
  ]

Option(S) ==
  {None} ++ [
    get      : S,
    isEmpty  : {FALSE},
    isDefined: {TRUE},
    toSet    : SUBSET S
  ]

OptionMap(o, f(_)) ==
  IF o.isEmpty THEN o ELSE Some(f(o.get))

OptionFlatmap(o, f(_)) ==
  IF o.isEmpty THEN o ELSE f(o.get)

SomeWhen(cond, s) ==
  IF cond THEN Some(s) ELSE None

NoneAndSome(a) ==
  {None, Some(a)}

------------------------------------------------------------------------------------------------------------------------
\* Sets

kSubset(k, S) ==
  (*************************************************************************)
  (* A k-subset ks of a set S has Cardinality(ks) = k.  The number of      *)
  (* k-subsets of a set S with Cardinality(S) = n is given by the binomial *)
  (* coefficients n over k.  A set S with Cardinality(S) = n has 2^n       *)
  (* k-subsets.  \A k \notin 0..Cardinality(S): kSubset(k, S) = {}.        *)
  (*                                                                       *)
  (* This operator is overridden by FiniteSetsExt#kSubset whose            *)
  (* implementation, contrary to  { s \in SUBSET S : Cardinality(s) = k }, *)
  (* only enumerates the k-subsets of S and not all subsets.               *)
  (*                                                                       *)
  (* Example:                                                              *)
  (*          kSubset(2, 1..3) = {{1,2},{2,3},{3,1}}                       *)
  (*************************************************************************)
  { s \in SUBSET S : Cardinality(s) = k }

SetMin[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a <= b
SetMax[as \in SUBSET Nat] == CHOOSE a \in as : \A b \in as : a >= b

SetFind(set, pred(_)) ==
  IF   \E x \in set : pred(x)
  THEN Some(CHOOSE x \in set : pred(x))
  ELSE None

(* (Set[A], A => Option[B]): Option[B] *)
SetCollectFirst(set, f(_)) ==
  LET p(a) == f(a).isDefined
      oa   == SetFind(set, p)
  IN OptionFlatmap(oa, f)

SetReplace(set, old, new) ==
  { IF a = old THEN new ELSE a : a \in set }

(* Example usage: Sum(set) == SetFold(set, 0, LAMBDA acc, a: acc + a) *)
SetFold(set, acc, op(_, _)) ==
  LET f[s \in SUBSET set] ==
    IF s = {} THEN acc
    ELSE LET x == CHOOSE x \in s: TRUE
         IN op(f[s -- {x}], x)
  IN f[set]

(* Example usage: Sum(set) == SetReduce(set, LAMBDA a, b: a + b) *)
SetReduce(set, op(_, _)) ==
  LET x == CHOOSE x \in set: TRUE
  IN SetFold(set -- {x}, x, op)

SetReduceOr(set, ifEmpty, op(_, _)) ==
  IF set = {} THEN
    ifEmpty
  ELSE
    SetReduce(set, op)

SetSoleElement(set) ==
  IF Cardinality(set) = 1
  THEN Some(CHOOSE x \in set: TRUE)
  ELSE None

------------------------------------------------------------------------------------------------------------------------
\* Sequences

SeqIndexOf(seq, pred(_)) ==
  LET f[i \in Nat] ==
    IF i \notin DOMAIN seq THEN
      0
    ELSE IF pred(seq[i]) THEN
      i
    ELSE
      f[i + 1]
  IN f[1]

SeqFind(seq, pred(_)) ==
  LET i == SeqIndexOf(seq, pred)
  IN IF i = 0
     THEN None
     ELSE Some(seq[i])

SeqExists(seq, f(_)) ==
  SeqIndexOf(seq, f) != 0

SeqForall(seq, f(_)) ==
  ~SeqExists(seq, LAMBDA a: ~f(a))

RemoveAt(s, i) ==
  SubSeq(s, 1, i-1) \o SubSeq(s, i+1, Len(s))

========================================================================================================================
