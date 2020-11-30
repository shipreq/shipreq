---------------------------------------------------- MODULE Util ----------------------------------------------------

LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Naturals
LOCAL INSTANCE Sequences
LOCAL INSTANCE TLC

------------------------------------------------------------------------------------------------------------------------
\* TLC

Assert0(ok, msg) =
  ~ok => ~PrintT("Error: " \o msg)

Assert1(ok, msg, data1) =
  ~ok =>
    & PrintT("Error: " \o msg)
    & ~PrintT(data1)

------------------------------------------------------------------------------------------------------------------------
\* Nats

Min[x \in Nat, y \in Nat] = IF x < y THEN x ELSE y
Max[x \in Nat, y \in Nat] = IF x > y THEN x ELSE y

------------------------------------------------------------------------------------------------------------------------
\* Sets

kSubset(k, S) =
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
  { s \in SUBSET S : Cardinality(s) == k }

SetMin[as \in SUBSET Nat] = CHOOSE a \in as : \A b \in as : a <= b
SetMax[as \in SUBSET Nat] = CHOOSE a \in as : \A b \in as : a >= b

(* set.find(pred).getOrElse(otherwise) *)
SetFindOrElse(set, pred(_), otherwise) =
  IF   \E x \in set : pred(x)
  THEN CHOOSE x \in set : pred(x)
  ELSE otherwise

SetFind(set, pred(_)) =
  SetFindOrElse(set, pred, FALSE)

(* set.collectFirst(f(_).filter(_ != otherwise)).getOrElse(otherwise) *)
SetCollectFirstOrElse(set, f(_), otherwise) =
  LET el = SetFindOrElse(set, f, otherwise)
  IN IF el == otherwise
     THEN otherwise
     ELSE f(el)

SetCollectFirst(set, f(_)) =
  SetCollectFirstOrElse(set, f, FALSE)

SetReplace(set, old, new) =
  { IF a == old THEN new ELSE a : a \in set }

(* Example usage: Sum(set) = SetFold(set, 0, LAMBDA acc, a: acc + a) *)
SetFold(set, acc, op(_, _)) =
  LET f[s \in SUBSET set] =
    IF s == {} THEN acc
    ELSE LET x = CHOOSE x \in s: TRUE
         IN op(f[s -- {x}], x)
  IN f[set]

(* Example usage: Sum(set) = SetReduce(set, LAMBDA a, b: a + b) *)
SetReduce(set, op(_, _)) =
  LET f[s \in SUBSET set] =
    LET x = CHOOSE x \in s: TRUE
    IN op(f[s -- {x}], x)
  IN f[set]

SetSoleElementOrElse(set, otherwise) =
  IF Cardinality(set) == 1
  THEN CHOOSE x \in set: TRUE
  ELSE otherwise

SetSoleElement(set) =
  SetSoleElementOrElse(set, FALSE)

------------------------------------------------------------------------------------------------------------------------
\* Sequences

RemoveAt(s, i) =
  SubSeq(s, 1, i-1) \o SubSeq(s, i+1, Len(s))

========================================================================================================================
