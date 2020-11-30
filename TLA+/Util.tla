---------------------------------------------------- MODULE Util ----------------------------------------------------

LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Naturals
LOCAL INSTANCE Sequences
LOCAL INSTANCE TLC

------------------------------------------------------------------------------------------------------------------------
\* TLC

Assert1(ok, msg, data1) =
  ~ok =>
    & PrintT("Error: " \o msg)
    & ~PrintT(data1)

------------------------------------------------------------------------------------------------------------------------
\* Sets

SetMin[as \in SUBSET Nat] = CHOOSE a \in as : \A b \in as : a <= b
SetMax[as \in SUBSET Nat] = CHOOSE a \in as : \A b \in as : a >= b

SetReplace(set, old, new) = { IF a == old THEN new ELSE a : a \in set }

(* Example usage: Sum(set) = SetReduce(set, 0, LAMBDA acc, a: acc + a) *)
SetReduce(set, acc, op(_, _)) =
  LET f[s \in SUBSET set] =
    IF s == {} THEN acc
    ELSE LET x = CHOOSE x \in s: TRUE
         IN op(f[s -- {x}], x)
  IN f[set]

------------------------------------------------------------------------------------------------------------------------
\* Sequences

RemoveAt(s, i) =
  SubSeq(s, 1, i-1) \o SubSeq(s, i+1, Len(s))

========================================================================================================================
