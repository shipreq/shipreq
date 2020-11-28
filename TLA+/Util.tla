---------------------------------------------------- MODULE Util ----------------------------------------------------

LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Naturals
LOCAL INSTANCE Sequences
LOCAL INSTANCE TLC

Assert1(ok, msg, data1) ==
  ~ok =>
    /\ PrintT("Error: " \o msg)
    /\ ~PrintT(data1)

RemoveAt(s, i) ==
  SubSeq(s, 1, i-1) \o SubSeq(s, i+1, Len(s))

(* Example usage: Sum(set) == ReduceSet(set, 0, LAMBDA acc, a: acc + a) *)
ReduceSet(set, acc, op(_, _)) ==
  LET f[s \in SUBSET set] ==
    IF s = {} THEN acc
    ELSE LET x == CHOOSE x \in s: TRUE
         IN op(f[s \ {x}], x)
  IN f[set]

========================================================================================================================
