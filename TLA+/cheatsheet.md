# TLA+ cheatsheet

```
+----------+------------------+----------+
|          | value            | type     |
+----------+------------------+----------+
| record   | [k       |-> v]  | [k :  V] |
| Map K V  | [k \in K |-> v]  | [K -> V] | <-- no pipe before the arrow
| Set A    | {a1,a2,...,an}   | SUBSET A |
| Seq A    | <<a1,a2,...,an>> | Seq(A)   |
| (A,B)    | <<a,b>>          | A \X B   |
+----------+------------------+----------+

+--------+---------------------+
| map    | {   f(e) : e \in S} | Remember `fmap` like in Haskell, f comes first. [f]map
| filter | {e \in S : f(e)   } | TLA+ makes me want to forfeit => ff => f…f => [f]ilter S [f]
+--------+---------------------+

CHOOSE x \in S : P(x)

CASE x = 1 -> a
  [] x = 2 -> b
  [] OTHER -> c
```

# Templates

```tla
---------------------------------------------------- MODULE project ----------------------------------------------------

EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANTS xxx

MCSymmetry == Permutations(xxx) \union Permutations(yyy)

(* check constants here *)
ASSUME /\ IsFiniteSet(xxx)
       /\ IsFiniteSet(yyy)

VARIABLES xxx

vars == << xxx >>

varDesc == [xxx |-> xxx]

TypeInvariants ==
  TRUE

------------------------------------------------------------------------------------------------------------------------

Init ==
  TRUE

DataInvariants ==
  /\ PrintT(varDesc)

------------------------------------------------------------------------------------------------------------------------

(* actions *)

------------------------------------------------------------------------------------------------------------------------

Next ==
  FALSE

Spec == Init /\ [][Next]_<<vars>>

Live ==
  TRUE

========================================================================================================================
```

```cfg
SPECIFICATION Spec

INVARIANTS TypeInvariants
           DataInvariants

CONSTRAINT MCContinue

CONSTANTS User = {u1,u2}

SYMMETRY MCSymmetry
```