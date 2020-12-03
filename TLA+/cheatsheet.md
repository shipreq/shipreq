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

□  = continuously
◇  = eventually
□◇ = infinitely occurring (eg. ABABABABABAB..)
◇□ = eventually becomes true and stays true continuously
⤳  = leads to. F ⤳ G = □(F ⇒ ◇G)

# Fairness

Fairness: X has to happen

Weak fairness:
* If A ever becomes CONTINUOUSLY enabled, then an A step must eventually occur.
* A cannot REMAIN enabled forever without another A step occurring.

Strong fairness:
* If A ever becomes REPEATEDLY enabled, then an A step must eventually occur.
* A cannot BE REPEATEDLY enabled forever without another A step occurring.

> fairness is specified and liveness is checked
https://old.reddit.com/r/tlaplus/comments/iwvw3b/very_basic_liveness_not_working/

Don't include liveness checks as part of the spec.


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

PROPERTIES Liveness
```