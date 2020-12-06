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

# Temporal properties

□  = continuously
◇  = eventually
□◇ = infinitely occurring (eg. ABABABABABAB..)
◇□ = eventually becomes true and stays true continuously
⤳  = leads to. F ⤳ G = □(F ⇒ ◇G)

A is an action
<<A>>_v is a step where action A changes v
<<A>>_v == A /\ (v' /= v)

A behaviour is any infinite sequence of states.
A step of a behaviour is any pair of consecutive states in the behaviour.
All behaviours are infinite even when state transitions stop because it can just stutter forever.
Properties are stuttering-insensitive.

Don't include liveness checks as part of the spec.
> fairness is specified and liveness is checked
> https://old.reddit.com/r/tlaplus/comments/iwvw3b/very_basic_liveness_not_working/
Intuition here is that
  * fairness describes the reality of the system
    (eg. a network msg will either be received or lost, it wont just sit unprocessed for eternity)
  * liveness tests are desired properties of a successful system


### Safety properties
* A property P is a safety property iff all behaviours, b satisfies P iff every prefix of b satisfies P
* `Init /\ [][Next]_vars` is a safety property
* `<>(x = 3)` is not safety property because we can't tell that b doesn't satisfy the formula by looking
  at any prefix of b. We need to view the entire infinite behaviour to know x ≠ 3 in any state.
* In other words: TRUE from beginning of time, to the end of time

### Liveness properties
* b is an extension of a finite sequence s iff s is a prefix of b
  In other words "b extends s" by proceeding it
* A property P is defined to be a liveness property iff every finite sequence of states can be extended to
  a behaviour that satisfies P.
* P is a liveness property iff every finite sequence of states can be extended to a behaviour that satisfies P
  In other words, any number of arbitrary states can be prefixes.
* <>(x = 3) is a liveness property; any finite sequence of states can be extended to a behaviour satisfying it
  by simply appending a state in which x = 3 and then appending any infinite sequence of states.
* In other words: TRUE for some period(s) of time

# Machine closure
* If S is a safety property and L a liveness property, then the pair S,L is said to be machine closed iff
  Every finite sequence of states that satisfies S can be extended to a behaviour that satisfies S ∧ L

# Fairness

* L is a fairness property for S iff S,L is machine closed
* Fairness: X has to happen

In WF_v(A) and SF_v(A), remember that A must still satisfy a pair of steps in S.

Weak and strong fairness are equivalent for an action that, once enabled, remains enabled until it is executed.
Strong fairness for actions that can be disabled by other actions.

Include fairness (but not other liveness properties) as part of the `Spec`.

### Weak fairness

* If A ever becomes *CONTINUOUSLY* enabled, then an A step must eventually occur.
  Continuously means without interruption.

* A cannot REMAIN enabled forever without another A step occurring.

* `WF_v(A) == □(□ENABLED A => ◇<A>ᵥ)`    - It’s always the case that, if A is enabled forever, then an A step eventually occurs.
* `WF_v(A) == ◇□(ENABLED<A>ᵥ) ⇒ □◇<A>ᵥ`  - If A is eventually enabled forever, then infinitely many A steps occur.
* `WF_v(A) == □◇(¬ENABLED<A>ᵥ) ∨ □◇<A>ᵥ` - A is infinitely often disabled, or infinitely many A steps occur.

* \/ `… → ENABLED → OCCURS → … → ENABLED → OCCURS → …`
  \/ Never become ENABLED

* WF_v(A) succeeds iff the following *equivalent* conditions are satisfied:
  * Any suffix of b whose states all satisfy ENABLED <<A>>_v, contains an <<A>>_v step
  * Any suffix of b must contain a <<A>>_v step or a state on which ENABLED <<A>>_v equals false
  * If b contains a suffix, all of whose states satisfy ENABLED <<A>>_v, then b contains infinitely many <<A>>_v steps
  * b must contain infinitely many <<A>>_v steps or infinitely many states that do not satisfy ENABLED <<A>>_v
  * b does not contain a suffix with no <<A>>_v step whose states all satisfy ENABLED <<A>>_v

* WF_v(A) in English:
  * It's always the case that, if A is enabled forever, then an A step eventually occurs
  * A is infinitely often disabled, or infinitely many A steps occur
  * If A is eventually enabled forever, then infinitely many A steps occur

### Strong fairness

* If A ever becomes *CONTINUALLY* enabled, then an A step must eventually occur.
  Continually means repeatedly, possibly with interruptions.

* A cannot BE REPEATEDLY enabled forever without another A step occurring.

* `SF_v(A) == ◇□(¬ENABLED<A>ᵥ) ∨ □◇<A>ᵥ` - A is eventually disabled forever, or infinitely many A steps occur
* `SF_v(A) == □◇ENABLED<A>ᵥ ⇒ □◇<A>ᵥ`    - If A is infinitely often enabled, then infinitely many A steps occur

* SF_v(A) succeeds iff the following *equivalent* conditions are satisfied:
  * Any suffix of b containing {infinitely many states that satisfy ENABLED <<A>>_v}, contains an <<A>>_v step
    (note: "infinitely many states" needn't be consecutive)
  * If b contains infinitely many states satisfying ENABLED <<A>>_v, then it contains infinitely many <<A>>_v steps
  * Any suffix of b must contain an <<A>>_v step or have a suffix in which ENABLED <<A>>_v equals false in all its states
  * b does not have a suffix that has infinitely many states satisfying ENABLED <<A>>_v and has no <<A>>_v step

* Any behaviour satisfying SF_v(A) also satisfies WF_v(A)

# TLC

- TLCGet("distinct") = total number of distinct states found by TLC so far, globally.
- TLCGet("queue")    = number of states currently in the queue to be checked.
- TLCGet("duration") = number of seconds elapsed since model checking began.
- TLCGet("diameter") = length of the longest behaviour found by TLC so far, globally (equals one in the initial predicate).
- TLCGet("level")    = length of the current behaviour (equals zero in the evaluation of the initial predicate).


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

Fairness ==
  & SF_<<vars>>(A)

Spec == Init /\ [][Next]_<<vars>> /\ Fairness

Liveness ==
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
