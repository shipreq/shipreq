first attempt: open/close tabs, user msgs, webapps, websockets etc.
remove easy (conn mgmt), irrelevant to specific problem...
what's my specific problem? I can manage out-of-order seqs & conn mgmt easily myself.
My problem is atomicity, cache invalidation, staleness, distributed txns wrt project caching/snapshots/event-processing

Ok Redis,DB,Webapps... but webapps are stateless so they don't matter,
Stateful things are: DB,Reids,usersStates
NO! Webapps have temporary state/req. It doesn't matter that it's a webapp, it's a req processor
Stateful things are: DB,Reids,userStates,processors

projects are independent, never conflict - it's trivial in code to separate them and accidentally using the wrong id is a typo, not something that requires a proof
therefore our model can be focused only on a single project

I deliberately wrote a bug in the spec that when when published events arrive in a non-consecutive order, they are dropped
but the model checker wasn't returning any errors. The problem was that users could always disconnect and establish a new
connection. For now I've disabled UserDisconnection ability.

Next it found a bug where the cache-hit condition on `Respond_InitRedis` should be that the cache returns a version higher than seen before (by the current proc)
instead of just whether it returns anything at all (because we have have come back after a failed DB write).

Having all the respond actions as functions meant that it was unclear in stack traces what was actually happening.

This spec models infinite behavior - one can keep adding modifications forever so TLC never reaches all states.
Instead I had to add a configurable limit, then specify that beyond that limit, the system only reacts and doesn't
produce any more modifications. Then add both a liveness condition to ensure we flush and stablise, and a constraint to
make TLC stop.
