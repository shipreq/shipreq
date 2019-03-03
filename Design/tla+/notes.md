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

It's not enough to just stop when events have been flushed/drained, to really confirm our
expected behaviour, add a test to ensure all users have the expected/correct content.
I added a new invariant: `MCDone => AllUsersUpToDate`

There is no need for this model to handle pub/sub potentially failing and msgs being lost.
Here we can just assume it works correctly and if it needs verification of its own,
it can be modelled separately. It's orthogonal. Relying on assumptions is common & beneficial.

Something fantastic about TLA+ is that you have some conditional logic, you can choose to not even include the condition in the model,
and just say "both cases can occur here". Eg. when updating the cache you might have some logic that decides whether you just store your
new event, or you update the snapshot. How you choose might be simply every-n-events, or binary size comparison of snapshot vs events,
but it doesn't matter. Just declare in the logic that both are possibilities.

Not sure how to test efficiency... Eg. if I have a bug in my spec/logic that results in 90% cache misses

Catches crazy edge caches. Like I've just created a new event, my very next action is to update the cache
then I found out it's already up-to-date, and with a snapshot to boot (!).
Also off-by-1 errors but not in arithmetic, in comparison. eg < vs <=

Add cache eviction by Redis. It's quickly proved that my logic is insufficient.
It's been great up until to verify my own logic. Once happy it took 1 min to introduce Redis' logic (eviction).
Once I get this working the confidence I'll have is fantastic. This is been an extremely valuable exercise.

Summary
* hard to write a spec for exactly what you want
  * safety props vs lifeness
  * many of the props in our head are implicit - your spec could pass but there's an implicit property you haven't specified that's being violated (e.g. all users have latest project)
  * adding props is like guiding a horse to water without interacting with it, but by modifying its env, eg. adding fences
