Goals
=====

* Drafts are saved (per user) locally and remotely

* A live draft is automatically updated when the underlying field is changed
  Sources:
    * it's externally and directly edited
    * a change somewhere else in the project affects the field

* Either:
  1) The most recently aborted drafts can be restored
  2) Drafts can be "hidden"/deferred and then later resumed

* [MAYBE] Users can see each others' live drafts (read-only)

* [MAYBE] Users can share (all? some?) live drafts with each other (meaning both edit the same draft & commit saves both)

* [MAYBE] Prune drafts from DB when...
  1) known to be useless, and/or:
  2) by age

Meta
====
* Start simple, add more functionality later/incrementally - just ensure that it's possible to add later
* Work out the model & strategy by testing incrementally by feature
* Consider all resulting scenarios by users PoV (after functionality proven possible)

Ideas
=====

* Maybe we just keep draft streams forever, it's only %50 larger than content apparently (?)
  Problem is, you have a divergence between event stream (source-of-truth) and drafts.
  Will drafts and events always be saved and sent together and atomically such that we can
  actually ignore events and rely on drafts? Seems dangerous.

* When a draft is committed, we can always extract a CRDT delta to represent just that change.
  Maybe save those forever and use to automatically update live drafts when a new event is received.
  In cases where it's not available, we could just diff revisions to create a CRDT delta that would work
  mechanically but possibly just lose user intention.
  (Still doesn't solve the indirectly affects cases but that should be done dynamically anyway because
  reprs and algos change.)












Questions
=========

* Should different users' drafts share the same CRDT?
  Maybe this doesn't matters if different CRDTs can be combined, and combined CRDTs can be split by user...

* Should users be able to see each others' drafts?
  If yes...
    * can the drafts interact? i.e. if A deletes a paragraph should it also disappear from B's draft?
    * when A commits a draft with changes from A & B, should B's changes also be committed?


Decisions
=========
* Start with CRDT streams separated by user - they can be merged at runtime if needed
