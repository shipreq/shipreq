UC Step Deletion
================
1. Test references to steps going dead.

Use Case Editor
===============
1. Parse step-text + flow.
2. Render step-text + flow.
3. Edit step-text + flow.
4. Autocomplete for UC steps.

Graphs
======
1. Graphviz & webworker
2. UC step graph (single UC)
2. Implication graph (single req on ReqDetail page)
4. UC step graph (all UCs! …maybe)
4. Imp graph (all reqs! …maybe)

Req Table
=========
1. Test & fix focus handling on edit start/end.

Finally
=======
* Fix and share KB-navigation-and-focus logic between Req{Table,Detail}.
* Fix project index.
* Redo DB schemas (old tables still exist).
* Redo all webapp assets
* Re-test all old stuff
* Make pretty.

SubReq implication
==================
1. Add SubReq
2. Need a SubReqId => SubReq lookup
3. Change imps and req refs to use SubReqId
4. Autocomplete for UC steps.

Other (?)
=========
* Saved views.
* Shares.

