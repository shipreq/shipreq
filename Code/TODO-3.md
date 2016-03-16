Req Detail
==========
* Make Delete/Restore button work.
* Test:
  * dead not editable, live editable. (invariant)
  * del/restore.
  * inapplicable fields hidden.
  * dead fields hidden/shown.

UC Step Deletion
================
1. Whole tree includes dead.
2. Have filter to live tree for VectorTree.Locations to equal step label
3. Need to add a scheme for referencing dead steps. UC-1.X.(1..n) so that refs to dead steps have something unique to display in text.
4. Restore dead steps.

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
1. Re-enable old tests and test with selenium.
2. Test & fix focus on edit start/end.

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

