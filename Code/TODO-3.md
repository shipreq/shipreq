* Purge old UC stuff

* Redo all webapp assets

UC Step Deletion
================
* Whole tree includes dead.
* Have filter to live tree for VectorTree.Locations to equal step label
* Need to add a scheme for referencing dead steps. UC-1.X.(1..n) so that refs to dead steps have something unique to display in text.
* Restore dead steps.

SubReq implication
==================
* Add SubReq
* Change imps and req refs to use SubReqId
* Need a SubReqId => SubReq lookup

Req Detail
==========
* Add show/hide deleted UI.
* Show dead fields, deletion reason, etc.
* Test dead not editable.
* Add Delete/Restore Req button.

Req Table
=========
* Re-enable old tests and test with selenium.
* Test & fix focus on edit start/end.

Graphs
======

* UC step graph (single UC)
* UC step graph (all UCs! …maybe)
* Implication graph (single req on ReqDetail page)

