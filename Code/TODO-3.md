Use Case Editor
===============
1. Fix ctrls: delete/restore button.
1. Fix ctrls: disable on async.
1. Tests for steps in UCE.
2. Clean up ReqDetail and uce package.

Graphs
======
1. Graphviz & webworker
2. UC step graph (single UC)
2. Implication graph (single req on ReqDetail page)
4. UC step graph (all UCs! …maybe)
4. Imp graph (all reqs! …maybe)

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
1. Allow steps to imply other reqs.
1. Allow reqs to imply UC steps (?).

Eventually
==========
* Autocomplete for UC step refs (in text & step flow).
* Ambiguity between [1.0.1] as UC ref or code ref.
* Fix PubidRegister. Types are terrible.
* Stop using scalaz.std for Traverse[Vector/List] etc.

Other (?)
=========
* Saved views.
* Shares.

