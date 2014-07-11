Worrying relationships
======================
* Symmetrical relationships
* Ref validity
* Incompletions
* Revisions

Goals
=====
* Send entire project to client.
* Send needed changes to client.
* Server validate all changes before saving (ie. guard project state validity)
  - refs are (in)valid, (no)issue.

Risks
=====
* Server-side memory
* Significant schema changes coming in phase3

Strategy
========
1. Model purely first.
2. Model simple change and undo/redo support.
3. Model complex change (each kind of knock-on effect).
4. Support delta transmission. (to keep clients up-to-date efficiently)
5. Worry about persistence.

Ideas
=====
* Have a string table separate from structure. Will allow server to load entire project structure in mem at hopefully low mem cost (if needed). Should also save space due to reuse.
