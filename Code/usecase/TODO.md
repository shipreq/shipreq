FUNC TODO
=========

* [M.0.5] Enforce max step count.
* [M.0.5] Enforce max indent.
* [ . . ] 

* [S.3.0] Keyboard shortcuts for step editing (ie. nav & indent)
* [S.4.2] Allow step reordering, probably via drag-n-drop.
* [S.1.2] Flow parsing fails both sides if either side has an error.
* [S.1.2] Flow parsing does weird things when flow exists then an edit with bad flow clause(s) comes in.
* [ . . ] 

* [C.1.0] While typing, click on step/label to insert a reference.
* [C.1.1] View step references. (Eg. Who has refs to 1.0.4?)
* [ . . ] 

TECH TODO
=========
* [M.0.3] [PERF] Combined stepLabelMap is horribly ineffecient.
* [S.0.1] [CODE] Remove Step. StepNode is enough.
* [C.0.0] [CODE] Title-to-NC sync should be done via msging.

NOTES
=====

Legend: [M/S/C] [Benefit 0-none,5-max] [Penalty 0-none,5-severe]

← ↑ → ↓ ↔ ↕ ↖ ↗ ↘ ↙
⬅ ⬆ ⬇ ⬈ ⬉ ⬊ ⬋ ⬌

<-- ok --> BAD
<-- BAD --> ok
--> BAD <-- ok
--> ok <-- BAD

<-- 1.0.1 --> 9.99
<-- 9.99 --> 1.0.1
--> 9.99 <-- 1.0.1
--> 1.0.1 <-- 9.99

1) Has: Blah ⬅ 1.1 ➡ 1.1
2) Type: Blah ⬅ 11.0.1
3) Get: Blah <- 11.0.1 ⬅ 1.1 ➡ 1.1
   Exp: Blah <- 11.0.1

