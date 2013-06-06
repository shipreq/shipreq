FUNC TODO
=========

* [M.0.5] Enforce max step count.
* [M.0.5] Enforce max indent.

* [S.4.2] Allow step reordering, probably via drag-n-drop.
* [S.3.0] Keyboard shortcuts for step editing (ie. nav & indent)
* [S.1.3] Text field values shouldn't lose audit trail when text is cleared.
* [S.1.2] Flow parsing fails both sides if either side has an error.
* [S.1.2] Flow parsing does weird things when flow exists then an edit with bad flow clause(s) comes in.

* [C.3.3] Support in-place updating (ie. no history disabled)
* [C.1.1] View step references. (Eg. Who has refs to 1.0.4?)
* [C.1.0] While typing, click on step/label to insert a reference.


TECH TODO
=========

* [S.1.1] [TEST] Change CourseField step manipulations into pure + web funcs.
* [S.0.5] [SAFE] Thread-safety of SmartText (and possibly Field) is a worry.
* [S.0.5] [FAIL] When converting LoadCtx into a SaveCtx, fieldValues isn't being converted. (Will affect in-place loading.)
* [S.0.4] [FAIL] When fields are loaded but no FV exists, fields should be cleared. (Will affect in-place loading.)

* [C.2.0] [PERF] DAO should cache certain actions and execute in bulk.
* [C.1.1] [TEST] Remove UseCaseCtx.init().
* [C.1.0] [RUSE] Improve reusability of CachedFunction classes (and switch tests back on).
* [C.0.0] [ARCH] Title-to-NC sync should be done via msging.
* [C.0.0] [FAIL] DatabaseEnumTest disabled due to DB deadlocks.


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

