FEATURE TODO
============

* [M] User management.
* [M] Use Case CRUD interface.
* [M] Usage limits per user (or team, etc).
* [S] Subscriptions.

FUNC TODO
=========

* [M.1.5] [LEGL] Add terms of service.

* [S.4.4] [UCE ] Keyboard shortcuts for step editing (ie. nav & indent)
* [S.3.2] [UCE ] New UCs should have the normal course head set to the UC title.
* [S.2.4] [UCE ] Allow step reordering, probably via drag-n-drop.
* [S.2.1] [ACCT] Change username & password (when logged in).
* [S.1.3] [UCE ] Text field values shouldn't lose audit trail when text is cleared.
* [S.1.2] [UCE ] Flow parsing fails both sides if either side has an error.
* [S.1.2] [UCE ] Flow parsing does weird things when flow exists then an edit with bad flow clause(s) comes in.
* [S.0.5] [ACCT] Password reset (when not logged in).

* [C.4.4] [UCI ] Reorder UCs. Might be superceded by better UC org func such as grouping.
* [C.3.4] [ACCT] Add JS validation to register2 (ie. account creation).
* [C.3.3] [UCE ] Support in-place DB updating (ie. no history disabled)
* [C.2.1] [ACCT] Add JS email corrector/validator like mailcheck.js.
* [C.1.1] [UCE ] View step references. (Eg. Who has refs to 1.0.4?)
* [C.1.0] [UCE ] While typing, click on step/label to insert a reference.


TECH TODO
=========

* [S.3.5] [PERF] Add proper DB indexes.
* [S.3.0] [PERF] Use pickling for JSON serialisation.
* [S.2.4] [PERF] DB connection pooling.
* [S.1.3] [PERF] Shiro caching.
* [S.1.1] [DEMO] Show flow deletion in flow demo.
* [S.1.1] [TEST] When SBT 0.13 comes out, shutdown Jetty et al in SBT hook.
* [S.1.1] [TEST] Fix test using UserFixture breaking other tests.
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

