Legend: [M/S/C] [Benefit 0-none,5-max] [Penalty 0-none,5-severe]

FUNC TODO
=========

* [M.3.3] [PROJ] Delete use cases.
* [M.2.2] [PROJ] Delete projects.
* [M.0.5] [LEGL] Check all licences and attribute as required.
* [M.0.5] [LEGL] Add terms of service.

* [S.2.5] [UCE ] Step is lost if accidentally deleted. No undo.
* [S.2.1] [ACCT] Change username & password (when logged in).
* [S.0.5] [ACCT] Password reset (when not logged in).
* [S.0.2] [UCE ] Text not corrected when post-correction matches prev value. (Title, text, step)

* [C.4.4] [UCE ] Symlink steps (and have flow targets point back). Eg "@sameas/@Copy [1.E.1.1]" What about children steps?
* [C.3.4] [ACCT] Add JS validation to register2 (ie. account creation).
* [C.3.3] [UCI ] Reorder UCs. Might be superceded by better UC org func such as grouping.
* [C.2.4] [UCE ] Reorder steps, probably via drag-n-drop.
* [C.2.1] [UCE ] Delete step via KB shortcut.
* [C.2.1] [ACCT] Add JS email corrector/validator like mailcheck.js.
* [C.2.1] [UCE ] Search and replace.
* [C.1.1] [UCE ] Alt+Shift+Enter to create next & indent.
* [C.1.1] [UCE ] View step references. (Eg. Who has refs to 1.0.4?)

TECH TODO
=========

* [S.3.5] [PERF] Add proper DB indexes.
* [S.2.4] [PERF] DB connection pooling.
* [S.1.3] [PERF] Shiro caching.
* [S.1.3] [PERF] Textareas' blur shouldn't send a request when input doesn't change.
* [S.1.2] [TEST] Test UseCaseCrudl.
* [S.1.1] [TEST] When SBT 0.13 comes out, shutdown Jetty et al in SBT hook.
* [S.0.5] [BUG ] When fields are loaded but no FV exists, fields should be cleared. (Will affect in-place loading.)

* [C.3.3] [CODE] Seal Field trait.
* [C.2.2] [CODE] Using tagged types for DB model attributes requiring validation/correction.
* [C.2.0] [PERF] DAO should cache certain actions and execute in bulk.
* [C.1.1] [DEMO] Show flow deletion in flow demo.

