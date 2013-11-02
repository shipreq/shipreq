Legend: [M/S/C] [Benefit 0-none,5-max] [Penalty 0-none,5-severe]

FUNC TODO
=========

* [M.3.3] [PROJ] Delete use cases.
* [M.2.2] [PROJ] Delete projects.
* [M.0.5] [LEGL] Check all licences and attribute as required.
* [M.0.5] [LEGL] Add terms of service.

* [S.3.4] [UCE ] Save button doesn't from disabled to enabled when text changes, only after blur. Confusing for user.
* [S.2.5] [UCE ] Step is lost if accidentally deleted. No undo.
* [S.2.1] [ACCT] Change username & password (when logged in).
* [S.1.1] [UX  ] Pages don't have acceptable titles.
* [S.0.5] [ACCT] Password reset (when not logged in).
* [S.0.5] [UCE ] Step refs are case-sensitive. [5.e.1] should work in place of [5.E.1].

* [C.2.4] [UCE ] Make step labels fade into links when clickable, else users wont know to click.
* [C.4.4] [UCE ] Symlink steps (and have flow targets point back). Eg "@sameas/@Copy [1.E.1.1]" What about children steps?
* [C.3.4] [ACCT] Add JS validation to register2 (ie. account creation).
* [C.3.3] [UCI ] Reorder UCs. Might be superceded by better UC org func such as grouping.
* [C.2.4] [UCE ] Reorder steps, probably via drag-n-drop.
* [C.2.1] [UCE ] Delete step via KB shortcut.
* [C.2.1] [ACCT] Add JS email corrector/validator like mailcheck.js.
* [C.2.1] [UCE ] Search and replace.
* [C.2.0] [UX  ] When a user account is created, a default project called "Untitled Project" should be created.
* [C.1.1] [UCE ] Alt+Shift+Enter to create next & indent.
* [C.1.1] [UCE ] View step references. (Eg. Who has refs to 1.0.4?)
* [C.0.1] [UX  ] System shouldn't provide a means to read UCs when no UCs exist.

TECH TODO
=========

* [S.3.5] [PERF] Add proper DB indexes.
* [S.2.4] [PERF] DB connection pooling.
* [S.1.3] [PERF] Shiro caching.
* [S.1.3] [PERF] Textareas' blur shouldn't send a request when input doesn't change.
* [S.1.1] [TEST] When SBT 0.13 comes out, shutdown Jetty et al in SBT hook.
* [S.0.5] [BUG ] When fields are loaded but no FV exists, fields should be cleared. (Will affect in-place loading.)
* [S.0.5] [TEST] Check \r on input from mac/windows. TextMarkup only respects \n.

* [C.2.0] [PERF] DAO should cache certain actions and execute in bulk.
* [C.1.1] [DEMO] Show flow deletion in flow demo.

UNTESTED
========
* [U] ChangeResult
* [I] /project
* [U] Publisher (page, JS, generation)
* [U] Navbar
* [U] UseCaseCrudl
* [U] ShareCreate
