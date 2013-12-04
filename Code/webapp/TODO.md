Legend: [M/S/C] [Benefit 0-none,5-max] [Penalty 0-none,5-severe]

FUNC TODO
=========

* [M.0.5=5] [LEGL] Check all licences and attribute as required.
* [M.0.5=5] [LEGL] Add terms of service.
* [M.3.2=5] [PROJ] Delete use cases.

* [S.3.4=7] [UCE ] Save button doesn't from disabled to enabled when text changes, only after blur. Confusing for user.
* [S.2.5=7] [UCE ] Step is lost if accidentally deleted. No undo.
* [S.4.1=5] [UA  ] Integrate feedback.js.
* [S.0.5=5] [PERF] Cascaded deletion VERY slow with non-trivial amount of data.
* [S.0.5=5] [ACCT] Password reset (when not logged in).
* [S.0.5=5] [UCE ] Step refs are case-sensitive. [5.e.1] should work in place of [5.E.1].
* [S.0.5=5] [FAIL] Robustness: Handle DB going down.
* [S.2.1=3] [ACCT] Change username.
* [S.1.1=2] [READ] Markup for http links.
* [S.1.1=2] [READ] Markup for mailto links.

* [C.3.4=7] [ACCT] Add JS validation to register2 (ie. account creation).
* [C.2.4=6] [UCE ] Make step labels fade into links when clickable, else users wont know to click.
* [C.3.3=6] [UCI ] Reorder UCs. Might be superceded by better UC org func such as grouping.
* [C.2.4=6] [UCE ] Reorder steps, probably via drag-n-drop.
* [C.4.1=5] [UCE ] Symlink steps (and have flow targets point back). Eg "@sameas/@Copy [1.E.1.1]" What about children steps?
* [C.2.2=4] [UX  ] Put spinning shit in Flow Graph until viz loads and renders it.
* [C.2.1=3] [UCE ] Delete step via KB shortcut.
* [C.2.1=3] [ACCT] Add JS email corrector/validator like mailcheck.js.
* [C.2.1=3] [UCE ] Search and replace.
* [C.2.0=2] [UX  ] When a user account is created, a default project called "Untitled Project" should be created.
* [C.1.1=2] [UCE ] Alt+Shift+Enter to create next & indent.
* [C.1.1=2] [UCE ] View step references. (Eg. Who has refs to 1.0.4?)
* [C.0.1=1] [UX  ] System shouldn't provide a means to read UCs when no UCs exist.

TECH TODO
=========

* [S.3.5=8] [PERF] Add proper DB indexes.
* [S.0.5=5] [BUG ] When fields are loaded but no FV exists, fields should be cleared. (Will affect in-place loading.)
* [S.1.3=4] [PERF] Textareas' blur shouldn't send a request when input doesn't change.
* [S.0.3=3] [    ] robots.txt

* [C.2.0=2] [PERF] DAO should cache certain actions and execute in bulk.
* [C.1.1=2] [DEMO] Show flow deletion in flow demo.
* [C.0.1=1] [PERF] Shiro caching.
* [C.0.0=0] [TEST] Shutdown Jetty et al in SBT hook.

UNTESTED
========
* [U] ChangeResult
* [I] /project
* [U] Publisher (page, JS, generation)
* [U] Navbar
* [U] UseCaseCrudl
* [U] ShareCreate
* [U] ShareEdit
* [U] Password changing: ShareList, DynModal
* [U] Share deletion (inc DynModal)
* [U] project/ActivateTab
* [U] UserAccount: View, pw change
* [U] Project deletion
* [U] Runtime props
* [U] DiagnosticEndpoints

MORE
====
See `../../Ideas.md`

