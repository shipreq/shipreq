Legend: [M/S/C] [Benefit 0-none,5-yay] [Penalty 0-none,5-severe]
        [TW_] = Taskman | Webapp | Type

Type:
  X = UX. Improve experience of existing functions.
  + = New function (user noticable)
  ! = Bug or correctness
  P = Performance
  $ = Security


Use Case Editor & Reader
========================

* [S.2.5=7] [ W+] Step is lost if accidentally deleted. No undo.
* [S.0.5=5] [ WX] With slow connections, UX is bad when text field loses focus. Appears to lose changes.
* [S.3.2=5] [ WX] Tab/shift-tab should move between input fields just like alt+up/down.
* [S.0.2=2] [ WX] Save button doesn't from disabled to enabled when text changes, only after blur. Confusing for user.

* [C.2.4=6] [ WX] Make step labels fade into links when clickable, else users wont know to click.
* [C.2.4=6] [ W+] Reorder steps, probably via drag-n-drop.
* [C.4.1=5] [ W+] Symlink steps (and have flow targets point back). Eg "@sameas/@Copy [1.E.1.1]" What about children steps?
* [C.2.2=4] [ W+] Markup for http links.
* [C.2.2=4] [ WX] Put spinning shit in Flow Graph until viz loads and renders it.
* [C.2.1=3] [ WX] Allow collapse/expand steps.
* [C.2.1=3] [ W+] Delete step via KB shortcut.
* [C.2.1=3] [ W+] Search and replace.
* [C.2.1=3] [ W+] Markup for ordered lists. (eg "#. abc")
* [C.1.1=2] [ W+] Markup for mailto links.
* [C.1.1=2] [ W+] Alt+Shift+Enter to create next & indent.
* [C.1.1=2] [ W+] View step references. (Eg. Who has refs to 1.0.4?)
* [C.1.1=2] [ W ] In UCE demo, show flow deletion.
* [C.0.2=2] [ W!] Either replace consecutive whitespace or maintain it when rendering to HTML.


Comms System
============

* [M.?.?=?] [   ] @shipreq email should be routed to FreshDesk.
* [M.?.?=?] [ W+] Allow user to change prefs.
* [M.?.?=?] [TW ] Update ML after user pref change.
* [M.?.?=?] [TW ] Change all bearded.logic@gmail.com links to contact@shipreq.com.
* [M.?.?=?] [T  ] Setup MailChimp prod & doc process.
* [M.?.?=?] [T  ] Setup FreshDesk prod & doc process.

* [S.?.?=?] [   ] @shipreq email should be routed to Gmail for redundancy.
* [S.?.?=?] [T  ] Certain emails sent from Taskman should BCC Gmail for redundancy.
* [S.?.?=?] [T P] Add indexes to Taskman tables.
* [S.?.?=?] [T !] Async workers should reassign themselves if time remaining under threshold.
* [S.?.?=?] [   ] Update comms system & system arch doco.

* [C.?.?=?] [T  ] Taskman msg aging.
* [C.?.?=?] [T  ] Taskman msg consolidation to avoid redundant work. Eg n instances of SyncToMailingList(None).
* [C.?.?=?] [T  ] Taskman msg batching. Eg every hour process all the UserUpdated(n) msgs in bulk.

### Insightly
* [?.?.?=?] [T  ] Should contact syncs to ML also happen to Insightly?
* [?.?.?=?] [T  ] All emails sent from FreshDesk should BCC Insightly.
* [?.?.?=?] [   ] @shipreq email should be routed to Insightly.
* [?.?.?=?] [T  ] All emails sent from Taskman should BCC Insightly.
* [?.?.?=?] [T  ] User pref change should update Insightly?
* [?.?.?=?] [T  ] Setup Insightly prod & doc process.


Functional
==========

* [S.5.4=9] [ W+] Allow users to provide feedback.
* [S.3.4=7] [ W+] Allow delete use cases.
* [S.0.4=4] [ W!] Bug: Ajax form, hit submit, Chrome rejects due to required fields, fix fields, blur on ajax text will then errornously submits form.
* [S.0.3=3] [TW+] Allow change email.
* [S.0.2=2] [TW+] Allow delete account.
* [S.0.2=2] [ WX] Page for 404 errors.
* [S.0.2=2] [ WX] Page for 5xx errors.

* [C.3.4=7] [ WX] Add JS validation to register2 (ie. account creation).
* [C.3.3=6] [ W+] Reorder UCs. Might be superceded by better UC org func such as grouping.
* [C.2.1=3] [ WX] Add JS email corrector/validator like mailcheck.js.
* [C.2.0=2] [ WX] When a user account is created, a default project called "Untitled Project" should be created.
* [C.0.2=2] [ W+] Allow change username.
* [C.0.1=1] [ WX] System shouldn't provide a means to read UCs when no UCs exist.


Technical
=========

* [S.3.5=8] [ WP] Add proper webapp DB indexes.
* [S.0.5=5] [TW ] Monitoring. (nagios, riemann.io)
* [S.0.5=5] [ W!] When fields are loaded but no FV exists, fields should be cleared. (Will affect in-place loading.)
* [S.1.3=4] [ WP] Textareas' blur shouldn't send a request when input doesn't change.
* [S.0.3=3] [ W$] Secure /diag endpoints.
* [S.2.0=2] [ W ] Webapp should use rolling appender log like Taskman
* [S.0.2=2] [ W ] Serve robots.txt via HTTP.
* [S.2.0=2] [ WP] Investigate different parsing engine: parboiled-scala or gll-combinators.

* [C.0.5=5] [TW ] Robustness: Handle DB going down.
* [C.0.1=1] [ W$] Add a HSTS header (HTTP Strict Transport Security)
* [C.0.1=1] [ WP] Shiro caching.
