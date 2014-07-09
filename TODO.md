High Freq UIs
=============
* Finish filter prototype.
* Ask for sort/col design/UX help on reddit.
* Mistake/accident prevention/recovery strategy.
  * Affects: Cfg.Fields, Cfg.ReqTypes, Incmp, Cfg.Incmp, GBrowser, LLReqs.
  * window top-right undo/redo, lock/unlock, partial undo? (ie. simply undo strategy that can fail on conflict)
  * First, catalog all change types.
* Prototype UC, SHR, subreqs on Incmp, GBrowser.

Med Freq UIs
============
* Catalogue incompletion types and prototype each.
  Think about how to quickly resolve from Incompletions page.
* Does incompletion view really have enough info/functionality?
  Confirm goals of page and re-read the reqs, maybe analysis doc too.
* Mock up allocation graphs.
* Grouping browser: revise reqs and analysis docs. Ensure it is sufficient for goals.
* Create a doc with UIs and goals, ensure they stay true to their purpose.
  Also clarify what they are *not* to prevent bloat and cross-UI duplication.
* The implication browser differs from the grouping one in that it considers
  implication transitivity in its content, but not its editing.
  i.e. remove all from MF-1 implied can still leave some transitively implied reqs remaining.
  Idea: Visually distinguish direct & indirect, have a button to hide indirect.
* GBrowser, so many buttons. Unneeded if just browsing.

Low Freq UIs
============
* Deleted rows displayed together/separate, restore/delete by drag/button?
  * Affects: Cfg.Fields, Cfg.ReqTypes, Cfg.Groupings.
  * Make consistent with common req view? (ie. inline, vis toggle checkbox)?

Other
=====
* Create reqs for text parsing and thus refkey rules such as fmt, uniqueness, case-sensitivity.
* Work out proper names for internal glossary (groupings, incmp, implication)
* Fucking subreqs, work out implications.
* MF column. When sorted should show dups for each MF, just like how sem IDs work.
* Split TODOs into req | prototype/UI | impl
* Idea: a project-wide long representing revision would be AWESOME.
  Eg. client's cache is at 15348, DB is at 16001, send changes. Could also use associativity and commutativity of (+) to compartmentalise and use revs for each data component (eg. grouping cfg rev, reqs rev) as long as all revs are monotonic.

Tech Prototype
==============
* Auto-complete.
  * Inserting a req ref by text would be awesome.
  * Implications (by id or text)
  * Groupings
* Prototype feasability of sending entire project's reqs to client.
  (local storage caching, payload size, mem size)

