Prototype / UI
==============

## High Freq UIs

* Mistake/accident prevention/recovery strategy.
  * Affects: Cfg.Fields, Cfg.ReqTypes, Incmp, Cfg.Incmp, GBrowser, LLReqs.
  * window top-right undo/redo, lock/unlock, partial undo? (ie. simply undo strategy that can fail on conflict)
  * First, catalog all change types.
* Fucking subreqs, work out implications.
* Prototype {UC, SHR, subreqs} on {Incmp, GroupingBrowser}.

## Med Freq UIs

* Mock up allocation graphs.
* Grouping browser: revise reqs and analysis docs. Ensure it is sufficient for goals.
* The implication browser differs from the grouping one in that it considers
  implication transitivity in its content, but not its editing.
  i.e. remove all from MF-1 implied can still leave some transitively implied reqs remaining.
  Idea: Visually distinguish direct & indirect, have a button to hide indirect.
* GBrowser, so many buttons. Unneeded if just browsing.

## Low Freq UIs

* Deleted rows displayed together/separate, restore/delete by drag/button?
  * Affects: Cfg.Fields, Cfg.ReqTypes, Cfg.Groupings.
  * Make consistent with common req view? (ie. inline, vis toggle checkbox)?

Implementation / Other
======================

* Work out proper names for internal glossary (groupings, incmp, implication)
* Idea: a project-wide long representing revision would be AWESOME.
  Eg. client's cache is at 15348, DB is at 16001, send changes. Could also use associativity and commutativity of (+) to compartmentalise and use revs for each data component (eg. grouping cfg rev, reqs rev) as long as all revs are monotonic.


UX Notes
========
* Context over consistency
* ≤ 5 columns in a table
