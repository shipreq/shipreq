Frontend
  * Switch to semantic-react
  * Replace yuku-t/jquery-textcomplete with yuku-t/textcomplete
  * Remove jQuery
  * Improve load times, maybe defer stuff? Or maybe on-demand loading?
    * Katex.js can be loaded on demand. Only the CSS is required for rendering.

Misc
  * Group req types in filter from ALL to ANY.
  * Say "no implications" in imp graph

* Tags
  * (?) Add new column type: all tags (as opposed to non-field tags)
  * We have implications fields and implication columns.
    We don't seem to need all-imps vs non-field-imps...should tags not work the same way?
    Or is there similar deficiency in imps cols too?

* ReqTable
  * No content.
  * All dead.
  * All filtered out.
  * New button & form.
  * Sort form.
  * Filter form (∅,ok,ko) & help.
  * Summary math.
  * Column selection.
  * Delete/restore buttons.
  * Restore reusability on ReqTable and editors

* Determine UI for:
  * ReqDetail load failure
  * Deletion screen
  * Cfg Fields
  * Cfg Issues
  * Cfg ReqTypes
  * Cfg Tags

* Redo front pages
  * name as one field is fine, call it "full name" like credit cards
  * plan UI
  * impl UI
  * test

* Data design
  * Extract string table so that project structure can be cached separately from project textual content?
  * Store text changes as patches instead full replacements?
    * Lose the ability to quickly grep from DB.
    * Less storage/transport cost.
    * More CPU required to build a project from events.
  * Will [saved views] be stored in projects' event streams?
    * Consider impact on existing event stuff.
    * What about user-space views (as opposed to project-wide/shared views)?
      * Should they even be supported?
      * Where/how should they be stored?

* Devops & Deployment
  * New amazon accounts
  * Automate deployment
  * Add healthchecks
  * Send logs to service
  * Add proper metrics

* Do more with errors.
    ClientData.init
    ClientData.applyEvents

* Tech
  * Stop using scalaz.std.anything which brings in too much other stuff;
    use custom instances that have the minimum typeclasses needed.
  * Remove specs2. Use scalatest/μtest.
  * Remove ScalaCheck. Use Nyaya.
  * Use fast boopickle codecs for webworkers: https://github.com/ochrons/boopickle#codecs
  * Test env: Use different DBs for each module
  * Remove unused styles
