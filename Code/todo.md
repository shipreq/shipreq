Phase 2
=======

### UX
* Autocomplete far away
* ReqTable: Refocus cell on editor close
* Replace yuku-t/jquery-textcomplete with yuku-t/textcomplete
* Improve load times, maybe defer stuff? Or maybe on-demand loading?
  * Katex.js can be loaded on demand. Only the CSS is required for rendering.

### Mechanics
* Add new column type: all tags (as opposed to non-field tags)
* We have implications fields and implication columns.
  We don't seem to need all-imps vs non-field-imps...should tags not work the same way?
  Or is there similar deficiency in imps cols too?

### Nice UI
* ReqDetail load failure
* Deletion screen
* Front pages

### Front pages
* Rewrite with scalajs-react?
* name as one field is fine, call it "full name" like credit cards

### Devops & Deployment
* New amazon accounts
* Automate deployment
* Add healthchecks
* Send logs to service
* Add proper metrics

### Tech
* Do more with errors, eg. ClientData.{init,applyEvents}
* Remove unused styles


------------------------------------------------------------------------------------------------------------------------
Backlog
=======

### UX
* Keyboard nav for ReqTable
* Keyboard nav for ReqDetail
* Show past IDs in ReqTable
* Group req types in filter from ALL to ANY.
* Say "no implications" in imp graph

### Nice UI
* Cfg Fields
* Cfg Issues
* Cfg ReqTypes
* Cfg Tags

### Data design
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

### Tech
* Stop using scalaz.std.anything which brings in too much other stuff;
  use custom instances that have the minimum typeclasses needed.
* Remove specs2. Use scalatest/μtest.
* Remove ScalaCheck. Use Nyaya.
* Use fast boopickle codecs for webworkers: https://github.com/ochrons/boopickle#codecs
* Test env: Use different DBs for each module
* Switch to semantic-react
* Remove jQuery

