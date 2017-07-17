Backlog
=======

### UX
* Keyboard nav for ReqTablePage (not just the table) (?)
* Keyboard nav for ReqDetail
* Show past IDs in ReqTable
* Group req types in filter from ALL to ANY.
* Say "no implications" in imp graph
* Autocomplete problems
  * The popup is far away from the textarea
  * Ideally replace jquery-textcomplete with textcomplete (problems encountered)
  * Maybe write my own based on React
* Improve load times, maybe defer stuff? Or maybe on-demand loading?
  * Katex.js can be loaded on demand. Only the CSS is required for rendering.

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
* Remove jQuery - lift and semantic UI blocking this
* Remove unused styles
* Change ScalaCSS to generate Scala.JS without the runtime/JS-size overhead
* webapp-base-member shares packages with webapp-base, plus most of that stuff should be under .project as well
* Extract webapp-base-member-test
* Add laws for webapp-server-logic and test in webapp-server
* Change Taskman algebras from initial to final encodings
* Rename webapp-client-{home ⇒ member} now that its ambiguous in regards to the public pages


------------------------------------------------------------------------------------------------------------------------
Phase 2
=======

### Social
* Presentation

### Devops & Deployment
* New AWS/GCP accounts
* Automate deployment
* Add healthchecks
* Send logs to service
* Add proper metrics
* Britoli support
* Do more with errors, eg. ClientData.{init,applyEvents}
* Proper 404/500 pages
* Duplication between public.js and member.js
* Add DB indicies
* Restore AdminStats
* Restore DiagnosticEndpoints
* Remove SessionStats

### User-Functional Design
* Add new column type: all tags (as opposed to non-field tags)
* We have implications fields and implication columns.
  We don't seem to need all-imps vs non-field-imps...should tags not work the same way?
  Or is there similar deficiency in imps cols too?
* Re-evaluate config: some data is useless (i.e. key of custom text fields)

### Tech
* Only create a single Lift function per SPA instead of one per ServerSideProc
* Fix member layout using flex instead of whatever stupidity exists atm
* Switch to textcomplete

### New Features
* User profile page
* Issues
* Saved views
* Anonymous shares
