Backlog
=======

### UX
* Keyboard nav for ReqTablePage (not just the table) (?)
* Show past IDs in ReqTable
* Group req types in filter from ALL to ANY.
* Say "no implications" in imp graph
* Create and use HomeSpaLoader (maybe - might have a negative effect)

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

### New features
* Anonymous shares and read-only/presentation mode
* Tag/Implication Browser (aka Distribution manager/console)

### DevOps
* Create db rollback plan

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
* webapp-base{,-member} packages are shit. Reorg!
* webapp-base-member shares packages with webapp-base, plus most of that stuff should be under .project as well
* Extract webapp-base-member-test
* Add laws for webapp-server-logic and test in webapp-server
* Rename webapp-client-{home ⇒ member} now that its ambiguous in regards to the public pages
* Make webtamp hash filenames of urls in Semantic CSS (`icons.*`)
* Add DB indicies (don't look at code! metrics dashboard should indicate)
* SSL shouldn't be in Docker - resolve TODO in WebappBuild.scala
* Hide Tags/Imps columns when guaranteed to be empty & useless.
  (i.e. all possible tags/imps are allocated to columns)
  Also consider FilterDead=ShowDead when designing this.
  Also consider dead tags in use in text are always displayed which shows in them in Tags columns even when HideDead
* Allow project deletion. Maybe soft delete with ShowFilterButton. What about unique name constraint?
* New Form preview.show? shouldn't consider focus (?)
* Warn when closing page and there are open, dirty editors
* Imp/Code editors are way too wide in NewReqForm
* Allow users to choose template when creating a project
* Add LastUpdated field to ReqTable/Detail
* Add Change Count field to ReqTable/Detail (help find most volatile/unstable reqs)
* Allow refs to custom text fields (e.g. [UC-1.detail])

------------------------------------------------------------------------------------------------------------------------
Phase 2
=======

### Social
* Co-founder criteria

### Devops & Deployment
* StackDriver
  * Monitoring
  * Logging
  * Metrics
  * Alerting
* Env
  * Automate releases
  * Automate ops (dashboards, alerts, etc)
* Devops
  * Code to send logs
  * Code to send metrics
  * Revise all logging
  * Determine and implement valuable metrics (tech & business)
  * Do more with errors (client & server), eg. ClientData.{init,applyEvents}
  * Restore or delete AdminStats
  * Restore or delete DiagnosticEndpoints
  * Restore or delete SessionStats

### Misc
* UseCase exception levels are off-by-one. 4.E.1.a.i should be 4.E.1.1.a (should it though?)
* After SavedViews done, add a nice Default view with some custom fields included
* Revise ProjectTemplate - reduce reqtypes, BL without BR is weird
* Upgrade JDK and audit crypto mechanisms (pending next Jetty release)
* ReqDetail KB shortcuts: ↑, ↓, F2, Tab in/out
* Add and use RestorationForm just like the DeletionForm
* Hashing
  * Clear DataHasher history
  * Revise the HashScheme evolution stuff. Seems a bit too complicated (and had to determine correct)...
  * Add HashScheme test that covers SavedViews
  * Make a decision about the inclusion of IdCeilings (it's calculatable state of other parts of the project, transient = should exclude right?)
  * Add a HashScope just for SavedViews
  * Remove the HashScope => Validity stuff. Too hard, too fickle, too complex
* Prevent -- (not not) in the FilterParser?
* Add ProjectContent, similar to ProjectConfig?

### New Features
* Issues
* Saved views
* Send feedback (with screenshot). Add link beside @username in top bar
* User profile page
