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

### New features
* Anonymous shares and read-only/presentation mode
* Tag/Implication Browser (aka Distribution manager/console)
* Project deletion. Maybe soft delete with ShowFilterButton. What about unique name constraint?
* Allow users to choose template when creating a project
* Add LastUpdated field to ReqTable/Detail
* Add Change Count field to ReqTable/Detail (help find most volatile/unstable reqs)
* Allow refs to custom text fields (e.g. [UC-1.detail])
* Warn when closing page and there are open, dirty editors
* Add KB shortcut to move colums in ReqTable

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
* New Form preview.show? shouldn't consider focus (?)
* Imp/Code editors are way too wide in NewReqForm
* Prevent -- (not not) in the FilterParser? Or allow /\-+/ and auto-correct on blur?
* Firefox: ctrl-home space doesn't work properly
* Firefox: UC step graph doesn't shrink

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

### New Features
* Issues
* Send feedback (with screenshot). Add link beside @username in top bar
* User profile page

### Other
* Allow system to add new field/columns in future without breaking existing projects.
  eg. User adds a "Last Updated" custom field, later ShipReq provides an auto-populated
  column with the same name. System needs a way to rename user's field without
  breaking user's project, history & hashes.
  Maybe a dynamic approach that compares versions, or maybe a migration task
  that adds a new event to everyone's projects to do the rename once when the
  new version is deployed.
* Issues prototype: add collapse/expand by issue{type,} columns


SQL makes no sense as a top-level
remove /login/xxx
remove /project/xxx
prefix all URLs with "URL: "
JDBC top-level? maybe suffix with method
RegisterSSP.xxx shouldn't be a top-level. at least move the proc path into tag
SSP.xxx -- move the proc path into tag. Change to ExecuteSSP or AJAX something.
Code top levels? Security dely, MakeEvent, UpdateProject. Prefix with "Fn: " or something?
Add user id tag to everything
JDBC (and maybe SQL) should be a different component (i.e. not webapp)
Link AJAX to initial request?

