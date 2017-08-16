Backlog
=======

### UX
* Keyboard nav for ReqTablePage (not just the table) (?)
* Keyboard nav for ReqDetail
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
* Make webtamp hash filenames of urls in Semantic CSS (icons.*)
* Add DB indicies (don't look at code! metrics dashboard should indicate)
* SSL shouldn't be in Docker - resolve TODO in WebappBuild.scala
* Hide Tags/Imps columns when guaranteed to be empty & useless.
  (i.e. all possible tags/imps are allocated to columns)
  Also consider FilterDead=ShowDead when designing this.

------------------------------------------------------------------------------------------------------------------------
Phase 2
=======

### Social
* Co-founder criteria
* Presentation

### Devops & Deployment
* Document
  * env/infra diagram
  * each service's details
  * everything below both wrt ShipReq employee, and as personal study notes
* Services - create/confirm, cleanse, configure accounts
  * Mailchimp
  * ZenDesk
  * GCP
    * Monitoring
    * Logging
    * Metrics
    * Alerting
    * DNS
    * SMTP
  * Email addresses
  * Google Analytics
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
* Recovery
  * Create db rollback plan
* Security
  * Upgrade JDK and audit crypto mechanisms

### Dev
* Allow refs to custom text fields (e.g. [UC-1.detail])
* Clicking a UCRef should open the UC in ReqDetail (just like ReqRefs)
* Revise ProjectTemplate - reduce reqtypes, BL without BR is weird

### New Features
* User profile page
* Issues
* Saved views
