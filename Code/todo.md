Backlog
=======

### UX
* Keyboard nav for ReqTablePage (not just the table) (?)
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

### User-affecting
* Hide Tags/Imps columns when guaranteed to be empty & useless.
  (i.e. all possible tags/imps are allocated to columns)
  Also consider FilterDead=ShowDead when designing this.
  Also consider dead tags in use in text are always displayed which shows in them in Tags columns even when HideDead
* New Form preview.show? shouldn't consider focus (?)
* Imp/Code editors are way too wide in NewReqForm
* Firefox: ctrl-home space doesn't work properly
* Firefox: UC step graph doesn't shrink
* Prevent -- (not not) in the FilterParser? Or allow /\-+/ and auto-correct on blur?
* Filter doesn't allow req-code filtering
* Issues page: allow custom sorting
* Issues page: allow custom columns
* Issues page: allow issue cat/cls filtering
* Issues page: allow editing of ID cells
* Issues page: allow editing of Title cells
* Use WebSockets on home SPA to see projects and project stats update

### Tech
* Remove ScalaCheck. Use Nyaya.
* Test env: Use different DBs for each module
* Switch to semantic-react
* Remove jQuery - Semantic UI blocking this
* Remove unused styles
* Change ScalaCSS to generate Scala.JS without the runtime/JS-size overhead
* webapp-base{,-member} packages are shit. Reorg!
* webapp-base-member shares packages with webapp-base, plus most of that stuff should be under .project as well
* Extract webapp-base-member-test
* Add laws for webapp-server-logic and test in webapp-server
* Rename webapp-client-{home ⇒ member} now that its ambiguous in regards to the public pages
* Make webtamp hash filenames of urls in Semantic CSS (`icons.*`)
* Automate visual testing so changes to styling (mostly Semantic UI upgrades) can be verified.
  Use BackstopJS. Will require a read-only test user with a pre-configured project


------------------------------------------------------------------------------------------------------------------------
Phase 2B
========

### New Features
* User profile page

### Other

* Add to imp graph page:
  * Saved views
  * Filter
  * Content summary
  * Configure display (eg. id + title, just title, add tags?)

* Copy to clipboard from cells in Req{Table,Detail}

* Allow system to add new field/columns in future without breaking existing projects.
  eg. User adds a "Last Updated" custom field, later ShipReq provides an auto-populated
  column with the same name. System needs a way to rename user's field without
  breaking user's project, history & hashes.
  Maybe a dynamic approach that compares versions, or maybe a migration task
  that adds a new event to everyone's projects to do the rename once when the
  new version is deployed.

* Failure
  * Do more with errors (client & server), eg. ClientData.{init,applyEvents}
  * Add React component error handling and possibly report to server
  * Add an error-catching top-level react component

* Metrics
  * Taskman metrics
  * Finish tech dashboards (inc using webapp metrics)
  * Business metrics: dashboards

* Unsaved changes
  * Allow user to see where they are (by PubId is probably enough)
  * Save to, and load from, localStorage (maybe spec with TLA+ cos merges?)
  * Provide ability to save all
