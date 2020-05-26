Backlog
=======

### UX
* Keyboard nav for ReqTablePage (not just the table) (?)
* Show past IDs in ReqTable
* Group req types in filter from ALL to ANY.

### Nice UI
* Cfg Fields
* Cfg Issues
* Cfg ReqTypes

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
* Taskman metrics


------------------------------------------------------------------------------------------------------------------------
Phase 3A
========

* Add gzip to WebSocket comms

* User profile page

* Bulk tag add/remove

* Markdown
  * Support nested lists
  * Support styling (bold, underline, italics, strikethrough)

* Investigate changes required to support phone / tablet

* Metrics
  * Reduce biz metrics to 5m
  * Wall metrics updating every 10 min only

* Add to imp graph page:
  * Saved views
  * Filter
  * Filter by chain size (eg. 0=all, 1 removes all nodes without imp, 2 removes A-B but not A-B-C)
  * Content summary
  * Configure display (eg. id + title, just title, add tags?)
  * Graph direction: TD / LR

* Two paths...

  1. B2C/OSS
    * github integration
    * git integration
    * anon shares
    * img graph improvements for next-task/progress tracking
    * public API

  2. B2B
    * multi user
    * audit trail (global)
    * audit trail (per req)
    * notifications

### Prototype
  * User profile: name
  * User profile: username
  * User profile: password
  * User profile: email
  * User profile: newsletter
  * Project deletion
  * Multi user

### Requirements
  * imp graph for task tracking
  * github integration
  * anon shares
  * audit trail
    * global/per req
    * open project at revision x in R/O mode
    * diff between revisions
    * tag/baseline
  * multi user
    * notification / change tracking
    * personal vs global saved views? impact on audit trail.
      maybe store views as Map[Option[UserId], List[SavedView]], just show "X created a personal saved view" in public audit trail
    * new filters: {createdBy,updatedBy,containsRefTo} {me,@blah}
    * refs to users in rich text
  * Common workflows


v2.1
========================================================================================================================

* add Option[ImpGraphConfig] to saved views
