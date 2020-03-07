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

* Add usages back to config screens

* User profile page

* Bulk tag add/remove

* put ajax/ws protocol stuff in its own package

* Might be a good idea to store a description & examples with ReqTypes. (eg. "BR" means xxx + examples)
  Alternatively this could be on a help page.

* Markdown
  * Support nested lists
  * Support styling (bold, underline, italics, strikethrough)

* Tags
  * ability to create child tag in 1-step instead of 2 [FB-4]
  * add tag colours [FR-8]
  * Users #must be able to prohibit certain tags being used with certain req-types [FR-14]
  * When a req has a tag that is invalid for its req type, it should be hidden from all views [FR-15]
  * When a req has a tag that is invalid for its req type, users shouldn't be allow to specify it [FR-16]
  * remove key from TagGroups, check all Tag fields against UI prototype
  * ensure dead filter & style everywhere
  * TagSetApplicableChildrenOrder has a bug or something - remember how priorty got fucked up with Actors & could added under should?
  * Add tests to ProjectSpaProtocolsTest
  * Add tests for TagConfig
  * Add stability test for ProjectTemplate - probably just JSON of events is enough
  * Uniqueness of tag names... just group names? or include ap-tag keys too?
  * stability tests for Rev1
  * track TagConfig state in UnsavedChanges
  * handle dead ReqTypes' states in ApplicableReqTypes editors
    (eg user says whitelist A; user deletes A, what should the editor show? Maybe just a litle warning (not error) under field.
     like "<triangle-icon> A, B and C are dead")

* Fields
  * redo config screen
  * add default values for tag fields [FR-6]
  * make properties/values per req-type [FR-12]
  * add a max-size for field names, apply to editors
  * update reqtable (and probably reqdetail) to render dead rows like cfg field

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

* Reappearances wrt tags

* Allow system to add new field/columns in future without breaking existing projects.
  eg. User adds a "Last Updated" custom field, later ShipReq provides an auto-populated
  column with the same name. System needs a way to rename user's field without
  breaking user's project, history & hashes.
  Maybe a dynamic approach that compares versions, or maybe a migration task
  that adds a new event to everyone's projects to do the rename once when the
  new version is deployed.

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
  * New tag config screen
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