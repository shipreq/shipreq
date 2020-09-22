v2.2
========================================================================================================================

### Plan
* Analyse [Collapsible Trees] and [Github integration]
* Analyse, dev, deploy
* Use new features to cut out all the noise in my ShipReq (so I can really see what's left)
* New plan for the rest of v2.2

### Analyse
* it would be a cool idea to have a button in req detail that would simplify the forward implication tree
  (simplify = remove redundant edges)
  Or maybe we can just apply this option to views without affecting data?
* [FB-11] Add a field to show where a req is being referenced (i.e. FB-9 references FR-5 in its detail field, I should be able to see that from FB-5's page. "References"? "Citations"?)
* Req Code config
  * completely disable (don't show in ReqDetail or ReqTable, even remove from auto-complete)
  * disable for some req types (and don't show in ReqDetail)
  * mandatory for some req types
* Common workflows pt2: SDLC to deployment
* Tag aliases - a map of ReqType -> ApTagId. eg #done = FR:#analysed, CO:#implemented -- probably a bad idea because
  if derivative tags are powerful enough you cover the underlying motivation with it appearing explicit instead of implicit
  (by that I mean you can see the real value in the column, not see one value and get another.
  everything in ShipReq should be transparent.)
  Need analysis: what am I trying to solve? Honestly even if I built this immediately I have a feeling I wouldn't use it (lol)
* Github integration
* ImpGraph column in ReqTable
* Multi user
  * notification / change tracking
  * personal vs global saved views? impact on audit trail.
    maybe store views as Map[Option[UserId], List[SavedView]], just show "X created a personal saved view" in public audit trail
  * new filters: {createdBy,updatedBy,containsRefTo} {me,@blah}
  * refs to users in rich text
  * Audit trail
    * global/per req
    * open project at revision x in R/O mode
    * diff between revisions
    * tag/baseline
    * Add LastUpdated field to ReqTable/Detail
* Project templates (and copies/user-defined)

### Prototype
* User profile: name
* User profile: username
* User profile: password
* User profile: email
* User profile: newsletter
* Project deletion (hard & soft - maybe even call soft "archive")

### Implement
* KB nav in req search
* allow lowercase pubids in filter
* bug: freshdesk #86
* reorder tags when expansions recombined
* Add percentages to deriv expl
* tag in text should combine tag desc and whence-explanation
* pressing enter on reqtable in pubid should navigate to req
* when session expired, it should still always retry in case re-auth occurred in another tab
* if I have a flat list of unconnected nodes in ReqGraph, they appear backwards (ie. IV1, FR3, FR2, FR1)
* req detail: add prev/next buttons

* Problem: I've set MF-33 to #should but it's still deriving #must as well
  SI: Might need to add a field to disable DT for a req for a specific field (granularity=req)
  SI: Or maybe a rule that for MFs, manual always wins / disables DT (granularity=req-type)

* ReqDetail ImpGraph when coloured by tag, should display the tag in the hover text
  Actually, maybe just always include all tags in the hover text - that's probably better



Backlog (maybe-probably soon)
========================================================================================================================

* Make code block lineNumbers configurable via attribute
* Use WebSockets on home SPA to see projects and project stats update
* Add Change Count field to ReqTable/Detail (help find most volatile/unstable reqs)
* Bulk tag add/remove
* Advanced colour picker doesn't work
* ReqTable doesn't respect `max-width` unless `table-layout:fixed`

* Remove Scalaz

* Investigate changes required to support phone / tablet

* Metrics
  * Reduce biz metrics to 5m
  * Wall metrics updating every 10 min only

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



Backlog (eventually)
========================================================================================================================

### New major features
* Anonymous shares and read-only/presentation mode
* Tag/Implication Browser (aka Distribution manager/console)

### Functional changes
* ability to embed tweets & images
* Dual clipboard formats: one for ShipReq, one for everywhere else. Exclude ref titles from ShipReq version.
  (waiting for Chrome/FF to implement https://www.w3.org/TR/clipboard-apis; Currently only text/plain & image/png supported.)
* big long (SVG) code blocks take over the screen. Preview not visible anymore when editing.
* support all kinds of URL schemes in WebAddress (maybe - what considerations are necessary? security?)
* Bug: field editor doesn't fit (and no scroll) when window is left 50% of screen
* Markdown: Support nested lists
* Keyboard nav for ReqTablePage (not just the table) (?)
* Show past IDs in ReqTable
* Group req types in filter from ALL to ANY.
* Allow refs to custom text fields (e.g. [UC-1.detail])
* Add KB shortcut to move colums in ReqTable
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
* Add `isImplied` & `implies` (or similar) to FilterAst to select req with{,out} and implicators/implicatees

### Non-functional changes
* Extract string table so that project structure can be cached separately from project textual content?
* Store text changes as patches instead full replacements?
  * Lose the ability to quickly grep from DB.
  * Less storage/transport cost.
  * More CPU required to build a project from events.
* Remove ScalaCheck. Use Nyaya.
* Test env: Use different DBs for each module
* Switch to semantic-react
* Remove jQuery - Semantic UI blocking this
* Change ScalaCSS to generate Scala.JS without the runtime/JS-size overhead
* webapp-base{,-member} packages are shit. Reorg!
* webapp-base-member shares packages with webapp-base, plus most of that stuff should be under .project as well
* Extract webapp-base-member-test
* Add laws for webapp-server-logic and test in webapp-server
* Rename webapp-client-{home ⇒ member} now that its ambiguous in regards to the public pages
* Make webtamp hash filenames of urls in Semantic CSS (`icons.*`)
* Automate visual testing so changes to styling (mostly Semantic UI upgrades) can be verified.
  Use BackstopJS. Will require a read-only test user with a pre-configured project
* Add gzip to WebSocket comms
* Taskman metrics
