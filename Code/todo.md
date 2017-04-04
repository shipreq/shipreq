Frontend
  * Upgrade deps
  * Switch to semantic-react
  * Find alternative for jquery-textcomplete
  * Remove jQuery
  * Improve load times, maybe defer stuff like KaTeX? Or maybe on-demand loading?

Misc
  * Group req types in filter from ALL to ANY.
  * Say "no implications" in imp graph
  * ProjectSpaLoader has wrong path in dev
  * Support running webapp from SBT with dev DB from Docker

Integrate async into ContentEditorFeature rendering
  * Remove EditValidationFeature
  * Redo ReqTable rowlocking async
  * Remove rendering from AAF

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
  * Remove specs2. Use scalatest/μtest.
  * Remove ScalaCheck. Use Nyaya.
  * Use fast boopickle codecs for webworkers: https://github.com/ochrons/boopickle#codecs
  * Test env: Use different DBs for each module
  * Remove unused styles

