* Group req types in filter from ALL to ANY.
* Say "no implications" in imp graph
* Remove unused styles

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
  * split name into first/last
  * plan UI
  * impl UI
  * test

* Devops & Deployment
  * Webapp docker
    * integrate `war-force_https`
    * HTTPS: Use real keystore
    * use quickstart
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
  * Upgrade Scala to 2.12 (problems: ScalaCheck, Specs2, scalajs-java-time)
  * Use fast boopickle codecs for webworkers: https://github.com/ochrons/boopickle#codecs
  * Test env: Use different DBs for each module

