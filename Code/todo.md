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

* Deployment
  * Upgrade to latest Jetty
  * HTTP/2
  * Security settings (TLS) and test
  * Compression and caching
  * New amazon accounts

* Do more with errors.
    ClientData.init
    ClientData.applyEvents

