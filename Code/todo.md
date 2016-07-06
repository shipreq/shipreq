* Group req types in filter from ALL to ANY.
* Say "no implications" in imp graph
* Remove unused styles

Integrate async into ContentEditorFeature rendering
  * Redo ReqTable rowlocking async
  * Remove rendering from AAF
  * Each editor in ContentEditorFeature will need to handle async rendering now.
    It used to expect usage: A renderOr E renderOr V
    New use is: E renderOr V
    Remove: commitK, commitAbortK
    Remove or revise: renderStatic, renderDynamic
    Don't forget UseCaseStepEditor!

* Determine UI for:
  * ReqDetail load failure
  * Deletion screen
  * Cfg Fields
  * Cfg Issues
  * Cfg ReqTypes
  * Cfg Tags

* Implement UI for:
  * ReqDetail
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

