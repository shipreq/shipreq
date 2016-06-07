* Group req types in filter from ALL to ANY.
* Fix GraphViz
  * No exceptions crashes due to ;;
  * Incorrect flow in graph when no flow at all and
    - 1.0{,.1,.2}
    - 1.1{,.1,.2}
    - 1.2{,.1}
    - 1.E.1
* Say "no implications" in imp graph
* Show dead IDs on ReqDetail screen.
* Show dead IDs on ReqTable screen.
* Change project SPA URLs to use #

* Determine UI for:
  * General
    * rich text editor help
    * req type selector
  * ReqTable
    * selected rows
  * Use Case detail
    * Steps and buttons
    * Dead steps?
  * Deletion screen
  * Cfg Fields
  * Cfg Issues
  * Cfg ReqTypes
  * Cfg Tags

* Implement UI & UX changes.

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

* Prop-test ProjectCatalogue SQL.

