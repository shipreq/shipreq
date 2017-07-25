Loading Strategy for Web Pages
==============================

* Have static loading pages (even for public?)
* Preload non-critical non-JS resources
* Preload/defer/loadjs as much JS as possible
* Prefetch resources for the next page
* Preload resources defined in CSS
* Katex is rarely used, can it be made lazy/on-demand?
  Could make it callback based in SJS: easy.
* Maybe add link headers to HTTP response in Dispatcher to prefetch resources (?)

(See also ../Misc/http-loading.md)


Specifically:
=============

* Extract Google Fonts @import from semantic.css and place in HTML for early discovery and fetching
* PublicSpa.{Login,Register2} prefetches assets for MemberSpa
* MemberSpa HTML prefetches ProjectSpa
* ProjectSpa HTML prefetches WebWorker & GraphViz
  NOTE: This is disabled because it could cause double-loads.
* ProjectSpa uses loadjs to render loader fast.
* There is a LoadJs class in webapp-server.
  ClientSideProcInvoker can accept a LoadJs.Bundle.

### TODO

* add async JS lib support in SJS, initially just for [Katex which needs ReactDOMServer too]
* add loading pages for public & home?

Notes
=====

* Async/defer tags don't work in place of loadjs
  Using defer + lift-provided script has unreliable execution order
