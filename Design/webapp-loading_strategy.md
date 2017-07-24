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
* PublicSpa.{Login,Register2} prefetchs assets for MembersHome

### TODO

* ClientSideProcInvoker must wait until async JS available
* add async JS lib support in SJS, initially just for Katex
* add loading pages for public & home?
* HomeSpa prefetch ProjectSpa (either in HTML or SJS)
* ProjectSpa prefetch [ww,viz]. (either in HTML or SJS)

