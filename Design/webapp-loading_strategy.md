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

* extract resources from Semantic CSS and add preload directives
* ClientSideProcInvoker must wait until async JS available
* add async JS lib support in SJS, initially just for Katex
* add loading pages for public & home?
* public:login prefetch [JQuery,Semantic,Member,HomeSpa] JS (dynamic in SJS)
* HomeSpa prefetch ProjectSpa (either in HTML or SJS)
* ProjectSpa prefetch [ww,viz]. (either in HTML or SJS)
* add to AssetManifest: [JQuery,Semantic,Member] JS
* add to AssetManifest: Katex JS & CSS
* add to AssetManifest: bundle info?

