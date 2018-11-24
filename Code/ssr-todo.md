* avoid hardcoded ssr fn names in annotations & Exprs
* warmup on idle (not just ssr)

* update bin/stats
* update modules doc

* Router in SSR tries to pushState/replaceState
  - let it crash and hide from logs?
  - or, have the router be SSR aware and....?

* need to render after hydration...
  - if window.location.hash is set (because server doesn't consider)
  - if localStorage isn't available and I decide to disable the remember me button

* webapp-ssr tests
