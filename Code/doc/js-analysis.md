Some of the JS assets are pretty big.
Here's how you can find out why, and what's inside...


Seeing
======

* For the `member-lib-bundle` stats, open `frontend/analysis/prod.html`
* For the SPAs, open `analysis/spa-*.html`


Updating
========

#### Frontend deps

Frontend deps are updated automatically by `frontend/build`

#### Scala.JS

1. `npm -g install source-map-explorer` or
   `npm -g update source-map-explorer`

2. `bin/analyse`
