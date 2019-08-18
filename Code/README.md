ShipReq code
============

Look in the `./doc` directory for dev documentation.


## Modules

base-db                 - Reusable utility code for DB access.
base-ops                - Reusable utility code for dev-ops purposes.
base-test               - Reusable utility code for testing.
base-util               - Reusable utility code, general purpose.

taskman-api             - Taskman API (real: side-effects)
taskman-api-logic       - Taskman API (pure: types and logic)
taskman-server          - Taskman server (real: side-effects). Exported via Docker.
taskman-server-logic    - Taskman server (pure: types and logic)
taskman-server-schema   - Taskman DB schema

webapp-base             - Shared code between client and/or server modules for public pages.
webapp-base-member      - Shared code between client and/or server modules for member (i.e. logged-in users) pages.
webapp-base-test        - Shared code for testing client and/or server modules. Tests for webapp-base.
webapp-client-public    - SPA for the public pages before a user logs in.
webapp-client-home      - SPA for when a user logs in. Project CRUDL, view/edit account, etc.
webapp-client-project   - SPA for working with a Project.
webapp-client-ww        - WebWorkers for big background tasks like graphviz→SVG generation.
webapp-client-ww-api    - API to above.
webapp-gen              - Provides a hardcoded loading screen for webapp-client-home to serve until the read JS loads and replaces it.
webapp-macro            - Macros used in webapp-base.
webapp-server-logic     - Webapp server logic. Agnostic to web-server library. Compiled to JS for use in frontend tests.
webapp-server           - Webapp server. Exported via Docker.
webapp-ssr              - Allows React Server-Side Rendering.

benchmark               - Various benchmarks.
utils                   - Utilities for devs to run manually. Not really used anymore.
