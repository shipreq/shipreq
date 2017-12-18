Development Environment Setup
=============================

### System

* Install:
  * SBT
  * Docker & docker-compose
  * parallel
  * pigz
  * brotli
  * unzip

### Dev account for web front-end

1. From SBT: `up`
2. Open http://localhost:8080/register
3. Create an account
4. Add the following env vars to your shell config:
    * `SHIPREQ_DEV_USER` - Account username or email address
    * `SHIPREQ_DEV_PASS` - Account password
    * `SHIPREQ_DEV_GOTO` - (Optional) A relative URL to goto after auto-login
5. Auto-login with the above credentials at http://localhost:8080/x (dev-mode only)


If Taskman runs into trouble, tickets should be raised at http://yoarmum.freshdesk.com/

You can also inspect the message queue via:
1. `bin/db/connect dev`
2. `set search_path TO taskman`
3. `select * from msgq;`

Also useful: `update msgq set effective_from = now();`



Updating Dependencies
=====================

### Scala

Dependencies and their versions are declared in `project/Dependencies.scala`.
Dependencies are assigned to modules in `project/*Build.scala`.
The version of SBT itself is configured in `project/build.properties`.
The version of SBT plugins are in `project/plugins.sbt`.

### Webapp resources

`cd res`

* Updating: TODO

* Adding/Removing: TODO



Development
===========

1. Build docker images by running `docker` from SBT.
2. Start up the dev environment by `bin/env dev up`.

### Testing

1. Start up the test environment (once): `bin/env test up -d`
2. From SBT: `test`



Releasing
=========

TODO

Modules
=======

base-db   - Reusable utility code for DB access.
base-ops  - Reusable utility code for dev-ops purposes.
base-test - Reusable utility code for testing.
base-util - Reusable utility code, general purpose.

taskman-api           - Taskman API (real: side-effects)
taskman-api-logic     - Taskman API (pure: types and logic)
taskman-server        - Taskman server (real: side-effects). Exported via Docker.
taskman-server-logic  - Taskman server (pure: types and logic)
taskman-server-schema - Taskman DB schema

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

benchmark - Various benchmarks.
utils     - Utilities for devs to run manually. Not really used anymore.

Ops
===

### Manual registration

You can trigger PublicSpa.Register1 from the CLI.
This is intended to be used by admin when public registration is off to issue registration tokens on others' behalves.

```
curl -v https://shipreq.com/api/register1 -X POST -d "secret=Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid&email=japgolly@gmail.com"
```

The secret is currently just a hardcoded value.


Security
========

Install testssl.sh
```sh
pacman -S testssl.sh
```

```sh
bin/env dev up webapp

testssl shipreq.com:14443
```

Ignore LUCKY13 (CVE-2013-0169), it's fixed in Java 8.
https://www.oracle.com/technetwork/topics/security/javacpufeb2013update-1905892.html
