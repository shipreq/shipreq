Development Environment Setup
=============================

### System

* Install SBT.
* Install docker-compose.

### Dev account for web front-end

1. From SBT: `up`
2. Open http://localhost:8080/register
3. Create an account with
    Email:    japgolly@gmail.com
    Username: devuser
    Password: dev123123
4. Auto login at http://localhost:8080/x (dev-mode only)


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

`bin/release`
Deployment TODO.
