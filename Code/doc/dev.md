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

* For Scala.JS unit testing:
  1. Install Node
  2. Run `npm install` in the `Code` directory

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

* `cd frontend`
* Edit `package.json`
* Run `./build-parallel`
* Test and commit


Development
===========

1. Build the docker base image: `docker/base-image/build`
2. Build docker images by running `docker` from SBT.
3. Start up the dev environment by `bin/env dev up`.

### Testing

1. Start up the test environment (once): `bin/env test up -d`
2. From SBT: `test`
