Goals
=====
* Build
  * Build docker images for taskman & webapp
  * Publish docker images (?)
* Test
  * Setup env via docker
* Dev run
  * Setup env via docker
  * Run apps via SBT (which points to docker env)
* Local run
  * docker compose starts up everything
* Deploy
  * TODO: ansible & docker?

Concerns
========
* admin tasks / one-off app runs (and implications around config)
* docker image repo
* jetty upgrade procedure

Strategies
==========
* Config
  * [-] Use a single props file name. No more mode.host.blah.props.
  * [ ] Non-test: remove from src/main/resources
  * [-] Test: put in src/test/resources
  * [-] Test: should reference docker-compose-test
* Logging
  * [ ] Non-test: to stdout
  * [-] Test: to file in /tmp

Tasks
=====
* Taskman ⇒ Docker
  * Ensure enough build info in jar (and dockerfile)
  * Config
  * Logging
  * Copy scripts and resources
  * Build docker

* ShipReq ⇒ Docker
  * Ensure enough build info in jar (and dockerfile)
  * Config
  * Logging
  * Copy scripts and resources
  * Port: `war-compress_static_resources`
  * Port: `war-force_https`
  * Build docker

* Dev env
  * Create docker compose setup for external resources
  * Configure run in SBT to provide settings to use ↑

* Local env
  * Create docker compose setup for everything
  * Make DB persisent

* Update release scripts in Code/bin/

* Update all READMEs.
