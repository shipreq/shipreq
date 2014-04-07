Development Environment Setup
=============================

### Scala

* Install SBT.

* Building local fork of Lift.
    Check out git@github.com:japgolly/framework.git or git://github.com/japgolly/framework.git somewhere.
    ./golly

### Web Front-End

First, be in webapp/

* yaourt -S --needed --noconfirm nodejs-{bower,grunt-cli}
* npm install
* bower list
* grunt

### Database

* Install PostgreSQL
    sudo pacman -Sy --needed postgresql

    sudo mkdir /var/lib/postgres/data
    sudo chown -c -R postgres:postgres /var/lib/postgres
    sudo -u postgres initdb -D /var/lib/postgres/data

    sudo systemctl enable postgresql
    sudo systemctl start postgresql

    sudo -u postgres cp /var/lib/postgres/data/pg_hba.conf{,.orig}
    sudo -u postgres vim /var/lib/postgres/data/pg_hba.conf
        Add a line: local all postgres ident
        Change trust to md5 where appropriate
    sudo systemctl restart postgresql

* Create local DBs
    db/create dev
    db/create test

* Initialise
  [TODO](improve this)
  1. Setup the taskman schema.
  2. Start the webapp. (Will store cfg values required by taskman.)


Updating Dependencies
=====================

### Scala

Dependencies and their versions are declared in `project/Dependencies.scala`.
Dependencies are assigned to modules in `project/Build.scala`, see `def deps` and `dependsOn`.
The version of SBT itself is configured in `project/build.properties`.

### Web Front-End

First, be in webapp/

* Updating
  1. bower list
  2. bower update
  3. grunt

* Adding/Removing
  1. Edit bower.json
  2. bower install
  3. Edit Gruntfile.js
  4. grunt


Development
===========

### Running

Taskman:
    sbt
    taskman-server-impl/runMain shipreq.taskman.server.app.Server

Webapp:
    sbt
    webapp/container:start


### Continuous Building

Scala:
    sbt
    project taskman # Example
    ~ct

Webapp front-end:
    grunt watch

Webapp back-end:
    sbt
    project webapp
    ~; container:stop; container:start


### Testing

Scala: `sbt test`

Javascript:
  1. Either load `webapp/src/test/javascript/tests.html` in a browser.
  2. Or `cd webapp && grunt test`.


Releasing
=========
[TODO](Release instructions are out-of-date)

1. ./release
2. cd ../../Release
3. ./install-latest_war
4. [OPTIONAL] cd webapp && ./jetty and test locally.
5. ./deploy-war <ip>

