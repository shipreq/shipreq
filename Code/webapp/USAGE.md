Getting Started
===============

### Libraries

* Building local fork of Lift.
    Check out git@github.com:japgolly/framework.git or git://github.com/japgolly/framework.git somewhere.
    ./golly

### Client-Side Tooling

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

* Setting up local DBs
    sudo -u postgres psql < db/create-dev.sql
    sudo -u postgres psql < db/create-test.sql

### IDE

* Generate Eclipse files
    sbt eclipse

* Generate IDEA files
    sbt gen-idea


Running and Developing
======================

### Client-Side

* 3rd-Party Dependency Updates
  1. bower list
  2. bower update
  3. grunt

* New 3rd-Party Dependencies
  1. Edit bower.json
  2. bower install
  3. Edit Gruntfile.js
  4. grunt

* JS & CSS Development

Simply run `grunt watch` and edit away.
To build manually, run `grunt js` or `grunt css`.


### Server-Side

* Start WebServer
    sbt
    container:start

* Stop WebServer
    container:stop

* Continuous Redeploy
    sbt
    ~; container:start; container:reload /

* Build WAR file
    sbt package


Testing
=======

* Running tests
    sbt test

* Running JS tests
    Load `src/test/javascript/tests.html` in a browser.


Making a Release
================

1. ./release
2. cd ../../Release
3. ./install-latest_war
4. [OPTIONAL] cd webapp && ./jetty and test locally.
5. ./deploy-war <ip>

