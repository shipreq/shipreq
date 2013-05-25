Getting Started
===============

* Generate Eclipse files
    sbt eclipse

* Generate IDEA files
    sbt gen-idea

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
    sudo -u postgres psql < db-create-dev.sql
    sudo -u postgres psql < db-create-test.sql

Running and Developing
======================

* Start WebServer
    sbt
    container:start

* Stop WebServer
    container:stop

* Continuous Redeploy
    sbt
    ~; container:start; container:reload /

* Stylesheet changes
  1. Edit SASS in src/main/sass
  2. Run `bin/generate-css.sh`
  3. Optionally spot-check new CSS in src/main/webapp/assets
