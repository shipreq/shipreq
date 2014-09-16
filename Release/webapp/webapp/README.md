Usage
=====

* Run in foreground
    bin/start

* Start daemon
    bin/jetty start

* Stop daemon
    bin/jetty stop


Config
======

* Inspection
    bin/start --list-config
    bin/start --list-modules
    bin/jetty check

* Jetty config:
    * start.ini
    * start.d/*.ini
    * etc/jetty.conf [daemon-mode only]
    * XMLs displayed in `bin/jetty --list-config`

* App config:
    * resources/*
    * start.d/shipreq.ini

