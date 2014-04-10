Usage
=====

* Run in foreground
    ./start

* Start daemon
    ./jetty start

* Stop daemon
    ./jetty stop


Config
======

* Inspection
    ./start --list-config
    ./start --list-modules
    ./jetty check

* Jetty config:
    * start.ini
    * start.d/*.ini
    * etc/jetty.conf [daemon-mode only]
    * XMLs displayed in `./jetty --list-config`

* App config:
    * resources/*
    * start.d/shipreq.ini

