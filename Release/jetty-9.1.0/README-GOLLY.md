Usage
=====

### Start
* bin/jetty.sh start
* java -jar start.jar

### Stop
* bin/jetty.sh stop



Config
======

* Type `java -jar start.jar --list-config` and it will list at the bottom, all the enabled args.

* Config comes from:
    * start.ini
    * etc/jetty.conf (only applied when launched via bin/jetty.sh)

* As is indicated by above, config is also loaded from:
  * etc/jetty.xml
  * etc/jetty-http.xml
  * etc/jetty-deploy.xml
  * start.d/*.ini

* Additional properties files are also added to the classpath by dropping them in resources/
  This is where Prod properties, DB connection and logging config will be found.


Requirements
============
* log/
* work/

