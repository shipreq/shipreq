* Generate Eclipse files
    sbt eclipse

* Start WebServer
    sbt container:start

* Stop WebServer
    sbt container:stop

* Continuous Redeploy
    sbt
    ~; container:start; container:reload /

* Stylesheet changes
  1. Edit SASS in src/main/stylesheets
  2. Run `./generate-css.sh`
  3. Optionally spot-check new CSS in src/main/webapp/assets
