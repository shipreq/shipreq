* Generate Eclipse files
    sbt eclipse

* Generate IDEA files
    sbt gen-idea

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
