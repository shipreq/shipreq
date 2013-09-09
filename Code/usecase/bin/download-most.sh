#!/bin/bash

v=1.10.2
echo "Downloading v$v of jQuery -- http://jquery.com/download/"
curl -s http://code.jquery.com/jquery-$v.min.js -o src/main/webapp/js/vendor/jquery.js
echo $?

echo "Downloading jQuery.autosize"
o=src/main/javascript/vendor/jquery-autosize.js
curl -s https://raw.github.com/jackmoore/autosize/master/jquery.autosize.js -o $o && dos2unix $o 2>/dev/null
echo $?

echo "Downloading jQuery.timeago"
curl -s http://timeago.yarp.com/jquery.timeago.js -o src/main/javascript/vendor/jquery-timeago.js
echo $?

v=2.0.2
echo "Downloading v$v of jQuery.serializeObject -- http://plugins.jquery.com/serializeObject/"
curl -s https://raw.github.com/hongymagic/jQuery.serializeObject/$v/jquery.serializeObject.js -o src/main/javascript/vendor/jquery-serializeObject.js
echo $?

echo "Downloading jQuery.liveQuery"
curl -s https://raw.github.com/hazzik/livequery/master/dist/jquery.livequery.min.js -o src/main/javascript/vendor/jquery-livequery.js
echo $?

v=2.3.0
echo "Downloading v$v of Knockout.js -- http://knockoutjs.com/"
curl -s http://knockoutjs.com/downloads/knockout-$v.js -o src/main/javascript/vendor/knockout.js
echo $?

v=2.4.1
echo "Downloading v$v of KO mapping -- https://github.com/SteveSanderson/knockout.mapping/tree/master/build/output"
curl -s https://raw.github.com/SteveSanderson/knockout.mapping/$v/build/output/knockout.mapping-latest.js -o src/main/javascript/vendor/knockout-mapping.js
echo $?

echo "Downloading Mousetrap"
curl -s https://raw.github.com/ccampbell/mousetrap/master/mousetrap.js -o src/main/javascript/vendor/mousetrap.js && \
curl -s https://raw.github.com/ccampbell/mousetrap/master/plugins/global-bind/mousetrap-global-bind.js -o src/main/javascript/vendor/mousetrap-global-bind.js
echo $?
sed -i -e '/@url/d' src/main/javascript/vendor/mousetrap.js

v=1.12.0
echo "Downloading v$v of QUnit -- http://qunitjs.com/"
curl -s http://code.jquery.com/qunit/qunit-$v.js -o vendor/qunit/qunit.js && \
curl -s http://code.jquery.com/qunit/qunit-$v.css -o vendor/qunit/qunit.css
echo $?

#echo "Downloading Viz.js"
#curl -s https://raw.github.com/mdaines/viz.js/master/viz.js -o src/main/webapp/js/vendor/viz.js
#echo $?

