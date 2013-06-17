#!/bin/bash

v=2.1.2
echo "Downloading v$v of normalize.css -- http://necolas.github.io/normalize.css/"
curl -s http://necolas.github.io/normalize.css/$v/normalize.css -o src/main/webapp/assets/normalize.css
echo $?

v=1.10.1
echo "Downloading v$v of jQuery -- http://jquery.com/download/"
curl -s http://code.jquery.com/jquery-$v.min.js -o src/main/webapp/js/vendor/jquery.js
echo $?

v=2.2.1
echo "Downloading v$v of knockout.js -- http://knockoutjs.com/"
curl -s http://knockoutjs.com/downloads/knockout-$v.js -o src/main/javascript/vendor/knockout.js
echo $?

v=2.4.1
echo "Downloading v$v of ko mapping -- https://github.com/SteveSanderson/knockout.mapping/tree/master/build/output"
curl -s https://raw.github.com/SteveSanderson/knockout.mapping/$v/build/output/knockout.mapping-latest.js -o src/main/javascript/vendor/knockout-mapping.js
echo $?

v=2.0.2
echo "Downloading v$v of jquery.serializeObject.js -- http://plugins.jquery.com/serializeObject/"
curl -s https://raw.github.com/hongymagic/jQuery.serializeObject/$v/jquery.serializeObject.js -o src/main/javascript/vendor/jquery-serializeObject.js
echo $?
