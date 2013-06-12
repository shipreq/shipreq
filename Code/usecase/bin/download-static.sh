#!/bin/bash

v=2.1.2
echo "Downloading v$v of normalize.css -- http://necolas.github.io/normalize.css/"
curl -s http://necolas.github.io/normalize.css/$v/normalize.css -o src/main/webapp/assets/normalize.css

v=2.2.1
echo "Downloading v$v of knockout.js -- http://knockoutjs.com/"
curl -s http://knockoutjs.com/downloads/knockout-$v.js -o src/main/webapp/assets/knockout.js

