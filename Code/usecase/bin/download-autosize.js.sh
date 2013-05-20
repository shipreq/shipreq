#!/bin/bash

o=src/main/webapp/assets/jquery-autosize.js
curl https://raw.github.com/jackmoore/autosize/master/jquery.autosize.js -o $o \
  && dos2unix $o
