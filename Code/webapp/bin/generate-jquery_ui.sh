#!/bin/bash

tgt=src/main/javascript/vendor/jquery-ui.js
> $tgt

echo "Adding:"
for f in \
  core \
  effect \
  effect-drop \
  effect-fade \
  effect-highlight \
  effect-slide \
; do
  echo "  $f"
  cat vendor/jquery-ui/minified/jquery.ui.$f.min.js >> $tgt
done

echo "Done."
ls -l $tgt
