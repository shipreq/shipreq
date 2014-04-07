#!/bin/bash

cd "$(dirname "$0")/.."
#tmp=/tmp/$(date +%Y%m%d-%H%M%S)-$$
#xxx="$(cd "$(dirname "$0")/xxx" && pwd)"

main="$1"; shift
[ -z "$main" ] && echo "Usage: $0 <main-class>" && exit 1

java_opt=()
conf=conf
lib=lib
[ ! -e "$lib" ] && echo "Lib dir not found: $(pwd)/$lib" && exit 1

libcp=$(
  ls -1 "$lib" \
  | perl -pe 's/(.*shipreq.*)/\t1\t\1/; s/(.*scalaz.*)/\t2\t\1/; s/(.*scala-lang.*)/\t3\t\1/;' | sort \
  | perl -pe 's/^\t.\t//g; s!^!'"$lib"'/!' | xargs | sed 's/ /:/g'
  )
cp="$conf:$libcp"

java_opt+=(-server -cp "$cp")

exec java $JAVA_OPTS "${java_opt[@]}" $main "$@"
