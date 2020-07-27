#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

f=versions.tf
rev="$(git rev-parse HEAD)"
echo "Upgrading to: $rev"

perl -pi -e 's/(shipreq += ").+/$1git-'$rev'"/' $f
wc="$(git diff $f | wc -l)"
[ "$wc" -ne 13 ] && git diff $f && echo && echo "Diff isn't what was expected." && exit 2

git add $f
git commit -m "Prod: Upgrade ShipReq" $f
git show
echo "Done."