#!/bin/bash

#cd "$(dirname "$0")"
#[ -z "$1" ] && echo "Usage: $0 <xxx>" && exit 1
#tmp=/tmp/$(date +%Y%m%d-%H%M%S)-$$
#xxx="$(cd "$(dirname "$0")/xxx" && pwd)"

dbdir="$(dirname "$0")"

function die {
  echo -e "$*" >&2
  exit 1
}

function lookup {
  db=
  use_sudo=
  sudo_opt=
  err=
  case "$1" in
    dev)
      db=shipreq_dev
      ;;
    test)
      db=shipreq_test
      ;;
    prod)
      db=shipreq_prod
      use_sudo=1
      sudo_opt='-k'
      ;;
    *)
      die "Unknown db: '$1'.\nExpected [dev|test|prod]."
  esac

  connect="psql -d $db"
  if [ -z "$use_sudo" ]; then
    cmd="$cmd -U $db"
  else
    cmd="sudo -u postgres $sudo_opt $cmd"
  fi
}

function lookup1 {
  [ "$#" -ne 1 -o -z "$1" ] && die "USAGE: $(basename "$0") [dev|test|prod]"
  lookup "$1"
}

sudo_psql='sudo -u postgres psql'
sudo_pg_dump='sudo -u postgres pg_dump --encoding=UTF-8'

