#!/bin/bash

#cd "$(dirname "$0")"
#[ -z "$1" ] && echo "Usage: $0 <xxx>" && exit 1
#tmp=/tmp/$(date +%Y%m%d-%H%M%S)-$$
#xxx="$(cd "$(dirname "$0")/xxx" && pwd)"

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

  user=$db

  connect="psql -d $db"
  if [ -z "$use_sudo" ]; then
    connect="$connect -U $user"
  else
    connect="sudo -u postgres $sudo_opt $connect"
  fi
}

function lookup1 {
  [ "$#" -ne 1 -o -z "$1" ] && die "USAGE: $(basename "$0") [dev|test|prod]"
  lookup "$1"
}

dbdir=$(dirname "$0")
sqldir="$(dirname "$0")/../../sql"; [ -e "$sqldir" ] || die "SQL dir not found: $sqldir"
sudo_psql='sudo -u postgres psql'
sudo_pg_dump='sudo -u postgres pg_dump --encoding=UTF-8'

