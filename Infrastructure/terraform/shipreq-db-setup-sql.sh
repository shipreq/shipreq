#!/bin/bash

set -euo pipefail

function read_var {
  v="$(cat *.tf | fgrep " $2 " | perl -pe 's/^.+= *"(.+?)".*/$1/')"
  [ -z "$v" ] && echo "Unable to read value for $2" && exit 2
  eval "$1=$v"
}

read_var "GRAFANA_PASSWORD" "grafana_db_password"
read_var "SHIPREQ_PASSWORD" "shipreq_db_password"
read_var "POSTGRES_EXPORTER_PASSWORD" "postgres_exporter_db_password"

cat <<EOB
REVOKE ALL ON SCHEMA public FROM PUBLIC;

--------------------------------------------------------------------------------

CREATE ROLE grafana WITH PASSWORD '${GRAFANA_PASSWORD}' LOGIN;
GRANT grafana TO root;

CREATE DATABASE grafana OWNER grafana;
ALTER SCHEMA public OWNER TO grafana;

ALTER ROLE grafana SET search_path TO public;

--------------------------------------------------------------------------------

CREATE DATABASE shipreq;
\c shipreq

CREATE USER shipreq WITH PASSWORD '${SHIPREQ_PASSWORD}';
GRANT shipreq TO root;

DROP SCHEMA public;
CREATE SCHEMA webapp AUTHORIZATION shipreq;
CREATE SCHEMA taskman AUTHORIZATION shipreq;

ALTER ROLE shipreq SET search_path TO webapp;

CREATE EXTENSION hll SCHEMA webapp;

CREATE ROLE shipreq_webapp_ro;
GRANT USAGE ON SCHEMA webapp TO shipreq_webapp_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA webapp TO shipreq_webapp_ro;
ALTER DEFAULT PRIVILEGES IN SCHEMA webapp GRANT SELECT ON TABLES TO shipreq_webapp_ro;

CREATE USER postgres_exporter WITH PASSWORD '${POSTGRES_EXPORTER_PASSWORD}';
GRANT shipreq_webapp_ro TO postgres_exporter;
ALTER ROLE postgres_exporter SET search_path TO webapp;

EOB