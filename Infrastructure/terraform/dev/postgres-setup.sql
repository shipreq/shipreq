CREATE EXTENSION hll;

REVOKE ALL ON SCHEMA public FROM PUBLIC;

--------------------------------------------------------------------------------

CREATE ROLE grafana WITH PASSWORD 'grafana' LOGIN;
GRANT grafana TO root;

CREATE DATABASE grafana OWNER grafana;
ALTER SCHEMA public OWNER TO grafana;

ALTER ROLE grafana SET search_path TO public;

--------------------------------------------------------------------------------

CREATE DATABASE shipreq;
\c shipreq

CREATE ROLE shipreq WITH PASSWORD 'dev' LOGIN;
GRANT shipreq TO root;

DROP SCHEMA public;
CREATE SCHEMA webapp AUTHORIZATION shipreq;
CREATE SCHEMA taskman AUTHORIZATION shipreq;

ALTER ROLE shipreq SET search_path TO webapp;
