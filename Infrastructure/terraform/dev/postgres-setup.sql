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

CREATE USER shipreq WITH PASSWORD 'dev';
GRANT shipreq TO root;

DROP SCHEMA public;
CREATE SCHEMA webapp AUTHORIZATION shipreq;
CREATE SCHEMA taskman AUTHORIZATION shipreq;

ALTER ROLE shipreq SET search_path TO webapp;

CREATE ROLE shipreq_webapp_ro;
GRANT USAGE ON SCHEMA webapp TO shipreq_webapp_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA webapp TO shipreq_webapp_ro;
ALTER DEFAULT PRIVILEGES IN SCHEMA webapp GRANT SELECT ON TABLES TO shipreq_webapp_ro;

CREATE USER postgres_exporter WITH PASSWORD 'dev-metrics';
GRANT shipreq_webapp_ro TO postgres_exporter;
ALTER ROLE postgres_exporter SET search_path TO webapp;
