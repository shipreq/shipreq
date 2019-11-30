CREATE SEQUENCE project_id_seq;

CREATE TABLE project (
  id             BIGINT      PRIMARY KEY DEFAULT nextval('project_id_seq'),
  usr_id         BIGINT      NOT NULL REFERENCES usr,
  name           TEXT        NOT NULL,
  events_init    INTEGER     NOT NULL CHECK (events_init >= 0),
  events_total   INTEGER     NOT NULL CHECK (events_total >= 0),
  reqs_live      INTEGER     NOT NULL CHECK (reqs_live >= 0),
  reqs_total     INTEGER     NOT NULL CHECK (reqs_total >= 0),
  created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  accessed_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (events_init <= events_total),
  CHECK (reqs_live <= reqs_total),
  CHECK (created_at <= accessed_at),
  CHECK (created_at <= updated_at)
);

CREATE INDEX ON project (usr_id);
