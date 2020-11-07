-- Disambiguate project events from global events
ALTER TABLE event RENAME TO project_event;

-- Global events make usrh_name redundant
DROP TRIGGER usrd_name_archive_trigger ON usrd;
DROP FUNCTION archive_usrd_name();
DROP TABLE usrh_name;

CREATE SEQUENCE global_event_seq START WITH 1;
CREATE TABLE global_event (
  id          BIGINT      PRIMARY KEY DEFAULT NEXTVAL('global_event_seq'),
  created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
  type        SMALLINT    NOT NULL CHECK (type > 0),
  data        JSONB       NOT NULL,
  ip          VARCHAR     NULL,
  usr_id      BIGINT      NULL REFERENCES usr
);
