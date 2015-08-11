CREATE TABLE event (
  project_id   BIGINT      NOT NULL REFERENCES project,
  seq          INTEGER     NOT NULL CHECK (seq >= 0),
  type_id      SMALLINT    NOT NULL CHECK (type_id >= 0),
  data_id_type SMALLINT    NOT NULL,
  data_id      INTEGER     NULL,
  data         JSON        NULL,
  hash_scheme  SMALLINT    NOT NULL CHECK (hash_scheme > 0),
  hash         INTEGER     NOT NULL,
  created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(project_id, seq));

COMMENT ON TABLE  event              IS 'An append-only sequence of modification events applied to an empty project.';
COMMENT ON COLUMN event.seq          IS 'Order in which events for a Project are to be applied.';
COMMENT ON COLUMN event.type_id      IS 'A number representing the type of event. Defined and maintained in Scala only.';
COMMENT ON COLUMN event.data_id_type IS 'The type of data_id as IDs can be polymorphic.';
COMMENT ON COLUMN event.data_id      IS 'Meaning depends on event type. Usually the local ID of an object in the Project.';
COMMENT ON COLUMN event.data         IS 'Meaning and format depends on event type.';
