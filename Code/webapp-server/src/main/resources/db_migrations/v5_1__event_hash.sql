CREATE TABLE event_hash (
  project_id   BIGINT      NOT NULL,
  seq          INTEGER     NOT NULL,
  scope        "char"      NOT NULL,
  logic_ver    "char"      NOT NULL,
  hash_scheme  "char"      NOT NULL,
  hash         INTEGER     NULL,
  PRIMARY KEY (project_id, seq, scope, logic_ver, hash_scheme),
  FOREIGN KEY (project_id, seq) REFERENCES event (project_id, seq));

COMMENT ON TABLE  event_hash             IS 'Data integrity in the form of hashes describing expected state after event application.';
COMMENT ON COLUMN event_hash.scope       IS 'Indicates the Project subset covered by the hash.';
COMMENT ON COLUMN event_hash.logic_ver   IS 'The version of event application logic used before calculating the hash.';
COMMENT ON COLUMN event_hash.hash_scheme IS 'The version of hash calculation logic used to generate the hash.';
COMMENT ON COLUMN event_hash.hash        IS 'Hash value, or NULL to disable integrity checking.';

