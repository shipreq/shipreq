ALTER TABLE event_hash ALTER COLUMN hash DROP NOT NULL;

COMMENT ON COLUMN event_hash.hash IS 'Hash value, or NULL to disable integrity checking.';
