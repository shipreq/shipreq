CREATE TABLE project_access_per_hour (
  date     DATE     NOT NULL,
  hour     SMALLINT NOT NULL CHECK(hour >= 0 AND hour <= 23),
  write    BOOLEAN  NOT NULL,
  projects HLL      NOT NULL,
  total    INTEGER  NOT NULL CHECK(total > 0),
  PRIMARY KEY(date, hour, write)
);

------------------------------------------------------------------------------------------------------------------------

CREATE FUNCTION project_access_per_hour_add(
  arg_id    project.id%TYPE,
  arg_time  TIMESTAMPTZ,
  arg_write BOOLEAN
) RETURNS void AS $$
DECLARE
  time_utc TIMESTAMP;
  tgt_ctid project_access_per_hour.ctid%TYPE;
  tgt_date project_access_per_hour.date%TYPE;
  tgt_hour project_access_per_hour.hour%TYPE;
BEGIN

  SELECT (arg_time at time zone 'UTC')::timestamp
    INTO time_utc;

  SELECT time_utc::date, EXTRACT(hours from time_utc)
    INTO tgt_date, tgt_hour;

  SELECT ctid
    INTO tgt_ctid
    FROM project_access_per_hour
   WHERE date = tgt_date AND hour = tgt_hour AND write = arg_write;

  IF NOT FOUND THEN
    -- Insert new row
    INSERT INTO project_access_per_hour(date, hour, write, projects, total)
      VALUES(tgt_date, tgt_hour, arg_write, hll_add(hll_empty(), hll_hash_bigint(arg_id)), 1);

  ELSE
    -- Update existing row
    UPDATE project_access_per_hour
       SET total = total + 1,
           projects = hll_add(projects, hll_hash_bigint(arg_id))
     WHERE ctid = tgt_ctid;

  END IF;

END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------------------------------------------------------------------
-- project triggers

CREATE FUNCTION project_on_insert() RETURNS TRIGGER AS $$
BEGIN
  PERFORM project_access_per_hour_add(NEW.id, NEW.updated_at, TRUE);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION project_on_update() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.events_total = OLD.events_total THEN
    PERFORM project_access_per_hour_add(NEW.id, NEW.accessed_at, FALSE);
  ELSE
    PERFORM project_access_per_hour_add(NEW.id, NEW.updated_at, TRUE);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER project_on_insert AFTER INSERT ON project FOR EACH ROW EXECUTE PROCEDURE project_on_insert();
CREATE TRIGGER project_on_update AFTER UPDATE ON project FOR EACH ROW EXECUTE PROCEDURE project_on_update();
