CREATE OR REPLACE FUNCTION project_on_update() RETURNS TRIGGER AS $$
BEGIN

  IF NEW.events_total != OLD.events_total THEN
    -- Event added
    PERFORM project_access_per_hour_add(NEW.id, NEW.updated_at, TRUE);

  ELSIF NEW.name != OLD.name THEN
    -- Project name has changed, don't count as a write because this always follows an update to events_total
    NULL;

  ELSE
    -- Project accessed
    PERFORM project_access_per_hour_add(NEW.id, NEW.accessed_at, FALSE);

  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER project_on_update ON project;
CREATE TRIGGER project_on_update AFTER UPDATE ON project FOR EACH ROW EXECUTE PROCEDURE project_on_update();
