/*
DROP TABLE    IF EXISTS usr_login_log;
DROP FUNCTION IF EXISTS usr_login_stats_update();
*/

-- select * from usr;
--ALTER TABLE usr DROP COLUMN last_login_ip;

CREATE TABLE usr_login_log (
  time       TIMESTAMPTZ  NOT NULL DEFAULT NOW()
  ,usr_id    BIGINT       NOT NULL -- Not an FK
  ,ip        VARCHAR      NULL
);

------------------------------------------------------------------------------------------------------------------------
-- Trigger: Update on insert to `usr_login_log`.

CREATE FUNCTION usr_add_login(
  arg_id   usr.id           %TYPE,
  arg_time usr.last_login_at%TYPE,
  arg_ip   VARCHAR
) RETURNS void AS $$
BEGIN
  UPDATE usr
  SET    login_count   = login_count + 1,
         last_login_at = arg_time,
         last_login_ip = coalesce(arg_ip,'?')
  WHERE  id = arg_id;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION usr_login_log_on_insert() RETURNS TRIGGER AS $$
BEGIN
  PERFORM usr_add_login(NEW.usr_id, NEW.time, NEW.ip);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER usr_login_log_on_insert AFTER INSERT ON usr_login_log FOR EACH ROW EXECUTE PROCEDURE usr_login_log_on_insert();
