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
-- Trigger: Update `usr` upon insert to `usr_login_log`.

CREATE OR REPLACE FUNCTION usr_login_stats_update() RETURNS TRIGGER AS $$
BEGIN

    update usr
    set login_count   = login_count + 1
       ,last_login_at = NEW.time
       ,last_login_ip = coalesce(NEW.ip,'?')
    where id = NEW.usr_id;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER usr_login_stats_update AFTER INSERT ON usr_login_log FOR EACH ROW EXECUTE PROCEDURE usr_login_stats_update();
