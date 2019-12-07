/*
DROP TRIGGER usrd_name_archive_trigger ON usrd;
DROP FUNCTION archive_usrd_name();
DROP VIEW taskman_users_v01;
DROP TABLE usrh_name;
DROP TABLE usrd;
*/

CREATE TABLE usrd (
  usr_id      BIGINT  PRIMARY KEY REFERENCES usr
  ,name       VARCHAR NOT NULL
  ,newsletter BOOLEAN NOT NULL
);

-------------------------------------------------------------------------------
-- Record name history

CREATE TABLE usrh_name (
  usr_id      BIGINT      NOT NULL REFERENCES usr
  ,name       VARCHAR     NOT NULL
  ,updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE FUNCTION archive_usrd_name() RETURNS TRIGGER AS $$
BEGIN
  insert into usrh_name(usr_id, name) values(OLD.usr_id, OLD.name);
  return NEW;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER usrd_name_archive_trigger
  BEFORE UPDATE OF name ON usrd
  FOR EACH ROW
  WHEN (OLD.name != NEW.name)
  EXECUTE PROCEDURE archive_usrd_name();

-------------------------------------------------------------------------------
-- Taskman interface

CREATE VIEW taskman_users_v01 AS
select id, username, email, name, newsletter, confirmed_at "joined"
from usr
left join usrd on id = usr_id
where username is not null;
