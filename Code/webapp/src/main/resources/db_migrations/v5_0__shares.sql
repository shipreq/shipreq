/*
DROP TABLE    IF EXISTS share;
DROP TABLE    IF EXISTS share_view_log;
DROP SEQUENCE IF EXISTS share_seq;
DROP FUNCTION IF EXISTS share_view_stats_update();
*/

CREATE SEQUENCE share_seq START WITH 1000;

CREATE TABLE share (
  id                    BIGINT      PRIMARY KEY DEFAULT NEXTVAL('share_seq')
  ,project_id           BIGINT      NOT NULL REFERENCES project
  ,url_token            VARCHAR     NOT NULL UNIQUE
  ,created_at           TIMESTAMPTZ NOT NULL DEFAULT NOW()
  ,name                 VARCHAR     NOT NULL
  ,password             VARCHAR     NOT NULL
  ,password_salt        VARCHAR     NOT NULL
  ,password_changed_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
  ,preface              TEXT        NULL
  ,uc_filter            JSON        NOT NULL
  ,view_count           BIGINT      NOT NULL DEFAULT 0 CHECK (view_count >= 0)
  ,last_viewed_at       TIMESTAMPTZ NULL

  ,CONSTRAINT view_stat_constraint CHECK(
     CASE WHEN view_count = 0
          THEN last_viewed_at IS NULL
          ELSE last_viewed_at IS NOT NULL
     END)
);

CREATE TABLE share_view_log (
  time       TIMESTAMPTZ  NOT NULL DEFAULT NOW()
  ,share_id  BIGINT       NOT NULL -- Not an FK, shares can be deleted but we still want to retain the stats
  ,ip        VARCHAR      NULL
);

------------------------------------------------------------------------------------------------------------------------
-- Trigger: Update `share` upon insert to `share_view_log`.

CREATE OR REPLACE FUNCTION share_view_stats_update() RETURNS TRIGGER AS $$
BEGIN

    update share
    set view_count = view_count + 1
        ,last_viewed_at = NOW()
    where id = NEW.share_id;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER share_view_stats_update AFTER INSERT ON share_view_log FOR EACH ROW EXECUTE PROCEDURE share_view_stats_update();
