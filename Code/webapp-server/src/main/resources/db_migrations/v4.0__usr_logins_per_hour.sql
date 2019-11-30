CREATE TABLE usr_logins_per_hour (
  date  DATE     NOT NULL,
  hour  SMALLINT NOT NULL CHECK(hour >= 0 AND hour <= 23),
  users HLL      NOT NULL,
  total INTEGER  NOT NULL CHECK(total > 0),
  PRIMARY KEY(date, hour)
);

------------------------------------------------------------------------------------------------------------------------

CREATE FUNCTION usr_logins_per_hour_add(
  arg_id   usr.id%TYPE,
  arg_time TIMESTAMPTZ
) RETURNS void AS $$
DECLARE
  time_utc TIMESTAMP;
  tgt_ctid usr_logins_per_hour.ctid%TYPE;
  tgt_date usr_logins_per_hour.date%TYPE;
  tgt_hour usr_logins_per_hour.hour%TYPE;
BEGIN

  SELECT (arg_time at time zone 'UTC')::timestamp
    INTO time_utc;

  SELECT time_utc::date, EXTRACT(hours from time_utc)
    INTO tgt_date, tgt_hour;

  SELECT ctid
    INTO tgt_ctid
    FROM usr_logins_per_hour
   WHERE date = tgt_date AND hour = tgt_hour;

  IF NOT FOUND THEN
    -- Insert new row
    INSERT INTO usr_logins_per_hour(date, hour, users, total)
      VALUES(tgt_date, tgt_hour, hll_add(hll_empty(), hll_hash_bigint(arg_id)), 1);

  ELSE
    -- Update existing row
    UPDATE usr_logins_per_hour
       SET total = total + 1,
           users = hll_add(users, hll_hash_bigint(arg_id))
     WHERE ctid = tgt_ctid;

  END IF;

END;
$$ LANGUAGE plpgsql;

------------------------------------------------------------------------------------------------------------------------
-- Trigger: Update on insert to `usr_login_log`.

CREATE OR REPLACE FUNCTION usr_login_log_on_insert() RETURNS TRIGGER AS $$
BEGIN
  PERFORM usr_add_login          (NEW.usr_id, NEW.time, NEW.ip);
  PERFORM usr_logins_per_hour_add(NEW.usr_id, NEW.time);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;
