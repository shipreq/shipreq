ALTER TABLE usr

  ALTER COLUMN confirmation_sent_at SET NOT NULL, ------------ change

  DROP CONSTRAINT usr_confirmation_invariants,
  ADD  CONSTRAINT usr_confirmation_invariants CHECK(
    CASE WHEN confirmed_at IS NULL THEN
      -- Before account confirmed...
          username                 IS NULL
      AND password                 IS NULL
      AND password_salt            IS NULL
      AND password_changed_at      IS NULL
      AND confirmation_token       IS NOT NULL
      AND reset_password_req_count = 0
      AND login_count              = 0
    ELSE
      -- After account confirmed...
          username                 IS NOT NULL
      AND password                 IS NOT NULL
      AND password_salt            IS NOT NULL
      AND password_changed_at      IS NOT NULL
      AND confirmation_token       IS NULL
      AND confirmed_at             >= confirmation_sent_at ------------ new
      AND reset_password_req_count >= 0
      AND login_count              >= 0
    END),


  DROP CONSTRAINT usr_login_meta,
  ADD  CONSTRAINT usr_login_meta CHECK(
    CASE WHEN login_count = 0 THEN
          last_login_at IS NULL
      AND last_login_ip IS NULL
    ELSE
      last_login_at IS NOT NULL
      -------------- removed: last_login_ip IS NOT NULL
    END);

CREATE OR REPLACE FUNCTION usr_login_stats_update() RETURNS TRIGGER AS $$
BEGIN

    update usr
    set login_count   = login_count + 1
       ,last_login_at = NEW.time
       ,last_login_ip = NEW.ip -------------- changed to accept NULL here
    where id = NEW.usr_id;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
