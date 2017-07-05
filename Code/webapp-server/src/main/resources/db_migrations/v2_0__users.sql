/*
DROP TABLE IF EXISTS usr;
DROP SEQUENCE IF EXISTS usr_seq;
*/

CREATE SEQUENCE usr_seq START WITH 1;

CREATE TABLE usr (
  id                          BIGINT      PRIMARY KEY DEFAULT NEXTVAL('usr_seq')
  ,username                   VARCHAR     UNIQUE
  ,email                      VARCHAR     NOT NULL UNIQUE
  ,password                   VARCHAR
  ,password_salt              VARCHAR
  ,password_changed_at        TIMESTAMPTZ
  ,confirmation_token         VARCHAR     UNIQUE
  ,confirmation_sent_at       TIMESTAMPTZ
  ,confirmed_at               TIMESTAMPTZ
  ,reset_password_token       VARCHAR     UNIQUE
  ,reset_password_sent_at     TIMESTAMPTZ
  ,reset_password_req_count   INT         NOT NULL DEFAULT(0)
  ,login_count                INT         NOT NULL DEFAULT(0)
  ,last_login_at              TIMESTAMPTZ
  ,last_login_ip              VARCHAR

  ,CONSTRAINT usr_confirmation_invariants CHECK(
    CASE WHEN confirmed_at IS NULL THEN
      -- Before account confirmed...
      username                     IS NULL
      AND password                 IS NULL
      AND password_salt            IS NULL
      AND password_changed_at      IS NULL
      AND confirmation_token       IS NOT NULL
      AND reset_password_req_count = 0
      AND login_count              = 0
    ELSE
      -- After account confirmed...
      username                     IS NOT NULL
      AND password                 IS NOT NULL
      AND password_salt            IS NOT NULL
      AND password_changed_at      IS NOT NULL
      AND confirmation_token       IS NULL
      AND reset_password_req_count >= 0
      AND login_count              >= 0
    END)

  ,CONSTRAINT usr_login_meta CHECK(
    CASE WHEN login_count = 0 THEN
      last_login_at IS NULL and last_login_ip IS NULL
    ELSE
      last_login_at IS NOT NULL and last_login_ip IS NOT NULL
    END)

  ,CONSTRAINT usr_reset_password_meta CHECK(
    CASE WHEN reset_password_req_count = 0 THEN
      reset_password_token IS NULL and reset_password_sent_at IS NULL
    ELSE
      -- (reset_password_token may be cleared after successful reset)
      reset_password_sent_at IS NOT NULL
    END)
);
