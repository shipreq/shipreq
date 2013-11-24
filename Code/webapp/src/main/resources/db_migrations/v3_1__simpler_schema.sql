/*
DROP TABLE    IF EXISTS uc_field;
DROP TABLE    IF EXISTS text_rev;
DROP TABLE    IF EXISTS text;
DROP TABLE    IF EXISTS field_key;
DROP TABLE    IF EXISTS field_key_type;
DROP TABLE    IF EXISTS usecase_rev CASCADE;
DROP TABLE    IF EXISTS usecase;
DROP SEQUENCE IF EXISTS field_key_seq;
DROP SEQUENCE IF EXISTS usecase_seq;
DROP SEQUENCE IF EXISTS usecase_rev_seq;
DROP SEQUENCE IF EXISTS text_seq;
DROP SEQUENCE IF EXISTS text_rev_seq;
*/

CREATE SEQUENCE field_key_seq   START WITH 10;
CREATE SEQUENCE usecase_seq     START WITH 1;
CREATE SEQUENCE usecase_rev_seq START WITH 100;
CREATE SEQUENCE text_seq        START WITH 1;
CREATE SEQUENCE text_rev_seq    START WITH 1000;

CREATE TABLE field_key_type (
    id               INT2       PRIMARY KEY
    ,name            VARCHAR    NOT NULL UNIQUE
    ,max_text_per_uc INT2       NULL CHECK (max_text_per_uc >= 0)
);

CREATE TABLE field_key (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('field_key_seq')
    ,type_id        INT2        NOT NULL REFERENCES field_key_type
    ,data           TEXT        NULL
);

CREATE TABLE usecase (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('usecase_seq')
);

CREATE TABLE usecase_rev (
    ident_id        BIGINT      NOT NULL REFERENCES usecase
    ,rev            INT         NOT NULL CHECK (rev > 0)
    ,id             BIGINT      PRIMARY KEY DEFAULT NEXTVAL('usecase_rev_seq')
    ,created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()

    ,number         INT2        NOT NULL CHECK (number > 0)
    ,title          VARCHAR     NOT NULL

    ,UNIQUE(ident_id,rev)
);

-- Default latest_rev_id to -1. The usecase_rev insert trigger will update it before the txn ends.
ALTER TABLE usecase ADD COLUMN latest_rev_id BIGINT NOT NULL DEFAULT(-1) REFERENCES usecase_rev INITIALLY DEFERRED;

CREATE TABLE text (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('text_seq')
    ,uc_id          BIGINT      NOT NULL REFERENCES usecase
    ,fk_id          BIGINT      NOT NULL REFERENCES field_key
);

CREATE TABLE text_rev (
    ident_id        BIGINT      NOT NULL REFERENCES text
    ,rev            INT         NOT NULL CHECK (rev > 0)
    ,id             BIGINT      PRIMARY KEY DEFAULT NEXTVAL('text_rev_seq')
    ,created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()

    ,text           TEXT        NOT NULL

    ,UNIQUE(ident_id,rev)
);

CREATE TABLE uc_field (
    uc_rev_id       BIGINT      NOT NULL REFERENCES usecase_rev
    ,label          VARCHAR     NULL
    ,parent_rev_id  BIGINT      NULL REFERENCES text_rev
    ,index          INT2        NOT NULL DEFAULT(-1)
    ,text_rev_id    BIGINT      NOT NULL REFERENCES text_rev

    ,UNIQUE(uc_rev_id,label)

    ,CONSTRAINT uc_field_step_or_text CHECK(
        CASE WHEN label IS NULL THEN
            -- Text field state
            parent_rev_id IS NULL AND INDEX = -1
        ELSE
            -- Step state
            parent_rev_id != text_rev_id AND INDEX >= 0
        END)
);

