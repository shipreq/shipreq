/*
DROP TABLE    IF EXISTS usecase;
DROP TABLE    IF EXISTS step;
DROP TABLE    IF EXISTS text;
DROP TABLE    IF EXISTS field_value;
DROP TABLE    IF EXISTS field_key;

DROP TABLE    IF EXISTS relation;
DROP TABLE    IF EXISTS value;
DROP TABLE    IF EXISTS data;

DROP SEQUENCE IF EXISTS value_seq;
DROP SEQUENCE IF EXISTS data_seq;

DROP TABLE    IF EXISTS field_key_type;
DROP TABLE    IF EXISTS relation_type;
DROP TABLE    IF EXISTS data_type;
*/

CREATE TABLE data_type (
    id              INT2        PRIMARY KEY
    ,name           VARCHAR     NOT NULL UNIQUE
);

CREATE TABLE relation_type (
    id              INT2        PRIMARY KEY
    ,name           VARCHAR     NOT NULL UNIQUE
);

CREATE TABLE field_key_type (
    id              INT2        PRIMARY KEY
    ,name           VARCHAR     NOT NULL UNIQUE
);

CREATE SEQUENCE data_seq START WITH 100;
CREATE TABLE data (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('data_seq')
    ,type_id        INT2        NOT NULL REFERENCES data_type
);

CREATE SEQUENCE value_seq START WITH 1000;
CREATE TABLE value (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('value_seq')
    ,data_id        BIGINT      NOT NULL REFERENCES data
    ,rev            INT         NOT NULL CHECK (rev > 0)
    ,updated_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()
    ,UNIQUE(data_id,rev)
);

CREATE TABLE relation (
    from_id         BIGINT      NOT NULL REFERENCES value DEFERRABLE
    ,type_id        INT2        NOT NULL REFERENCES relation_type
    ,index          INT2        NOT NULL DEFAULT(-1)
    ,to_id          BIGINT      NOT NULL REFERENCES value DEFERRABLE
);

CREATE TABLE field_key (
    id              BIGINT      PRIMARY KEY REFERENCES value DEFERRABLE INITIALLY DEFERRED
    ,type_id        INT2        NOT NULL REFERENCES field_key_type
    ,data           TEXT        NULL
);

CREATE TABLE field_value (
    id              BIGINT      PRIMARY KEY REFERENCES value DEFERRABLE INITIALLY DEFERRED
    ,field_key_id   BIGINT      NOT NULL REFERENCES field_key
    ,text           TEXT        NULL
);

CREATE TABLE usecase (
    id              BIGINT      PRIMARY KEY REFERENCES value DEFERRABLE INITIALLY DEFERRED
    ,title          VARCHAR     NOT NULL
    ,number         INT2        NOT NULL
    ,field_list_id  BIGINT      NOT NULL REFERENCES value DEFERRABLE
);

CREATE TABLE step (
    id              BIGINT      PRIMARY KEY REFERENCES value DEFERRABLE INITIALLY DEFERRED
    ,text           TEXT        NOT NULL
);
