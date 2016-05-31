/*
DROP TABLE    IF EXISTS project;
DROP SEQUENCE IF EXISTS project_seq;
*/

CREATE SEQUENCE project_seq     START WITH 1;

CREATE TABLE project (
    id              BIGINT      PRIMARY KEY DEFAULT NEXTVAL('project_seq')
    ,usr_id         BIGINT      NOT NULL REFERENCES usr
    ,created_at     TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
