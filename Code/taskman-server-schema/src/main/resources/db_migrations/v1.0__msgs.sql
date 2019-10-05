/*
DROP FUNCTION IF EXISTS create_msg_v01(int2,jsonb,int2);
DROP TABLE    IF EXISTS msgq;
DROP TABLE    IF EXISTS msg_history_seq;
DROP SEQUENCE IF EXISTS node_seq;
DROP SEQUENCE IF EXISTS msg_history_seq;
DROP SEQUENCE IF EXISTS msgq_seq;
*/

CREATE SEQUENCE node_seq START WITH 1;
CREATE SEQUENCE msgq_seq START WITH 1000;
CREATE TABLE msgq (
  id               BIGINT       PRIMARY KEY DEFAULT NEXTVAL('msgq_seq')
  ,type            INT2         NOT NULL
  ,data            JSONB        NOT NULL
  ,priority        INT2         NOT NULL
  ,priority_base   INT2         NOT NULL
  ,node            INT4         NULL
  ,worker          INT2         NULL CHECK(NOT(node IS NULL AND worker IS NOT NULL)) -- Prevent worker being assigned without a node
  ,failure_count   INT2         NOT NULL DEFAULT 0 CHECK(failure_count >= 0)
  ,created_at      TIMESTAMPTZ  NOT NULL
  ,updated_at      TIMESTAMPTZ  NOT NULL CHECK(updated_at >= created_at)
  ,effective_from  TIMESTAMPTZ  NOT NULL
);


CREATE TABLE msg_history (
  id               BIGINT       PRIMARY KEY
  ,type            INT2         NOT NULL
  ,data            JSONB        NOT NULL
  ,result          "char"       NOT NULL CHECK(result='s' OR result='f') -- Success/Failure
  ,failure_count   INT2         NOT NULL CHECK(failure_count >= 0)
  ,created_at      TIMESTAMPTZ  NOT NULL
  ,archived_at     TIMESTAMPTZ  NOT NULL CHECK(archived_at >= created_at)
);


CREATE FUNCTION create_msg_v01(IN type INT2, IN data JSONB, IN pri INT2)
RETURNS msgq.id%TYPE AS $$
  INSERT INTO msgq(type, data, priority_base, priority, created_at, updated_at, effective_from)
  VALUES($1, $2, $3, $3, now(), now(), now())
  RETURNING id
$$ LANGUAGE SQL
SET search_path FROM CURRENT;


/*
select create_msg_v01(1::int2, '{}'::jsonb, 50::int2);
select create_msg_v01(1::int2, '{}'::jsonb, 60::int2);
select * from msg;
*/
