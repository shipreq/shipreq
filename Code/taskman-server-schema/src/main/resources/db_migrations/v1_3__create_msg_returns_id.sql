-- Needs to be explicitly dropped first because return type differs
DROP FUNCTION IF EXISTS create_msg_v01(int2,json,int2);

CREATE FUNCTION create_msg_v01(IN type INT2, IN data JSON, IN pri INT2)
RETURNS msgq.id%TYPE AS $$
  INSERT INTO msgq(type, data, priority_base, priority, created_at, updated_at, effective_from)
  VALUES($1, $2, $3, $3, now(), now(), now())
  RETURNING id
$$ LANGUAGE SQL
SET search_path FROM CURRENT;
