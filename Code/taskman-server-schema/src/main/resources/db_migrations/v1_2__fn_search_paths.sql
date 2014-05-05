-- These functions can't find the underlying tables without
--   SET search_path FROM CURRENT
;

CREATE OR REPLACE FUNCTION create_msg_v01(IN type INT2, IN data JSON, IN pri INT2)
RETURNS void AS $$
DECLARE n TIMESTAMPTZ;
BEGIN
  SELECT now() INTO n;
  INSERT INTO msgq(type, data, priority_base, priority, created_at, updated_at, effective_from)
    VALUES($1, $2, $3, $3, n, n, n);
END
$$ LANGUAGE plpgsql
SET search_path FROM CURRENT;


CREATE OR REPLACE FUNCTION cfg_update (IN VARCHAR, IN TEXT) RETURNS VOID AS $$
BEGIN
  BEGIN
    INSERT INTO cfg VALUES ($1,$2);
  EXCEPTION WHEN unique_violation THEN
    UPDATE cfg SET v = $2 WHERE k = $1;
  END;
END;
$$ LANGUAGE plpgsql
SET search_path FROM CURRENT;
