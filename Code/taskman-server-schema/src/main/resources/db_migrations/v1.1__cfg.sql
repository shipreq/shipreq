/*
DROP FUNCTION IF EXISTS cfg_update(VARCHAR,TEXT);
DROP TABLE    IF EXISTS cfg;
*/

CREATE TABLE cfg(k VARCHAR PRIMARY KEY, v TEXT NOT NULL);

CREATE FUNCTION cfg_update (IN VARCHAR, IN TEXT) RETURNS VOID AS $$
BEGIN
  BEGIN
    INSERT INTO cfg VALUES ($1,$2);
  EXCEPTION WHEN unique_violation THEN
    UPDATE cfg SET v = $2 WHERE k = $1;
  END;
END;
$$ LANGUAGE plpgsql
SET search_path FROM CURRENT;

/*
select cfg_update('hehe', 'no'); select * from cfg order by 1;
*/