CREATE TYPE msg_status_v01 AS ENUM ('unassigned', 'node_assigned', 'working', 'complete', 'aborted');

CREATE FUNCTION query_msg_status_v01(IN msgq.id%TYPE)
RETURNS msg_status_v01 AS $$
DECLARE r msg_status_v01;
BEGIN
  SELECT CASE
    WHEN worker IS NOT NULL THEN 'working'
    WHEN node   IS NOT NULL THEN 'node_assigned'
    ELSE                         'unassigned'
    END INTO r
  FROM msgq WHERE id=$1;

  IF r IS NULL THEN
    SELECT CASE result
      WHEN 's' THEN 'complete'
      WHEN 'f' THEN 'aborted'
      END INTO r
    FROM msg_history WHERE id=$1;
  END IF;

  RETURN r;
END
$$ LANGUAGE plpgsql
  IMMUTABLE
  RETURNS NULL ON NULL INPUT
  SET search_path FROM CURRENT;